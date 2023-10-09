
;======================================
;   FILE: comp.main.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


;======================================
;         Main compiler entry
;======================================
ccompile


;======================================
;
;======================================
spl             ;.proc
                jsr splend

                lda nxttoken
                cmp #quote
                bne _spl1               ; no name

                jsr lexget.lget
                jmp _spl2

_spl1           lda top1
                sta top+1
                jsr chkcur._ldtop
                beq _splrtn             ; no program !

                jsr getnext
_spl2           jsr getnext

; <program> _:= <module list> MODULE <module>
;                 | (MODULE) <module>
; <module> _:= (<dcl list>) (<segment list>)

                jsr declare
                jsr segment

                cmp #modid              ; another module ?
                beq _spl2               ; yes

                cmp #eofid
                bne _spl4

                lda #1                  ; save run address
                jsr cprop

                sta INITAD
                stx INITAD+1

    ; insert return, just in case
_rtn            lda #$60                ; RTS
                jsr push1

    ; get qcode size
                sec
                lda qcode
                sbc codebase
                sta codesize
                lda qcode+1
                sbc codebase+1
                sta codesize+1

    ; patch array addresses
                lda arrayptr+1

    ; ORA arrayPtr
                beq _splrtn

_spl3           ldy #1
                lda (arrayptr),y
                sta arg1
                dey
                lda (arrayptr),y
                sta arg0
                jsr getcdoff

                sta (arrayptr),y
                txa
                iny
                sta (arrayptr),y

                clc
                iny
                lda qcode
                adc (arrayptr),y
                sta qcode
                iny
                lda qcode+1
                adc (arrayptr),y
                sta qcode+1

                lda arg0
                sta arrayptr
                lda arg1
                sta arrayptr+1
                bne _spl3

; LDA arrayPtr
; BNE _SPL3

                lda qcode
                cmp MEMTOP
                lda qcode+1
                sbc MEMTOP+1
                bcs enderr._spl5

_splrtn         rts

_spl4           jsr getcdoff            ; no main PROC

                sta INITAD
                stx INITAD+1
                jsr stmtlist

                cmp #eofid
                beq _rtn

;EndErr LDY #0
; STY $2E3                              ; zap run address
enderr          ldy #ender
                jmp dostmt.fierr

_spl5           jmp codeincr.cderr      ; out of qcode space
                ;.endproc


;=====================================
;    Declaration processing
;=====================================
; <dcl list> _:= <dcl list> <dcl> | <dcl>
; <dcl> _:= <simple dcl> | <array dcl> | <def dcl>

cderr           lda #0                  ; reset qcode before err
                tay
                jsr loadcd

_derr           jmp dclerr

_type           lda #+record-(vart-char)-1
                sta type
                jsr makeentry

                lda addr
                ldx addr+1
                ldy #2
                jsr savecd.savstk

                ldy #0
                jsr savecd
                jsr getnext

                cmp #equalid
                bne _derr

                jsr getnext

                cmp #lbrack
                bne _derr

                sec
                lda #0
                sbc codeoff
                sta qcode
                lda #0
                sbc codeoff+1
                sta qcode+1
                jsr getnext

_t1             cmp #char
                bcc cderr

                cmp #define
                bcs cderr

                tax
                clc
                adc #+typet-vart
                sta type
                lda varsize-char,x
                sta afcur
_t2             jsr makeentry

                lda afcur
                jsr codeincr
                jsr getnext

                cmp #comma
                beq _t2

                cmp #rbrack
                bne _t1

                ldy #2
                jsr stkp

                ldx qcode+1
                bne cderr

                lda qcode
                ldy #0
                jsr storprops

                lda #0
                tay
                jsr loadcd
                jsr getnext
declare         jsr dclend

                cmp #char
                bcs _dcl0

_drtn           rts

_dcl0           cmp #typeid
                beq cderr._type

                cmp #record
                bne _dcl1

    ; record dcl.
                lda #0
                jsr getprop

                stx afcur
                ldx nxttoken
                lda #typet-(vart-char)-1
                sta type
                bne _dcl2               ; [unc]

_dcl1           cmp #define
                beq _define
                bcs _drtn

                sta type
                tax
                ldy varsize-char,x
                sty afcur
                ldx nxttoken
                cpx #func
                beq _drtn

                cpx #array
                beq dclerr._arrdcl

_dcl2           cpx #pointer
                bne _sdcl

                ldy #0
                sty afcur
                beq dclerr._arrdcl      ; [unc]

; <simple dcl> _:= <type> <id eq list>
; <id eq list> _:= <id eq list> , <id eq> | <id eq>
; <id eq> _:= <id> (= <constant>)

_sdcl           jsr makeentry

                ldx param
                beq _sdcl0

                jsr params

                ldx param
                bpl _sdcl1
                bmi _sdcl2

_sdcl0          lda nxttoken
                cmp #equalid
                bne _sdcl1

                jsr ideq

                iny
                jsr storprops
                jsr getnext

                bne _sdcl2

_sdcl1          lda afcur
                jsr codeincr
_sdcl2          jsr getnext

                cmp #comma
_sdcl3          beq _sdcl

                jmp declare


; <def dcl> _:= DEFINE <def list>
; <def list> _:= <def list> , <def> | <def>
; <def> _:= <id> = <str const>

_define
                jsr makeentry

                ldy #0
                lda #defid
                sta (props),y
                jsr getnext

                cmp #equalid
                bne dclerr

                lda nxttoken
                cmp #quote
                bne dclerr

                ldy #0
                lda (symtab),y
                clc
                adc #2                  ; real size + EOL
                jsr stincr
                jsr getnext             ; string itself
                jsr getnext             ; dummy string
                jsr getnext

                cmp #comma
_def1           bne _sdcl3
                beq _define

dclerr          ldy #dcler
                jmp splerr

; <array dcl> _:= <type> ARRAY <array list>
; <array list> _:= <array list> , <array> | <array>
; <array> _:= <id> ((<constant>)) (= <constant>)

_arrdcl
                clc
                adc #8
                sta type
                jsr getnext
_arrd1          jsr makeentry

                lda #2
                sta numargs             ; variable space

                ldx param
                bne _arrd5

                lda nxttoken
                ldx afcur
                beq _arrd2              ; no size for pointers

                cmp #lparen
                bne _arrd2

                lda #4
                sta numargs
                lda arrayptr
                ldx arrayptr+1
                ldy #0
                jsr storevar
                jsr getarsz
                jsr getnext

    ; check for small byte array

                ldy #2
                lda #0
                cmp (qcode),y
                iny
                lda #1
                sbc (qcode),y
                bcs _arrd6              ; size <= 256

_ard2a          jsr getnext

                cmp #rparen
                bne dclerr

                lda nxttoken
                cmp #equalid
                beq _ard2c

_arrd3          lda numargs
                bne _ard3a

    ; small array
                ldy #2
                lda (qcode),y
                bne _ard3b

                inc qcode+1
                bne _arrd4          ; [unc]

    ; array var

_ard3a          cmp #4
                bmi _ard3b

    ; large array with memory
                ldx qcode
                stx arrayptr
                ldx qcode+1
                stx arrayptr+1

_ard3b          jsr codeincr
_arrd4          jsr getnext

                cmp #comma
                beq _arrd1

                jmp declare

_arrd5          jsr params

                ldx param
                bpl _arrd3
                bmi _arrd4

_arrd2          cmp #equalid
                bne _arrd3

_ard2c          jsr ideq

                ldy numargs
                beq _ard2b

                ldy #0
                jsr storevar
                jsr getcdoff

_ard2b          ldy #1
                jsr storprops
                jsr getnext

                lda numargs
                jmp _ard3b

_arrd6          ldy #0
                lda (props),y
                cmp #arrayt+intt
                bcs _ard2a

    ; small byte array
                sty numargs
                ora #8
                sta (props),y
                iny
                jsr getcdoff
                jsr storprops

                bne _ard2a              ; [unc]

chkparam
                ldx param
chkp            beq makeentry.derr

                pla
                pla
                jmp getnext


;======================================
;   MakeEntry()
;======================================
makeentry       .proc
                lda nxttoken
                cmp #undec
                beq derr._me1
                bcs _mev                ; var of some kind

                cmp #record
                beq _mev

                cmp #typet
                bcc chkparam

                cmp #eofid
                bcs chkparam

_mev            lda qglobal
                beq chkp

                jsr gnlocal

                cmp #undec
                beq derr._me0
derr            jmp dclerr

_me0            sta nxttoken
_me1            lda #0
                jsr nxtprop

                sec
                lda #vart-char
                adc type
                sta (props),y           ; type
                and #7
                tax
                lda vartype-1,x
                sta op

                iny
                jsr getcdoff
                jsr storprops

                jmp getnext

                .endproc


;======================================
;   IdEq()
;======================================
ideq            .proc
                jsr getnext

                ldx param
                bne params.perr

                jmp mnum
                .endproc


;======================================
;   GetArSz()
;======================================
getarsz         .proc
                jsr ideq

                sty arg0                ; Y should = 0
                sty arg1
                ldy afcur               ; #elements * element size
_gas            clc
                lda arg0
                adc afsize
                sta arg0
                lda arg1
                adc afsize+1
                sta arg1
                dey
                bne _gas

                tax
                lda arg0
                ldy #2
                jmp storevar

                .endproc


;======================================
;   StorProps(low, high, index)
;======================================
storprops       .proc
                sta (props),y
                txa
                iny
                sta (props),y
                rts
                .endproc


;ChkNext PROC ; ChkNext()
; LDA nxtToken
; CMP #undec
; BNE _ChkN
; JSR GNlocal
;:ChkN JMP GetNext


;======================================
;   Params()
;======================================
params          .proc
                ldy #0
                lda (props),y           ; get var type
                pha
                lda #3
                jsr cprop

                cmp #8
                bcs perr

                adc #1
                sta (props),y
                tay

    ; see if time to update gbase
                ldx nxttoken
                cpx #rparen
                bne _p1

    ; see AMPL.SEG
                clc
                adc gbase
                sta gbase
                bcc _p1

                inc gbase+1
_p1             pla
                bmi perr._par1

                cmp #typet+8
                beq perr._par2

perr            jmp segment.argerr

;:Par1 CMP #varT+realT
; BEQ PErr

_par1           cmp #vart+intt
                bcc _par3               ; one byte arg

    ; two byte arg
_par2           and #$1F
                inc argbytes
_par3           and #$9F
                inc argbytes
                sta (props),y
                rts
                .endproc

;--------------------------------------
;--------------------------------------

varsize         .byte 1,1,2,2,2,6


;====================================
;    Statement processing
;====================================

; <stmt list> _:= <stmt list> <stmt> | <stmt>
; <stmt> _:= <assign> |
;            <array assign> |
;            <if> |
;            <for> |
;            <while> |
;            <do> |
;            <call> |
;            <return> |
;            EXIT

stmtlist        .proc
                jsr clrtemps

                sta op
                jsr smtend

                cmp #lbrack
                bne _sl5

    ; machine qcode block
                jsr trashy

_sl1            ldx nxttoken
                cpx #rbrack
                beq _sl4

                jsr mnum

                cpx #0
                beq _sl2

                jsr push2               ; 2 byte number
                bcs _sl3                ; [unc]

_sl2            jsr push1               ; single byte
_sl3            jsr getnext
                bne _sl1                ; [unc]

_sl4            jsr getnext

                jmp recret.nxtstmt

_sl5            ldx nxttoken
                cmp #vart+chart
                bcc _sl6

                cmp #funct
                bcc assign

    ; routine reference
                cpx #lparen
                beq call

                jsr procref
                bne assign.ass0         ; [unc]

_sl6            cmp #undec
                bne _sl7

                jsr getalias

                bne _sl5                ; [unc]

_sl7            cmp #typet
                bne _sl8

                jsr etype

                jmp arassign.arass1

_sl8            cmp #typet+8
                bne _sl9

                jsr etypea
                jmp arassign.arass1

_sl9            ldx #<stmtlst
                ldy #>stmtlst
                jmp lookup

                .endproc


; <call> _:= <proc var>((<arglist>))
; <proc var> _:= <id>
; <arglist> _:= <arglist> , <exp> | <exp>

call            .proc
                jsr pf
                jsr popst

                jmp recret.nxtstmt

                .endproc


; <assign> _:= <id> = <exp>

assign          .proc
                cmp #arrayt
                bcs arassign

ass0            jsr pushnext

ass1            eor #equalid
                bne asserr

                jsr pushop              ; push 0 on op stack
                jsr getnext

                sta op
                cmp #equalid
                bne _ass2

                lda #0
                sta op
                jsr copyst

    ; check for temps
                iny
                and #$F8
                cmp #arrayt+8
                bne _a1a

                ldy #3
                lda (stack),y
_a1a            and #$20
                beq _a1b

                iny
                lda (stack),y
                tax
    ; incr temps
                cpx #args               ;
                bcc _a1b                ; are these 4 instr.

                cpx #args+16            ; needed?
                bcs _a1b                ;

                inc temps-args,x
_a1b            jsr getnext
_ass2           jsr exp.exp1
                jsr cgassign
                jmp stmtlist

asserr          ldy #asser
                jmp splerr

                .endproc


; <array assign> _:= <id> ( <exp> ) = <exp>

arassign        .proc
                jsr arrref

arass1          ldy #0
                lda (stack),y
                bpl _ara1               ; record element

                cmp #vart
                bcc assign.asserr       ; const

_ara1           jsr getnext
                bne assign.ass1         ; [unc]

                .endproc


; <if> _:= IF <exp> THEN <stmt list
;          (ELSE <stmt list>) FI

ifstmt          .proc
                lda #7
                jsr getframe

                ldy #5
                lda #0
                sta (frame),y

_if             jsr condexp

                cmp #then
                bne thnerr

    ; save current Y
                lda cury
                ldy #6
                sta (frame),y
                jsr recret.nxtstmt

    ; restore Y
                tax
                ldy #6
                lda (frame),y
                sta cury
                txa

                cmp #elseif
                bne _else

                ldy #4
                jsr frameadr.fadr1

                ldy #4
                jsr framecd.fcd1

                ldx arg4
                ldy arg5
                jsr pushjmp
                jsr frameadr
                jsr filljmp.fjmp1

                jmp _if

_else           cmp #else
                bne _fi
                jsr frameadr
                jsr framecd

                ldy #0                  ; flag as end of list
                jsr pushjmp
                jsr filljmp.fjmp1
                jsr recret.nxtstmt

_fi             ldy #fier
                cmp #fi
                bne dostmt.fierr

                ldy #4
                jsr frameadr.fadr1
                beq if1                 ; if no ELSEIF

                jsr filljmp.fjmp1
if1             jsr trashy
                jsr frameadr

                beq recret              ; in case of DO loop
                jsr filljmp.fjmp1

                .endproc


;    RecRet() pops stack and returns
;    -------------------------------
recret          .proc
                jsr freeframe
nxtstmt         jsr getnext

                jmp stmtlist

                .endproc


;    pops stack
;    ----------
freeframe       .proc
                ldy #0
                lda (frame),y
                tax
                iny
                lda (frame),y
                stx frame
                sta frame+1
                rts
                .endproc

thnerr          ldy #thener
sterr           jmp splerr


; <do> _:= DO <stmt list> (UNTIL <exp>) OD

dostmt          .proc
                jsr doinit

                lda #0
                ldy #3
                sta (frame),y
                bne whstmt.wh1          ; [unc]

fierr           cmp #undec
                bne sterr

                jmp mnum._varerr

                .endproc

; <while> _:= WHILE <exp> <do>

whstmt          .proc
                jsr doinit
                jsr condexp

                ldy #doer
                cmp #do
                bne sterr

wh1             jsr recret.nxtstmt

wh2             cmp #untilid
                bne _wh3

                jsr condexp
                bne _wh4                ; [unc]

_wh3            ldy #4
                jsr frameadr.fadr1
                jsr pushjmp

                lda token
_wh4            ldy #oder
                cmp #od
                bne dostmt.fierr

                ldy #6
                jsr frameadr.fadr1

                stx whaddr
                sty whaddr+1
                jmp ifstmt.if1

                .endproc


;======================================
;   EXITstmt()
;======================================
exitstmt        .proc
                ldy #exiter
                ldx whaddr+1
                beq sterr

                ldy #2                  ; get pointer to EXIT list
                lda (whaddr),y
                tax
                iny
                lda (whaddr),y
                pha

                lda qcode+1             ; link in JMP for EXIT
                sta (whaddr),y
                lda qcode
                dey
                sta (whaddr),y

                pla
                tay
                jsr pushjmp

                jmp recret.nxtstmt

                .endproc




;======================================
; <for> _:= FOR <id> = <exp> TO <exp>
;             (STEP <exp>) <do>
;======================================
forerr          ldy #forer
                bne dostmt.fierr


;======================================
;
;======================================
forstmt         .proc
                lda #23
                jsr getframe
                jsr whadr

    ; make sure simple var for index
                jsr getnext

                cmp #vart+chart
                bcc forerr

                cmp #arrayt
                bcc _fs1

                cmp #arrayt+realt
                bcs forerr

                lda #vart+cardt
                sta token

    ; get initial value
_fs1            ldy #8
                sta (frame),y
                iny
                lda addr
                ldx addr+1
                jsr framecd.fcd2
                jsr pushnext

                cmp #equalid
                bne forerr

                jsr getexp
                jsr cgassign

    ; set default STEP size
                lda token
                cmp #to
_f1             bne forerr

                lda #0
                ldx #9
                ldy #12
_fz             sta (frame),y
                iny
                dex
                bne _fz

                ldy #8
                jsr fstk

                lda token
                and #7
                ora #constt
                ldx #1
                ldy #11
                jsr framecd.fcd2

    ; get ending value
                lda #16
                jsr forexp

    ; get step value
                lda token
                cmp #step
                bne _fnostep

                lda #11
                jsr forexp

                lda token
_fnostep        cmp #do
                bne _f1

    ; generate end test
                jsr getcdoff

                ldy #4
                jsr framecd.fcd2
                jsr trashy
                jsr genops.gops

                ldy #16
                lda (frame),y
                cmp #vart
                bcs _f3                 ; temp variable

    ; constant
                iny
                lda (frame),y
                tax
                lda #$A9
                jsr push2               ; LDA #low

                lda #$C1                ; CMP
                jsr op2l

                lda arg3
                beq _fbody

                ldy #18
                lda (frame),y
                tax
_f2             lda #$A9
                jsr push2               ; LDA #high

                jmp _f4

_f3             ldy #17
                sty arg0
                lda #$AD                ; LDA addr16
                jsr forexp.fexp2

                lda #$C1                ; CMP
                jsr op2l

                lda arg3
                beq _fbody

                ldx #0
                ldy #16
                lda (frame),y
                cmp #vart+intt
                bcc _f2                 ; only byte var

                lda #$AD                ; LDA addr16
                jsr forexp.fexp2

_f4             lda #$E1                ; SBC
                jsr op2h

_fbody          lda arg3
                ror a                   ; get type
                lda #$B0                ; BCS, CARD
                bcc _f5

                lda #$10                ; BPL, INT
_f5             jsr push1

                ldy #21
                jsr framecd.fcd1
                jsr push1
                jsr popst

                jsr framecd

                ldy #0
                jsr pushjmp

    ; save space for vars
                ldy #16
                jsr fmem

                ldy #11
                jsr fmem

    ; handle symtab
                ldy #11
                lda (frame),y
                cmp #vart
                bcc _f6

                lda symtab
                ldx symtab+1
                iny
                jsr framecd.fcd2

                lda #0
                tay
                sta (symtab),y
                lda #4
                jsr stincr

    ; patch branch
_f6             ldy #21
                jsr frameadr.fadr1
                jsr comprel

    ; handle stmt list
                jsr recret.nxtstmt

    ; handle incr
                pha                     ; save token
                ldy #8
                jsr fstk
                jsr copyst

                ldy #11
                jsr fstk

                lda #plusid
                jsr cgplus

                lda arg5
                beq _f7

                jsr chasseq.chstkeq     ; see if INC

_f7             pha
                jsr cgassign

                pla
                beq _f8                 ; not INC
                                        ; see if we can branch to top of loop
                pla
                cmp #untilid
                beq _f9                 ; can't go to top of loop

                pha
                clc
                ldy #4
                lda (frame),y
                sbc stkbase-9           ; see CGPlus
                bpl _f8

                tax
                iny
                lda (frame),y
                sbc stkbase-8
                cmp #$FF
                bne _f8                 ; yes, branch to top

                lda stkbase-9
                sta arg0
                lda stkbase-8
                sta arg1
                ldy #0
                txa
                sta (arg0),y
_f8             pla
_f9             sta token
                jmp whstmt.wh2

                .endproc


;======================================
;
;======================================
forexp          .proc
                pha
                jsr getexp
                jsr genops.gops

                pla
                sta arg0
                jsr _fe0

                jmp popst


_fe0            lda arg1
                cmp #vart               ; see if const
                bcs _fe1

    ; constant
                ldy #11
                lda (frame),y
                ldy arg0
                sta (frame),y
                ldy #2
                jsr loadi

                ldy arg0
                iny
                jmp framecd.fcd2

_fe1            ldy #8
                lda (frame),y
                and #7
                cmp #intt
                bmi _fe2

                lda arg1
                and #7
_fe2            ora #vart
                ldy #1
                sta (symtab),y
                sta arg2
                ldy arg0
                sta (frame),y
                inc arg0

                lda arg1
                bit arrmode             ; array?
                bne _fevar              ; yes

                bit tempmode            ; temp?
                bne fexp2._ftemp        ; yes

    ; var of some kind
_fevar          jsr load2l
                jsr fexp1

                lda arg2
                cmp #vart+intt          ; see if byte
                bcc fexp2._fe3

                jsr load2h

fexp1           lda #$8D                ; STA data16
fexp2           pha
                ldy arg0
                jsr frameadr.fadr1

                ldy arg0
                jsr framecd.fcd1

                iny
                sty arg0
                pla
                ldx arg4
                ldy arg5
                jmp push3

_fe3            rts

_ftemp          ldy #4
                jsr loadi

                ldy #1
                jsr addcdsp

                ldy #3
                lda #0
                jsr _ft1

                lda arg2
                cmp #vart+intt          ; see if byte
                bcc _fe3

                ldy #5
                lda #1
_ft1            jsr loadcd

                jmp fexp1

                .endproc


;======================================
;
;======================================
fmem            lda (frame),y
                cmp #vart
                bcc forexp.fexp2._fe3   ; const

                sty arg2
                jsr getcdoff            ; save address for step

                ldy #2
                sta (symtab),y
                txa
                iny
                sta (symtab),y
                ldy arg2
                jsr _fm1

                ldy arg2
                lda (frame),y
                cmp #vart+intt
                bcc forexp.fexp2._fe3   ; byte only

                iny
                iny
_fm1            iny
                jsr frameadr.fadr1
                jsr filljmp.fjmp1

                lda #1
                jmp codeincr


;======================================
;
;======================================
fstk            lda (frame),y
                sta token
                iny
                jsr frameadr.fadr1

                stx addr
                sty addr+1
                jmp pushst


;======================================
;
;======================================
doinit          .proc
                lda #8
                jsr getframe
                jsr whadr
                jsr getcdoff

                ldy #4
                jsr framecd.fcd2

                jmp trashy

                .endproc


; <return> _:= RETURN ((<exp>))

retstmt         .proc
                lda #0
                jsr cprop

                and #7
                beq _r1

                ora #tempt
                tay
                lda #args
                jsr storst

                ldx nxttoken
                cpx #lparen
                bne reterr

                jsr getnext
                jsr getexp

                cmp #rparen
                bne reterr

                jsr cgassign

_r1             lda #$60
                jsr push1

                jmp recret.nxtstmt


;======================================
;
;======================================
reterr          ldy #reter
                jmp splerr

                .endproc

;======================================
;   CondExp()
;======================================
condexp         .proc
                jsr getexp

                pha
                ldy #0
                lda (stack),y
                cmp #condt
                beq _cexp1

    ; not boolean
                jsr zerost

                lda #neid
                jsr codegen

_cexp1          pla                     ; token value
                pha
                cmp #od
                bne _cexp2

    ; until <exp> od
                ldy #1
                jsr stkaddr
                beq _ce1a               ; no JMPs

    ; JMP to JMP to top of loop
    ; yek!, should be improved
                jsr filljmp             ; fill in jmps

_ce1a           ldy #4
                jsr frameadr.fadr1
                bne _cexp3              ; [unc]

_cexp2          jsr framecd

                ldy #1
                jsr stkaddr
_cexp3          jsr pushjmp

    ; fill in branch addresses
                ldy #4
                jsr fillbr
                jsr popst

                pla                     ; get token value
                rts
                .endproc


;======================================
;
;======================================
whadr           .proc
                lda whaddr
                ldx whaddr+1
                ldy #6
                jsr framecd.fcd2

                lda frame
                sta whaddr
                lda frame+1
                sta whaddr+1
                rts
                .endproc


;======================================
;   FillBr(,,offset)
;======================================
fillbr          .proc
                jsr setrel
_fb1            jsr savrel
                jsr comprel
                jsr loadn
                bne _fb1
                rts
                .endproc


;======================================
;   FrameAdr()
;======================================
frameadr        .proc
                ldy #2
fadr1           lda (frame),y
                tax
                iny
                lda (frame),y
                tay
fadr2           stx arg4
                sty arg5
                rts
                .endproc


;======================================
;   FrameCd()
;======================================
framecd         .proc
                ldy #2
fcd1            lda qcode
                ldx qcode+1
fcd2            sta (frame),y
                iny
                txa
                sta (frame),y
                rts
                .endproc


;======================================
;   FillJmp(,addr)
;======================================
filljmp         .proc
                jsr frameadr.fadr2
fjmp1           jsr saven
                jsr getcdoff
                jsr save4
                jsr loadn
                bne fjmp1
                rts
                .endproc


;======================================
;   Save4(value)
;======================================
save4           .proc
                sta (arg4),y
                iny
                txa
                sta (arg4),y
                rts
                .endproc


;======================================
;   SaveN()
;======================================
saven           .proc
                ldy #1
                lda (arg4),y
                sta arg0
                iny
                lda (arg4),y
                sta arg1
                dey
                rts
                .endproc


;======================================
;   LoadN()
;======================================
loadn           .proc
                lda arg0
                ldx arg1
                beq ln1._ln2            ; is this needed anymore?

ln1             sta arg4
                stx arg5
_ln2            rts
                .endproc


;======================================
;   PushJMP(, addr)
;======================================
pushjmp         .proc
                lda #$4C                ; JMP addr16
                jmp push3

                .endproc


;======================================
;   GetFrame(size)
;======================================
getframe        .proc
                sta arg0
                ldy frame
                ldx frame+1
                sec
                tya
                sbc arg0
                sta frame
                txa
                sbc #0
                sta frame+1
                lda frame
                cmp sparem
                lda frame+1
                sbc sparem+1
                bcc nsterr

                tya
                ldy #0
                jmp framecd.fcd2

nsterr          ldy #nster
                jmp splerr

                .endproc


;======================================
;   SetRel(,,offset)
;======================================
setrel          .proc
                jsr loadi

                jmp loadn.ln1

                .endproc


;======================================
;
;======================================
savrel          .proc
                ldy #0
                ldx #0
                lda (arg4),y
                beq _sr2
                bpl _sr1

                dex                     ; sign extend
_sr1            clc
                adc arg4
                sta arg0
                txa
                adc arg5
_sr2            sta arg1
                rts
                .endproc


;======================================
;
;======================================
comprel         .proc
                lda qcode
crel1           clc                     ; extra -1 for offset byte
crel2           sbc arg4
                ldy #0
                sta (arg4),y
                rts
                .endproc


;======================================
;
;======================================
clrtemps        .proc
                lda #0
                ldx #16
_sl0            sta temps-1,x
                dex
                bne _sl0                ; free temps

                rts
                .endproc

;--------------------------------------
;--------------------------------------

stmtlst         .word smtend            ; not found
                .byte 20                ; #entries*3 - 1
                .word ifstmt
                .byte ifid
                .word forstmt
                .byte forid
                .word whstmt
                .byte whileid
                .word retstmt
                .byte retid
                .word dostmt
                .byte do
                .word exitstmt
                .byte exitid


;=====================================
;    Expression processing
;=====================================

; <exp> _:= <exp> XOR <equiv> | <equiv>
; <equiv> _:= <equiv> OR <logical prod> | <logical prod>
; <logical prod> _:= <logical prod> AND <relation> | <relation>
; <relation> _:= <relation> <rel op> <add exp> | <add exp>
; <add exp> _:= <add exp> <add op> <mult exp> | <mult exp>
; <mult exp> _:= <mult exp> <mult op> <factor> | <factor>
; <factor> _:= <unary op> <primary> | <primary>
; <primary> _:= <constant> | <var> | ( <exp> )

; <rel op> _:= # | = | <high  | < | high  | <= | high =
; <add op> _:= + | -
; <mult op> _:= * | / | LSH | RSH | REM
; <unary op> _:= @ | -

;======================================
;   GetExp()
;======================================
getexp          .proc
        .if ramzap
                inc cgopscd
        .else
                nop
                nop
                nop
        .endif
                jsr getnext
                .endproc


;======================================
;
;======================================

exp             .proc
                lda #0
                jsr pushop

                lda token               ; always non-zero
                sta op
exp1            jsr expend

                cmp #scolon
                bcc eerr._expop

                cmp #rparen
                bne _exp2

                ldx op
                bne eerr

                jsr rollops

                cmp #lparen
                beq eerr._exp7

                lda token
                rts


;======================================
;
;======================================
_exp2           cmp #lparen
                bne eerr._exp3

                ldx op
                bne eerr._exp6

eerr            jmp experr


;======================================
;
;======================================
_exp3           ldx op
                beq _exp8

                ldx #0
                stx op
                cmp #quote
                beq _expstr

_exp3a          ldx nxttoken
                cmp #record
                beq _exp3d              ; record

                cmp #typet
                bcc eerr
                bne _exp3c

_exp3b          jsr etype
                bne _exp7               ; [unc]

_exp3c          cmp #typet+8
                bcc _exp3d              ; field
                bne _exp3e

                jsr etypea

                jmp _exp7


    ; record name or field
_exp3d          cpx #period
                bne _exp3b

    ; will generate error if falls through

_exp3e          cmp #constt
                bcc eerr                ; missing operand?

                cmp #arrayt
                bcs _exp4               ; array or func

                cmp #undec
                beq _expund

                cmp #constt+realt
                beq eerr                ; real constant

_expvar
                jsr pushnext
                bne exp1                ; [unc]

_exp4           cmp #funct
                bcs _expproc

_exparr
                jsr arrref

                jmp _exp7

_expop          ldx op
                beq _exp5

                cmp #atid
                beq _exp5

                cmp #minusid
                bne eerr

                lda #uminusid
                sta token
_exp5           tax
                lda prec-1,x
                sta op
                jsr rollops
                jsr pushop

                lda token
_exp6           jsr pushop
_exp7           jsr getnext

                jmp exp1

_exp8           jsr rollops

                cmp #lparen
                beq parenerr

_exp9           lda token
                rts

_expund
                jsr getalias
                bne _exp3a              ; [unc]

_expproc
                bne parenerr._expfunc

_expp1          jsr procref
                bne _expvar             ; [unc]

_expstr
                lda #$4C                ; JMP around string
                jsr push1
                jsr getcdoff

                adc #3                  ; includes size byte
                bcc _es1

                inx
                clc
_es1            ldy #0
                adc (addr),y            ; size
                bcc _es2

                inx
_es2            jsr push2

                jsr copystr

                ldy #constt+strt
                jsr storst

                dec choff
                jsr getnext
                bne _exp7               ; [unc]

parenerr
                ldy #parer
                jmp splerr


    ; qcode to handle function ref
_expfunc
                cmp #funct+8
                beq eerr._expp1

                cpx #lparen
                bne eerr._expp1

                lda #17
                jsr getframe

                ldy #16
                lda op
                sta (frame),y

    ; save temps
                sty arg0
                lda #args+15
                sta arg1
_ef1            dec arg0
                ldy arg0
                lda temps,y
                sta (frame),y
                beq _ef2

                lda #$A5                ; LDA addr
                ldx arg1
                ldy #$48                ; PHA
                jsr push3

                ldy arg0
_ef2            dec arg1
                cpy #2
                bne _ef1

                lda temps
                bne experr              ; nested functions
                jsr clrtemps
                jsr pf                  ; call the function

    ; restore temps
                ldy #1
                sty temps               ; flag result reg.
                sty arg0
                lda #args+2
                sta arg1
_ef3            inc arg0
                ldy arg0
                lda (frame),y
                sta temps,y
                beq _ef4

                lda #$68                ; PLA
                ldx #$85                ; STA addr
                ldy arg1
                jsr push3

                ldy arg0
_ef4            inc arg1
                cpy #15
                bne _ef3

                iny
                lda (frame),y
                sta op

                jsr freeframe

    ; set result type
                ldy #0
                lda (stack),y
                and #7
                ora #tempt
                ldx #args
                jsr savecd.savstk

                jmp eerr._exp7

                .endproc


;:ExpReal LDX vars
; LDY vars+1
; JSR FST0R
; LDA varsOff
; LDX varsOff+1
; JSR SST1
; LDA #6
; JSR VarIncr
; JMP _Exp7

experr          ldy #exper
eerr1           jmp splerr


;======================================
;   PopOp()
;======================================
popop           .proc
                dec stackptr
                bmi experr              ; this should never happen

                ldy stackptr
                lda opstack,y
                rts
                .endproc


;======================================
;
;======================================
zerost          .proc
                lda #0
                tax
                ldy #constt+bytet
                .endproc


;======================================
;   StorST(addr16,token)
;======================================
storst          .proc
                sty token
sst1            sta addr
                stx addr+1
                .endproc


;======================================
;   PushST()
;======================================
pushst          .proc
                sec
                lda stack
                sbc #7
                sta stack
                bcc experr

                ldy #0
                lda token
                sta (stack),y
                iny
                lda addr
                ldx addr+1
                jmp savecd.savstk

                .endproc


;======================================
;
;======================================
etypep          .proc
                jsr pushst
                jsr getnext
                jsr getnext

                ldy #0
                sta (stack),y
                tax
                and #$F8
                ldy #typer
                cmp #typet
                bne eerr1

                txa
                and #7
                beq eerr1

                sta token

    ; get offset
                lda #1
                jmp getprop

                .endproc


;======================================
;
;======================================
etype           .proc
                cpx #period
                beq _e1

    ; record addr, size or field offset
    ; A reg must be nonzero before call
                jmp arrref.arrconst

_e1             jsr etypep              ; set type

    ; get var address
                ldy #1
                jsr stkps

                tya
                ldy #2
                sta (stack),y
                dey
                txa
                sta (stack),y
                rts
                .endproc


;======================================
;
;======================================
etypea          .proc
                cpx #period
                beq _e1

                jmp arrref.arrvar

_e1             jsr etypep              ; set type

                tay
                lda token
                pha
                tya
                ldy #constt+bytet
                jsr storst

                lda #arrayt+bytet
                ldy #7
                jsr arrref.arra0

                ldy #0
                pla
                ora #$B0                ; temp array
                sta (stack),y
                rts
                .endproc


;======================================
;
;======================================
procref         .proc
                ldy #funct+cardt        ; make sure CARD
                cmp #funct+8
                bcc stconst._pr1

stconst         jsr getargs             ; A#0, no arg types

                ldy #constt+cardt       ; sys proc
_pr1            sty token
                rts
                .endproc


;======================================
;   CopyST()
;======================================
copyst          .proc
                lda stack
                ldx stack+1
                jsr loadn.ln1
                jsr pushst

                ldy #6
_cst1           lda (arg4),y
                sta (stack),y
                dey
                bpl _cst1

                rts
                .endproc


;======================================
;   RollOps()
;======================================
rollops         .proc
                jsr popop
                beq _rops1

                cmp #lparen
                beq _rops1

                tax
                ldy prec-1,x
                cpy op
                bcc _rops1              ; prec < op
                                        ; check for simple add
                ldy op                  ; see if end of exp
                bne _ro2

                ldy stack
                cpy #<(stkbase-21)
                bne _ro2

                cpx #plusid
                beq _ro0

                cpx #rshid
                beq _ro0

                cpx #lshid
                bne _ro2

_ro0            jsr popop               ; see if last op
                bne _ro1

    ; check for increament
    ; We know at least this is an
    ; assignment or an array subscript.
    ; If ChStkEq in CGPlus is true then
    ; this is an assignment.
                txa
                cmp #plusid
                bne _rosh

                jsr cgplus

                lda #0
                rts

_rosh           jsr cgsh

                lda #0
                rts

_ro1            jsr pushop

                txa
_ro2            jsr codegen

                jmp rollops

_rops1          rts
                .endproc


;======================================
;   PopST()
;======================================
popst           .proc
                clc
                lda stack
                adc #7
                sta stack
                rts
                .endproc


;======================================
;   PushOp(op)
;======================================
pushop          .proc
                ldy stackptr
                inc stackptr
                sta opstack,y
                rts
                .endproc


;======================================
;   PushNext()
;======================================
pushnext        .proc
                jsr pushst

                jmp getnext

                .endproc

;--------------------------------------
;--------------------------------------

prec            .byte 5,5,6,6,2,3,4,4,4,4,4,4
                .byte 6,1,6,6,7,7


;======================================
;    Code generation
;======================================

;======================================
;   GenOps(op)
;======================================
genops          .proc
                sta arg0
gops            ldy #0
                lda (stack),y
                sta arg1
                and #7
                tax
                lda vartype-1,x
                sta arg3
                asl a
                asl a
                sta arg5
                ldy #7
                lda (stack),y
                sta arg2
                and #7
                tax
                lda vartype-1,x
                sta arg4
                ora arg5
                tax
                lda outtype,x
                sta arg6                ; high bit on for card.
                and #7                  ; get rid of flag bits
                tax
                ora #tempt
                sta arg7
                lda vartype-1,x
                sta arg5                ; output type

                jmp push0

                .endproc


;======================================
;
;======================================
cgsh            .proc
                jsr chasseq
                beq codegen.cg1

_cgs1           lda stkbase-20
                beq _cgs5               ; no shift!

                cmp #5
                bcs codegen.cg1         ; too large a shift

    ; whew!, we can now shift it
                ldy arg0
                cpy #rshid
                beq _cgs2               ; right shift

                lda #$06                ; ASL
                jsr loadx.op1l

                lda arg4
                beq _cgs4

                lda #$26                ; ROL
                jsr op1h

                jmp _cgs4

_cgs2           lda #$46                ; LSR
                ldx arg4
                beq _cgs3

                jsr op1h

                lda #$66                ; ROR
_cgs3           jsr loadx.op1l

_cgs4           dec stkbase-20
                bne _cgs1

_cgs5           jmp popst

                .endproc


;======================================
;   CodeGen(op)
;======================================
codegen         .proc
                jsr genops
cg1             jsr cgend

                lda arg0
                asl a
                clc
                adc arg0
                tay
                lda cgops-3,y
                sta arg8
                lda cgops-2,y
                ldx cgops-1,y
                jmp jsrind              ; jmp to qcode for op

                .endproc


;======================================
;   CGPlus()
;======================================
cgplus          .proc
                jsr chasseq
                beq codegen.cg1

                lda stkbase-20
                cmp #1                  ; see if const = 1
                bne codegen.cg1         ; no

    ; whew!, we can now increament it
                lda #$E6                ; INC
                jsr loadx.op1l

                lda arg4
                beq _cga2               ; byte var

                lda #$D0                ; BNE
                jsr push1

                ldy #12
                jsr savecd

                lda #0
                jsr push1               ; offset

                lda #$E6                ; INC
                jsr op1h

                ldy #13
                jsr fillbr

_cga2           jmp popst

                .endproc

cgexperr
                jmp experr


;======================================
;   CGAssign()
;======================================
cgassign        .proc
                lda #0
                jsr genops
                jsr cgend
                jsr chasseq.chstkeq     ; see if INC
                bne cga1                ; yes, just return

                lda arg1
                bpl _ass1               ; cond. exp. of record

                bit tempmode            ; rhs temp?
                bne cga1._cgat          ; yes

                cmp #vart               ; const?
                bcs _cgav               ; no

    ; rhs constant
                lda arg2
                cmp #arrayt             ; simple var
                bcs _cgav

                ; if STY addr16,X was supported
                ; BCC _CGC0 ; yes
                ; BIT tempMode ; lhs temp?
                ; BNE _CGAV ; yes, large array

_cgc0           ldx arg4
                beq _cgc1               ; byte

                ldy #2
                lda (stack),y
                cmp #2
                bcs _cgavi

                sta arg12
                jsr loady

                lda #$84                ; STY
                jsr op1h

_cgc1           ldy #1
                lda (stack),y
                cmp #2
                bcs _cgavb

                sta arg12
                jsr loady

                lda #$84                ; STY
                bne _avb2               ; [unc]

_ass1           cmp #typet
                bcc cgexperr            ; cond. exp.

    ; rhs var
_cgav           ldx arg4                ; lhs type=byte?
                beq _cgavb              ; yes

                ; CPX #3                ; lhs = int?
                ; BNE _CGAVI            ; yes
                ; lhs type = real
                ; JMP AssErr
_cgavi          jsr load2h

_avi1           lda #$81                ; STA
                jsr op1h
_cgavb          jsr load2l

_avb1           lda #$81                ; STA
_avb2           jsr loadx.op1l
cga1            jsr popst

                jmp popst

_cgat           and #$10                ; rhs array?
                bne cgassign._cgav      ; yes

    ; special case for arg0
                ldy #1
                lda (stack),y
                cmp #args
                beq cgassign._cgav      ; function return value

                lda arg4                ; lhs byte?
                beq _cgatb              ; yes

    ; int/card temp
                lda arg2
                and #$10                ; array?
                bne _cgata              ; yes

                lda arg3
                bne _cgat1

    ; rhs type is BYTE
                jsr load2h              ; generate LDA #0 instr.

                ldy #5
                jsr savecd

_cgat1          ldy #4
                jsr loadi

                ldy #8
                lda arg2
                and #$60                ; lhs proc or temp(argument)?
                bne _cgat3              ; yes

                jsr stkpz
                beq _cgat4              ; page zero

                sty arg13
                ldy #1
                lda #$8D                ; STA addr16
                jsr insrt3.i30          ; insert STA data16

                lda #1
_cgat2          ldy #5
                jsr loadcd

                lda #$81                ; STA
                jsr op1h

                jmp cga1

_cgat3          and #$40                ; proc?
                bne cgassign._cgavi     ; yes, punt(not the best qcode)

                jsr stkaddr             ; temp (proc argument)

_cgat4          ldy #1
                txa
                sta (arg14),y
                lda #0
                beq _cgat2

    ; temp array
_cgata          lda arg3
                beq cgassign._cgavi

                ldy #5
                jsr ldcdz
                bne cgassign._avi1      ; [unc]

_cgatb          ldy #3
                jsr loadcd

                lda arg3                ; see if rhs BYTE or CHAR
                beq cgassign._avb1      ; yes

                jsr trashy              ; in case INT ARRAY in rhs

    ; oh if I only had more qcode space!
    ; could handle Y, see _OpA in CGU
                bne cgassign._avb1      ; [unc]

                .endproc


;======================================
;   chasseq (mode)
;======================================
chasseq         .proc
                jsr genops

                lda arg1
                cmp #vart               ; see if const
                bcs chstkeq._cse2       ; no

                lda stkbase-19          ; see if byte
                bne chstkeq._cse2       ; no
                                        ; see if left and right hand are =


;======================================
;   JMP ChStkEq
;======================================
chstkeq         ldx #2
                lda stkbase-7
                and #$F8
                cmp #$B0                ; large array?
                beq _cse2               ; yes, can't INC or shift

                cmp #arrayt+8           ; small array?
                bne _cse1               ; no

                ldx #5
_cse1           lda stkbase-14,x
                cmp stkbase-7,x
                bne _cse2

                dex
                bpl _cse1

                rts

_cse2           lda #0
                rts
                .endproc


;======================================
;   CGAdd()
;======================================
cgadd           .proc
                lda arg1
                bpl _a3

                ora arg2
                cmp #vart               ; see if both ops consts
                bcs _a3                 ; not const

                lda arg7
                and #7
                ora #constt
                sta arg7
                ldy #8
                ldx arg8
                bne _a1                 ; subtract constants

    ; add constants
                clc
                lda (stack),y
                ldy #1
                adc (stack),y
                sta arg9
                ldy #9
                lda (stack),y
                ldy #2
                adc (stack),y
                jmp _a2

_a1             sec
                lda (stack),y
                ldy #1
                sbc (stack),y
                sta arg9
                ldy #9
                lda (stack),y
                ldy #2
                sbc (stack),y

_a2             tax
                beq cgadd4

                lda arg5
                bne cgadd4

                lda #constt+intt
                bne cgadd5              ; [unc]

    ; normal add or sub.
_a3             ldx arg8
                lda cgopscd,x
                jsr push1

cgadd1          jsr gettemps
                jsr loadx.load1l
                jsr opcd1
                jsr op2l
                jsr stempl

                lda arg5
                beq cgadd3

                jsr load1h
                jsr opcd1
                jsr op2h

cgadd2          jsr stemph

cgadd3          ldx #0
cgadd4          lda arg7
cgadd5          ldy #7
                sta (stack),y
                iny
                lda arg9
                jsr savecd.savstk

cgadd6          jmp popst

                .endproc


;======================================
;
;======================================
cgshift         .proc
                lda arg5                ; byte ?
                bne cgmul.cgmd          ; no

                lda arg1
                cmp #vart               ; see if constant
                bcs cgmul.cgmd          ; no

                ldy #1
                lda (stack),y
                beq cgadd.cgadd6        ; ignore shift

                cmp #8                  ; shift high  7
                bcc _s1                 ; no

                ldx #0
                stx arg9
                lda #constt+bytet
                bne cgadd.cgadd5        ; [unc]

_s1             sta arg1
                lda #$0A                ; ASL A
                ldx arg8
                beq _s2

                lda #$4A                ; LSR A
_s2             sta arg3
                jsr gettemps
                jsr loadx.load1l

_s3             lda arg3                ; shift Op
                jsr push1

                dec arg1
                bne _s3

                jsr stempl

                jmp cgadd.cgadd3

                .endproc


;======================================
;
;======================================
cgmul           .proc
                lda #tempt+intt         ; force output to INT
                sta arg5
                sta arg7

        .if ramzap
                sta (adress),y
        .else
                nop
                nop
        .endif

cgdiv           jsr load2h

                lda #$85                ; STA AFcur+1
                ldx #afcur+1
                jsr push2
cgmd            jsr gettemps
                jsr load2l

                lda #$85                ; STA AFcur
                ldx #afcur
                jsr push2

                lda arg4
                beq _md1                ; first op byte

                jsr load1h

                lda #$AA                ; TAX
                jsr push1
_md1            jsr loadx.load1l

                lda arg4
                bne _md2

                lda #$A2                ; LDX #0
                ldx #0
                jsr push2

_md2            ldx arg8
                jsr jsrtable
                jsr stempl
                jsr trashy

                lda arg5
                beq _md3

                lda #$8A                ; TXA
                jsr push1
                jsr stemph

_md3            jmp cgadd.cgadd3

                .endproc


;======================================
;   CGOr()
;======================================
cgor            .proc
                jsr chkcond

                ldy #12
                jsr ldcdz

                ldy #8
                jsr stkaddr
                beq _or1                ; no JMPs

                jsr filljmp

_or1            ldy #5
                jsr ldcdz

                ldy #11                 ; link T2 to T1
                jsr setrel
_or2            jsr savrel
                jsr loadn
                bne _or2                ; get end of T1

                ldy #3
                lda (stack),y
                sec
                jsr comprel.crel2       ; patch in T2
                beq cgand.and3          ; [unc]

                .endproc


;======================================
;   CGAnd()
;======================================
cgand           .proc
                jsr chkcond

                lda stack
                ldx stack+1
                jsr loadn.ln1

_and1           jsr saven               ; patch addresses

                clc
                lda arg0
                adc #3
                sta (arg4),y
                iny
                lda (arg4),y
                adc #0
                sta (arg4),y
                jsr loadn
                bne _and1

                ldy #13
                jsr loadi

                ldy #1
                jsr save4               ; link F1 to F2

                ldy #8
                jsr stkaddr

                lda #$4C                ; JMP
                jsr insrt3              ; patch in JMP false

                lda qcode
                pha
                clc
                lda arg14
                adc #3
                sta qcode
                ldy #11
                jsr fillbr              ; make T1 -> Cond2

                pla
                sta qcode

                ldy #4
                jsr loadi

                clc
                adc #3
                bcc _and2

                inx
_and2           ldy #10
                jsr savecd.savstk

and3            ldy #2
                jsr loadi

                ldy #8
                jsr savecd.savstk

                ldy #12
                jsr savecd
                jsr trashy              ; just in case array in cond.
                jmp popst               ; done at last, whew!

                .endproc


;======================================
;
;======================================
chkcond         .proc
                lda #condt
                cmp arg1
                bne _cc1

                cmp arg2
                bne conderr

                rts

_cc1            pla
                pla
                jmp cgadd.cgadd1

conderr         ldy #cnder
                jmp splerr

                .endproc


;======================================
;   CGEq()
;======================================
cgeq            .proc
cgne            jsr loadx.load1l
                jsr chkzero
                beq _cge0

                lda #$41                ; EOR
                jsr op2l

_cge0           lda arg5
                beq cge2

                jsr chkzero

                php                     ; save status
                beq _cge1

                lda #$D0                ; BNE
                jsr pushtrue

_cge1           lda #$01                ; ORA
                jsr op1h

                plp                     ; ChkZero() status
                beq cge2

                lda #$41                ; EOR
                jsr op2h

                ldy #11
                jsr fillbr

cge2            jsr opcd1
                jsr pushtrue            ; sets arg9 to zero

                lda #condt
                sta arg7
                ldy #12
                jsr savecd

                jmp cgadd.cgadd3

                .endproc


;======================================
;   CGLS()
;======================================
cgls            .proc
cgge            jsr relop
                jsr loadx.load1l

                lda #$C1                ; CMP
                jsr op2l

                lda arg5
                beq cgeq.cge2

                jsr load1h

                lda #$E1                ; SBC
                jsr op2h
                bcs cgeq.cge2           ; [unc], see CodeIncr

                .endproc


;======================================
;   CGGR()
;======================================
cggr            .proc
cgle            jsr relop
                jsr load2l

                lda #$C1                ; CMP
                jsr loadx.op1l

                lda arg5
                beq cgeq.cge2

                jsr load2h

                lda #$E1                ; SBC
                jsr op1h
                bcs cgeq.cge2           ; [unc], see CodeIncr

                .endproc


;======================================
;   CGUM()
;======================================
cgum            .proc
                lda arg1
                and #$78
                bne _cgum1              ; not constant

    ; constant, just negate it
                sec
                ldy #0
                lda #constt+intt
                sta (stack),y
                sec
                tya
                iny
                sbc (stack),y
                sta (stack),y
                iny
                lda #0
                sbc (stack),y
                sta (stack),y
                rts

_cgum1          jsr copyst

                ldy #7
                lda #constt+intt
                sta (stack),y
                lda #0
                tax
                iny
                jsr savecd.savstk

                lda #minusid
                jmp codegen

                .endproc


;======================================
;   CGAt()
;======================================
cgat            .proc
                lda arg1
                cmp #vart
                bcc _cgat2

                cmp #arrayt+8
                bcs _cgaterr

                ldy #1
                jsr stkp

                iny
                jsr savecd.savstk

_cgat1          ldy #0
                lda #constt+cardt
                sta (stack),y
                rts

_cgat2          and #$F8
                cmp #typet              ; check for record field
                beq _cgat1
                                        ; constant or cond. exp. (error)
_cgaterr        jmp    experr

                .endproc

;--------------------------------------
;--------------------------------------

                .byte 0                 ; for records!
vartype         .byte 0,0,1,2,2,3

; moved to CGU
;outtype .BYTE $82,3,$84,realT
; .BYTE 3,3,$84,realT
; .BYTE $84,$84,$84,realT
; .BYTE realT,realT,realT,realT

cgopscd         .byte $18,$61           ; CLC ADC
                .byte $38,$e1           ; SEC SBC
                .byte $41,$21,$01       ; EOR AND ORA
                .byte $30,$90           ; BMI BCC
                .byte $10,$b0           ; BPL BCS
                .byte $f0,$d0           ; BEQ BNE

cgops           .byte 0
                .word cgadd
                .byte 2
                .word cgadd             ; minus
                .byte 4
                .word cgmul             ; multiply
                .byte 6
                .word cgmul.cgdiv       ; divide
                .byte 5
                .word cgor
                .byte 4
                .word cgand
                .byte 10
                .word cgeq
                .byte 11
                .word cgeq.cgne
                .byte 6
                .word cggr
                .byte 8
                .word cgls.cgge
                .byte 6
                .word cgls
                .byte 8
                .word cggr.cgle
                .byte 8
                .word cgmul.cgdiv       ; remainder
                .byte 3
                .word cgadd.cgadd1      ; XOR
                .byte 0
                .word cgshift           ; LSH
                .byte 2
                .word cgshift           ; RSH
                .byte 12
                .word cgum              ; unary minus
                .byte 0
                .word cgat
