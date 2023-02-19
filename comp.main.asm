
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
                jsr jt_splend

                lda nxttoken
                cmp #tokQuote
                bne _spl1               ; no name

                jsr lexget.lget
                jmp _spl2

_spl1           lda top1
                sta top+1
                jsr chkcur._ldtop
                beq _splrtn             ; no program !

                jsr GetNext
_spl2           jsr GetNext

; <program> _:= <module list> MODULE <module>
;                 | (MODULE) <module>
; <module> _:= (<dcl list>) (<segment list>)

                jsr declare
                jsr Segment

                cmp #tokMOD              ; another module ?
                beq _spl2               ; yes

                cmp #tokEOF
                bne _spl4

                lda #1                  ; save run address
                jsr cprop

                sta INITAD
                stx INITAD+1

    ; insert return, just in case
_rtn            lda #$60                ; RTS
                jsr Push1

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

                ; lda arrayPtr
                ; bne _SPL3

                lda qcode
                cmp MEMTOP
                lda qcode+1
                sbc MEMTOP+1
                bcs enderror._spl5

_splrtn         rts

_spl4           jsr getcdoff            ; no main PROC

                sta INITAD
                stx INITAD+1
                jsr stmtlist

                cmp #tokEOF
                beq _rtn

;EndErr LDY #0
; STY $2E3                              ; zap run address
enderror        ldy #endERR
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
                jsr LoadCd

_derr           jmp dclerr

_type           lda #+tokRECORD-(tokVAR_t-tokCHAR)-1
                sta type
                jsr makeentry

                lda addr
                ldx addr+1
                ldy #2
                jsr SaveCd.savstk

                ldy #0
                jsr SaveCd
                jsr GetNext

                cmp #tokEQU
                bne _derr

                jsr GetNext

                cmp #tokLBracket
                bne _derr

                sec
                lda #0
                sbc codeoff
                sta qcode
                lda #0
                sbc codeoff+1
                sta qcode+1
                jsr GetNext

_t1             cmp #tokCHAR
                bcc cderr

                cmp #tokDEFINE
                bcs cderr

                tax
                clc
                adc #+tokTYPE_t-tokVAR_t
                sta type
                lda varsize-tokCHAR,x
                sta afcur
_t2             jsr makeentry

                lda afcur
                jsr codeincr
                jsr GetNext

                cmp #tokComma
                beq _t2

                cmp #tokRBracket
                bne _t1

                ldy #2
                jsr StkP

                ldx qcode+1
                bne cderr

                lda qcode
                ldy #0
                jsr storprops

                lda #0
                tay
                jsr LoadCd
                jsr GetNext
declare         jsr jt_dclend

                cmp #tokCHAR
                bcs _dcl0

_drtn           rts

_dcl0           cmp #tokTYPE
                beq cderr._type

                cmp #tokRECORD
                bne _dcl1

    ; record dcl.
                lda #0
                jsr getprop

                stx afcur
                ldx nxttoken
                lda #tokTYPE_t-(tokVAR_t-tokCHAR)-1
                sta type
                bra _dcl2

_dcl1           cmp #tokDEFINE
                beq _define
                bcs _drtn

                sta type
                tax
                ldy varsize-tokCHAR,x
                sty afcur
                ldx nxttoken
                cpx #tokFUNC
                beq _drtn

                cpx #tokARRAY
                beq dclerr._arrdcl

_dcl2           cpx #tokPOINTER
                bne _sdcl

                ldy #0
                sty afcur
                bra dclerr._arrdcl

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
                cmp #tokEQU
                bne _sdcl1

                jsr ideq

                iny
                jsr storprops
                jsr GetNext

                bne _sdcl2

_sdcl1          lda afcur
                jsr codeincr
_sdcl2          jsr GetNext

                cmp #tokComma
_sdcl3          beq _sdcl

                jmp declare


; <def dcl> _:= DEFINE <def list>
; <def list> _:= <def list> , <def> | <def>
; <def> _:= <id> = <str const>

_define
                jsr makeentry

                ldy #0
                lda #tokDef
                sta (props),y
                jsr GetNext

                cmp #tokEQU
                bne dclerr

                lda nxttoken
                cmp #tokQuote
                bne dclerr

                ldy #0
                lda (symtab),y
                clc
                adc #2                  ; real size + EOL
                jsr stincr
                jsr GetNext             ; string itself
                jsr GetNext             ; dummy string
                jsr GetNext

                cmp #tokComma
_def1           bne _sdcl3
                beq _define

dclerr          ldy #declERR
                jmp splerr

; <array dcl> _:= <type> ARRAY <array list>
; <array list> _:= <array list> , <array> | <array>
; <array> _:= <id> ((<constant>)) (= <constant>)

_arrdcl
                clc
                adc #8
                sta type
                jsr GetNext
_arrd1          jsr makeentry

                lda #2
                sta numargs             ; variable space

                ldx param
                bne _arrd5

                lda nxttoken
                ldx afcur
                beq _arrd2              ; no size for pointers

                cmp #tokLParen
                bne _arrd2

                lda #4
                sta numargs
                lda arrayptr
                ldx arrayptr+1
                ldy #0
                jsr storevar
                jsr getarsz
                jsr GetNext

    ; check for small byte array

                ldy #2
                lda #0
                cmp (qcode),y
                iny
                lda #1
                sbc (qcode),y
                bcs _arrd6              ; size <= 256

_ard2a          jsr GetNext

                cmp #tokRParen
                bne dclerr

                lda nxttoken
                cmp #tokEQU
                beq _ard2c

_arrd3          lda numargs
                bne _ard3a

    ; small array
                ldy #2
                lda (qcode),y
                bne _ard3b

                inc qcode+1
                bra _arrd4

    ; array var

_ard3a          cmp #4
                bmi _ard3b

    ; large array with memory
                ldx qcode
                stx arrayptr
                ldx qcode+1
                stx arrayptr+1

_ard3b          jsr codeincr
_arrd4          jsr GetNext

                cmp #tokComma
                beq _arrd1

                jmp declare

_arrd5          jsr params

                ldx param
                bpl _arrd3
                bmi _arrd4

_arrd2          cmp #tokEQU
                bne _arrd3

_ard2c          jsr ideq

                ldy numargs
                beq _ard2b

                ldy #0
                jsr storevar
                jsr getcdoff

_ard2b          ldy #1
                jsr storprops
                jsr GetNext

                lda numargs
                jmp _ard3b

_arrd6          ldy #0
                lda (props),y
                cmp #tokARRAY_t+tokINT_t
                bcs _ard2a

    ; small byte array
                sty numargs
                ora #8
                sta (props),y
                iny
                jsr getcdoff
                jsr storprops

                bra _ard2a

chkparam
                ldx param
chkp            beq makeentry.derr

                pla
                pla
                jmp GetNext


;======================================
;   MakeEntry()
;======================================
makeentry       .proc
                lda nxttoken
                cmp #tokUNDEC
                beq derr._me1
                bcs _mev                ; var of some kind

                cmp #tokRECORD
                beq _mev

                cmp #tokTYPE_t
                bcc chkparam

                cmp #tokEOF
                bcs chkparam

_mev            lda qglobal
                beq chkp

                jsr gnlocal

                cmp #tokUNDEC
                beq derr._me0
derr            jmp dclerr

_me0            sta nxttoken
_me1            lda #0
                jsr nxtprop

                sec
                lda #tokVAR_t-tokCHAR
                adc type
                sta (props),y           ; type
                and #7
                tax
                lda vartype-1,x
                sta op

                iny
                jsr getcdoff
                jsr storprops

                jmp GetNext

                .endproc


;======================================
;   IdEq()
;======================================
ideq            .proc
                jsr GetNext

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
;      lda nxtToken
;      cmp #tokUNDEC
;      bne _ChkN
;      jsr GNlocal
;:ChkN jmp GetNext


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
                cpx #tokRParen
                bne _p1

    ; see AMPL.SEG
                clc
                adc gbase
                sta gbase
                bcc _p1

                inc gbase+1
_p1             pla
                bmi perr._par1

                cmp #tokTYPE_t+8
                beq perr._par2

perr            jmp Segment.argerror

;:Par1 cmp #varT+realT
;      beq PErr

_par1           cmp #tokVAR_t+tokINT_t
                bcc _par3               ; one byte arg

    ; two byte arg
_par2           and #$1f
                inc argbytes
_par3           and #$9f
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
                jsr jt_smtend

                cmp #tokLBracket
                bne _sl5

    ; machine qcode block
                jsr TrashY

_sl1            ldx nxttoken
                cpx #tokRBracket
                beq _sl4

                jsr mnum

                cpx #0
                beq _sl2

                jsr Push2               ; 2 byte number
                bra _sl3

_sl2            jsr Push1               ; single byte
_sl3            jsr GetNext
                bra _sl1

_sl4            jsr GetNext

                jmp recret.nxtstmt

_sl5            ldx nxttoken
                cmp #tokVAR_t+tokCHAR_t
                bcc _sl6

                cmp #tokFUNC_t
                bcc assign

    ; routine reference
                cpx #tokLParen
                beq call

                jsr procref
                bra assign.ass0

_sl6            cmp #tokUNDEC
                bne _sl7

                jsr GetAlias
                bra _sl5

_sl7            cmp #tokTYPE_t
                bne _sl8

                jsr etype

                jmp arassign.arass1

_sl8            cmp #tokTYPE_t+8
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
                cmp #tokARRAY_t
                bcs arassign

ass0            jsr pushnext

ass1            eor #tokEQU
                bne asserr

                jsr pushop              ; push 0 on op stack
                jsr GetNext

                sta op
                cmp #tokEQU
                bne _ass2

                lda #0
                sta op
                jsr copyst

    ; check for temps
                iny
                and #$f8
                cmp #tokARRAY_t+8
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
_a1b            jsr GetNext
_ass2           jsr exp.exp1
                jsr cgassign
                jmp stmtlist

asserr          ldy #assgnERR
                jmp splerr

                .endproc


; <array assign> _:= <id> ( <exp> ) = <exp>

arassign        .proc
                jsr ArrRef

arass1          ldy #0
                lda (stack),y
                bpl _ara1               ; record element

                cmp #tokVAR_t
                bcc assign.asserr       ; const

_ara1           jsr GetNext
                bra assign.ass1

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

                cmp #tokTHEN
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

                cmp #tokELSEIF
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

_else           cmp #tokELSE
                bne _fi
                jsr frameadr
                jsr framecd

                ldy #0                  ; flag as end of list
                jsr pushjmp
                jsr filljmp.fjmp1
                jsr recret.nxtstmt

_fi             ldy #fiERR
                cmp #tokFI
                bne dostmt.fierr

                ldy #4
                jsr frameadr.fadr1
                beq if1                 ; if no ELSEIF

                jsr filljmp.fjmp1
if1             jsr TrashY
                jsr frameadr

                beq recret              ; in case of DO loop
                jsr filljmp.fjmp1

                .endproc


;    RecRet() pops stack and returns
;    -------------------------------
recret          .proc
                jsr freeframe
nxtstmt         jsr GetNext

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

thnerr          ldy #thenERR
sterr           jmp splerr


; <do> _:= DO <stmt list> (UNTIL <exp>) OD

dostmt          .proc
                jsr doinit

                lda #0
                ldy #3
                sta (frame),y
                bra whstmt.wh1

fierr           cmp #tokUNDEC
                bne sterr

                jmp mnum._varerr

                .endproc

; <while> _:= WHILE <exp> <do>

whstmt          .proc
                jsr doinit
                jsr condexp

                ldy #doERR
                cmp #tokDO
                bne sterr

wh1             jsr recret.nxtstmt

wh2             cmp #tokUNTIL
                bne _wh3

                jsr condexp
                bra _wh4

_wh3            ldy #4
                jsr frameadr.fadr1
                jsr pushjmp

                lda token
_wh4            ldy #odERR
                cmp #tokOD
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
                ldy #exitERR
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
forerror        ldy #forERR
                bne dostmt.fierr


;======================================
;
;======================================
forstmt         .proc
                lda #23
                jsr getframe
                jsr whadr

    ; make sure simple var for index
                jsr GetNext

                cmp #tokVAR_t+tokCHAR_t
                bcc forerror

                cmp #tokARRAY_t
                bcc _fs1

                cmp #tokARRAY_t+tokREAL_t
                bcs forerror

                lda #tokVAR_t+tokCARD_t
                sta token

    ; get initial value
_fs1            ldy #8
                sta (frame),y
                iny
                lda addr
                ldx addr+1
                jsr framecd.fcd2
                jsr pushnext

                cmp #tokEQU
                bne forerror

                jsr getexp
                jsr cgassign

    ; set default STEP size
                lda token
                cmp #tokTO
_f1             bne forerror

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
                ora #tokCONST_t
                ldx #1
                ldy #11
                jsr framecd.fcd2

    ; get ending value
                lda #16
                jsr forexp

    ; get step value
                lda token
                cmp #tokSTEP
                bne _fnostep

                lda #11
                jsr forexp

                lda token
_fnostep        cmp #tokDO
                bne _f1

    ; generate end test
                jsr getcdoff

                ldy #4
                jsr framecd.fcd2
                jsr TrashY
                jsr genops.gops

                ldy #16
                lda (frame),y
                cmp #tokVAR_t
                bcs _f3                 ; temp variable

    ; constant
                iny
                lda (frame),y
                tax
                lda #$a9
                jsr Push2               ; LDA #low

                lda #$c1                ; CMP
                jsr Op2L

                lda arg3
                beq _fbody

                ldy #18
                lda (frame),y
                tax
_f2             lda #$a9
                jsr Push2               ; LDA #high

                jmp _f4

_f3             ldy #17
                sty arg0
                lda #$ad                ; LDA addr16
                jsr forexp.fexp2

                lda #$c1                ; CMP
                jsr Op2L

                lda arg3
                beq _fbody

                ldx #0
                ldy #16
                lda (frame),y
                cmp #tokVAR_t+tokINT_t
                bcc _f2                 ; only byte var

                lda #$ad                ; LDA addr16
                jsr forexp.fexp2

_f4             lda #$e1                ; SBC
                jsr Op2H

_fbody          lda arg3
                ror a                   ; get type
                lda #$b0                ; BCS, CARD
                bcc _f5

                lda #$10                ; BPL, INT
_f5             jsr Push1

                ldy #21
                jsr framecd.fcd1
                jsr Push1
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
                cmp #tokVAR_t
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

                lda #tokPLUS
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
                cmp #tokUNTIL
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
                cmp #$ff
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
                cmp #tokVAR_t               ; see if const
                bcs _fe1

    ; constant
                ldy #11
                lda (frame),y
                ldy arg0
                sta (frame),y
                ldy #2
                jsr LoadI

                ldy arg0
                iny
                jmp framecd.fcd2

_fe1            ldy #8
                lda (frame),y
                and #7
                cmp #tokINT_t
                bmi _fe2

                lda arg1
                and #7
_fe2            ora #tokVAR_t
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
_fevar          jsr Load2L
                jsr fexp1

                lda arg2
                cmp #tokVAR_t+tokINT_t          ; see if byte
                bcc fexp2._fe3

                jsr Load2H

fexp1           lda #$8d                ; STA data16
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
                jmp Push3

_fe3            rts

_ftemp          ldy #4
                jsr LoadI

                ldy #1
                jsr AddCdSp

                ldy #3
                lda #0
                jsr _ft1

                lda arg2
                cmp #tokVAR_t+tokINT_t          ; see if byte
                bcc _fe3

                ldy #5
                lda #1
_ft1            jsr LoadCd

                jmp fexp1

                .endproc


;======================================
;
;======================================
fmem            lda (frame),y
                cmp #tokVAR_t
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
                cmp #tokVAR_t+tokINT_t
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

                jmp TrashY

                .endproc


; <return> _:= RETURN ((<exp>))

retstmt         .proc
                lda #0
                jsr cprop

                and #7
                beq _r1

                ora #tokTEMP_t
                tay
                lda #args
                jsr storst

                ldx nxttoken
                cpx #tokLParen
                bne reterr

                jsr GetNext
                jsr getexp

                cmp #tokRParen
                bne reterr

                jsr cgassign

_r1             lda #$60
                jsr Push1

                jmp recret.nxtstmt


;======================================
;
;======================================
reterr          ldy #retrnERR
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
                cmp #tokCOND_t
                beq _cexp1

    ; not boolean
                jsr zerost

                lda #tokNOTEQU
                jsr codegen

_cexp1          pla                     ; token value
                pha
                cmp #tokOD
                bne _cexp2

    ; until <exp> od
                ldy #1
                jsr StkAddr
                beq _ce1a               ; no JMPs

    ; JMP to JMP to top of loop
    ; yek!, should be improved
                jsr filljmp             ; fill in jmps

_ce1a           ldy #4
                jsr frameadr.fadr1
                bra _cexp3

_cexp2          jsr framecd

                ldy #1
                jsr StkAddr
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
                lda #$4c                ; JMP addr16
                jmp Push3

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

nsterr          ldy #nestERR
                jmp splerr

                .endproc


;======================================
;   SetRel(,,offset)
;======================================
setrel          .proc
                jsr LoadI

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

stmtlst         .addr jt_smtend         ; not found
                .byte 20                ; #entries*3 - 1
                .addr ifstmt
                .byte tokIF
                .addr forstmt
                .byte tokFOR
                .addr whstmt
                .byte tokWHILE
                .addr retstmt
                .byte tokRET
                .addr dostmt
                .byte tokDO
                .addr exitstmt
                .byte tokEXIT


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
                jsr GetNext
                .endproc


;======================================
;
;======================================

exp             .proc
                lda #0
                jsr pushop

                lda token               ; always non-zero
                sta op
exp1            jsr jt_expend

                cmp #tokSColon
                bcc eerr._expop

                cmp #tokRParen
                bne _exp2

                ldx op
                bne eerr

                jsr rollops

                cmp #tokLParen
                beq eerr._exp7

                lda token
                rts


;======================================
;
;======================================
_exp2           cmp #tokLParen
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
                cmp #tokQuote
                beq _expstr

_exp3a          ldx nxttoken
                cmp #tokRECORD
                beq _exp3d              ; record

                cmp #tokTYPE_t
                bcc eerr
                bne _exp3c

_exp3b          jsr etype
                bra _exp7

_exp3c          cmp #tokTYPE_t+8
                bcc _exp3d              ; field
                bne _exp3e

                jsr etypea

                jmp _exp7


    ; record name or field
_exp3d          cpx #tokPeriod
                bne _exp3b

    ; will generate error if falls through

_exp3e          cmp #tokCONST_t
                bcc eerr                ; missing operand?

                cmp #tokARRAY_t
                bcs _exp4               ; array or func

                cmp #tokUNDEC
                beq _expund

                cmp #tokCONST_t+tokREAL_t
                beq eerr                ; real constant

_expvar
                jsr pushnext
                bra exp1

_exp4           cmp #tokFUNC_t
                bcs _expproc

_exparr
                jsr ArrRef

                jmp _exp7

_expop          ldx op
                beq _exp5

                cmp #tokAT
                beq _exp5

                cmp #tokMINUS
                bne eerr

                lda #tokUMINUS
                sta token
_exp5           tax
                lda prec-1,x
                sta op
                jsr rollops
                jsr pushop

                lda token
_exp6           jsr pushop
_exp7           jsr GetNext

                jmp exp1

_exp8           jsr rollops

                cmp #tokLParen
                beq parenerr

_exp9           lda token
                rts

_expund
                jsr GetAlias
                bra _exp3a

_expproc
                bne parenerr._expfunc

_expp1          jsr procref
                bra _expvar

_expstr
                lda #$4c                ; JMP around string
                jsr Push1
                jsr getcdoff

                adc #3                  ; includes size byte
                bcc _es1

                inx
                clc
_es1            ldy #0
                adc (addr),y            ; size
                bcc _es2

                inx
_es2            jsr Push2

                jsr copystr

                ldy #tokCONST_t+tokSTR_t
                jsr storst

                dec choff
                jsr GetNext
                bra _exp7

parenerr
                ldy #parenthERR
                jmp splerr


    ; qcode to handle function ref
_expfunc
                cmp #tokFUNC_t+8
                beq eerr._expp1

                cpx #tokLParen
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

                lda #$a5                ; LDA addr
                ldx arg1
                ldy #$48                ; PHA
                jsr Push3

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
                jsr Push3

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
                ora #tokTEMP_t
                ldx #args
                jsr SaveCd.savstk

                jmp eerr._exp7

                .endproc


;:ExpReal ldx vars
;         ldy vars+1
;         jsr FST0R
;         lda varsOff
;         ldx varsOff+1
;         jsr SST1
;         lda #6
;         jsr VarIncr
;         jmp _Exp7

experr          ldy #exprERR
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
                ldy #tokCONST_t+tokBYTE_t
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
                jmp SaveCd.savstk

                .endproc


;======================================
;
;======================================
etypep          .proc
                jsr pushst
                jsr GetNext
                jsr GetNext

                ldy #0
                sta (stack),y
                tax
                and #$f8
                ldy #typERR
                cmp #tokTYPE_t
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
                cpx #tokPeriod
                beq _e1

    ; record addr, size or field offset
    ; A reg must be nonzero before call
                jmp ArrRef.arrconst

_e1             jsr etypep              ; set type

    ; get var address
                ldy #1
                jsr StkPS

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
                cpx #tokPeriod
                beq _e1

                jmp ArrRef.arrvar

_e1             jsr etypep              ; set type

                tay
                lda token
                pha
                tya
                ldy #tokCONST_t+tokBYTE_t
                jsr storst

                lda #tokARRAY_t+tokBYTE_t
                ldy #7
                jsr ArrRef.arra0

                ldy #0
                pla
                ora #$b0                ; temp array
                sta (stack),y
                rts
                .endproc


;======================================
;
;======================================
procref         .proc
                ldy #tokFUNC_t+tokCARD_t        ; make sure CARD
                cmp #tokFUNC_t+8
                bcc stconst._pr1

stconst         jsr GetArgs             ; A#0, no arg types

                ldy #tokCONST_t+tokCARD_t       ; sys proc
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

                cmp #tokLParen
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

                cpx #tokPLUS
                beq _ro0

                cpx #tokRSH
                beq _ro0

                cpx #tokLSH
                bne _ro2

_ro0            jsr popop               ; see if last op
                bne _ro1

    ; check for increament
    ; We know at least this is an assignment or an array subscript.
    ; If ChStkEq in CGPlus is true then this is an assignment.
                txa
                cmp #tokPLUS
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

                jmp GetNext

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
                ora #tokTEMP_t
                sta arg7
                lda vartype-1,x
                sta arg5                ; output type

                jmp Push0

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
                cpy #tokRSH
                beq _cgs2               ; right shift

                lda #$06                ; ASL
                jsr LoadX.Op1L

                lda arg4
                beq _cgs4

                lda #$26                ; ROL
                jsr Op1H

                jmp _cgs4

_cgs2           lda #$46                ; LSR
                ldx arg4
                beq _cgs3

                jsr Op1H

                lda #$66                ; ROR
_cgs3           jsr LoadX.Op1L

_cgs4           dec stkbase-20
                bne _cgs1

_cgs5           jmp popst

                .endproc


;======================================
;   CodeGen(op)
;======================================
codegen         .proc
                jsr genops
cg1             jsr jt_cgend

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
                lda #$e6                ; INC
                jsr LoadX.Op1L

                lda arg4
                beq _cga2               ; byte var

                lda #$d0                ; BNE
                jsr Push1

                ldy #12
                jsr SaveCd

                lda #0
                jsr Push1               ; offset

                lda #$e6                ; INC
                jsr Op1H

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
                jsr jt_cgend
                jsr chasseq.chstkeq     ; see if INC
                bne cga1                ; yes, just return

                lda arg1
                bpl _ass1               ; cond. exp. of record

                bit tempmode            ; rhs temp?
                bne cga1._cgat          ; yes

                cmp #tokVAR_t               ; const?
                bcs _cgav               ; no

    ; rhs constant
                lda arg2
                cmp #tokARRAY_t             ; simple var
                bcs _cgav

                ; if sty addr16,x was supported
                ;    bcc _CGC0    ; yes
                ;    bit tempMode ; lhs temp?
                ;    bne _CGAV    ; yes, large array

_cgc0           ldx arg4
                beq _cgc1               ; byte

                ldy #2
                lda (stack),y
                cmp #2
                bcs _cgavi

                sta arg12
                jsr LoadY

                lda #$84                ; STY
                jsr Op1H

_cgc1           ldy #1
                lda (stack),y
                cmp #2
                bcs _cgavb

                sta arg12
                jsr LoadY

                lda #$84                ; STY
                bra _avb2

_ass1           cmp #tokTYPE_t
                bcc cgexperr            ; cond. exp.

    ; rhs var
_cgav           ldx arg4                ; lhs type=byte?
                beq _cgavb              ; yes

                ; cpx #3                ; lhs = int?
                ; bne _CGAVI            ; yes
                ; lhs type = real
                ; jmp AssErr

_cgavi          jsr Load2H

_avi1           lda #$81                ; STA
                jsr Op1H
_cgavb          jsr Load2L

_avb1           lda #$81                ; STA
_avb2           jsr LoadX.Op1L
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
                jsr Load2H              ; generate LDA #0 instr.

                ldy #5
                jsr SaveCd

_cgat1          ldy #4
                jsr LoadI

                ldy #8
                lda arg2
                and #$60                ; lhs proc or temp(argument)?
                bne _cgat3              ; yes

                jsr StkPZ
                beq _cgat4              ; page zero

                sty arg13
                ldy #1
                lda #$8d                ; STA addr16
                jsr Insrt3.i30          ; insert STA data16

                lda #1
_cgat2          ldy #5
                jsr LoadCd

                lda #$81                ; STA
                jsr Op1H

                jmp cga1

_cgat3          and #$40                ; proc?
                bne cgassign._cgavi     ; yes, punt(not the best qcode)

                jsr StkAddr             ; temp (proc argument)

_cgat4          ldy #1
                txa
                sta (arg14),y
                lda #0
                beq _cgat2

    ; temp array
_cgata          lda arg3
                beq cgassign._cgavi

                ldy #5
                jsr LdCdZ
                bra cgassign._avi1

_cgatb          ldy #3
                jsr LoadCd

                lda arg3                ; see if rhs BYTE or CHAR
                beq cgassign._avb1      ; yes

                jsr TrashY              ; in case INT ARRAY in rhs

    ; oh if I only had more qcode space!
    ; could handle Y, see _OpA in CGU
                bra cgassign._avb1

                .endproc


;======================================
;   chasseq (mode)
;======================================
chasseq         .proc
                jsr genops

                lda arg1
                cmp #tokVAR_t               ; see if const
                bcs chstkeq._cse2       ; no

                lda stkbase-19          ; see if byte
                bne chstkeq._cse2       ; no
                                        ; see if left and right hand are =


;======================================
;   JMP ChStkEq
;======================================
chstkeq         ldx #2
                lda stkbase-7
                and #$f8
                cmp #$b0                ; large array?
                beq _cse2               ; yes, can't INC or shift

                cmp #tokARRAY_t+8           ; small array?
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
                cmp #tokVAR_t               ; see if both ops consts
                bcs _a3                 ; not const

                lda arg7
                and #7
                ora #tokCONST_t
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

                lda #tokCONST_t+tokINT_t
                bra cgadd5

    ; normal add or sub.
_a3             ldx arg8
                lda cgopscd,x
                jsr Push1

cgadd1          jsr GetTemps
                jsr LoadX.Load1L
                jsr OpCd1
                jsr Op2L
                jsr STempL

                lda arg5
                beq cgadd3

                jsr Load1H
                jsr OpCd1
                jsr Op2H

cgadd2          jsr STempH

cgadd3          ldx #0
cgadd4          lda arg7
cgadd5          ldy #7
                sta (stack),y
                iny
                lda arg9
                jsr SaveCd.savstk

cgadd6          jmp popst

                .endproc


;======================================
;
;======================================
cgshift         .proc
                lda arg5                ; byte ?
                bne cgmul.cgmd          ; no

                lda arg1
                cmp #tokVAR_t               ; see if constant
                bcs cgmul.cgmd          ; no

                ldy #1
                lda (stack),y
                beq cgadd.cgadd6        ; ignore shift

                cmp #8                  ; shift high  7
                bcc _s1                 ; no

                ldx #0
                stx arg9
                lda #tokCONST_t+tokBYTE_t
                bra cgadd.cgadd5

_s1             sta arg1
                lda #$0a                ; ASL A
                ldx arg8
                beq _s2

                lda #$4a                ; LSR A
_s2             sta arg3
                jsr GetTemps
                jsr LoadX.Load1L

_s3             lda arg3                ; shift Op
                jsr Push1

                dec arg1
                bne _s3

                jsr STempL

                jmp cgadd.cgadd3

                .endproc


;======================================
;
;======================================
cgmul           .proc
                lda #tokTEMP_t+tokINT_t         ; force output to INT
                sta arg5
                sta arg7

        .if ramzap
                sta (ADRESS),y
        .else
                nop
                nop
        .endif

cgdiv           jsr Load2H

                lda #$85                ; STA AFcur+1
                ldx #afcur+1
                jsr Push2
cgmd            jsr GetTemps
                jsr Load2L

                lda #$85                ; STA AFcur
                ldx #afcur
                jsr Push2

                lda arg4
                beq _md1                ; first op byte

                jsr Load1H

                lda #$aa                ; TAX
                jsr Push1
_md1            jsr LoadX.Load1L

                lda arg4
                bne _md2

                lda #$a2                ; LDX #0
                ldx #0
                jsr Push2

_md2            ldx arg8
                jsr JSRTable
                jsr STempL
                jsr TrashY

                lda arg5
                beq _md3

                lda #$8a                ; TXA
                jsr Push1
                jsr STempH

_md3            jmp cgadd.cgadd3

                .endproc


;======================================
;   CGOr()
;======================================
cgor            .proc
                jsr chkcond

                ldy #12
                jsr LdCdZ

                ldy #8
                jsr StkAddr
                beq _or1                ; no JMPs

                jsr filljmp

_or1            ldy #5
                jsr LdCdZ

                ldy #11                 ; link T2 to T1
                jsr setrel
_or2            jsr savrel
                jsr loadn
                bne _or2                ; get end of T1

                ldy #3
                lda (stack),y
                sec
                jsr comprel.crel2       ; patch in T2
                bra cgand.and3

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
                jsr LoadI

                ldy #1
                jsr save4               ; link F1 to F2

                ldy #8
                jsr StkAddr

                lda #$4c                ; JMP
                jsr Insrt3              ; patch in JMP false

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
                jsr LoadI

                clc
                adc #3
                bcc _and2

                inx
_and2           ldy #10
                jsr SaveCd.savstk

and3            ldy #2
                jsr LoadI

                ldy #8
                jsr SaveCd.savstk

                ldy #12
                jsr SaveCd
                jsr TrashY              ; just in case array in cond.
                jmp popst               ; done at last, whew!

                .endproc


;======================================
;
;======================================
chkcond         .proc
                lda #tokCOND_t
                cmp arg1
                bne _cc1

                cmp arg2
                bne conderr

                rts

_cc1            pla
                pla
                jmp cgadd.cgadd1

conderr         ldy #condtERR
                jmp splerr

                .endproc


;======================================
;   CGEq()
;======================================
cgeq            .proc
cgne            jsr LoadX.Load1L
                jsr ChkZero
                beq _cge0

                lda #$41                ; EOR
                jsr Op2L

_cge0           lda arg5
                beq cge2

                jsr ChkZero

                php                     ; save status
                beq _cge1

                lda #$d0                ; BNE
                jsr PushTrue

_cge1           lda #$01                ; ORA
                jsr Op1H

                plp                     ; ChkZero() status
                beq cge2

                lda #$41                ; EOR
                jsr Op2H

                ldy #11
                jsr fillbr

cge2            jsr OpCd1
                jsr PushTrue            ; sets arg9 to zero

                lda #tokCOND_t
                sta arg7
                ldy #12
                jsr SaveCd

                jmp cgadd.cgadd3

                .endproc


;======================================
;   CGLS()
;======================================
cgls            .proc
cgge            jsr RelOp
                jsr LoadX.Load1L

                lda #$c1                ; CMP
                jsr Op2L

                lda arg5
                beq cgeq.cge2

                jsr Load1H

                lda #$e1                ; SBC
                jsr Op2H
                bra cgeq.cge2           ; see CodeIncr

                .endproc


;======================================
;   CGGR()
;======================================
cggr            .proc
cgle            jsr RelOp
                jsr Load2L

                lda #$c1                ; CMP
                jsr LoadX.Op1L

                lda arg5
                beq cgeq.cge2

                jsr Load2H

                lda #$e1                ; SBC
                jsr Op1H
                bra cgeq.cge2           ; see CodeIncr

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
                lda #tokCONST_t+tokINT_t
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
                lda #tokCONST_t+tokINT_t
                sta (stack),y
                lda #0
                tax
                iny
                jsr SaveCd.savstk

                lda #tokMINUS
                jmp codegen

                .endproc


;======================================
;   CGAt()
;======================================
cgat            .proc
                lda arg1
                cmp #tokVAR_t
                bcc _cgat2

                cmp #tokARRAY_t+8
                bcs _cgaterr

                ldy #1
                jsr StkP

                iny
                jsr SaveCd.savstk

_cgat1          ldy #0
                lda #tokCONST_t+tokCARD_t
                sta (stack),y
                rts

_cgat2          and #$f8
                cmp #tokTYPE_t          ; check for record field
                beq _cgat1
                                        ; constant or cond. exp. (error)
_cgaterr        jmp experr

                .endproc

;--------------------------------------
;--------------------------------------

                .byte 0                 ; for records!
vartype         .byte 0,0,1,2,2,3

; moved to CGU
;outtype .byte $82,3,$84,realT
;        .byte 3,3,$84,realT
;        .byte $84,$84,$84,realT
;        .byte realT,realT,realT,realT

cgopscd         .byte $18,$61           ; CLC ADC
                .byte $38,$e1           ; SEC SBC
                .byte $41,$21,$01       ; EOR AND ORA
                .byte $30,$90           ; BMI BCC
                .byte $10,$b0           ; BPL BCS
                .byte $f0,$d0           ; BEQ BNE

cgops           .byte 0
                .addr cgadd
                .byte 2
                .addr cgadd             ; minus
                .byte 4
                .addr cgmul             ; multiply
                .byte 6
                .addr cgmul.cgdiv       ; divide
                .byte 5
                .addr cgor
                .byte 4
                .addr cgand
                .byte 10
                .addr cgeq
                .byte 11
                .addr cgeq.cgne
                .byte 6
                .addr cggr
                .byte 8
                .addr cgls.cgge
                .byte 6
                .addr cgls
                .byte 8
                .addr cggr.cgle
                .byte 8
                .addr cgmul.cgdiv       ; remainder
                .byte 3
                .addr cgadd.cgadd1      ; XOR
                .byte 0
                .addr cgshift           ; LSH
                .byte 2
                .addr cgshift           ; RSH
                .byte 12
                .addr cgum              ; unary minus
                .byte 0
                .addr cgat
