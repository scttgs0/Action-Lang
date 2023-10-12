
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
                bne _1                  ; no name

                jsr lexget._ENTRY1
                jmp _next1

_1              lda top1
                sta top+1

                jsr chkcur._ENTRY1
                beq _XIT1               ; no program !

                jsr getnext
_next1          jsr getnext

; <program> _:= <module list> MODULE <module>
;                 | (MODULE) <module>
; <module> _:= (<dcl list>) (<segment list>)

                jsr declare
                jsr segment

                cmp #modid              ; another module ?
                beq _next1              ;   yes

                cmp #eofid
                bne _2

                lda #1                  ; save run address
                jsr cprop

                sta INITAD
                stx INITAD+1

;   insert return, just in case
_next2          lda #$60                ; RTS
                jsr push1

;   get qcode size
                sec
                lda qcode
                sbc codebase
                sta codesize
                lda qcode+1
                sbc codebase+1
                sta codesize+1

;   patch array addresses
                lda arrayptr+1

;               ora arrayPtr
                beq _XIT1

_next3          ldy #1
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
                bne _next3

;               lda arrayPtr
;               bne _SPL3

                lda qcode
                cmp MEMTOP
                lda qcode+1
                sbc MEMTOP+1
                bcs _err2

_XIT1           rts

_2              jsr getcdoff            ; no main PROC

                sta INITAD
                stx INITAD+1

                jsr stmtlist

                cmp #eofid
                beq _next2

;EndErr         ldy #0
;               sty $2E3                ; zap run address

_err            ldy #ender
                jmp fierr

_err2           jmp codeincr.cderr      ; out of qcode space

                ;.endproc


;=====================================
;    Declaration processing
;=====================================
; <dcl list> _:= <dcl list> <dcl> | <dcl>
; <dcl> _:= <simple dcl> | <array dcl> | <def dcl>

cderr           lda #0                  ; reset qcode before err
                tay
                jsr loadcd

_errDeclaration jmp errDefine

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
                bne _errDeclaration

                jsr getnext

                cmp #lbrack
                bne _errDeclaration

                sec
                lda #0
                sbc codeoff
                sta qcode
                lda #0
                sbc codeoff+1
                sta qcode+1

                jsr getnext

_next1          cmp #char
                bcc cderr

                cmp #define
                bcs cderr

                tax
                clc
                adc #+typet-vart
                sta type

                lda varsize-char,x
                sta afcur

_next2          jsr makeentry

                lda afcur
                jsr codeincr
                jsr getnext

                cmp #comma
                beq _next2

                cmp #rbrack
                bne _next1

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


;======================================
;
;======================================
declare         jsr dclend

                cmp #char
                bcs _1

_XIT1           rts

_1              cmp #typeid
                beq cderr._type

                cmp #record
                bne _2

;   record dcl.
                lda #0
                jsr getprop

                stx afcur

                ldx nxttoken
                lda #typet-(vart-char)-1
                sta type
                bne _3                  ; [unc]

_2              cmp #define
                beq _define
                bcs _XIT1

                sta type

                tax
                ldy varsize-char,x
                sty afcur

                ldx nxttoken
                cpx #func
                beq _XIT1

                cpx #array
                beq arrDeclaration

_3              cpx #pointer
                bne _next1

                ldy #0
                sty afcur
                beq arrDeclaration      ; [unc]

; <simple dcl> _:= <type> <id eq list>
; <id eq list> _:= <id eq list> , <id eq> | <id eq>
; <id eq> _:= <id> (= <constant>)

_next1          jsr makeentry

                ldx param
                beq _4

                jsr params

                ldx param
                bpl _5
                bmi _6                  ; [unc]

_4              lda nxttoken
                cmp #equalid
                bne _5

                jsr ideq

                iny
                jsr storprops

                jsr getnext
                bne _6

_5              lda afcur
                jsr codeincr

_6              jsr getnext

                cmp #comma
_next2          beq _next1

                jmp declare


; <def dcl> _:= DEFINE <def list>
; <def list> _:= <def list> , <def> | <def>
; <def> _:= <id> = <str const>

_define         jsr makeentry

                ldy #0
                lda #defid
                sta (props),y

                jsr getnext

                cmp #equalid
                bne errDefine

                lda nxttoken
                cmp #quote
                bne errDefine

                ldy #0
                lda (symtab),y
                clc
                adc #2                  ; real size + EOL

                jsr stincr
                jsr getnext             ; string itself
                jsr getnext             ; dummy string
                jsr getnext

                cmp #comma
                bne _next2
                beq _define             ; [unc]


;--------------------------------------
;
;--------------------------------------
errDefine       ldy #dcler
                jmp splerr


;--------------------------------------
; <array dcl> _:= <type> ARRAY <array list>
; <array list> _:= <array list> , <array> | <array>
; <array> _:= <id> ((<constant>)) (= <constant>)
;--------------------------------------
arrDeclaration  clc
                adc #8
                sta type

                jsr getnext
_next1          jsr makeentry

                lda #2
                sta numargs             ; variable space

                ldx param
                bne _2

                lda nxttoken
                ldx afcur
                beq _3                  ; no size for pointers

                cmp #lparen
                bne _3

                lda #4
                sta numargs

                lda arrayptr
                ldx arrayptr+1
                ldy #0

                jsr storevar
                jsr getarsz
                jsr getnext

;   check for small byte array

                ldy #2
                lda #0
                cmp (qcode),y

                iny
                lda #1
                sbc (qcode),y
                bcs _6                  ; size <= 256

_next2          jsr getnext

                cmp #rparen
                bne errDefine

                lda nxttoken
                cmp #equalid
                beq _4

_next3          lda numargs
                bne _1

;   small array
                ldy #2
                lda (qcode),y
                bne _next4

                inc qcode+1
                bne _next5              ; [unc]

;   array var

_1              cmp #4
                bmi _next4

;   large array with memory
                ldx qcode
                stx arrayptr
                ldx qcode+1
                stx arrayptr+1

_next4          jsr codeincr

_next5          jsr getnext

                cmp #comma
                beq _next1

                jmp declare

_2              jsr params

                ldx param
                bpl _next3
                bmi _next5

_3              cmp #equalid
                bne _next3

_4              jsr ideq

                ldy numargs
                beq _5

                ldy #0
                jsr storevar
                jsr getcdoff

_5              ldy #1
                jsr storprops
                jsr getnext

                lda numargs
                jmp _next4

_6              ldy #0
                lda (props),y
                cmp #arrayt+intt
                bcs _next2

;   small byte array
                sty numargs
                ora #8
                sta (props),y

                iny
                jsr getcdoff
                jsr storprops
                bne _next2              ; [unc]


;--------------------------------------
;
;--------------------------------------
chkparam        .proc
                ldx param

_ENTRY1         beq makeentry._err

                pla
                pla

                jmp getnext

                .endproc


;======================================
;   MakeEntry()
;======================================
makeentry       .proc
                lda nxttoken
                cmp #undec
                beq _3
                bcs _1                  ; var of some kind

                cmp #record
                beq _1

                cmp #typet
                bcc chkparam

                cmp #eofid
                bcs chkparam

_1              lda qglobal
                beq chkparam._ENTRY1

                jsr gnlocal

                cmp #undec
                beq _2

_err            jmp errDefine

_2              sta nxttoken

_3              lda #0
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
                bne params._err

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
_next1          clc
                lda arg0
                adc afsize
                sta arg0

                lda arg1
                adc afsize+1
                sta arg1

                dey
                bne _next1

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


;---------------------------------------
;---------------------------------------
;ChkNext PROC ; ChkNext()
;               lda nxtToken
;               cmp #undec
;               bne _ChkN
;               jsr GNlocal
;:ChkN          jmp GetNext


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
                bcs _err

                adc #1
                sta (props),y
                tay

;   see if time to update gbase
                ldx nxttoken
                cpx #rparen
                bne _1

;   see AMPL.SEG
                clc
                adc gbase
                sta gbase
                bcc _1

                inc gbase+1

_1              pla
                bmi _2

                cmp #typet+8
                beq _3

_err            jmp segment._argerr

;:Par1          cmp #varT+realT
;               beq PErr

_2              cmp #vart+intt
                bcc _4                  ; one-byte arg

;   two-byte arg
_3              and #$1F
                inc argbytes

_4              and #$9F
                inc argbytes
                sta (props),y

                rts
                .endproc


;--------------------------------------
;--------------------------------------

varsize         .byte 1,1,2,2,2,6


;====================================
;    Statement processing
;------------------------------------
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
;====================================
stmtlist        .proc
                jsr clrtemps

                sta op
                jsr smtend

                cmp #lbrack
                bne _next2

;   machine qcode block
                jsr trashy

_next1          ldx nxttoken
                cpx #rbrack
                beq _3

                jsr mnum

                cpx #0
                beq _1

                jsr push2               ; 2-byte number
                bcs _2                  ; [unc]

_1              jsr push1               ; single byte
_2              jsr getnext
                bne _next1              ; [unc]

_3              jsr getnext

                jmp recret.nxtstmt

_next2          ldx nxttoken
                cmp #vart+chart
                bcc _4

                cmp #funct
                bcc assign

;   routine reference
                cpx #lparen
                beq call

                jsr procref
                bne assign._ENTRY1        ; [unc]

_4              cmp #undec
                bne _5

                jsr getalias
                bne _next2                ; [unc]

_5              cmp #typet
                bne _6

                jsr etype
                jmp arassign._ENTRY1

_6              cmp #typet+8
                bne _7

                jsr etypea
                jmp arassign._ENTRY1

_7              ldx #<stmtlst
                ldy #>stmtlst
                jmp lookup

                .endproc


;--------------------------------------
; <call> _:= <proc var>((<arglist>))
; <proc var> _:= <id>
; <arglist> _:= <arglist> , <exp> | <exp>
;--------------------------------------
call            .proc
                jsr pf
                jsr popst

                jmp recret.nxtstmt

                .endproc


;--------------------------------------
; <assign> _:= <id> = <exp>
;--------------------------------------
assign          .proc
                cmp #arrayt
                bcs arassign

_ENTRY1         jsr pushnext

_ENTRY2         eor #equalid
                bne errAssign

                jsr pushop              ; push 0 on op stack
                jsr getnext

                sta op
                cmp #equalid
                bne _3

                lda #0
                sta op

                jsr copyst

;   check for temps
                iny
                and #$F8
                cmp #arrayt+8
                bne _1

                ldy #3
                lda (stack),y
_1              and #$20
                beq _2

                iny
                lda (stack),y
                tax
;   incr temps
                cpx #args               ;
                bcc _2                  ; are these 4 instr.

                cpx #args+16            ; needed?
                bcs _2                  ;

                inc temps-args,x

_2              jsr getnext
_3              jsr exp._ENTRY1
                jsr cgassign

                jmp stmtlist

                .endproc


;--------------------------------------
;
;--------------------------------------
errAssign       .proc
                ldy #asser
                jmp splerr

                .endproc


;--------------------------------------
; <array assign> _:= <id> ( <exp> ) = <exp>
;--------------------------------------
arassign        .proc
                jsr arrref

_ENTRY1         ldy #0
                lda (stack),y
                bpl _1                  ; record element

                cmp #vart
                bcc errAssign           ; const

_1              jsr getnext
                bne assign._ENTRY2      ; [unc]

                .endproc


;---------------------------------------
; <if> _:= IF <exp> THEN <stmt list
;          (ELSE <stmt list>) FI
;---------------------------------------
ifstmt          .proc
                lda #7
                jsr getframe

                ldy #5
                lda #0
                sta (frame),y

_if             jsr condexp

                cmp #then
                bne thnerr

;   save current Y
                lda cury
                ldy #6
                sta (frame),y

                jsr recret.nxtstmt

;   restore Y
                tax
                ldy #6
                lda (frame),y
                sta cury

                txa
                cmp #elseif
                bne _else

                ldy #4
                jsr frameadr._ENTRY1

                ldy #4
                jsr framecd._ENTRY1

                ldx arg4
                ldy arg5

                jsr pushjmp
                jsr frameadr
                jsr filljmp._ENTRY1

                jmp _if

_else           cmp #else
                bne _fi

                jsr frameadr
                jsr framecd

                ldy #0                  ; flag as end of list
                jsr pushjmp
                jsr filljmp._ENTRY1
                jsr recret.nxtstmt

_fi             ldy #fier
                cmp #fi
                bne fierr

                ldy #4
                jsr frameadr._ENTRY1
                beq _ifnoelse           ; if no ELSEIF

                jsr filljmp._ENTRY1

_ifnoelse       jsr trashy
                jsr frameadr

                beq recret              ; in case of DO loop
                jsr filljmp._ENTRY1

                .endproc


;--------------------------------------
;    RecRet() pops stack and returns
;--------------------------------------
recret          .proc
                jsr freeframe

nxtstmt         jsr getnext
                jmp stmtlist

                .endproc


;--------------------------------------
;    pops stack
;--------------------------------------
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


;--------------------------------------
;
;--------------------------------------
thnerr          ldy #thener

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
sterr           jmp splerr


;--------------------------------------
; <do> _:= DO <stmt list> (UNTIL <exp>) OD
;--------------------------------------
dostmt          .proc
                jsr doinit

                lda #0
                ldy #3
                sta (frame),y
                bne whstmt._ENTRY1      ; [unc]

                .endproc


;--------------------------------------
;
;--------------------------------------
fierr           .proc
                cmp #undec
                bne sterr

                jmp mnum._varerr

                .endproc


;--------------------------------------
; <while> _:= WHILE <exp> <do>
;--------------------------------------
whstmt          .proc
                jsr doinit
                jsr condexp

                ldy #doer
                cmp #do
                bne sterr

_ENTRY1         jsr recret.nxtstmt

_ENTRY2         cmp #untilid
                bne _1

                jsr condexp
                bne _2                  ; [unc]

_1              ldy #4
                jsr frameadr._ENTRY1
                jsr pushjmp

                lda token
_2              ldy #oder
                cmp #od
                bne fierr

                ldy #6
                jsr frameadr._ENTRY1

                stx whaddr
                sty whaddr+1

                jmp ifstmt._ifnoelse

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
                bne fierr        ; [unc]


;======================================
;
;======================================
forstmt         .proc
                lda #23
                jsr getframe
                jsr whadr

;   make sure simple var for index
                jsr getnext

                cmp #vart+chart
                bcc forerr

                cmp #arrayt
                bcc _1

                cmp #arrayt+realt
                bcs forerr

                lda #vart+cardt
                sta token

;   get initial value
_1              ldy #8
                sta (frame),y

                iny
                lda addr
                ldx addr+1

                jsr framecd._ENTRY2
                jsr pushnext

                cmp #equalid
                bne forerr

                jsr getexp
                jsr cgassign

;   set default STEP size
                lda token
                cmp #to
_next1          bne forerr

                lda #0
                ldx #9
                ldy #12
_next2          sta (frame),y

                iny
                dex
                bne _next2

                ldy #8
                jsr fstk

                lda token
                and #7
                ora #constt

                ldx #1
                ldy #11
                jsr framecd._ENTRY2

;   get ending value
                lda #16
                jsr forexp

;   get step value
                lda token
                cmp #step
                bne _2

                lda #11
                jsr forexp

                lda token
_2              cmp #do
                bne _next1

;   generate end test
                jsr getcdoff

                ldy #4
                jsr framecd._ENTRY2
                jsr trashy
                jsr genops._ENTRY1

                ldy #16
                lda (frame),y
                cmp #vart
                bcs _3                  ; temp variable

;   constant
                iny
                lda (frame),y
                tax

                lda #$A9
                jsr push2               ; LDA #low

                lda #$C1                ; CMP
                jsr op2l

                lda arg3
                beq _5

                ldy #18
                lda (frame),y
                tax

_next3          lda #$A9
                jsr push2               ; LDA #high

                jmp _4

_3              ldy #17
                sty arg0

                lda #$AD                ; LDA addr16
                jsr forexp._ENTRY1

                lda #$C1                ; CMP
                jsr op2l

                lda arg3
                beq _5

                ldx #0
                ldy #16
                lda (frame),y
                cmp #vart+intt
                bcc _next3              ; only byte var

                lda #$AD                ; LDA addr16
                jsr forexp._ENTRY1

_4              lda #$E1                ; SBC
                jsr op2h

;   body
_5              lda arg3
                ror a                   ; get type

                lda #$B0                ; BCS, CARD
                bcc _6

                lda #$10                ; BPL, INT
_6              jsr push1

                ldy #21
                jsr framecd._ENTRY1
                jsr push1
                jsr popst

                jsr framecd

                ldy #0
                jsr pushjmp

;   save space for vars
                ldy #16
                jsr fmem

                ldy #11
                jsr fmem

;   handle symtab
                ldy #11
                lda (frame),y
                cmp #vart
                bcc _7

                lda symtab
                ldx symtab+1
                iny

                jsr framecd._ENTRY2

                lda #0
                tay
                sta (symtab),y

                lda #4
                jsr stincr

;   patch branch
_7              ldy #21
                jsr frameadr._ENTRY1
                jsr comprel

;   handle stmt list
                jsr recret.nxtstmt

;   handle incr
                pha                     ; save token

                ldy #8
                jsr fstk
                jsr copyst

                ldy #11
                jsr fstk

                lda #plusid
                jsr cgplus

                lda arg5
                beq _8

                jsr chasseq.chstkeq     ; see if INC

_8              pha

                jsr cgassign

                pla
                beq _9                  ; not INC
                                        ; see if we can branch to top of loop
                pla
                cmp #untilid
                beq _10                 ; can't go to top of loop

                pha

                clc
                ldy #4
                lda (frame),y
                sbc stkbase-9           ; see CGPlus
                bpl _9

                tax
                iny
                lda (frame),y
                sbc stkbase-8
                cmp #$FF
                bne _9                  ; yes, branch to top

                lda stkbase-9
                sta arg0
                lda stkbase-8
                sta arg1

                ldy #0
                txa
                sta (arg0),y

_9              pla
_10             sta token

                jmp whstmt._ENTRY2

                .endproc


;======================================
;
;======================================
forexp          .proc
                pha
                jsr getexp
                jsr genops._ENTRY1

                pla
                sta arg0

                jsr _1

                jmp popst


_1              lda arg1
                cmp #vart               ; see if const
                bcs _2

;   constant
                ldy #11
                lda (frame),y
                ldy arg0
                sta (frame),y

                ldy #2
                jsr loadi

                ldy arg0
                iny

                jmp framecd._ENTRY2

_2              ldy #8
                lda (frame),y
                and #7
                cmp #intt
                bmi _3

                lda arg1
                and #7
_3              ora #vart

                ldy #1
                sta (symtab),y
                sta arg2

                ldy arg0
                sta (frame),y

                inc arg0

                lda arg1
                bit arrmode             ; array?
                bne _4                  ;   yes

                bit tempmode            ; temp?
                bne _5                  ;   yes

;   var of some kind
_4              jsr load2l
                jsr _next1

                lda arg2
                cmp #vart+intt          ; see if byte
                bcc _XIT1

                jsr load2h

_next1          lda #$8D                ; STA data16

_ENTRY1         pha

                ldy arg0
                jsr frameadr._ENTRY1

                ldy arg0
                jsr framecd._ENTRY1

                iny
                sty arg0

                pla
                ldx arg4
                ldy arg5

                jmp push3

_XIT1           rts

;   temp
_5              ldy #4
                jsr loadi

                ldy #1
                jsr addcdsp

                ldy #3
                lda #0
                jsr _6

                lda arg2
                cmp #vart+intt          ; see if byte
                bcc _XIT1

                ldy #5
                lda #1
_6              jsr loadcd

                jmp _next1

                .endproc


;======================================
;
;======================================
fmem            lda (frame),y
                cmp #vart
                bcc forexp._XIT1         ; const

                sty arg2
                jsr getcdoff            ; save address for step

                ldy #2
                sta (symtab),y

                txa
                iny
                sta (symtab),y

                ldy arg2
                jsr _1

                ldy arg2
                lda (frame),y
                cmp #vart+intt
                bcc forexp._XIT1         ; byte only

                iny
                iny

_1              iny

                jsr frameadr._ENTRY1
                jsr filljmp._ENTRY1

                lda #1
                jmp codeincr


;======================================
;
;======================================
fstk            lda (frame),y
                sta token

                iny
                jsr frameadr._ENTRY1

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
                jsr framecd._ENTRY2

                jmp trashy

                .endproc


;--------------------------------------
; <return> _:= RETURN ((<exp>))
;--------------------------------------
retstmt         .proc
                lda #0
                jsr cprop

                and #7
                beq _1

                ora #tempt
                tay

                lda #args
                jsr storst

                ldx nxttoken
                cpx #lparen
                bne _err

                jsr getnext
                jsr getexp

                cmp #rparen
                bne _err

                jsr cgassign

_1              lda #$60
                jsr push1

                jmp recret.nxtstmt

_err            ldy #reter
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
                beq _1

;   not boolean
                jsr zerost

                lda #neid
                jsr codegen

_1              pla                     ; token value
                pha
                cmp #od
                bne _3

;   until <exp> od
                ldy #1
                jsr stkaddr
                beq _2                  ; no JMPs

;   JMP to JMP to top of loop
;   yek!, should be improved
                jsr filljmp             ; fill in jmps

_2              ldy #4
                jsr frameadr._ENTRY1
                bne _4                  ; [unc]

_3              jsr framecd

                ldy #1
                jsr stkaddr

_4              jsr pushjmp

;   fill in branch addresses
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

                jsr framecd._ENTRY2

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

_next1          jsr savrel
                jsr comprel
                jsr loadn
                bne _next1

                rts
                .endproc


;======================================
;   FrameAdr()
;======================================
frameadr        .proc
                ldy #2

_ENTRY1         lda (frame),y
                tax

                iny
                lda (frame),y
                tay

_ENTRY2         stx arg4
                sty arg5

                rts
                .endproc


;======================================
;   FrameCd()
;======================================
framecd         .proc
                ldy #2

_ENTRY1         lda qcode
                ldx qcode+1

_ENTRY2         sta (frame),y

                iny
                txa
                sta (frame),y

                rts
                .endproc


;======================================
;   FillJmp(,addr)
;======================================
filljmp         .proc
                jsr frameadr._ENTRY2

_ENTRY1         jsr saven
                jsr getcdoff
                jsr save4
                jsr loadn
                bne _ENTRY1

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
                beq _XIT                ; is this needed anymore?

_ENTRY1         sta arg4
                stx arg5

_XIT            rts
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
                bcc _err

                tya
                ldy #0

                jmp framecd._ENTRY2

_err            ldy #nster
                jmp splerr

                .endproc


;======================================
;   SetRel(,,offset)
;======================================
setrel          .proc
                jsr loadi

                jmp loadn._ENTRY1

                .endproc


;======================================
;
;======================================
savrel          .proc
                ldy #0
                ldx #0
                lda (arg4),y
                beq _2
                bpl _1

                dex                     ; sign extend

_1              clc
                adc arg4
                sta arg0

                txa
                adc arg5
_2              sta arg1

                rts
                .endproc


;======================================
;
;======================================
comprel         .proc
                lda qcode
                clc                     ; extra -1 for offset byte
_ENTRY1         sbc arg4

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
_next1          sta temps-1,x

                dex
                bne _next1              ; free temps

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

                ;[fall-through]


;======================================
;
;======================================

exp             .proc
                lda #0
                jsr pushop

                lda token               ; always non-zero
                sta op

_ENTRY1         jsr expend

                cmp #scolon
                bcc _7

                cmp #rparen
                bne _1

                ldx op
                bne _err

                jsr rollops

                cmp #lparen
                beq _next4

                lda token

                rts

_1              cmp #lparen
                bne _2

                ldx op
                bne _9

_err            jmp experr

_2              ldx op
                beq _10

                ldx #0
                stx op
                cmp #quote
                beq _13

_next1          ldx nxttoken
                cmp #record
                beq _4                  ; record

                cmp #typet
                bcc exp._err
                bne _3

_next2          jsr etype
                bne _next4              ; [unc]

_3              cmp #typet+8
                bcc _4                  ; field
                bne _5

                jsr etypea

                jmp _next4


;   record name or field
_4              cpx #period
                bne _next2

;   will generate error if falls through

_5              cmp #constt
                bcc exp._err            ; missing operand?

                cmp #arrayt
                bcs _6                  ; array or func

                cmp #undec
                beq _11

                cmp #constt+realt
                beq exp._err            ; real constant

_next3          jsr pushnext
                bne exp._ENTRY1         ; [unc]

_6              cmp #funct
                bcs _12

;   array
                jsr arrref

                jmp _next4

;   pop
_7              ldx op
                beq _8

                cmp #atid
                beq _8

                cmp #minusid
                bne exp._err

                lda #uminusid
                sta token

_8              tax
                lda prec-1,x
                sta op

                jsr rollops
                jsr pushop

                lda token
_9              jsr pushop
_next4          jsr getnext

                jmp exp._ENTRY1

_10             jsr rollops

                cmp #lparen
                beq _errParen

                lda token

                rts

;   undefined
_11             jsr getalias
                bne _next1              ; [unc]

;   proc
_12             bne _16

_next5          jsr procref
                bne _next3             ; [unc]

;   string
_13             lda #$4C                ; JMP around string
                jsr push1
                jsr getcdoff

                adc #3                  ; includes size byte
                bcc _14

                inx

                clc
_14             ldy #0
                adc (addr),y            ; size
                bcc _15

                inx

_15             jsr push2
                jsr copystr

                ldy #constt+strt
                jsr storst

                dec choff

                jsr getnext
                bne _next4              ; [unc]

_errParen       ldy #parer
                jmp splerr


;   qcode to handle function ref
_16             cmp #funct+8
                beq _next5

                cpx #lparen
                bne _next5

                lda #17
                jsr getframe

                ldy #16
                lda op
                sta (frame),y

;   save temps
                sty arg0

                lda #args+15
                sta arg1

_next6          dec arg0
                ldy arg0
                lda temps,y
                sta (frame),y
                beq _17

                lda #$A5                ; LDA addr
                ldx arg1
                ldy #$48                ; PHA
                jsr push3

                ldy arg0

_17             dec arg1
                cpy #2
                bne _next6

                lda temps
                bne experr              ; nested functions

                jsr clrtemps
                jsr pf                  ; call the function

;   restore temps
                ldy #1
                sty temps               ; flag result reg.
                sty arg0

                lda #args+2
                sta arg1

_next7          inc arg0
                ldy arg0
                lda (frame),y
                sta temps,y
                beq _18

                lda #$68                ; PLA
                ldx #$85                ; STA addr
                ldy arg1
                jsr push3

                ldy arg0

_18             inc arg1
                cpy #15
                bne _next7

                iny
                lda (frame),y
                sta op

                jsr freeframe

;   set result type
                ldy #0
                lda (stack),y
                and #7
                ora #tempt

                ldx #args
                jsr savecd.savstk

                jmp _next4

                .endproc


;--------------------------------------
;--------------------------------------
;:ExpReal       ldx vars
;               ldy vars+1
;               jsr FST0R
;               lda varsOff
;               ldx varsOff+1
;               jsr SST1
;               lda #6
;               jsr VarIncr
;               jmp _Exp7


;--------------------------------------
;
;--------------------------------------
experr          ldy #exper


;--------------------------------------
;
;--------------------------------------
experr2          jmp splerr


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

                ;[fall-through]


;======================================
;   StorST(addr16,token)
;======================================
storst          .proc
                sty token
                sta addr
                stx addr+1

                .endproc

                ;[fall-through]


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
                bne experr2

                txa
                and #7
                beq experr2

                sta token

;   get offset
                lda #1
                jmp getprop

                .endproc


;======================================
;
;======================================
etype           .proc
                cpx #period
                beq _1

;   record addr, size or field offset
;   A reg must be nonzero before call
                jmp arrref.arrconst

_1              jsr etypep              ; set type

;   get var address
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
                beq _1

                jmp arrref.arrvar

_1              jsr etypep              ; set type

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
                bcc _1

_ENTRY1         jsr getargs             ; A#0, no arg types

                ldy #constt+cardt       ; sys proc
_1              sty token

                rts
                .endproc


;======================================
;   CopyST()
;======================================
copyst          .proc
                lda stack
                ldx stack+1

                jsr loadn._ENTRY1
                jsr pushst

                ldy #6
_next1          lda (arg4),y
                sta (stack),y

                dey
                bpl _next1

                rts
                .endproc


;======================================
;   RollOps()
;======================================
rollops         .proc
                jsr popop
                beq _XIT

                cmp #lparen
                beq _XIT

                tax
                ldy prec-1,x
                cpy op
                bcc _XIT                ; prec < op
                                        ; check for simple add
                ldy op                  ; see if end of exp
                bne _4

                ldy stack
                cpy #<(stkbase-21)
                bne _4

                cpx #plusid
                beq _1

                cpx #rshid
                beq _1

                cpx #lshid
                bne _4

_1              jsr popop               ; see if last op
                bne _3

;   check for increament
;   We know at least this is an assignment or an array subscript.
;   If ChStkEq in CGPlus is true then this is an assignment.
                txa
                cmp #plusid
                bne _2

                jsr cgplus

                lda #0
                rts

_2              jsr cgsh

                lda #0
                rts

_3              jsr pushop

                txa
_4              jsr codegen
                jmp rollops

_XIT            rts
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

prec            .byte 5,5,6,6,2,3
                .byte 4,4,4,4,4,4
                .byte 6,1,6,6,7,7


;======================================
;    Code generation
;======================================

;======================================
;   GenOps(op)
;======================================
genops          .proc
                sta arg0

_ENTRY1         ldy #0
                lda (stack),y
                sta arg1

                and #7
                tax
                lda vartype-1,x
                sta arg3

                asl
                asl
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
                beq codegen._ENTRY1

_next1          lda stkbase-20
                beq _XIT                ; no shift!

                cmp #5
                bcs codegen._ENTRY1         ; too large a shift

;   whew!, we can now shift it
                ldy arg0
                cpy #rshid
                beq _1                  ; right shift

                lda #$06                ; ASL
                jsr loadx.op1l

                lda arg4
                beq _3

                lda #$26                ; ROL
                jsr op1h

                jmp _3

_1              lda #$46                ; LSR
                ldx arg4
                beq _2

                jsr op1h

                lda #$66                ; ROR
_2              jsr loadx.op1l

_3              dec stkbase-20
                bne _next1

_XIT            jmp popst

                .endproc


;======================================
;   CodeGen(op)
;======================================
codegen         .proc
                jsr genops
_ENTRY1         jsr cgend

                lda arg0
                asl
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
                beq codegen._ENTRY1

                lda stkbase-20
                cmp #1                  ; see if const = 1
                bne codegen._ENTRY1     ;   no

;   whew!, we can now increment it
                lda #$E6                ; INC
                jsr loadx.op1l

                lda arg4
                beq _XIT                ; byte var

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

_XIT            jmp popst

                .endproc


;--------------------------------------
;
;--------------------------------------
cgexperr        jmp experr


;======================================
;   CGAssign()
;======================================
cgassign        .proc
                lda #0
                jsr genops
                jsr cgend
                jsr chasseq.chstkeq     ; see if INC
                bne _ENTRY5             ;   yes, just return

                lda arg1
                bpl _2                  ; cond. exp. of record

                bit tempmode            ; rhs temp?
                bne _5                  ;   yes

                cmp #vart               ; const?
                bcs _ENTRY1             ;   no

;   rhs constant
                lda arg2
                cmp #arrayt             ; simple var
                bcs _ENTRY1

;   if STY addr16,X was supported
;               bcc _CGC0               ;   yes
;               bit tempMode            ; lhs temp?
;               bne _CGAV               ;   yes, large array

                ldx arg4
                beq _1                  ; byte

                ldy #2
                lda (stack),y
                cmp #2
                bcs _ENTRY2

                sta arg12
                jsr loady

                lda #$84                ; STY
                jsr op1h

_1              ldy #1
                lda (stack),y
                cmp #2
                bcs _3

                sta arg12

                jsr loady

                lda #$84                ; STY
                bne _4                  ; [unc]

_2              cmp #typet
                bcc cgexperr            ; cond. exp.

;   rhs var
_ENTRY1         ldx arg4                ; lhs type=byte?
                beq _3                  ;   yes

                ; cpx #3                ; lhs = int?
                ; bne _CGAVI            ;   yes

;   lhs type = real
                ; jmp AssErr

_ENTRY2         jsr load2h

_ENTRY3         lda #$81                ; STA
                jsr op1h
_3              jsr load2l

_ENTRY4         lda #$81                ; STA
_4              jsr loadx.op1l
_ENTRY5         jsr popst

                jmp popst

_5              and #$10                ; rhs array?
                bne cgassign._ENTRY1    ;   yes

;   special case for arg0
                ldy #1
                lda (stack),y
                cmp #args
                beq cgassign._ENTRY1    ; function return value

                lda arg4                ; lhs byte?
                beq _10                 ;   yes

;   int/card temp
                lda arg2
                and #$10                ; array?
                bne _9                  ;   yes

                lda arg3
                bne _6

;   rhs type is BYTE
                jsr load2h              ; generate LDA #0 instr.

                ldy #5
                jsr savecd

_6              ldy #4
                jsr loadi

                ldy #8
                lda arg2
                and #$60                ; lhs proc or temp(argument)?
                bne _7                  ;   yes

                jsr stkpz
                beq _8                  ; page zero

                sty arg13

                ldy #1
                lda #$8D                ; STA addr16
                jsr insrt3._ENTRY1      ; insert STA data16

                lda #1
_next1          ldy #5
                jsr loadcd

                lda #$81                ; STA
                jsr op1h

                jmp _ENTRY5

_7              and #$40                ; proc?
                bne cgassign._ENTRY2    ;   yes, punt(not the best qcode)

                jsr stkaddr             ; temp (proc argument)

_8              ldy #1
                txa
                sta (arg14),y

                lda #0
                beq _next1

;   temp array
_9              lda arg3
                beq cgassign._ENTRY2

                ldy #5
                jsr ldcdz
                bne cgassign._ENTRY3    ; [unc]

_10             ldy #3
                jsr loadcd

                lda arg3                ; see if rhs BYTE or CHAR
                beq cgassign._ENTRY4      ;   yes

                jsr trashy              ; in case INT ARRAY in rhs

;   oh if I only had more qcode space!
;   could handle Y, see _OpA in CGU
                bne cgassign._ENTRY4      ; [unc]

                .endproc


;======================================
;   chasseq (mode)
;======================================
chasseq         .proc
                jsr genops

                lda arg1
                cmp #vart               ; see if const
                bcs chstkeq._ENTRY1     ;   no

                lda stkbase-19          ; see if byte
                bne chstkeq._ENTRY1     ;   no
                                        ; see if left and right-hand are =


;======================================
;   JMP ChStkEq
;======================================
chstkeq         ldx #2
                lda stkbase-7
                and #$F8
                cmp #$B0                ; large array?
                beq _ENTRY1             ;   yes, can't INC or shift

                cmp #arrayt+8           ; small array?
                bne _next1              ;   no

                ldx #5
_next1          lda stkbase-14,x
                cmp stkbase-7,x
                bne _ENTRY1

                dex
                bpl _next1

                rts

_ENTRY1         lda #0

                rts
                .endproc


;======================================
;   CGAdd()
;======================================
cgadd           .proc
                lda arg1
                bpl _3

                ora arg2
                cmp #vart               ; see if both ops consts
                bcs _3                  ;   not const

                lda arg7
                and #7
                ora #constt
                sta arg7

                ldy #8
                ldx arg8
                bne _1                  ; subtract constants

;   add constants
                clc
                lda (stack),y

                ldy #1
                adc (stack),y
                sta arg9

                ldy #9
                lda (stack),y
                ldy #2
                adc (stack),y

                jmp _2

_1              sec
                lda (stack),y

                ldy #1
                sbc (stack),y
                sta arg9

                ldy #9
                lda (stack),y
                ldy #2
                sbc (stack),y

_2              tax
                beq _4

                lda arg5
                bne _4

                lda #constt+intt
                bne _ENTRY4             ; [unc]

;   normal add or sub.
_3              ldx arg8
                lda cgopscd,x

                jsr push1

_ENTRY1         jsr gettemps
                jsr loadx.load1l
                jsr opcd1
                jsr op2l
                jsr stempl

                lda arg5
                beq _ENTRY3

                jsr load1h
                jsr opcd1
                jsr op2h

_ENTRY2         jsr stemph

_ENTRY3         ldx #0
_4              lda arg7
_ENTRY4         ldy #7
                sta (stack),y

                iny
                lda arg9

                jsr savecd.savstk

_XIT            jmp popst

                .endproc


;======================================
;
;======================================
cgshift         .proc
                lda arg5                ; byte ?
                bne cgmd                ;   no

                lda arg1
                cmp #vart               ; see if constant
                bcs cgmd                ;   no

                ldy #1
                lda (stack),y
                beq cgadd._XIT          ; ignore shift

                cmp #8                  ; shift high  7
                bcc _1                  ;   no

                ldx #0
                stx arg9

                lda #constt+bytet
                bne cgadd._ENTRY4        ; [unc]

_1              sta arg1

                lda #$0A                ; ASL A
                ldx arg8
                beq _2

                lda #$4A                ; LSR A
_2              sta arg3

                jsr gettemps
                jsr loadx.load1l

_next1          lda arg3                ; shift Op

                jsr push1

                dec arg1
                bne _next1

                jsr stempl

                jmp cgadd._ENTRY3

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

                .endproc

                ;[fall-through]


;======================================
;
;======================================
cgdiv           .proc
                jsr load2h

                lda #$85                ; STA AFcur+1
                ldx #afcur+1

                jsr push2

                .endproc

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
cgmd            .proc
                jsr gettemps
                jsr load2l

                lda #$85                ; STA AFcur
                ldx #afcur

                jsr push2

                lda arg4
                beq _1                  ; first op byte

                jsr load1h

                lda #$AA                ; TAX
                jsr push1

_1              jsr loadx.load1l

                lda arg4
                bne _2

                lda #$A2                ; LDX #0
                ldx #0
                jsr push2

_2              ldx arg8

                jsr jsrtable
                jsr stempl
                jsr trashy

                lda arg5
                beq _XIT

                lda #$8A                ; TXA
                jsr push1

                jsr stemph

_XIT            jmp cgadd._ENTRY3

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
                beq _1                  ; no JMPs

                jsr filljmp

_1              ldy #5
                jsr ldcdz

                ldy #11                 ; link T2 to T1
                jsr setrel

_next1          jsr savrel
                jsr loadn
                bne _next1              ; get end of T1

                ldy #3
                lda (stack),y

                sec
                jsr comprel._ENTRY1     ; patch in T2
                beq cgand._ENTRY1       ; [unc]

                .endproc


;======================================
;   CGAnd()
;======================================
cgand           .proc
                jsr chkcond

                lda stack
                ldx stack+1
                jsr loadn._ENTRY1

_next1          jsr saven               ; patch addresses

                clc
                lda arg0
                adc #3
                sta (arg4),y

                iny
                lda (arg4),y
                adc #0
                sta (arg4),y

                jsr loadn
                bne _next1

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
                bcc _1

                inx

_1              ldy #10
                jsr savecd.savstk

_ENTRY1         ldy #2
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
                bne _1

                cmp arg2
                bne conderr

                rts

_1              pla
                pla

                jmp cgadd._ENTRY1

                .endproc


;--------------------------------------
;
;--------------------------------------
conderr         .proc
                ldy #cnder
                jmp splerr

                .endproc


;======================================
;   CGEq()
;======================================
cgeq            .proc
cgne            jsr loadx.load1l
                jsr chkzero
                beq _1

                lda #$41                ; EOR
                jsr op2l

_1              lda arg5
                beq _ENTRY1

                jsr chkzero

                php                     ; save status
                beq _2

                lda #$D0                ; BNE
                jsr pushtrue

_2              lda #$01                ; ORA
                jsr op1h

                plp                     ; ChkZero() status
                beq _ENTRY1

                lda #$41                ; EOR
                jsr op2h

                ldy #11
                jsr fillbr

_ENTRY1         jsr opcd1
                jsr pushtrue            ; sets arg9 to zero

                lda #condt
                sta arg7

                ldy #12
                jsr savecd

                jmp cgadd._ENTRY3

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
                beq cgeq.cgne._ENTRY1

                jsr load1h

                lda #$E1                ; SBC
                jsr op2h
                bcs cgeq.cgne._ENTRY1   ; [unc], see CodeIncr

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
                beq cgeq.cgne._ENTRY1

                jsr load2h

                lda #$E1                ; SBC
                jsr op1h
                bcs cgeq.cgne._ENTRY1   ; [unc], see CodeIncr

                .endproc


;======================================
;   CGUM()
;======================================
cgum            .proc
                lda arg1
                and #$78
                bne _1                  ; not constant

;   constant, just negate it
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

_1              jsr copyst

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
                bcc _1

                cmp #arrayt+8
                bcs _err

                ldy #1
                jsr stkp

                iny
                jsr savecd.savstk

_next1          ldy #0
                lda #constt+cardt
                sta (stack),y

                rts

_1              and #$F8
                cmp #typet              ; check for record field
                beq _next1
                                        ; constant or cond. exp. (error)
_err            jmp experr

                .endproc

;--------------------------------------
;--------------------------------------

                .byte 0                 ; for records!
vartype         .byte 0,0,1,2,2,3

; moved to CGU
;outtype        .byte $82,3,$84,realT
;               .byte 3,3,$84,realT
;               .byte $84,$84,$84,realT
;               .byte realT,realT,realT,realT

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
                .word cgdiv             ; divide
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
                .word cgdiv             ; remainder
                .byte 3
                .word cgadd._ENTRY1     ; XOR
                .byte 0
                .word cgshift           ; LSH
                .byte 2
                .word cgshift           ; RSH
                .byte 12
                .word cgum              ; unary minus
                .byte 0
                .word cgat
