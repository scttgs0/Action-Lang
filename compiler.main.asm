
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: compiler.main.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


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
                bne _1                  ; no name

                jsr lexget._ENTRY1
                jmp _next1

_1              lda top1
                sta top+1

                jsr chkcur._ENTRY1
                beq _XIT1               ; no program !

                jsr GetNext
_next1          jsr GetNext


;--------------------------------------
; <program> _:= <module list> MODULE <module>
;                 | (MODULE) <module>
; <module> _:= (<dcl list>) (<segment list>)
;--------------------------------------
                jsr declare
                jsr Segment

                cmp #tokMOD             ; another module ?
                beq _next1              ; yes

                cmp #tokEOF
                bne _2

                lda #1                  ; save run address
                jsr cprop

                sta INITAD
                stx INITAD+1

;   insert return, just in case
_next2          lda #$60                ; RTS
                jsr Push1

;   get QCODE size
                sec
                lda QCODE
                sbc codebase
                sta codesize
                lda QCODE+1
                sbc codebase+1
                sta codesize+1

;   patch array addresses
                lda arrayptr+1

                ; ora arrayPtr
                beq _XIT1

_next3          ldy #1
                lda (arrayptr),Y
                sta arg1

                dey
                lda (arrayptr),Y
                sta arg0

                jsr getcdoff

                sta (arrayptr),Y

                txa
                iny
                sta (arrayptr),Y

                clc
                iny
                lda QCODE
                adc (arrayptr),Y
                sta QCODE

                iny
                lda QCODE+1
                adc (arrayptr),Y
                sta QCODE+1

                lda arg0
                sta arrayptr
                lda arg1
                sta arrayptr+1
                bne _next3

                ; lda arrayPtr
                ; bne _SPL3

                lda QCODE
                cmp MEMTOP
                lda QCODE+1
                sbc MEMTOP+1
                bcs _err2

_XIT1           rts

_2              jsr getcdoff            ; no main PROC

                sta INITAD
                stx INITAD+1
                jsr stmtlist

                cmp #tokEOF
                beq _next2

;EndErr         ldy #0
;               sty $2E3                ; zap run address

_err            ldy #endERR
                jmp errFI

_err2           jmp codeincr.cderr      ; out of QCODE space

                ;.endproc


;======================================
;    Declaration processing
;--------------------------------------
; <dcl list> _:= <dcl list> <dcl> | <dcl>
; <dcl> _:= <simple dcl> | <array dcl> | <def dcl>
;======================================
cderr           lda #0                  ; reset QCODE before err
                tay
                jsr LoadCd

_errDeclaration jmp errDefine

_type           lda #+tokRECORD-(tokVAR_t-tokCHAR)-1
                sta type

                jsr makeentry

                lda addr
                ldx addr+1
                ldy #2
                jsr SaveCd._saveStack

                ldy #0
                jsr SaveCd
                jsr GetNext

                cmp #tokEQU
                bne _errDeclaration

                jsr GetNext

                cmp #tokLBracket
                bne _errDeclaration

                sec
                lda #0
                sbc codeoff
                sta QCODE
                lda #0
                sbc codeoff+1
                sta QCODE+1

                jsr GetNext

_next1          cmp #tokCHAR
                bcc cderr

                cmp #tokDEFINE
                bcs cderr

                tax
                clc
                adc #+tokTYPE_t-tokVAR_t
                sta type

                lda varsize-tokCHAR,X
                sta afcur

_next2          jsr makeentry

                lda afcur
                jsr codeincr
                jsr GetNext

                cmp #tokComma
                beq _next2

                cmp #tokRBracket
                bne _next1

                ldy #2
                jsr StkP

                ldx QCODE+1
                bne cderr

                lda QCODE
                ldy #0
                jsr storprops

                lda #0
                tay
                jsr LoadCd
                jsr GetNext


;======================================
;
;======================================
declare         jsr jt_dclend

                cmp #tokCHAR
                bcs _1

_XIT1           rts

_1              cmp #tokTYPE
                beq cderr._type

                cmp #tokRECORD
                bne _2

;   record dcl.
                lda #0
                jsr getprop

                stx afcur

                ldx nxttoken
                lda #tokTYPE_t-(tokVAR_t-tokCHAR)-1
                sta type
                bra _3

_2              cmp #tokDEFINE
                beq _define
                bcs _XIT1

                sta type

                tax
                ldy varsize-tokCHAR,X
                sty afcur

                ldx nxttoken
                cpx #tokFUNC
                beq _XIT1

                cpx #tokARRAY
                beq arrDeclaration

_3              cpx #tokPOINTER
                bne _next1

                ldy #0
                sty afcur
                bra arrDeclaration


;--------------------------------------
; <simple dcl> _:= <type> <id eq list>
; <id eq list> _:= <id eq list> , <id eq> | <id eq>
; <id eq> _:= <id> (= <constant>)
;--------------------------------------
_next1          jsr makeentry

                ldx param
                beq _4

                jsr params

                ldx param
                bpl _5
                bra _6

_4              lda nxttoken
                cmp #tokEQU
                bne _5

                jsr ideq

                iny
                jsr storprops

                jsr GetNext
                bne _6

_5              lda afcur
                jsr codeincr

_6              jsr GetNext

                cmp #tokComma
_next2          beq _next1

                jmp declare


;--------------------------------------
; <def dcl> _:= DEFINE <def list>
; <def list> _:= <def list> , <def> | <def>
; <def> _:= <id> = <str const>
;--------------------------------------
_define         jsr makeentry

                ldy #0
                lda #tokDef
                sta (props),Y

                jsr GetNext

                cmp #tokEQU
                bne errDefine

                lda nxttoken
                cmp #tokQuote
                bne errDefine

                ldy #0
                lda (symtab),Y
                clc
                adc #2                  ; real size + EOL

                jsr stincr
                jsr GetNext             ; string itself
                jsr GetNext             ; dummy string
                jsr GetNext

                cmp #tokComma
                bne _next2
                bra _define


;--------------------------------------
;
;--------------------------------------
errDefine       ldy #declERR
                jmp splerr


;--------------------------------------
; <array dcl> _:= <type> ARRAY <array list>
; <array list> _:= <array list> , <array> | <array>
; <array> _:= <id> ((<constant>)) (= <constant>)
;--------------------------------------
arrDeclaration  clc
                adc #8
                sta type

                jsr GetNext
_next1          jsr makeentry

                lda #2
                sta numargs             ; variable space

                ldx param
                bne _2

                lda nxttoken
                ldx afcur
                beq _3                  ; no size for pointers

                cmp #tokLParen
                bne _3

                lda #4
                sta numargs

                lda arrayptr
                ldx arrayptr+1
                ldy #0

                jsr storevar
                jsr getarsz
                jsr GetNext

;   check for small byte array

                ldy #2
                lda #0
                cmp (QCODE),Y

                iny
                lda #1
                sbc (QCODE),Y
                bcs _6                  ; size <= 256

_next2          jsr GetNext

                cmp #tokRParen
                bne errDefine

                lda nxttoken
                cmp #tokEQU
                beq _4

_next3          lda numargs
                bne _1

;   small array
                ldy #2
                lda (QCODE),Y
                bne _next4

                inc QCODE+1
                bra _next5

;   array var

_1              cmp #4
                bmi _next4

;   large array with memory
                ldx QCODE
                stx arrayptr
                ldx QCODE+1
                stx arrayptr+1

_next4          jsr codeincr

_next5          jsr GetNext

                cmp #tokComma
                beq _next1

                jmp declare

_2              jsr params

                ldx param
                bpl _next3
                bmi _next5

_3              cmp #tokEQU
                bne _next3

_4              jsr ideq

                ldy numargs
                beq _5

                ldy #0
                jsr storevar
                jsr getcdoff

_5              ldy #1
                jsr storprops
                jsr GetNext

                lda numargs
                jmp _next4

_6              ldy #0
                lda (props),Y
                cmp #tokARRAY_t+tokINT_t
                bcs _next2

;   small byte array
                sty numargs
                ora #8
                sta (props),Y

                iny
                jsr getcdoff
                jsr storprops
                bra _next2


;--------------------------------------
;
;--------------------------------------
chkparam        .proc
                ldx param

_ENTRY1         beq makeentry._err

                pla
                pla

                jmp GetNext

                .endproc


;======================================
;   MakeEntry()
;======================================
makeentry       .proc
                lda nxttoken
                cmp #tokUNDEC
                beq _3
                bcs _1                  ; var of some kind

                cmp #tokRECORD
                beq _1

                cmp #tokTYPE_t
                bcc chkparam

                cmp #tokEOF
                bcs chkparam

_1              lda qglobal
                beq chkparam._ENTRY1

                jsr gnlocal

                cmp #tokUNDEC
                beq _2

_err            jmp errDefine

_2              sta nxttoken

_3              lda #0
                jsr nxtprop

                sec
                lda #tokVAR_t-tokCHAR
                adc type
                sta (props),Y           ; type

                and #7
                tax
                lda vartype-1,X
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
                sta (props),Y

                txa
                iny
                sta (props),Y

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
                lda (props),Y           ; get var type
                pha

                lda #3
                jsr cprop

                cmp #8
                bcs _err

                adc #1
                sta (props),Y
                tay

;   see if time to update gbase
                ldx nxttoken
                cpx #tokRParen
                bne _1

;   see AMPL.SEG
                clc
                adc gbase
                sta gbase
                bcc _1

                inc gbase+1

_1              pla
                bmi _2

                cmp #tokTYPE_t+8
                beq _3

_err            jmp Segment._argerr

;:Par1          cmp #varT+realT
;               beq PErr

_2              cmp #tokVAR_t+tokINT_t
                bcc _4                 ; one-byte arg

;   two-byte arg
_3              and #$1F
                inc argbytes

_4              and #$9F
                inc argbytes
                sta (props),Y

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
                jsr jt_smtend

                cmp #tokLBracket
                bne _next2

;   machine QCODE block
                jsr TrashY

_next1          ldx nxttoken
                cpx #tokRBracket
                beq _3

                jsr mnum

                cpx #0
                beq _1

                jsr Push2               ; 2-byte number
                bra _2

_1              jsr Push1               ; single byte
_2              jsr GetNext
                bra _next1

_3              jsr GetNext

                jmp recret.nxtstmt

_next2          ldx nxttoken
                cmp #tokVAR_t+tokCHAR_t
                bcc _4

                cmp #tokFUNC_t
                bcc assign

;   routine reference
                cpx #tokLParen
                beq call

                jsr procref
                bra assign._ENTRY1

_4              cmp #tokUNDEC
                bne _5

                jsr GetAlias
                bra _next2

_5              cmp #tokTYPE_t
                bne _6

                jsr etype
                jmp arassign._ENTRY1

_6              cmp #tokTYPE_t+8
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
                cmp #tokARRAY_t
                bcs arassign

_ENTRY1         jsr pushnext

_ENTRY2         eor #tokEQU
                bne errAssign

                jsr pushop              ; push 0 on op stack
                jsr GetNext

                sta op
                cmp #tokEQU
                bne _3

                lda #0
                sta op

                jsr copyst

;   check for temps
                iny
                and #$F8
                cmp #tokARRAY_t+8
                bne _1

                ldy #3
                lda (stack),Y
_1              and #$20
                beq _2

                iny
                lda (stack),Y
                tax
;   incr temps
                cpx #args               ;
                bcc _2                  ; are these 4 instr.

                cpx #args+16            ; needed?
                bcs _2                  ;

                inc temps-args,X

_2              jsr GetNext
_3              jsr exp._ENTRY1
                jsr cgassign

                jmp stmtlist

                .endproc


;--------------------------------------
;
;--------------------------------------
errAssign       .proc
                ldy #assgnERR
                jmp splerr

                .endproc


;--------------------------------------
; <array assign> _:= <id> ( <exp> ) = <exp>
;--------------------------------------
arassign        .proc
                jsr ArrRef

_ENTRY1         ldy #0
                lda (stack),Y
                bpl _1                  ; record element

                cmp #tokVAR_t
                bcc errAssign           ; const

_1              jsr GetNext
                bra assign._ENTRY2

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
                sta (frame),Y

_if             jsr condexp

                cmp #tokTHEN
                bne thnerr

;   save current Y
                lda cury
                ldy #6
                sta (frame),Y

                jsr recret.nxtstmt

;   restore Y
                tax
                ldy #6
                lda (frame),Y
                sta cury

                txa
                cmp #tokELSEIF
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

_else           cmp #tokELSE
                bne _fi

                jsr frameadr
                jsr framecd

                ldy #0                  ; flag as end of list
                jsr pushjmp
                jsr filljmp._ENTRY1
                jsr recret.nxtstmt

_fi             ldy #fiERR
                cmp #tokFI
                bne errFI

                ldy #4
                jsr frameadr._ENTRY1
                beq _ifnoelse           ; if no ELSEIF

                jsr filljmp._ENTRY1

_ifnoelse       jsr TrashY
                jsr frameadr

                beq recret              ; in case of DO loop
                jsr filljmp._ENTRY1

                .endproc


;--------------------------------------
;    RecRet() pops stack and returns
;--------------------------------------
recret          .proc
                jsr freeframe

nxtstmt         jsr GetNext
                jmp stmtlist

                .endproc


;--------------------------------------
;    pops stack
;--------------------------------------
freeframe       .proc
                ldy #0
                lda (frame),Y
                tax

                iny
                lda (frame),Y
                stx frame
                sta frame+1

                rts
                .endproc


;--------------------------------------
;
;--------------------------------------
thnerr          ldy #thenERR

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
                sta (frame),Y
                bra whstmt._ENTRY1

                .endproc


;--------------------------------------
;
;--------------------------------------
errFI           .proc
                cmp #tokUNDEC
                bne sterr

                jmp mnum._varerr

                .endproc


;--------------------------------------
; <while> _:= WHILE <exp> <do>
;--------------------------------------
whstmt          .proc
                jsr doinit
                jsr condexp

                ldy #doERR
                cmp #tokDO
                bne sterr

_ENTRY1         jsr recret.nxtstmt

_ENTRY2         cmp #tokUNTIL
                bne _1

                jsr condexp
                bra _2

_1              ldy #4
                jsr frameadr._ENTRY1
                jsr pushjmp

                lda token
_2              ldy #odERR
                cmp #tokOD
                bne errFI

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
                ldy #exitERR
                ldx whaddr+1
                beq sterr

                ldy #2                  ; get pointer to EXIT list
                lda (whaddr),Y
                tax

                iny
                lda (whaddr),Y
                pha

                lda QCODE+1             ; link in JMP for EXIT
                sta (whaddr),Y

                lda QCODE
                dey
                sta (whaddr),Y

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
                bne errFI        ; [unc]


;======================================
;
;======================================
forstmt         .proc
                lda #23
                jsr getframe
                jsr whadr

;   make sure simple var for index
                jsr GetNext

                cmp #tokVAR_t+tokCHAR_t
                bcc forerror

                cmp #tokARRAY_t
                bcc _1

                cmp #tokARRAY_t+tokREAL_t
                bcs forerror

                lda #tokVAR_t+tokCARD_t
                sta token

;   get initial value
_1              ldy #8
                sta (frame),Y

                iny
                lda addr
                ldx addr+1

                jsr framecd._ENTRY2
                jsr pushnext

                cmp #tokEQU
                bne forerror

                jsr getexp
                jsr cgassign

;   set default STEP size
                lda token
                cmp #tokTO
_next1          bne forerror

                lda #0
                ldx #9
                ldy #12
_next2          sta (frame),Y

                iny
                dex
                bne _next2

                ldy #8
                jsr fstk

                lda token
                and #7
                ora #tokCONST_t

                ldx #1
                ldy #11
                jsr framecd._ENTRY2

;   get ending value
                lda #16
                jsr forexp

;   get step value
                lda token
                cmp #tokSTEP
                bne _2

                lda #11
                jsr forexp

                lda token
_2              cmp #tokDO
                bne _next1

;   generate end test
                jsr getcdoff

                ldy #4
                jsr framecd._ENTRY2
                jsr TrashY
                jsr genops._ENTRY1

                ldy #16
                lda (frame),Y
                cmp #tokVAR_t
                bcs _3                  ; temp variable

;   constant
                iny
                lda (frame),Y
                tax

                lda #$A9
                jsr Push2               ; LDA #low

                lda #$C1                ; CMP
                jsr Op2L

                lda arg3
                beq _5

                ldy #18
                lda (frame),Y
                tax

_next3          lda #$A9
                jsr Push2               ; LDA #high

                jmp _4

_3              ldy #17
                sty arg0

                lda #$AD                ; LDA addr16
                jsr forexp._ENTRY1

                lda #$C1                ; CMP
                jsr Op2L

                lda arg3
                beq _5

                ldx #0
                ldy #16
                lda (frame),Y
                cmp #tokVAR_t+tokINT_t
                bcc _next3              ; only byte var

                lda #$AD                ; LDA addr16
                jsr forexp._ENTRY1

_4              lda #$E1                ; SBC
                jsr Op2H

;   body
_5              lda arg3
                ror a                   ; get type

                lda #$B0                ; BCS, CARD
                bcc _6

                lda #$10                ; BPL, INT
_6              jsr Push1

                ldy #21
                jsr framecd._ENTRY1
                jsr Push1
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
                lda (frame),Y
                cmp #tokVAR_t
                bcc _7

                lda symtab
                ldx symtab+1
                iny

                jsr framecd._ENTRY2

                lda #0
                tay
                sta (symtab),Y

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

                lda #tokPLUS
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
                cmp #tokUNTIL
                beq _10                 ; can't go to top of loop

                pha

                clc
                ldy #4
                lda (frame),Y
                sbc stkbase-9           ; see CGPlus
                bpl _9

                tax
                iny
                lda (frame),Y
                sbc stkbase-8
                cmp #$FF
                bne _9                  ; yes, branch to top

                lda stkbase-9
                sta arg0
                lda stkbase-8
                sta arg1

                ldy #0
                txa
                sta (arg0),Y

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
                cmp #tokVAR_t               ; see if const
                bcs _2

;   constant
                ldy #11
                lda (frame),Y
                ldy arg0
                sta (frame),Y

                ldy #2
                jsr LoadI

                ldy arg0
                iny

                jmp framecd._ENTRY2

_2              ldy #8
                lda (frame),Y
                and #7
                cmp #tokINT_t
                bmi _3

                lda arg1
                and #7
_3              ora #tokVAR_t

                ldy #1
                sta (symtab),Y
                sta arg2

                ldy arg0
                sta (frame),Y

                inc arg0

                lda arg1
                bit arrmode             ; array?
                bne _4                  ;   yes

                bit tempmode            ; temp?
                bne _5                  ;   yes

;   var of some kind
_4              jsr Load2L
                jsr _next1

                lda arg2
                cmp #tokVAR_t+tokINT_t  ; see if byte
                bcc _XIT1

                jsr Load2H

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

                jmp Push3

_XIT1           rts

;   temp
_5              ldy #4
                jsr LoadI

                ldy #1
                jsr AddCdSp

                ldy #3
                lda #0
                jsr _6

                lda arg2
                cmp #tokVAR_t+tokINT_t          ; see if byte
                bcc _XIT1

                ldy #5
                lda #1
_6              jsr LoadCd

                jmp _next1

                .endproc


;======================================
;
;======================================
fmem            lda (frame),Y
                cmp #tokVAR_t
                bcc forexp._XIT1        ; const

                sty arg2
                jsr getcdoff            ; save address for step

                ldy #2
                sta (symtab),Y

                txa
                iny
                sta (symtab),Y

                ldy arg2
                jsr _1

                ldy arg2
                lda (frame),Y
                cmp #tokVAR_t+tokINT_t
                bcc forexp._XIT1        ; byte only

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
fstk            lda (frame),Y
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

                jmp TrashY

                .endproc


;--------------------------------------
; <return> _:= RETURN ((<exp>))
;--------------------------------------
retstmt         .proc
                lda #0
                jsr cprop

                and #7
                beq _1

                ora #tokTEMP_t
                tay

                lda #args
                jsr storst

                ldx nxttoken
                cpx #tokLParen
                bne _err

                jsr GetNext
                jsr getexp

                cmp #tokRParen
                bne _err

                jsr cgassign

_1              lda #$60
                jsr Push1

                jmp recret.nxtstmt

_err            ldy #retrnERR
                jmp splerr

                .endproc


;======================================
;   CondExp()
;======================================
condexp         .proc
                jsr getexp

                pha

                ldy #0
                lda (stack),Y
                cmp #tokCOND_t
                beq _1

;   not boolean
                jsr zerost

                lda #tokNOTEQU
                jsr codegen

_1              pla                     ; token value
                pha
                cmp #tokOD
                bne _3

;   until <exp> od
                ldy #1
                jsr StkAddr
                beq _2                  ; no JMPs

;   JMP to JMP to top of loop
;   yek!, should be improved
                jsr filljmp             ; fill in jmps

_2              ldy #4
                jsr frameadr._ENTRY1
                bra _4

_3              jsr framecd

                ldy #1
                jsr StkAddr

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

_ENTRY1         lda (frame),Y
                tax

                iny
                lda (frame),Y
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

_ENTRY1         lda QCODE
                ldx QCODE+1

_ENTRY2         sta (frame),Y

                iny
                txa
                sta (frame),Y

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
                sta (arg4),Y

                iny
                txa
                sta (arg4),Y

                rts
                .endproc


;======================================
;   SaveN()
;======================================
saven           .proc
                ldy #1
                lda (arg4),Y
                sta arg0

                iny
                lda (arg4),Y
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
                bcc _err

                tya
                ldy #0

                jmp framecd._ENTRY2

_err            ldy #nestERR
                jmp splerr

                .endproc


;======================================
;   SetRel(,,offset)
;======================================
setrel          .proc
                jsr LoadI

                jmp loadn._ENTRY1

                .endproc


;======================================
;
;======================================
savrel          .proc
                ldy #0
                ldx #0
                lda (arg4),Y
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
                lda QCODE
                clc                     ; extra -1 for offset byte
_ENTRY1         sbc arg4

                ldy #0
                sta (arg4),Y

                rts
                .endproc


;======================================
;
;======================================
clrtemps        .proc
                lda #0
                ldx #16
_next1          sta temps-1,X

                dex
                bne _next1              ; free temps

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
            .if ZAPRAM
                inc cgopscd
            .else
                nop
                nop
                nop
            .endif

                jsr GetNext

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

_ENTRY1         jsr jt_expend

                cmp #tokSColon
                bcc _7

                cmp #tokRParen
                bne _1

                ldx op
                bne _err

                jsr rollops

                cmp #tokLParen
                beq _next4

                lda token

                rts

_1              cmp #tokLParen
                bne _2

                ldx op
                bne _9

_err            jmp experr

_2              ldx op
                beq _10

                ldx #0
                stx op
                cmp #tokQuote
                beq _13

_next1          ldx nxttoken
                cmp #tokRECORD
                beq _4                  ; record

                cmp #tokTYPE_t
                bcc exp._err
                bne _3

_next2          jsr etype
                bra _next4

_3              cmp #tokTYPE_t+8
                bcc _4                  ; field
                bne _5

                jsr etypea

                jmp _next4


;   record name or field
_4              cpx #tokPeriod
                bne _next2

;   will generate error if falls through

_5              cmp #tokCONST_t
                bcc exp._err            ; missing operand?

                cmp #tokARRAY_t
                bcs _6                  ; array or func

                cmp #tokUNDEC
                beq _11

                cmp #tokCONST_t+tokREAL_t
                beq exp._err            ; real constant

_next3          jsr pushnext
                bra exp._ENTRY1

_6              cmp #tokFUNC_t
                bcs _12

;   array
                jsr arrref

                jmp _next4

;   pop
_7              ldx op
                beq _8

                cmp #tokAT
                beq _8

                cmp #tokMINUS
                bne exp._err

                lda #tokUMINUS
                sta token

_8              tax
                lda prec-1,X
                sta op

                jsr rollops
                jsr pushop

                lda token
_9              jsr pushop
_next4          jsr GetNext

                jmp exp._ENTRY1

_10             jsr rollops

                cmp #tokLParen
                beq _errParen

                lda token

                rts

;   undefined
_11             jsr GetAlias
                bra _next1

;   proc
_12             bne _16

_next5          jsr procref
                bra _next3

;   string
_13             lda #$4C                ; JMP around string
                jsr Push1
                jsr getcdoff

                adc #3                  ; includes size byte
                bcc _14

                inx

                clc
_14             ldy #0
                adc (addr),Y            ; size
                bcc _15

                inx

_15             jsr Push2
                jsr copystr

                ldy #tokCONST_t+tokSTR_t
                jsr storst

                dec choff

                jsr GetNext
                bra _next4

_errParen       ldy #parenthERR
                jmp splerr


;   QCODE to handle function ref
_16             cmp #tokFUNC_t+8
                beq _next5

                cpx #tokLParen
                bne _next5

                lda #17
                jsr getframe

                ldy #16
                lda op
                sta (frame),Y

;   save temps
                sty arg0

                lda #args+15
                sta arg1

_next6          dec arg0
                ldy arg0
                lda temps,Y
                sta (frame),Y
                beq _17

                lda #$A5                ; LDA addr
                ldx arg1
                ldy #$48                ; PHA
                jsr Push3

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
                lda (frame),Y
                sta temps,Y
                beq _18

                lda #$68                ; PLA
                ldx #$85                ; STA addr
                ldy arg1
                jsr Push3

                ldy arg0

_18             inc arg1
                cpy #15
                bne _next7

                iny
                lda (frame),Y
                sta op

                jsr freeframe

;   set result type
                ldy #0
                lda (stack),Y
                and #7
                ora #tokTEMP_t

                ldx #args
                jsr SaveCd._saveStack

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
experr          ldy #exprERR


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
                lda opstack,Y

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
                sta (stack),Y

                iny
                lda addr
                ldx addr+1

                jmp SaveCd._saveStack

                .endproc


;======================================
;
;======================================
etypep          .proc
                jsr pushst
                jsr GetNext
                jsr GetNext

                ldy #0
                sta (stack),Y

                tax
                and #$F8

                ldy #typERR
                cmp #tokTYPE_t
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
                cpx #tokPeriod
                beq _1

;   record addr, size or field offset
;   A reg must be nonzero before call
                jmp ArrRef.arrconst

_1              jsr etypep              ; set type

;   get var address
                ldy #1
                jsr StkPS

                tya
                ldy #2
                sta (stack),Y

                dey
                txa
                sta (stack),Y

                rts
                .endproc


;======================================
;
;======================================
etypea          .proc
                cpx #tokPeriod
                beq _1

                jmp ArrRef.arrvar

_1              jsr etypep              ; set type

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
                ora #$B0                ; temp array
                sta (stack),Y

                rts
                .endproc


;======================================
;
;======================================
procref         .proc
                ldy #tokFUNC_t+tokCARD_t        ; make sure CARD
                cmp #tokFUNC_t+8
                bcc _1

_ENTRY1         jsr GetArgs             ; A#0, no arg types

                ldy #tokCONST_t+tokCARD_t       ; sys proc
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
_next1          lda (arg4),Y
                sta (stack),Y

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

                cmp #tokLParen
                beq _XIT

                tax
                ldy prec-1,X
                cpy op
                bcc _XIT                ; prec < op
                                        ; check for simple add
                ldy op                  ; see if end of exp
                bne _4

                ldy stack
                cpy #<(stkbase-21)
                bne _4

                cpx #tokPLUS
                beq _1

                cpx #tokRSH
                beq _1

                cpx #tokLSH
                bne _4

_1              jsr popop               ; see if last op
                bne _3

;   check for increament
;   We know at least this is an assignment or an array subscript.
;   If ChStkEq in CGPlus is true then this is an assignment.
                txa
                cmp #tokPLUS
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
                sta opstack,Y

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
                lda (stack),Y
                sta arg1

                and #7
                tax
                lda vartype-1,X
                sta arg3

                asl
                asl
                sta arg5

                ldy #7
                lda (stack),Y
                sta arg2

                and #7
                tax
                lda vartype-1,X
                sta arg4

                ora arg5
                tax
                lda outtype,X
                sta arg6                ; high bit on for card.

                and #7                  ; get rid of flag bits
                tax
                ora #tokTEMP_t
                sta arg7

                lda vartype-1,X
                sta arg5                ; output type

                jmp Push0

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
                bcs codegen._ENTRY1     ; too large a shift

;   whew!, we can now shift it
                ldy arg0
                cpy #tokRSH
                beq _1                  ; right shift

                lda #$06                ; ASL
                jsr LoadX.Op1L

                lda arg4
                beq _3

                lda #$26                ; ROL
                jsr Op1H

                jmp _3

_1              lda #$46                ; LSR
                ldx arg4
                beq _2

                jsr Op1H

                lda #$66                ; ROR
_2              jsr LoadX.Op1L

_3              dec stkbase-20
                bne _next1

_XIT            jmp popst

                .endproc


;======================================
;   CodeGen(op)
;======================================
codegen         .proc
                jsr genops
_ENTRY1         jsr jt_cgend

                lda arg0
                asl
                clc
                adc arg0

                tay
                lda cgops-3,Y
                sta arg8

                lda cgops-2,Y
                ldx cgops-1,Y

                jmp jsrind              ; jmp to QCODE for op

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
                jsr LoadX.Op1L

                lda arg4
                beq _XIT                ; byte var

                lda #$D0                ; BNE
                jsr Push1

                ldy #12
                jsr SaveCd

                lda #0
                jsr Push1               ; offset

                lda #$E6                ; INC
                jsr Op1H

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
                jsr jt_cgend
                jsr chasseq.chstkeq     ; see if INC
                bne _ENTRY5             ;   yes, just return

                lda arg1
                bpl _2                  ; cond. exp. of record

                bit tempmode            ; rhs temp?
                bne _5                  ;   yes

                cmp #tokVAR_t           ; const?
                bcs _ENTRY1             ;   no

;   rhs constant
                lda arg2
                cmp #tokARRAY_t         ; simple var
                bcs _ENTRY1

;   if STY addr16,X was supported
;               bcc _CGC0               ;   yes
;               bit tempMode            ; lhs temp?
;               bne _CGAV               ;   yes, large array

                ldx arg4
                beq _1                  ; byte

                ldy #2
                lda (stack),Y
                cmp #2
                bcs _ENTRY2

                sta arg12
                jsr LoadY

                lda #$84                ; STY
                jsr Op1H

_1              ldy #1
                lda (stack),Y
                cmp #2
                bcs _3

                sta arg12

                jsr LoadY

                lda #$84                ; STY
                bra _4

_2              cmp #tokTYPE_t
                bcc cgexperr            ; cond. exp.

;   rhs var
_ENTRY1         ldx arg4                ; lhs type=byte?
                beq _3                  ;   yes

                ; cpx #3                ; lhs = int?
                ; bne _CGAVI            ; yes

                ; lhs type = real
                ; jmp AssErr

_ENTRY2         jsr Load2H

_ENTRY3         lda #$81                ; STA
                jsr Op1H
_3              jsr Load2L

_ENTRY4         lda #$81                ; STA
_4              jsr LoadX.Op1L
_ENTRY5         jsr popst

                jmp popst

_5              and #$10                ; rhs array?
                bne cgassign._ENTRY1    ;   yes

;   special case for arg0
                ldy #1
                lda (stack),Y
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
                jsr Load2H              ; generate LDA #0 instr.

                ldy #5
                jsr SaveCd

_6              ldy #4
                jsr LoadI

                ldy #8
                lda arg2
                and #$60                ; lhs proc or temp(argument)?
                bne _7                  ;   yes

                jsr StkPZ
                beq _8                  ; page zero

                sty arg13

                ldy #1
                lda #$8D                ; STA addr16
                jsr Insrt3._ENTRY1      ; insert STA data16

                lda #1
_next1          ldy #5
                jsr LoadCd

                lda #$81                ; STA
                jsr Op1H

                jmp _ENTRY5

_7              and #$40                ; proc?
                bne cgassign._ENTRY2    ;   yes, punt(not the best QCODE)

                jsr StkAddr             ; temp (proc argument)

_8              ldy #1
                txa
                sta (arg14),Y

                lda #0
                beq _next1

;   temp array
_9              lda arg3
                beq cgassign._ENTRY2

                ldy #5
                jsr LdCdZ
                bra cgassign._ENTRY3

_10             ldy #3
                jsr LoadCd

                lda arg3                ; see if rhs BYTE or CHAR
                beq cgassign._ENTRY4      ;   yes

                jsr TrashY              ; in case INT ARRAY in rhs

;   oh if I only had more QCODE space!
;   could handle Y, see _OpA in CGU
                bra cgassign._ENTRY4

                .endproc


;======================================
;   chasseq (mode)
;======================================
chasseq         .proc
                jsr genops

                lda arg1
                cmp #tokVAR_t           ; see if const
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

                cmp #tokARRAY_t+8       ; small array?
                bne _next1              ;   no

                ldx #5
_next1          lda stkbase-14,X
                cmp stkbase-7,X
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
                cmp #tokVAR_t           ; see if both ops consts
                bcs _3                  ;   not const

                lda arg7
                and #7
                ora #tokCONST_t
                sta arg7

                ldy #8
                ldx arg8
                bne _1                  ; subtract constants

;   add constants
                clc
                lda (stack),Y

                ldy #1
                adc (stack),Y
                sta arg9

                ldy #9
                lda (stack),Y
                ldy #2
                adc (stack),Y

                jmp _2

_1              sec
                lda (stack),Y

                ldy #1
                sbc (stack),Y
                sta arg9

                ldy #9
                lda (stack),Y
                ldy #2
                sbc (stack),Y

_2              tax
                beq _4

                lda arg5
                bne _4

                lda #tokCONST_t+tokINT_t
                bra _ENTRY4

;   normal add or sub.
_3              ldx arg8
                lda cgopscd,X

                jsr Push1

_ENTRY1         jsr GetTemps
                jsr LoadX.Load1L
                jsr OpCd1
                jsr Op2L
                jsr STempL

                lda arg5
                beq _ENTRY3

                jsr Load1H
                jsr OpCd1
                jsr Op2H

_ENTRY2         jsr STempH

_ENTRY3         ldx #0
_4              lda arg7
_ENTRY4         ldy #7
                sta (stack),Y

                iny
                lda arg9

                jsr SaveCd._saveStack

_XIT            jmp popst

                .endproc


;======================================
;
;======================================
cgshift         .proc
                lda arg5                ; byte ?
                bne cgmd                ;   no

                lda arg1
                cmp #tokVAR_t           ; see if constant
                bcs cgmd                ;   no

                ldy #1
                lda (stack),Y
                beq cgadd._XIT          ; ignore shift

                cmp #8                  ; shift high  7
                bcc _1                  ;   no

                ldx #0
                stx arg9

                lda #tokCONST_t+tokBYTE_t
                bra cgadd._ENTRY4

_1              sta arg1

                lda #$0A                ; ASL A
                ldx arg8
                beq _2

                lda #$4A                ; LSR A
_2              sta arg3

                jsr GetTemps
                jsr LoadX.Load1L

_next1          lda arg3                ; shift Op

                jsr Push1

                dec arg1
                bne _next1

                jsr STempL

                jmp cgadd._ENTRY3

                .endproc


;======================================
;
;======================================
cgmul           .proc
                lda #tokTEMP_t+tokINT_t ; force output to INT
                sta arg5
                sta arg7

            .if ZAPRAM
                sta (ADRESS),Y
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
                jsr Load2H

                lda #$85                ; STA AFcur+1
                ldx #afcur+1

                jsr Push2

                .endproc

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
cgmd            .proc
                jsr GetTemps
                jsr Load2L

                lda #$85                ; STA AFcur
                ldx #afcur

                jsr Push2

                lda arg4
                beq _1                  ; first op byte

                jsr Load1H

                lda #$AA                ; TAX
                jsr Push1

_1              jsr LoadX.Load1L

                lda arg4
                bne _2

                lda #$A2                ; LDX #0
                ldx #0
                jsr Push2

_2              ldx arg8

                jsr JSRTable
                jsr STempL
                jsr TrashY

                lda arg5
                beq _XIT

                lda #$8A                ; TXA
                jsr Push1

                jsr STempH

_XIT            jmp cgadd._ENTRY3

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
                beq _1                  ; no JMPs

                jsr filljmp

_1              ldy #5
                jsr LdCdZ

                ldy #11                 ; link T2 to T1
                jsr setrel

_next1          jsr savrel
                jsr loadn
                bne _next1              ; get end of T1

                ldy #3
                lda (stack),Y

                sec
                jsr comprel._ENTRY1     ; patch in T2
                bra cgand._ENTRY1

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
                sta (arg4),Y

                iny
                lda (arg4),Y
                adc #0
                sta (arg4),Y

                jsr loadn
                bne _next1

                ldy #13
                jsr LoadI

                ldy #1
                jsr save4               ; link F1 to F2

                ldy #8
                jsr StkAddr

                lda #$4C                ; JMP
                jsr Insrt3              ; patch in JMP false

                lda QCODE
                pha

                clc
                lda arg14
                adc #3
                sta QCODE

                ldy #11
                jsr fillbr              ; make T1 -> Cond2

                pla
                sta QCODE

                ldy #4
                jsr LoadI

                clc
                adc #3
                bcc _1

                inx

_1              ldy #10
                jsr SaveCd._saveStack

_ENTRY1         ldy #2
                jsr LoadI

                ldy #8
                jsr SaveCd._saveStack

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
                ldy #condtERR
                jmp splerr

                .endproc


;======================================
;   CGEq()
;======================================
cgeq            .proc
cgne            jsr LoadX.Load1L
                jsr ChkZero
                beq _1

                lda #$41                ; EOR
                jsr Op2L

_1              lda arg5
                beq _ENTRY1

                jsr ChkZero

                php                     ; save status
                beq _2

                lda #$D0                ; BNE
                jsr PushTrue

_2              lda #$01                ; ORA
                jsr Op1H

                plp                     ; ChkZero() status
                beq _ENTRY1

                lda #$41                ; EOR
                jsr Op2H

                ldy #11
                jsr fillbr

_ENTRY1         jsr OpCd1
                jsr PushTrue            ; sets arg9 to zero

                lda #tokCOND_t
                sta arg7

                ldy #12
                jsr SaveCd

                jmp cgadd._ENTRY3

                .endproc


;======================================
;   CGLS()
;======================================
cgls            .proc
cgge            jsr RelOp
                jsr LoadX.Load1L

                lda #$C1                ; CMP
                jsr Op2L

                lda arg5
                beq cgeq.cgne._ENTRY1

                jsr Load1H

                lda #$E1                ; SBC
                jsr Op2H
                bra cgeq.cgne._ENTRY1   ; see CodeIncr

                .endproc


;======================================
;   CGGR()
;======================================
cggr            .proc
cgle            jsr RelOp
                jsr Load2L

                lda #$C1                ; CMP
                jsr LoadX.Op1L

                lda arg5
                beq cgeq.cgne._ENTRY1

                jsr Load2H

                lda #$E1                ; SBC
                jsr Op1H
                bra cgeq.cgne._ENTRY1   ; see CodeIncr

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
                lda #tokCONST_t+tokINT_t
                sta (stack),Y

                sec
                tya
                iny
                sbc (stack),Y
                sta (stack),Y

                iny
                lda #0
                sbc (stack),Y
                sta (stack),Y

                rts

_1              jsr copyst

                ldy #7
                lda #tokCONST_t+tokINT_t
                sta (stack),Y

                lda #0
                tax
                iny

                jsr SaveCd._saveStack

                lda #tokMINUS
                jmp codegen

                .endproc


;======================================
;   CGAt()
;======================================
cgat            .proc
                lda arg1
                cmp #tokVAR_t
                bcc _1

                cmp #tokARRAY_t+8
                bcs _err

                ldy #1
                jsr StkP

                iny
                jsr SaveCd._saveStack

_next1          ldy #0
                lda #tokCONST_t+tokCARD_t
                sta (stack),Y

                rts

_1              and #$F8
                cmp #tokTYPE_t          ; check for record field
                beq _next1
                                        ; constant or cond. exp. (error)
_err            jmp experr

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
                .addr cgdiv             ; divide

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
                .addr cgdiv             ; remainder

                .byte 3
                .addr cgadd._ENTRY1     ; XOR

                .byte 0
                .addr cgshift           ; LSH

                .byte 2
                .addr cgshift           ; RSH

                .byte 12
                .addr cgum              ; unary minus

                .byte 0
                .addr cgat
