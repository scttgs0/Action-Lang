
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: compiler.main.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;         Main compiler entry
;======================================
Compile


;======================================
;======================================
ScanParseLex    ;.proc
                jsr jt_splend

                lda nxttoken
                cmp #tokQuote
                bne _1                  ; no name

                jsr compiler.lexicon.Get._ENTRY1
                jmp _next1

_1              lda top1
                sta top+1

                jsr ioChkCursor._ENTRY1
                beq _XIT1               ; no program !

                jsr compiler.lexicon.GetNext
_next1          jsr compiler.lexicon.GetNext


;--------------------------------------
; <program> _:= <module list> MODULE <module>
;                 | (MODULE) <module>
; <module> _:= (<dcl list>) (<segment list>)
;--------------------------------------
                jsr Declare
                jsr ampl.Segment

                cmp #tokMOD             ; another module ?
                beq _next1              ; yes

                cmp #tokEOF
                bne _2

                lda #$01                ; save run address
                jsr mscCProp

                sta INITAD
                stx INITAD+1

;   insert return, just in case
_next2          lda #$60                ; RTS
                jsr ampl.cgu.Push1

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

_next3          ldy #$01
                lda (arrayptr),Y
                sta arg1

                dey
                lda (arrayptr),Y
                sta arg0

                jsr mscGetCodeOffset

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

_2              jsr mscGetCodeOffset    ; no main PROC

                sta INITAD
                stx INITAD+1
                jsr StmtList

                cmp #tokEOF
                beq _next2

;EndErr         ldy #$00
;               sty $2E3                ; zap run address

_err            ldy #endERR
                jmp ErrorFI

_err2           jmp mscCodeIncr.cderr   ; out of QCODE space

                ;.endproc


;======================================
; Declaration processing
;--------------------------------------
; <dcl list> _:= <dcl list> <dcl> | <dcl>
; <dcl> _:= <simple dcl> | <array dcl> | <def dcl>
;======================================
DeclError       lda #$00                ; reset QCODE before err
                tay
                jsr ampl.cgu.LoadCd

_errDecl        jmp DefineError

_type           lda #+tokRECORD-(tokVAR_t-tokCHAR)-1
                sta type

                jsr MakeEntry

                lda addr
                ldx addr+1
                ldy #$02
                jsr ampl.cgu.SaveCd._ToStack

                ldy #$00
                jsr ampl.cgu.SaveCd
                jsr compiler.lexicon.GetNext

                cmp #tokEQU
                bne _errDecl

                jsr compiler.lexicon.GetNext

                cmp #tokLBracket
                bne _errDecl

                sec
                lda #$00
                sbc codeoff
                sta QCODE
                lda #$00
                sbc codeoff+1
                sta QCODE+1

                jsr compiler.lexicon.GetNext

_next1          cmp #tokCHAR
                bcc DeclError

                cmp #tokDEFINE
                bcs DeclError

                tax
                clc
                adc #+tokTYPE_t-tokVAR_t
                sta type

                lda varsize-tokCHAR,X
                sta zpAllocCurrent

_next2          jsr MakeEntry

                lda zpAllocCurrent
                jsr mscCodeIncr
                jsr compiler.lexicon.GetNext

                cmp #tokComma
                beq _next2

                cmp #tokRBracket
                bne _next1

                ldy #$02
                jsr ampl.cgu.StkP

                ldx QCODE+1
                bne DeclError

                lda QCODE
                ldy #$00
                jsr StoreProps

                lda #$00
                tay
                jsr ampl.cgu.LoadCd
                jsr compiler.lexicon.GetNext


;======================================
;
;======================================
Declare         jsr jt_dclend
                cmp #tokCHAR
                bcs _1

_XIT1           rts

_1              cmp #tokTYPE
                beq DeclError._type

                cmp #tokRECORD
                bne _2

;   record decl.
                lda #$00
                jsr mscGetProp

                stx zpAllocCurrent

                ldx nxttoken
                lda #tokTYPE_t-(tokVAR_t-tokCHAR)-1
                sta type
                bne _3                  ; [unc]

_2              cmp #tokDEFINE
                beq _define
                bcs _XIT1

                sta type

                tax
                ldy varsize-tokCHAR,X
                sty zpAllocCurrent

                ldx nxttoken
                cpx #tokFUNC
                beq _XIT1

                cpx #tokARRAY
                beq ArrayDecl

_3              cpx #tokPOINTER
                bne _simple

                ldy #$00
                sty zpAllocCurrent
                beq ArrayDecl           ; [unc]


;--------------------------------------
; <simple dcl> _:= <type> <id eq list>
; <id eq list> _:= <id eq list> , <id eq> | <id eq>
; <id eq> _:= <id> (= <constant>)
;--------------------------------------
_simple         jsr MakeEntry

                ldx param
                beq _4

                jsr Params

                ldx param
                bpl _5
                bmi _6                  ; [unc]

_4              lda nxttoken
                cmp #tokEQU
                bne _5

                jsr IDEqual

                iny
                jsr StoreProps

                jsr compiler.lexicon.GetNext
                bne _6

_5              lda zpAllocCurrent
                jsr mscCodeIncr

_6              jsr compiler.lexicon.GetNext

                cmp #tokComma
_leapSimple     beq _simple

                jmp Declare


;--------------------------------------
; <def dcl> _:= DEFINE <def list>
; <def list> _:= <def list> , <def> | <def>
; <def> _:= <id> = <str const>
;--------------------------------------
_define         jsr MakeEntry

                ldy #$00
                lda #tokDef
                sta (zpAllocProps),Y

                jsr compiler.lexicon.GetNext

                cmp #tokEQU
                bne DefineError

                lda nxttoken
                cmp #tokQuote
                bne DefineError

                ldy #$00
                lda (symtab),Y
                clc
                adc #$02                ; real size + EOL

                jsr mscSTIncr
                jsr compiler.lexicon.GetNext    ; string itself
                jsr compiler.lexicon.GetNext    ; dummy string
                jsr compiler.lexicon.GetNext

                cmp #tokComma
                bne _leapSimple
                beq _define             ; [unc]


;--------------------------------------
;
;--------------------------------------
DefineError     ldy #declERR
                jmp bankSPLErr


;--------------------------------------
; <array dcl> _:= <type> ARRAY <array list>
; <array list> _:= <array list> , <array> | <array>
; <array> _:= <id> ((<constant>)) (= <constant>)
;--------------------------------------
ArrayDecl       clc
                adc #$08
                sta type

                jsr compiler.lexicon.GetNext
_next1          jsr MakeEntry

                lda #$02
                sta numargs             ; variable space

                ldx param
                bne _2

                lda nxttoken
                ldx zpAllocCurrent
                beq _3                  ; no size for pointers

                cmp #tokLParen
                bne _3

                lda #$04
                sta numargs

                lda arrayptr
                ldx arrayptr+1
                ldy #$00

                jsr mscStoreVar
                jsr GetArraySize
                jsr compiler.lexicon.GetNext

;   check for small byte array

                ldy #$02
                lda #$00
                cmp (QCODE),Y

                iny
                lda #$01
                sbc (QCODE),Y
                bcs _6                  ; size <= 256

_next2          jsr compiler.lexicon.GetNext

                cmp #tokRParen
                bne DefineError

                lda nxttoken
                cmp #tokEQU
                beq _4

_next3          lda numargs
                bne _1

;   small array
                ldy #$02
                lda (QCODE),Y
                bne _next4

                inc QCODE+1
                bne _next5              ; [unc]

;   array var

_1              cmp #$04
                bmi _next4

;   large array with memory
                ldx QCODE
                stx arrayptr
                ldx QCODE+1
                stx arrayptr+1

_next4          jsr mscCodeIncr

_next5          jsr compiler.lexicon.GetNext

                cmp #tokComma
                beq _next1
                jmp Declare

_2              jsr Params

                ldx param
                bpl _next3
                bmi _next5

_3              cmp #tokEQU
                bne _next3

_4              jsr IDEqual

                ldy numargs
                beq _5

                ldy #$00
                jsr mscStoreVar
                jsr mscGetCodeOffset

_5              ldy #$01
                jsr StoreProps
                jsr compiler.lexicon.GetNext

                lda numargs
                jmp _next4

_6              ldy #$00
                lda (zpAllocProps),Y
                cmp #tokARRAY_t+tokINT_t
                bcs _next2

;   small byte array
                sty numargs
                ora #$08
                sta (zpAllocProps),Y

                iny
                jsr mscGetCodeOffset
                jsr StoreProps
                bne _next2              ; [unc]


;--------------------------------------
;
;--------------------------------------
CheckParam      .proc
                ldx param

_ENTRY1         beq MakeEntry._err

                pla
                pla

                jmp compiler.lexicon.GetNext

                .endproc


;======================================
; MakeEntry()
;======================================
MakeEntry       .proc
                lda nxttoken
                cmp #tokUNDEC
                beq _3
                bcs _1                  ; var of some kind

                cmp #tokRECORD
                beq _1

                cmp #tokTYPE_t
                bcc CheckParam

                cmp #tokEOF
                bcs CheckParam

_1              lda qglobal
                beq CheckParam._ENTRY1

                jsr bankLocalName

                cmp #tokUNDEC
                beq _2

_err            jmp DefineError

_2              sta nxttoken

_3              lda #$00
                jsr mscNextProp

                sec
                lda #tokVAR_t-tokCHAR
                adc type
                sta (zpAllocProps),Y    ; type

                and #$07
                tax
                lda vartype-1,X
                sta zpAllocOP

                iny

                jsr mscGetCodeOffset
                jsr StoreProps
                jmp compiler.lexicon.GetNext

                .endproc


;======================================
; IDEqual()
;======================================
IDEqual         .proc
                jsr compiler.lexicon.GetNext

                ldx param
                bne Params._err

                jmp mscMNum

                .endproc


;======================================
; GetArraySize()
;======================================
GetArraySize    .proc
                jsr IDEqual

                sty arg0                ; Y should = 0
                sty arg1

                ldy zpAllocCurrent      ; #elements * element size
_next1          clc
                lda arg0
                adc zpAllocSize
                sta arg0

                lda arg1
                adc zpAllocSize+1
                sta arg1

                dey
                bne _next1

                tax
                lda arg0
                ldy #$02

                jmp mscStoreVar

                .endproc


;======================================
; StoreProps(low, high, index)
;======================================
StoreProps      .proc
                sta (zpAllocProps),Y

                txa
                iny
                sta (zpAllocProps),Y

                rts
                .endproc


;---------------------------------------
;---------------------------------------
;ChkNext PROC ; ChkNext()
;               lda nxtToken
;               cmp #undec
;               bne _XIT
;
;               jsr GNlocal
;_XIT           jmp compiler.lexicon.GetNext


;======================================
; Params()
;======================================
Params          .proc
                ldy #$00
                lda (zpAllocProps),Y    ; get var type
                pha

                lda #$03
                jsr mscCProp

                cmp #$08
                bcs _err

                adc #$01
                sta (zpAllocProps),Y
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

_err            jmp ampl.Segment._argerr

;:Par1          cmp #varT+realT
;               beq PErr

_2              cmp #tokVAR_t+tokINT_t
                bcc _4                  ; one-byte arg

;   two-byte arg
_3              and #$1F
                inc argbytes

_4              and #$9F
                inc argbytes
                sta (zpAllocProps),Y

                rts
                .endproc


;--------------------------------------
;--------------------------------------

varsize         .byte 1,1,2,2,2,6


;====================================
; Statement processing
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
StmtList        .proc
                jsr ClearTemps

                sta zpAllocOP
                jsr jt_smtend

                cmp #tokLBracket
                bne _next2

;   machine QCODE block
                jsr ampl.cgu.TrashY

_next1          ldx nxttoken
                cpx #tokRBracket
                beq _3

                jsr mscMNum

                cpx #$00
                beq _1

                jsr ampl.cgu.Push2      ; 2-byte number
                bcs _2                  ; [unc]

_1              jsr ampl.cgu.Push1      ; single byte
_2              jsr compiler.lexicon.GetNext
                bne _next1              ; [unc]

_3              jsr compiler.lexicon.GetNext
                jmp RecRet.nxtstmt

_next2          ldx nxttoken
                cmp #tokVAR_t+tokCHAR_t
                bcc _4

                cmp #tokFUNC_t
                bcc Assign

;   routine reference
                cpx #tokLParen
                beq Call

                jsr ProcRef
                bne Assign._ENTRY1      ; [unc]

_4              cmp #tokUNDEC
                bne _5

                jsr bankGetAlias
                bne _next2              ; [unc]

_5              cmp #tokTYPE_t
                bne _6

                jsr EType
                jmp AssignArray._ENTRY1

_6              cmp #tokTYPE_t+8
                bne _7

                jsr ETypeA
                jmp AssignArray._ENTRY1

_7              ldx #<tblStmtList
                ldy #>tblStmtList
                jmp mscLookup

                .endproc


;--------------------------------------
; <call> _:= <proc var>((<arglist>))
; <proc var> _:= <id>
; <arglist> _:= <arglist> , <exp> | <exp>
;--------------------------------------
Call            .proc
                jsr ampl.pf.ProcFunc
                jsr PopST
                jmp RecRet.nxtstmt

                .endproc


;--------------------------------------
; <assign> _:= <id> = <exp>
;--------------------------------------
Assign          .proc
                cmp #tokARRAY_t
                bcs AssignArray

_ENTRY1         jsr PushNext

_ENTRY2         eor #tokEQU
                bne AssignError

                jsr PushOp              ; push 0 on op stack
                jsr compiler.lexicon.GetNext

                sta zpAllocOP
                cmp #tokEQU
                bne _3

                lda #$00
                sta zpAllocOP

                jsr CopyST

;   check for temps
                iny
                and #$F8
                cmp #tokARRAY_t+8
                bne _1

                ldy #$03
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

_2              jsr compiler.lexicon.GetNext
_3              jsr Expression._ENTRY1
                jsr CGAssign

                jmp StmtList

                .endproc


;--------------------------------------
;
;--------------------------------------
AssignError     .proc
                ldy #assgnERR
                jmp bankSPLErr

                .endproc


;--------------------------------------
; <array assign> _:= <id> ( <exp> ) = <exp>
;--------------------------------------
AssignArray     .proc
                jsr ampl.array.Ref

_ENTRY1         ldy #$00
                lda (stack),Y
                bpl _1                  ; record element

                cmp #tokVAR_t
                bcc AssignError         ; const

_1              jsr compiler.lexicon.GetNext
                bne assign._ENTRY2      ; [unc]

                .endproc


;---------------------------------------
; <if> _:= IF <exp> THEN <stmt list
;          (ELSE <stmt list>) FI
;---------------------------------------
StmtIF          .proc
                lda #$07
                jsr GetFrame

                ldy #$05
                lda #$00
                sta (frame),Y

_if             jsr ExpressionCond

                cmp #tokTHEN
                bne ThenError

;   save current Y
                lda cury
                ldy #$06
                sta (frame),Y

                jsr RecRet.nxtstmt

;   restore Y
                tax
                ldy #$06
                lda (frame),Y
                sta cury

                txa
                cmp #tokELSEIF
                bne _else

                ldy #$04
                jsr FrameAdr._ENTRY1

                ldy #$04
                jsr FrameCd._ENTRY1

                ldx arg4
                ldy arg5

                jsr PushJMP
                jsr FrameAdr
                jsr FillJmp._ENTRY1

                jmp _if

_else           cmp #tokELSE
                bne _fi

                jsr FrameAdr
                jsr FrameCd

                ldy #$00                ; flag as end of list
                jsr PushJMP
                jsr FillJmp._ENTRY1
                jsr RecRet.nxtstmt

_fi             ldy #fiERR
                cmp #tokFI
                bne ErrorFI

                ldy #$04
                jsr FrameAdr._ENTRY1
                beq _ifnoelse           ; if no ELSEIF

                jsr FillJmp._ENTRY1

_ifnoelse       jsr ampl.cgu.TrashY
                jsr FrameAdr

                beq RecRet              ; in case of DO loop
                jsr FillJmp._ENTRY1

                .endproc


;--------------------------------------
; RecRet() pops stack and returns
;--------------------------------------
RecRet          .proc
                jsr FreeFrame

nxtstmt         jsr compiler.lexicon.GetNext
                jmp StmtList

                .endproc


;--------------------------------------
; pops stack
;--------------------------------------
FreeFrame       .proc
                ldy #$00
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
ThenError       ldy #thenERR

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
StmtErr         jmp bankSPLErr


;--------------------------------------
; <do> _:= DO <stmt list> (UNTIL <exp>) OD
;--------------------------------------
StmtDO          .proc
                jsr doinit

                lda #$00
                ldy #$03
                sta (frame),Y
                bne StmtWHILE._ENTRY1   ; [unc]

                .endproc


;--------------------------------------
;
;--------------------------------------
ErrorFI         .proc
                cmp #tokUNDEC
                bne StmtErr

                jmp mscMNum._varerr

                .endproc


;--------------------------------------
; <while> _:= WHILE <exp> <do>
;--------------------------------------
StmtWHILE       .proc
                jsr doinit
                jsr ExpressionCond

                ldy #doERR
                cmp #tokDO
                bne StmtErr

_ENTRY1         jsr RecRet.nxtstmt

_ENTRY2         cmp #tokUNTIL
                bne _1

                jsr ExpressionCond
                bne _2                  ; [unc]

_1              ldy #$04
                jsr FrameAdr._ENTRY1
                jsr PushJMP

                lda token
_2              ldy #odERR
                cmp #tokOD
                bne ErrorFI

                ldy #$06
                jsr FrameAdr._ENTRY1

                stx whaddr
                sty whaddr+1

                jmp StmtIF._ifnoelse

                .endproc


;======================================
; StmtEXIT()
;======================================
StmtEXIT        .proc
                ldy #exitERR
                ldx whaddr+1
                beq StmtErr

                ldy #$02                ; get pointer to EXIT list
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
                jsr PushJMP
                jmp RecRet.nxtstmt

                .endproc


;======================================
; <for> _:= FOR <id> = <exp> TO <exp>
;             (STEP <exp>) <do>
;======================================
ErrorFOR        ldy #forERR
                bne ErrorFI             ; [unc]


;======================================
;
;======================================
StmtFOR         .proc
                lda #$17
                jsr GetFrame
                jsr AddressWHILE

;   make sure simple var for index
                jsr compiler.lexicon.GetNext

                cmp #tokVAR_t+tokCHAR_t
                bcc ErrorFOR

                cmp #tokARRAY_t
                bcc _1

                cmp #tokARRAY_t+tokREAL_t
                bcs ErrorFOR

                lda #tokVAR_t+tokCARD_t
                sta token

;   get initial value
_1              ldy #$08
                sta (frame),Y

                iny
                lda addr
                ldx addr+1

                jsr FrameCd._ENTRY2
                jsr PushNext

                cmp #tokEQU
                bne ErrorFOR

                jsr GetExp
                jsr CGAssign

;   set default STEP size
                lda token
                cmp #tokTO
_next1          bne ErrorFOR

                lda #$00
                ldx #$09
                ldy #$0C
_next2          sta (frame),Y

                iny
                dex
                bne _next2

                ldy #$08
                jsr fstk

                lda token
                and #$07
                ora #tokCONST_t

                ldx #$01
                ldy #$0B
                jsr FrameCd._ENTRY2

;   get ending value
                lda #$10
                jsr ExpressionFOR

;   get step value
                lda token
                cmp #tokSTEP
                bne _2

                lda #$0B
                jsr ExpressionFOR

                lda token
_2              cmp #tokDO
                bne _next1

;   generate end test
                jsr mscGetCodeOffset

                ldy #$04
                jsr FrameCd._ENTRY2
                jsr ampl.cgu.TrashY
                jsr GenOps._ENTRY1

                ldy #$10
                lda (frame),Y
                cmp #tokVAR_t
                bcs _3                  ; temp variable

;   constant
                iny
                lda (frame),Y
                tax

                lda #$A9
                jsr ampl.cgu.Push2      ; LDA #low

                lda #$C1                ; CMP
                jsr ampl.cgu.Op2L

                lda arg3
                beq _5

                ldy #$12
                lda (frame),Y
                tax

_next3          lda #$A9
                jsr ampl.cgu.Push2      ; LDA #high

                jmp _4

_3              ldy #$11
                sty arg0

                lda #$AD                ; LDA addr16
                jsr ExpressionFOR._ENTRY1

                lda #$C1                ; CMP
                jsr ampl.cgu.Op2L

                lda arg3
                beq _5

                ldx #$00
                ldy #$10
                lda (frame),Y
                cmp #tokVAR_t+tokINT_t
                bcc _next3              ; only byte var

                lda #$AD                ; LDA addr16
                jsr ExpressionFOR._ENTRY1

_4              lda #$E1                ; SBC
                jsr ampl.cgu.Op2H

;   body
_5              lda arg3
                ror a                   ; get type

                lda #$B0                ; BCS, CARD
                bcc _6

                lda #$10                ; BPL, INT
_6              jsr ampl.cgu.Push1

                ldy #$15
                jsr FrameCd._ENTRY1
                jsr ampl.cgu.Push1
                jsr PopST

                jsr FrameCd

                ldy #$00
                jsr PushJMP

;   save space for vars
                ldy #$10
                jsr fmem

                ldy #$0B
                jsr fmem

;   handle symtab
                ldy #$0B
                lda (frame),Y
                cmp #tokVAR_t
                bcc _7

                lda symtab
                ldx symtab+1
                iny

                jsr FrameCd._ENTRY2

                lda #$00
                tay
                sta (symtab),Y

                lda #$04
                jsr mscSTIncr

;   patch branch
_7              ldy #$15
                jsr FrameAdr._ENTRY1
                jsr CompareRel

;   handle stmt list
                jsr RecRet.nxtstmt

;   handle incr
                pha                     ; save token

                ldy #$08
                jsr fstk
                jsr CopyST

                ldy #$0B
                jsr fstk

                lda #tokPLUS
                jsr CGPlus

                lda arg5
                beq _8

                jsr ChAssEQ.ChStkEQ     ; see if INC

_8              pha

                jsr CGAssign

                pla
                beq _9                  ; not INC
                                        ; see if we can branch to top of loop
                pla
                cmp #tokUNTIL
                beq _10                 ; can't go to top of loop

                pha

                clc
                ldy #$04
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

                ldy #$00
                txa
                sta (arg0),Y

_9              pla
_10             sta token

                jmp StmtWHILE._ENTRY2

                .endproc


;======================================
;
;======================================
ExpressionFOR   .proc
                pha
                jsr GetExp
                jsr GenOps._ENTRY1

                pla
                sta arg0

                jsr _1
                jmp PopST


_1              lda arg1
                cmp #tokVAR_t           ; see if const
                bcs _2

;   constant
                ldy #$0B
                lda (frame),Y
                ldy arg0
                sta (frame),Y

                ldy #$02
                jsr ampl.cgu.LoadI

                ldy arg0
                iny

                jmp FrameCd._ENTRY2

_2              ldy #$08
                lda (frame),Y
                and #$07
                cmp #tokINT_t
                bmi _3

                lda arg1
                and #$07
_3              ora #tokVAR_t

                ldy #$01
                sta (symtab),Y
                sta arg2

                ldy arg0
                sta (frame),Y

                inc arg0

                lda arg1
                bit ampl.cgu.modeArr    ; array?
                bne _4                  ;   yes

                bit ampl.cgu.modeTemp   ; temp?
                bne _5                  ;   yes

;   var of some kind
_4              jsr ampl.cgu.Load2L
                jsr _next1

                lda arg2
                cmp #tokVAR_t+tokINT_t  ; see if byte
                bcc _XIT1

                jsr ampl.cgu.Load2H

_next1          lda #$8D                ; STA data16

_ENTRY1         pha

                ldy arg0
                jsr FrameAdr._ENTRY1

                ldy arg0
                jsr FrameCd._ENTRY1

                iny
                sty arg0

                pla
                ldx arg4
                ldy arg5

                jmp ampl.cgu.Push3

_XIT1           rts

;   temp
_5              ldy #$04
                jsr ampl.cgu.LoadI

                ldy #$01
                jsr ampl.cgu.AddCdSp

                ldy #$03
                lda #$00
                jsr _6

                lda arg2
                cmp #tokVAR_t+tokINT_t  ; see if byte
                bcc _XIT1

                ldy #$05
                lda #$01
_6              jsr ampl.cgu.LoadCd
                jmp _next1

                .endproc


;======================================
;
;======================================
fmem            lda (frame),Y
                cmp #tokVAR_t
                bcc ExpressionFOR._XIT1 ; const

                sty arg2
                jsr mscGetCodeOffset    ; save address for step

                ldy #$02
                sta (symtab),Y

                txa
                iny
                sta (symtab),Y

                ldy arg2
                jsr _1

                ldy arg2
                lda (frame),Y
                cmp #tokVAR_t+tokINT_t
                bcc ExpressionFOR._XIT1 ; byte only

                iny
                iny

_1              iny

                jsr FrameAdr._ENTRY1
                jsr FillJmp._ENTRY1

                lda #$01
                jmp mscCodeIncr


;======================================
;
;======================================
fstk            lda (frame),Y
                sta token

                iny
                jsr FrameAdr._ENTRY1

                stx addr
                sty addr+1

                jmp PushST


;======================================
;
;======================================
doinit          .proc
                lda #$08
                jsr GetFrame
                jsr AddressWHILE
                jsr mscGetCodeOffset

                ldy #$04
                jsr FrameCd._ENTRY2

                jmp ampl.cgu.TrashY

                .endproc


;--------------------------------------
; <return> _:= RETURN ((<exp>))
;--------------------------------------
StmtRETURN      .proc
                lda #$00
                jsr mscCProp

                and #$07
                beq _1

                ora #tokTEMP_t
                tay

                lda #args
                jsr StoreST

                ldx nxttoken
                cpx #tokLParen
                bne _err

                jsr compiler.lexicon.GetNext
                jsr GetExp

                cmp #tokRParen
                bne _err

                jsr CGAssign

_1              lda #$60
                jsr ampl.cgu.Push1
                jmp RecRet.nxtstmt

_err            ldy #retrnERR
                jmp bankSPLErr

                .endproc


;======================================
; ExpressionCond()
;======================================
ExpressionCond  .proc
                jsr GetExp

                pha

                ldy #$00
                lda (stack),Y
                cmp #tokCOND_t
                beq _1

;   not boolean
                jsr ZeroST

                lda #tokNOTEQU
                jsr CodeGen

_1              pla                     ; token value
                pha
                cmp #tokOD
                bne _3

;   until <exp> od
                ldy #$01
                jsr ampl.cgu.StkAddr
                beq _2                  ; no JMPs

;   JMP to JMP to top of loop
;   yek!, should be improved
                jsr FillJmp             ; fill in jmps

_2              ldy #$04
                jsr FrameAdr._ENTRY1
                bne _4                  ; [unc]

_3              jsr FrameCd

                ldy #$01
                jsr ampl.cgu.StkAddr

_4              jsr PushJMP

;   fill in branch addresses
                ldy #$04
                jsr FillBr
                jsr PopST

                pla                     ; get token value
                rts
                .endproc


;======================================
;
;======================================
AddressWHILE    .proc
                lda whaddr
                ldx whaddr+1
                ldy #$06

                jsr FrameCd._ENTRY2

                lda frame
                sta whaddr
                lda frame+1
                sta whaddr+1

                rts
                .endproc


;======================================
; FillBr(,,offset)
;======================================
FillBr          .proc
                jsr SetRel

_next1          jsr SaveRel
                jsr CompareRel
                jsr LoadN
                bne _next1

                rts
                .endproc


;======================================
; FrameAdr()
;======================================
FrameAdr        .proc
                ldy #$02

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
; FrameCd()
;======================================
FrameCd         .proc
                ldy #$02

_ENTRY1         lda QCODE
                ldx QCODE+1

_ENTRY2         sta (frame),Y

                iny
                txa
                sta (frame),Y

                rts
                .endproc


;======================================
; FillJmp(,addr)
;======================================
FillJmp         .proc
                jsr FrameAdr._ENTRY2

_ENTRY1         jsr SaveN
                jsr mscGetCodeOffset
                jsr Save4
                jsr LoadN
                bne _ENTRY1

                rts
                .endproc


;======================================
; Save4(value)
;======================================
Save4           .proc
                sta (arg4),Y

                iny
                txa
                sta (arg4),Y

                rts
                .endproc


;======================================
; SaveN()
;======================================
SaveN           .proc
                ldy #$01
                lda (arg4),Y
                sta arg0

                iny
                lda (arg4),Y
                sta arg1

                dey
                rts
                .endproc


;======================================
; LoadN()
;======================================
LoadN           .proc
                lda arg0
                ldx arg1
                beq _XIT                ; is this needed anymore?

_ENTRY1         sta arg4
                stx arg5

_XIT            rts
                .endproc


;======================================
; PushJMP(, addr)
;======================================
PushJMP         .proc
                lda #$4C                ; JMP addr16
                jmp ampl.cgu.Push3

                .endproc


;======================================
; GetFrame(size)
;======================================
GetFrame        .proc
                sta arg0

                ldy frame
                ldx frame+1
                sec
                tya
                sbc arg0
                sta frame

                txa
                sbc #$00
                sta frame+1

                lda frame
                cmp sparem
                lda frame+1
                sbc sparem+1
                bcc _err

                tya
                ldy #$00

                jmp FrameCd._ENTRY2

_err            ldy #nestERR
                jmp bankSPLErr

                .endproc


;======================================
; SetRel(,,offset)
;======================================
SetRel          .proc
                jsr ampl.cgu.LoadI
                jmp LoadN._ENTRY1

                .endproc


;======================================
;
;======================================
SaveRel         .proc
                ldy #$00
                ldx #$00
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
CompareRel      .proc
                lda QCODE
                clc                     ; extra -1 for offset byte
_ENTRY1         sbc arg4

                ldy #$00
                sta (arg4),Y

                rts
                .endproc


;======================================
;
;======================================
ClearTemps      .proc
                lda #$00
                ldx #$10
_next1          sta temps-1,X

                dex
                bne _next1              ; free temps

                rts
                .endproc

;--------------------------------------
;--------------------------------------

tblStmtList     .addr jt_smtend         ; not found
                .byte 20                ; #entries*3 - 1
                .addr StmtIF
                .byte tokIF
                .addr StmtFOR
                .byte tokFOR
                .addr StmtWHILE
                .byte tokWHILE
                .addr StmtRETURN
                .byte tokRET
                .addr StmtDO
                .byte tokDO
                .addr StmtEXIT
                .byte tokEXIT


;--------------------------------------
; Expression processing
;--------------------------------------
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
; GetExp()
;======================================
GetExp          .proc
            .if ZAPRAM
                inc cgopscd
            .else
                nop
                nop
                nop
            .endif

                jsr compiler.lexicon.GetNext

                .endproc

                ;[fall-through]


;======================================
;
;======================================
Expression      .proc
                lda #$00
                jsr PushOp

                lda token               ; always non-zero
                sta zpAllocOP

_ENTRY1         jsr jt_expend
                cmp #tokSColon
                bcc _7

                cmp #tokRParen
                bne _1

                ldx zpAllocOP
                bne _err

                jsr RollOps
                cmp #tokLParen
                beq _next4

                lda token

                rts

_1              cmp #tokLParen
                bne _2

                ldx zpAllocOP
                bne _9
_err            jmp ErrorExpression

_2              ldx zpAllocOP
                beq _10

                ldx #$00
                stx zpAllocOP
                cmp #tokQuote
                beq _13

_next1          ldx nxttoken
                cmp #tokRECORD
                beq _4                  ; record

                cmp #tokTYPE_t
                bcc Expression._err
                bne _3

_next2          jsr EType
                bne _next4              ; [unc]

_3              cmp #tokTYPE_t+8
                bcc _4                  ; field
                bne _5

                jsr ETypeA

                jmp _next4


;   record name or field
_4              cpx #tokPeriod
                bne _next2

;   will generate error if falls through

_5              cmp #tokCONST_t
                bcc Expression._err     ; missing operand?

                cmp #tokARRAY_t
                bcs _6                  ; array or func

                cmp #tokUNDEC
                beq _11

                cmp #tokCONST_t+tokREAL_t
                beq Expression._err     ; real constant

_next3          jsr PushNext
                bne Expression._ENTRY1  ; [unc]

_6              cmp #tokFUNC_t
                bcs _12

;   array
                jsr ampl.array.Ref

                jmp _next4

;   pop
_7              ldx zpAllocOP
                beq _8

                cmp #tokAT
                beq _8

                cmp #tokMINUS
                bne Expression._err

                lda #tokUMINUS
                sta token

_8              tax
                lda prec-1,X
                sta zpAllocOP

                jsr RollOps
                jsr PushOp

                lda token
_9              jsr PushOp
_next4          jsr compiler.lexicon.GetNext

                jmp Expression._ENTRY1

_10             jsr RollOps

                cmp #tokLParen
                beq _errParen

                lda token

                rts

;   undefined
_11             jsr bankGetAlias
                bne _next1              ; [unc]

;   proc
_12             bne _16

_next5          jsr ProcRef
                bne _next3             ; [unc]

;   string
_13             lda #$4C                ; JMP around string
                jsr ampl.cgu.Push1
                jsr mscGetCodeOffset

                adc #$03                ; includes size byte
                bcc _14

                inx

                clc
_14             ldy #$00
                adc (addr),Y            ; size
                bcc _15

                inx

_15             jsr ampl.cgu.Push2
                jsr mscCopyStr

                ldy #tokCONST_t+tokSTR_t
                jsr StoreST

                dec choff

                jsr compiler.lexicon.GetNext
                bne _next4              ; [unc]

_errParen       ldy #parenthERR
                jmp bankSPLErr


;   QCODE to handle function ref
_16             cmp #tokFUNC_t+8
                beq _next5

                cpx #tokLParen
                bne _next5

                lda #$11
                jsr GetFrame

                ldy #$10
                lda zpAllocOP
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
                jsr ampl.cgu.Push3

                ldy arg0

_17             dec arg1
                cpy #$02
                bne _next6

                lda temps
                bne ErrorExpression     ; nested functions

                jsr ClearTemps
                jsr ampl.pf.ProcFunc    ; call the function

;   restore temps
                ldy #$01
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
                jsr ampl.cgu.Push3

                ldy arg0

_18             inc arg1
                cpy #$0F
                bne _next7

                iny
                lda (frame),Y
                sta zpAllocOP

                jsr FreeFrame

;   set result type
                ldy #$00
                lda (stack),Y
                and #$07
                ora #tokTEMP_t

                ldx #args
                jsr ampl.cgu.SaveCd._ToStack

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
;               lda #$06
;               jsr VarIncr
;               jmp _Exp7


;--------------------------------------
;
;--------------------------------------
ErrorExpression ldy #exprERR


;--------------------------------------
;
;--------------------------------------
ErrorExp2       jmp bankSPLErr


;======================================
; PopOp()
;======================================
PopOp           .proc
                dec stackptr
                bmi ErrorExpression     ; this should never happen

                ldy stackptr
                lda opstack,Y

                rts
                .endproc


;======================================
;
;======================================
ZeroST          .proc
                lda #$00
                tax
                ldy #tokCONST_t+tokBYTE_t

                .endproc

                ;[fall-through]


;======================================
; StoreST(addr16,token)
;======================================
StoreST         .proc
                sty token
                sta addr
                stx addr+1

                .endproc

                ;[fall-through]


;======================================
; PushST()
;======================================
PushST          .proc
                sec
                lda stack
                sbc #$07
                sta stack
                bcc ErrorExpression

                ldy #$00
                lda token
                sta (stack),Y

                iny
                lda addr
                ldx addr+1

                jmp ampl.cgu.SaveCd._ToStack

                .endproc


;======================================
;
;======================================
ETypeP          .proc
                jsr PushST
                jsr compiler.lexicon.GetNext
                jsr compiler.lexicon.GetNext

                ldy #$00
                sta (stack),Y

                tax
                and #$F8

                ldy #typERR
                cmp #tokTYPE_t
                bne ErrorExp2

                txa
                and #$07
                beq ErrorExp2

                sta token

;   get offset
                lda #$01
                jmp mscGetProp

                .endproc


;======================================
;
;======================================
EType           .proc
                cpx #tokPeriod
                beq _1

;   record addr, size or field offset
;   A reg must be nonzero before call
                jmp ampl.array.Ref.arrconst

_1              jsr ETypeP              ; set type

;   get var address
                ldy #$01
                jsr ampl.cgu.StkPS

                tya
                ldy #$02
                sta (stack),Y

                dey
                txa
                sta (stack),Y

                rts
                .endproc


;======================================
;
;======================================
ETypeA          .proc
                cpx #tokPeriod
                beq _1
                jmp ampl.array.Ref.arrvar

_1              jsr ETypeP              ; set type

                tay
                lda token
                pha

                tya
                ldy #tokCONST_t+tokBYTE_t

                jsr StoreST

                lda #tokARRAY_t+tokBYTE_t
                ldy #$07

                jsr ampl.array.Ref.arra0

                ldy #$00
                pla
                ora #$B0                ; temp array
                sta (stack),Y

                rts
                .endproc


;======================================
;
;======================================
ProcRef         .proc
                ldy #tokFUNC_t+tokCARD_t    ; make sure CARD
                cmp #tokFUNC_t+8
                bcc _1

_ENTRY1         jsr bankGetArgs             ; A#0, no arg types

                ldy #tokCONST_t+tokCARD_t   ; sys proc
_1              sty token

                rts
                .endproc


;======================================
; CopyST()
;======================================
CopyST          .proc
                lda stack
                ldx stack+1

                jsr LoadN._ENTRY1
                jsr PushST

                ldy #$06
_next1          lda (arg4),Y
                sta (stack),Y

                dey
                bpl _next1

                rts
                .endproc


;======================================
; RollOps()
;======================================
RollOps         .proc
                jsr PopOp
                beq _XIT

                cmp #tokLParen
                beq _XIT

                tax
                ldy prec-1,X
                cpy zpAllocOP
                bcc _XIT                ; prec < op
                                        ; check for simple add
                ldy zpAllocOP           ; see if end of exp
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

_1              jsr PopOp               ; see if last op
                bne _3

;   check for increament
;   We know at least this is an assignment or an array subscript.
;   If ChStkEq in CGPlus is true then this is an assignment.
                txa
                cmp #tokPLUS
                bne _2

                jsr CGPlus

                lda #$00
                rts

_2              jsr CGSh

                lda #$00
                rts

_3              jsr PushOp

                txa
_4              jsr CodeGen
                jmp RollOps

_XIT            rts
                .endproc


;======================================
; PopST()
;======================================
PopST           .proc
                clc
                lda stack
                adc #$07
                sta stack

                rts
                .endproc


;======================================
; PushOp(op)
;======================================
PushOp          .proc
                ldy stackptr
                inc stackptr
                sta opstack,Y

                rts
                .endproc


;======================================
; PushNext()
;======================================
PushNext        .proc
                jsr PushST
                jmp compiler.lexicon.GetNext

                .endproc


;--------------------------------------
;--------------------------------------

prec            .byte 5,5,6,6,2,3
                .byte 4,4,4,4,4,4
                .byte 6,1,6,6,7,7


;--------------------------------------
;    Code generation
;--------------------------------------

;======================================
; GenOps(op)
;======================================
GenOps          .proc
                sta arg0

_ENTRY1         ldy #$00
                lda (stack),Y
                sta arg1

                and #$07
                tax
                lda vartype-1,X
                sta arg3

                asl
                asl
                sta arg5

                ldy #$07
                lda (stack),Y
                sta arg2

                and #$07
                tax
                lda vartype-1,X
                sta arg4

                ora arg5
                tax
                lda ampl.cgu.outType,X
                sta arg6                ; high bit on for card.

                and #$07                ; get rid of flag bits
                tax
                ora #tokTEMP_t
                sta arg7

                lda vartype-1,X
                sta arg5                ; output type

                jmp ampl.cgu.Push0

                .endproc


;======================================
;
;======================================
CGSh            .proc
                jsr ChAssEQ
                beq CodeGen._ENTRY1

_next1          lda stkbase-20
                beq _XIT                ; no shift!

                cmp #$05
                bcs CodeGen._ENTRY1     ; too large a shift

;   whew!, we can now shift it
                ldy arg0
                cpy #tokRSH
                beq _1                  ; right shift

                lda #$06                ; ASL
                jsr ampl.cgu.Op1L

                lda arg4
                beq _3

                lda #$26                ; ROL
                jsr ampl.cgu.Op1H

                jmp _3

_1              lda #$46                ; LSR
                ldx arg4
                beq _2

                jsr ampl.cgu.Op1H

                lda #$66                ; ROR
_2              jsr ampl.cgu.Op1L

_3              dec stkbase-20
                bne _next1

_XIT            jmp PopST

                .endproc


;======================================
; CodeGen(op)
;======================================
CodeGen         .proc
                jsr GenOps
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

                jmp mscJSRIndirect      ; jmp to QCODE for op

                .endproc


;======================================
; CGPlus()
;======================================
CGPlus          .proc
                jsr ChAssEQ
                beq CodeGen._ENTRY1

                lda stkbase-20
                cmp #$01                ; see if const = 1
                bne CodeGen._ENTRY1     ;   no

;   whew!, we can now increment it
                lda #$E6                ; INC
                jsr ampl.cgu.Op1L

                lda arg4
                beq _XIT                ; byte var

                lda #$D0                ; BNE
                jsr ampl.cgu.Push1

                ldy #$0C
                jsr ampl.cgu.SaveCd

                lda #$00
                jsr ampl.cgu.Push1      ; offset

                lda #$E6                ; INC
                jsr ampl.cgu.Op1H

                ldy #$0D
                jsr FillBr

_XIT            jmp PopST

                .endproc


;--------------------------------------
;
;--------------------------------------
CGExpError      jmp ErrorExpression


;======================================
; CGAssign()
;======================================
CGAssign        .proc
                lda #$00
                jsr GenOps
                jsr jt_cgend
                jsr ChAssEQ.ChStkEQ     ; see if INC
                bne _ENTRY5             ;   yes, just return

                lda arg1
                bpl _2                  ; cond. exp. of record

                bit ampl.cgu.modeTemp   ; rhs temp?
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

                ldy #$02
                lda (stack),Y
                cmp #$02
                bcs _ENTRY2

                sta arg12
                jsr ampl.cgu.LoadY

                lda #$84                ; STY
                jsr ampl.cgu.Op1H

_1              ldy #$01
                lda (stack),Y
                cmp #$02
                bcs _3

                sta arg12

                jsr ampl.cgu.LoadY

                lda #$84                ; STY
                bne _4                  ; [unc]

_2              cmp #tokTYPE_t
                bcc CGExpError          ; cond. exp.

;   rhs var
_ENTRY1         ldx arg4                ; lhs type=byte?
                beq _3                  ;   yes

                ; cpx #$03              ; lhs = int?
                ; bne _CGAVI            ; yes

                ; lhs type = real
                ; jmp AssErr

_ENTRY2         jsr ampl.cgu.Load2H

_ENTRY3         lda #$81                ; STA
                jsr ampl.cgu.Op1H
_3              jsr ampl.cgu.Load2L

_ENTRY4         lda #$81                ; STA
_4              jsr ampl.cgu.Op1L
_ENTRY5         jsr PopST
                jmp PopST

_5              and #$10                ; rhs array?
                bne CGAssign._ENTRY1    ;   yes

;   special case for arg0
                ldy #$01
                lda (stack),Y
                cmp #args
                beq CGAssign._ENTRY1    ; function return value

                lda arg4                ; lhs byte?
                beq _10                 ;   yes

;   int/card temp
                lda arg2
                and #$10                ; array?
                bne _9                  ;   yes

                lda arg3
                bne _6

;   rhs type is BYTE
                jsr ampl.cgu.Load2H     ; generate LDA #$00 instr.

                ldy #$05
                jsr ampl.cgu.SaveCd

_6              ldy #$04
                jsr ampl.cgu.LoadI

                ldy #$08
                lda arg2
                and #$60                ; lhs proc or temp(argument)?
                bne _7                  ;   yes

                jsr ampl.cgu.StkPZ
                beq _8                  ; page zero

                sty arg13

                ldy #$01
                lda #$8D                ; STA addr16
                jsr ampl.cgu.Insrt3._ENTRY1   ; insert STA data16

                lda #$01
_next1          ldy #$05
                jsr ampl.cgu.LoadCd

                lda #$81                ; STA
                jsr ampl.cgu.Op1H

                jmp _ENTRY5

_7              and #$40                ; proc?
                bne CGAssign._ENTRY2    ;   yes, punt(not the best QCODE)

                jsr ampl.cgu.StkAddr    ; temp (proc argument)

_8              ldy #$01
                txa
                sta (arg14),Y

                lda #$00
                beq _next1

;   temp array
_9              lda arg3
                beq CGAssign._ENTRY2

                ldy #$05
                jsr ampl.cgu.LdCdZ
                bne CGAssign._ENTRY3    ; [unc]

_10             ldy #$03
                jsr ampl.cgu.LoadCd

                lda arg3                ; see if rhs BYTE or CHAR
                beq CGAssign._ENTRY4    ;   yes

                jsr ampl.cgu.TrashY     ; in case INT ARRAY in rhs

;   oh if I only had more QCODE space!
;   could handle Y, see _OpA in CGU
                bne CGAssign._ENTRY4    ; [unc]

                .endproc


;======================================
; ChAssEQ (mode)
;======================================
ChAssEQ         .proc
                jsr GenOps

                lda arg1
                cmp #tokVAR_t           ; see if const
                bcs ChStkEQ._ENTRY1     ;   no

                lda stkbase-19          ; see if byte
                bne ChStkEQ._ENTRY1     ;   no
                                        ; see if left and right-hand are =


;======================================
; JMP ChStkEq
;======================================
ChStkEQ         ldx #$02
                lda stkbase-7
                and #$F8
                cmp #$B0                ; large array?
                beq _ENTRY1             ;   yes, can't INC or shift

                cmp #tokARRAY_t+8       ; small array?
                bne _next1              ;   no

                ldx #$05
_next1          lda stkbase-14,X
                cmp stkbase-7,X
                bne _ENTRY1

                dex
                bpl _next1

                rts

_ENTRY1         lda #$00

                rts
                .endproc


;======================================
; CGAdd()
;======================================
CGAdd           .proc
                lda arg1
                bpl _3

                ora arg2
                cmp #tokVAR_t           ; see if both ops consts
                bcs _3                  ;   not const

                lda arg7
                and #$07
                ora #tokCONST_t
                sta arg7

                ldy #$08
                ldx arg8
                bne _1                  ; subtract constants

;   add constants
                clc
                lda (stack),Y

                ldy #$01
                adc (stack),Y
                sta arg9

                ldy #$09
                lda (stack),Y
                ldy #$02
                adc (stack),Y

                jmp _2

_1              sec
                lda (stack),Y

                ldy #$01
                sbc (stack),Y
                sta arg9

                ldy #$09
                lda (stack),Y
                ldy #$02
                sbc (stack),Y

_2              tax
                beq _4

                lda arg5
                bne _4

                lda #tokCONST_t+tokINT_t
                bne _ENTRY4             ; [unc]

;   normal add or sub.
_3              ldx arg8
                lda cgopscd,X

                jsr ampl.cgu.Push1

_ENTRY1         jsr ampl.cgu.GetTemps
                jsr ampl.cgu.Load1L
                jsr ampl.cgu.OpCd1
                jsr ampl.cgu.Op2L
                jsr ampl.cgu.STempL

                lda arg5
                beq _ENTRY3

                jsr ampl.cgu.Load1H
                jsr ampl.cgu.OpCd1
                jsr ampl.cgu.Op2H

_ENTRY2         jsr ampl.cgu.STempH

_ENTRY3         ldx #$00
_4              lda arg7
_ENTRY4         ldy #$07
                sta (stack),Y

                iny
                lda arg9

                jsr ampl.cgu.SaveCd._ToStack

_XIT            jmp PopST

                .endproc


;======================================
;
;======================================
CGShift         .proc
                lda arg5                ; byte ?
                bne CGMd                ;   no

                lda arg1
                cmp #tokVAR_t           ; see if constant
                bcs CGMd                ;   no

                ldy #$01
                lda (stack),Y
                beq CGAdd._XIT          ; ignore shift

                cmp #$08                ; shift high  7
                bcc _1                  ;   no

                ldx #$00
                stx arg9

                lda #tokCONST_t+tokBYTE_t
                bne CGAdd._ENTRY4       ; [unc]

_1              sta arg1

                lda #$0A                ; ASL A
                ldx arg8
                beq _2

                lda #$4A                ; LSR A
_2              sta arg3

                jsr ampl.cgu.GetTemps
                jsr ampl.cgu.Load1L

_next1          lda arg3                ; shift Op

                jsr ampl.cgu.Push1

                dec arg1
                bne _next1

                jsr ampl.cgu.STempL
                jmp CGAdd._ENTRY3

                .endproc


;======================================
;
;======================================
CGMul           .proc
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
CGDiv           .proc
                jsr ampl.cgu.Load2H

                lda #$85                ; STA AFcur+1
                ldx #zpAllocCurrent+1

                jsr ampl.cgu.Push2

                .endproc

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
CGMd            .proc
                jsr ampl.cgu.GetTemps
                jsr ampl.cgu.Load2L

                lda #$85                ; STA AFcur
                ldx #zpAllocCurrent

                jsr ampl.cgu.Push2

                lda arg4
                beq _1                  ; first op byte

                jsr ampl.cgu.Load1H

                lda #$AA                ; TAX
                jsr ampl.cgu.Push1

_1              jsr ampl.cgu.Load1L

                lda arg4
                bne _2

                lda #$A2                ; LDX #$00
                ldx #$00
                jsr ampl.cgu.Push2

_2              ldx arg8

                jsr ampl.cgu.JSRTable
                jsr ampl.cgu.STempL
                jsr ampl.cgu.TrashY

                lda arg5
                beq _XIT

                lda #$8A                ; TXA
                jsr ampl.cgu.Push1
                jsr ampl.cgu.STempH

_XIT            jmp CGAdd._ENTRY3

                .endproc


;======================================
; CGOr()
;======================================
CGOr            .proc
                jsr CheckCond

                ldy #$0C
                jsr ampl.cgu.LdCdZ

                ldy #$08
                jsr ampl.cgu.StkAddr
                beq _1                  ; no JMPs

                jsr FillJmp

_1              ldy #$05
                jsr ampl.cgu.LdCdZ

                ldy #$0B                ; link T2 to T1
                jsr SetRel

_next1          jsr SaveRel
                jsr LoadN
                bne _next1              ; get end of T1

                ldy #$03
                lda (stack),Y

                sec
                jsr CompareRel._ENTRY1  ; patch in T2
                beq CGAnd._ENTRY1       ; [unc]

                .endproc


;======================================
;   CGAnd()
;======================================
CGAnd           .proc
                jsr CheckCond

                lda stack
                ldx stack+1
                jsr LoadN._ENTRY1

_next1          jsr SaveN               ; patch addresses

                clc
                lda arg0
                adc #$03
                sta (arg4),Y

                iny
                lda (arg4),Y
                adc #$00
                sta (arg4),Y

                jsr LoadN
                bne _next1

                ldy #$0D
                jsr ampl.cgu.LoadI

                ldy #$01
                jsr Save4               ; link F1 to F2

                ldy #$08
                jsr ampl.cgu.StkAddr

                lda #$4C                ; JMP
                jsr ampl.cgu.Insrt3     ; patch in JMP false

                lda QCODE
                pha

                clc
                lda arg14
                adc #$03
                sta QCODE

                ldy #$0B
                jsr FillBr              ; make T1 -> Cond2

                pla
                sta QCODE

                ldy #$04
                jsr ampl.cgu.LoadI

                clc
                adc #$03
                bcc _1

                inx

_1              ldy #$0A
                jsr ampl.cgu.SaveCd._ToStack

_ENTRY1         ldy #$02
                jsr ampl.cgu.LoadI

                ldy #$08
                jsr ampl.cgu.SaveCd._ToStack

                ldy #$0C
                jsr ampl.cgu.SaveCd
                jsr ampl.cgu.TrashY     ; just in case array in cond.
                jmp PopST               ; done at last, whew!

                .endproc


;======================================
;
;======================================
CheckCond       .proc
                lda #tokCOND_t
                cmp arg1
                bne _1

                cmp arg2
                bne ErrCond

                rts

_1              pla
                pla

                jmp CGAdd._ENTRY1

                .endproc


;--------------------------------------
;
;--------------------------------------
ErrCond         .proc
                ldy #condtERR
                jmp bankSPLErr

                .endproc


;======================================
; CGEqual()
;======================================
CGEqual         .proc
cgne            jsr ampl.cgu.Load1L
                jsr ampl.cgu.ChkZero
                beq _1

                lda #$41                ; EOR
                jsr ampl.cgu.Op2L

_1              lda arg5
                beq _ENTRY1

                jsr ampl.cgu.ChkZero

                php                     ; save status
                beq _2

                lda #$D0                ; BNE
                jsr ampl.cgu.PushTrue

_2              lda #$01                ; ORA
                jsr ampl.cgu.Op1H

                plp                     ; ampl.cgu.ChkZero() status
                beq _ENTRY1

                lda #$41                ; EOR
                jsr ampl.cgu.Op2H

                ldy #$0B
                jsr FillBr

_ENTRY1         jsr ampl.cgu.OpCd1
                jsr ampl.cgu.PushTrue   ; sets arg9 to zero

                lda #tokCOND_t
                sta arg7

                ldy #$0C
                jsr ampl.cgu.SaveCd
                jmp CGAdd._ENTRY3

                .endproc


;======================================
; CGLess()
;======================================
CGLess          .proc
cgge            jsr ampl.cgu.RelOp
                jsr ampl.cgu.Load1L

                lda #$C1                ; CMP
                jsr ampl.cgu.Op2L

                lda arg5
                beq CGEqual.cgne._ENTRY1

                jsr ampl.cgu.Load1H

                lda #$E1                ; SBC
                jsr ampl.cgu.Op2H
                bcs CGEqual.cgne._ENTRY1   ; [unc], see CodeIncr

                .endproc


;======================================
; CGGreater()
;======================================
CGGreater       .proc
cgle            jsr ampl.cgu.RelOp
                jsr ampl.cgu.Load2L

                lda #$C1                ; CMP
                jsr ampl.cgu.Op1L

                lda arg5
                beq CGEqual.cgne._ENTRY1

                jsr ampl.cgu.Load2H

                lda #$E1                ; SBC
                jsr ampl.cgu.Op1H
                bcs CGEqual.cgne._ENTRY1   ; [unc], see CodeIncr

                .endproc


;======================================
; CGUnaryMinus()
;======================================
CGUnaryMinus    .proc
                lda arg1
                and #$78
                bne _1                  ; not constant

;   constant, just negate it
                sec
                ldy #$00
                lda #tokCONST_t+tokINT_t
                sta (stack),Y

                sec
                tya
                iny
                sbc (stack),Y
                sta (stack),Y

                iny
                lda #$00
                sbc (stack),Y
                sta (stack),Y

                rts

_1              jsr CopyST

                ldy #$07
                lda #tokCONST_t+tokINT_t
                sta (stack),Y

                lda #$00
                tax
                iny

                jsr ampl.cgu.SaveCd._ToStack

                lda #tokMINUS
                jmp CodeGen

                .endproc


;======================================
; CGAt()
;======================================
CGAt            .proc
                lda arg1
                cmp #tokVAR_t
                bcc _1

                cmp #tokARRAY_t+8
                bcs _err

                ldy #$01
                jsr ampl.cgu.StkP

                iny
                jsr ampl.cgu.SaveCd._ToStack

_next1          ldy #$00
                lda #tokCONST_t+tokCARD_t
                sta (stack),Y

                rts

_1              and #$F8
                cmp #tokTYPE_t          ; check for record field
                beq _next1
                                        ; constant or cond. exp. (error)
_err            jmp ErrorExpression

                .endproc

;--------------------------------------
;--------------------------------------

                .byte 0                 ; for records!
vartype         .byte 0,0,1,2,2,3

cgopscd         .byte $18,$61           ; CLC ADC
                .byte $38,$e1           ; SEC SBC
                .byte $41,$21,$01       ; EOR AND ORA
                .byte $30,$90           ; BMI BCC
                .byte $10,$b0           ; BPL BCS
                .byte $f0,$d0           ; BEQ BNE

cgops           .byte 0
                .addr CGAdd

                .byte 2
                .addr CGAdd             ; minus

                .byte 4
                .addr CGMul             ; multiply

                .byte 6
                .addr CGDiv             ; divide

                .byte 5
                .addr CGOr

                .byte 4
                .addr CGAnd

                .byte 10
                .addr CGEqual

                .byte 11
                .addr CGEqual.cgne

                .byte 6
                .addr CGGreater

                .byte 8
                .addr CGLess.cgge

                .byte 6
                .addr CGLess

                .byte 8
                .addr CGGreater.cgle

                .byte 8
                .addr CGDiv             ; remainder

                .byte 3
                .addr CGAdd._ENTRY1     ; XOR

                .byte 0
                .addr CGShift           ; LSH

                .byte 2
                .addr CGShift           ; RSH

                .byte 12
                .addr CGUnaryMinus

                .byte 0
                .addr CGAt
