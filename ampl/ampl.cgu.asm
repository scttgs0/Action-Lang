
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.cgu.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; cguLoadY() value in arg12
;======================================
cguLoadY        .proc
                lda cury
                cmp arg12
                beq _XIT

                jsr cguPush0

                cmp #$01
                bne _1

                lda #$88                ; DEY
_next1          jsr cguInsrt1

                jmp _3

_1              cmp #$00
                bne _2

                lda #$C8                ; INY
                bne _next1

_2              lda #$A0
                ldx arg12
                jsr cguInsrt2           ; LDY #$00 or #$01

_3              lda arg12
                sta cury

                ldy arg13
_XIT            rts
                .endproc


;======================================
; cguTrashY()
;======================================
cguTrashY       .proc
                lda #$FF
                sta cury

                rts
                .endproc


;======================================
; cguLoadX(,,offset)
;--------------------------------------
; NOTE:  this proc can only be called
;        from Op below, see _LXC
;======================================
cguLoadX        .proc
                lda (stack),Y
                iny
                bit modeTemp
                bne _1

                bit modeConst
                beq _3

;   var to load
                jsr cguStkProp

                beq _2

                lda #$AE                ; LDX addr16
                jmp cguPush3

_1              lda (stack),Y
                tax
                dec temps-args,X

_2              lda #$A6                ; LDX addr
_XIT1           jmp cguPush2

_3              lda (stack),Y

;               tax
;               lda #$A2                ; LDX data
;               bne _XIT1

                sta arg12

                pla
                pla
                pla
                tay
                bra cguOp1L.ophigh._opv

_optype         and #$20
                beq cguOp1L.ophigh._operr   ; con. exp.

                jsr cguStkAddr

                lda arg12
                beq cguOp1L.ophigh._4

                inx
                bne cguOp1L.ophigh._4

                iny
                jmp cguOp1L.ophigh._4

                .endproc


;======================================
; cguLoad1L()
;======================================
cguLoad1L       lda #$A1                ; LDA op

                ;[fall-through]


;======================================
; cguOp1L(op)
;======================================
cguOp1L         .proc
                pha

                lda arg2
                ldy #$08
oplow           ldx #$00
ophigh          stx arg12

; NOTE:  the order of following
; comparisons is important!

                tax
                bpl cguLoadX._optype

                bit modeProc
                bne _5

                bit modeArr
                bne _6                  ; array

                bit modeTemp
                bne _13                 ; temp

                bit modeConst
                beq _9                  ; constant

;   var if we get here
_opv            jsr cguStkProp

                beq _14                 ; page zero var

;   16 bit address
_4              pla
                ora #$0C                ; addr16

                jmp cguPush3

;   proc
_5              inc arg12               ; skip JMP byte
                and #$08
                beq _opv

_operr          jmp conderr             ; cond. exp.

;   array
_6              bit modeConst
                bne _7

                jsr cguLoadY

                ; lda arg7
                ; and #$F7
                ; sta arg7 ; flag Y reg used

                lda #$00
                sta arg12

                lda #$10                ; (addr),Y
_next1          sta arg10

                lda (stack),Y
                clc
                adc arg12
                cmp #args
                bcc _11

                cmp #args+16
                bcs _11

                tax
                dec temps-args,X        ; free temp
                bra _12

_7              tya                     ; small array
                pha

                iny
                iny

                jsr cguLoadX

                pla
                tay
                jsr cguStkProp
                beq _8                  ; page zero

                pla
                ora #$1C                ; addr16,X

                jmp cguPush3

_8              pla
                ora #$14                ; addr,X

                jmp cguPush2

;   constant
_9              lda #$08                ; data
                sta arg10

                lda arg12
                beq _10

                iny

_10             lda (stack),Y
_11             tax
_12             pla
                ora arg10               ; op mode
                jmp cguPush2

;   temp
_13             lda #$04                ; addr
                bra _next1

;   zero-page variable
_14             pla
                ora #$04                ; addr
                jmp cguPush2

                .endproc


;======================================
; cguLoad2L()
;======================================
cguLoad2L       lda #$A1                ; LDA op

                ;[fall-through]


;======================================
; cguOp2L(op)
;======================================
cguOp2L         .proc
                pha

                lda arg1
                ldy #$01
                jmp cguOp1L.oplow

                .endproc


;======================================
; cguLoad1H()
;======================================
cguLoad1H       .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
; cguOp1H(op)
;======================================
cguOp1H         .proc
                ldx arg4
                beq cguOp2H._ophz

                pha

                lda arg2
                ldy #$08
_ENTRY1         ldx #$01
                jmp cguOp1L.ophigh

                .endproc


;======================================
; cguLoad2H()
;======================================
cguLoad2H       .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
; cguOp2H(op)
;======================================
cguOp2H         .proc
                ldx arg3
                beq _ophz

                pha
                lda arg1
                ldy #$01
                bne cguOp1H._ENTRY1

_ophz           ora #$08
                jmp cguPush2

                .endproc


;--------------------------------------
;--------------------------------------

modeArr         .byte $10
modeConst       .byte $08
modeTemp        .byte $20
modeProc        .byte $40

; see CG
outType         .byte $82,3,$84,tokREAL_t
                .byte 3,3,$84,tokREAL_t
                .byte $84,$84,$84,tokREAL_t
                .byte tokREAL_t,tokREAL_t,tokREAL_t,tokREAL_t


;======================================
; cguGetTemps()
;======================================
cguGetTemps     .proc
                ldx #args+16
                ldy #$07
_next1          dex
                dex
                dey
                bmi _err                ; exp. too complex

                lda temps-args,X
                bne _next1

                inc temps-args,X

                lda arg5                ; see if byte temp
                beq _1                  ;   yes

                inc temps-args+1,X

            .if ZAPRAM
                inc mathSArgs,X
            .else
                nop
                nop
                nop
            .endif

_1              stx arg9

                rts

_err            jmp experr

                .endproc


;======================================
; cguLoadI(,,offset)
;======================================
cguLoadI        .proc
                lda (stack),Y
                sta arg15

                tax
                dey

                lda (stack),Y
                sta arg14

                rts
                .endproc


;======================================
; cguLdCdZ(,,stkoff)
;======================================
cguLdCdZ        .proc
                lda #$00

                .endproc

                ;[fall-through]


;======================================
; cguLoadCd(cdoff,,stkoff)
;======================================
cguLoadCd       .proc
                clc
                adc (stack),Y
                sta QCODE

                iny
                lda #$00
                adc (stack),Y
                sta QCODE+1

                rts
                .endproc


;======================================
; cguSaveCd(,,offset)
;======================================
cguSaveCd       .proc
                lda QCODE
                ldx QCODE+1
_ToStack        sta (stack),Y

                txa
                iny
                sta (stack),Y

                rts
                .endproc


;======================================
; cguRelOp()
;======================================
cguRelOp        .proc
                lda arg6
                bpl _XIT

                inc arg8

_XIT            rts
                .endproc


;======================================
; cguChkZero()
;======================================
cguChkZero      .proc
                lda arg3
                bne _XIT

                lda arg1
                bpl _XIT                ; not const

                cmp #tokVAR_t
                bcs _XIT

                ldy #$01
                lda (stack),Y

_XIT            rts
                .endproc


;======================================
; cguOpCd1()
;======================================
cguOpCd1        .proc
                ldx arg8
                lda cgopscd+1,X

                rts
                .endproc


;======================================
; cguStkAddr(,,offset)
;======================================
cguStkAddr      .proc
                lda (stack),Y
                tax

                iny
                lda (stack),Y
                tay

                rts
                .endproc


;======================================
; cguStkP(,,offset)
;======================================
cguStkP         .proc
                jsr cguStkAddr

                lda #$01
                jmp mscGProp

                .endproc


;======================================
; cguStkPZ(,,offset)
;======================================
cguStkPZ        .proc
                lda #$00

                .endproc

                ;[fall-through]


;======================================
; cguStkPS(,,offset)
;======================================
cguStkPS        .proc
                sta arg12

                .endproc

                ;[fall-through]


;======================================
; cguStkProp(,,offset)
;======================================
cguStkProp      .proc
                jsr cguStkP

                clc
                adc arg12

                tax
                iny

                lda (zpAllocProps),Y
                adc #$00
                tay

                rts
                .endproc


;======================================
; cguJSRTable(,index)
;======================================
cguJSRTable     .proc
;           .if RAMzap
;               ldy LTab+1,X
;               lda LTab,X
;           .else
                ldy jt_lsh+1,X
                lda jt_lsh,X
;           .endif

                tax
                lda #$20                ; JSR opcode

                jmp cguPush3

                .endproc


;======================================
; cguPush0()
;======================================
cguPush0        .proc
                sty arg13

                ldy QCODE
                sty arg14
                ldy QCODE+1
                sty arg15

                ldy #$00

                rts
                .endproc


;======================================
; cguPushTrue(op)
;======================================
cguPushTrue     .proc
                jsr cguPush1

                ldy #$0A
                jsr cguSaveCd

                lda #$00                ; no other true branches
                sta arg9

                .endproc

                ;[fall-through]


;======================================
; cguPush1(op)
;======================================
cguPush1        .proc
                jsr cguPush0

                sta (arg14),Y
                beq cguInsrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; cguInsrt1(op)
;======================================
cguInsrt1       .proc
                ldy #$01
                jsr cguAddCdSp

_ENTRY1         iny
                tya

                jmp mscCodeIncr

                .endproc


;======================================
; cguSTempH()
;======================================
cguSTempH       .proc
                inc arg9

                ldy #$0C
                bra cguSTempL._ENTRY1

                .endproc


;======================================
; cguSTempL()
;======================================
cguSTempL       .proc
                ldy #$0A

_ENTRY1         jsr cguSaveCd

                lda arg9
                tax
                and #$FE                ; set to low address
                sta arg9

                lda #$85                ; STA addr

                .endproc

                ;[fall-through]


;======================================
; cguPush2(op,,data)
;======================================
cguPush2        .proc
                jsr cguPush0

                sta (arg14),Y
                beq cguInsrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; cguInsrt2(op,data)
;======================================
cguInsrt2       .proc
                ldy #$02
                jsr cguAddCdSp

_ENTRY1         txa
                iny
                sta (arg14),Y
                bne cguInsrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; cguPush3(op,data16)
;======================================
cguPush3        .proc
                jsr cguPush0

                sta (arg14),Y
                beq cguInsrt3._ENTRY2

                .endproc

                ;[fall-through]


;======================================
; cguInsrt3(op,data16)
;======================================
cguInsrt3       .proc
                sty arg13

                ldy #$03
_ENTRY1         jsr cguAddCdSp

_ENTRY2         txa
                ldx arg13
                iny
                sta (arg14),Y
                bne cguInsrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; cguAddCdSp()
;--------------------------------------
; cguAddCdSp(,,size) add QCODE space
; does NOT change QCODE or codeOff
;======================================
cguAddCdSp      .proc
                pha

                clc
                tya
                adc arg14
                sta arg10

                lda #$00
                adc arg15
                sta arg11

                sec
                lda QCODE
                sbc arg14

                tay
                beq _1

_next1          lda (arg14),Y
                sta (arg10),Y

                dey
                bne _next1

                lda (arg14),Y
                sta (arg10),Y

_1              pla
                sta (arg14),Y

                rts
                .endproc
