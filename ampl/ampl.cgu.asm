
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.cgu.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   LoadY() value in arg12
;======================================
loady           .proc
                lda cury
                cmp arg12
                beq _XIT

                jsr push0

                cmp #1
                bne _1

                lda #$88                ; DEY
_next1          jsr insrt1
                jmp _3

_1              cmp #0
                bne _2

                lda #$C8                ; INY
                bne _next1

_2              lda #$A0
                ldx arg12
                jsr insrt2              ; LDY #0 or #1

_3              lda arg12
                sta cury

                ldy arg13
_XIT            rts
                .endproc


;======================================
;   TrashY()
;======================================
trashy          .proc
                lda #$FF
                sta cury

                rts
                .endproc


;======================================
;   LoadX(,,offset)
;--------------------------------------
;   NOTE:  this proc can only be called
;   from Op below, see _LXC
;======================================
loadx           .proc
                lda (stack),Y
                iny
                bit tempmode
                bne _1

                bit cnstmode
                beq _3

;   var to load
                jsr stkprop

                beq _2

                lda #$AE                ; LDX addr16
                jmp push3

_1              lda (stack),Y
                tax
                dec temps-args,X

_2              lda #$A6                ; LDX addr
_XIT1           jmp push2

_3              lda (stack),Y

;               tax
;               lda #$A2                ; LDX data
;               bne _XIT1

                sta arg12

                pla
                pla
                pla
                tay
                bne ophigh._opv         ; [unc]

_optype         and #$20
                beq ophigh._operr       ; con. exp.

                jsr stkaddr

                lda arg12
                beq ophigh._4

                inx
                bne ophigh._4

                iny
                jmp ophigh._4


;======================================
;   Load1L
;======================================
load1l          lda #$A1                ; LDA op

                ;[fall-through]


;======================================
;   Op1L(op)
;======================================
op1l            pha

                lda arg2
                ldy #8
oplow           ldx #0
ophigh          stx arg12

;   NOTE:  the order of following
;   comparisons is important!

                tax
                bpl loadx._optype

                bit procmode
                bne _5

                bit arrmode
                bne _6                  ; array

                bit tempmode
                bne _13                 ; temp

                bit cnstmode
                beq _9                  ; constant

;   var if we get here
_opv            jsr stkprop

                beq _14                 ; page zero var

;   16 bit address
_4              pla
                ora #$0C                ; addr16

                jmp push3

;   proc
_5              inc arg12               ; skip JMP byte
                and #8
                beq _opv

_operr          jmp conderr             ; cond. exp.

;   array
_6              bit cnstmode
                bne _7

                jsr loady

;               lda arg7
;               and #$F7
;               sta arg7                ; flag Y reg used

                lda #0
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
                bcc _12                 ; [unc]

_7              tya                     ; small array
                pha

                iny
                iny

                jsr loadx

                pla
                tay
                jsr stkprop
                beq _8                  ; page zero

                pla
                ora #$1C                ; addr16,X

                jmp push3

_8              pla
                ora #$14                ; addr,X

                jmp push2

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
                jmp push2

;   temp
_13             lda #$04                ; addr
                bne _next1              ; [unc]

;   zero-page variable
_14             pla
                ora #$04                ; addr
                jmp push2

                .endproc


;======================================
;   Load2L()
;======================================
load2l          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op2L(op)
;======================================
op2l            .proc
                pha

                lda arg1
                ldy #1
                jmp loadx.oplow

                .endproc


;======================================
;   Load1H()
;======================================
load1h          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op1H(op)
;======================================
op1h            .proc
                ldx arg4
                beq op2h._ophz

                pha
                lda arg2
                ldy #8
_ENTRY1         ldx #1
                jmp loadx.ophigh

                .endproc


;======================================
;   Load2H()
;======================================
load2h          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op2H(op)
;======================================
op2h            .proc
                ldx arg3
                beq _ophz

                pha
                lda arg1
                ldy #1
                bne op1h._ENTRY1

_ophz           ora #$08
                jmp Push2

                .endproc


;--------------------------------------
;--------------------------------------

arrmode         .byte $10
cnstmode        .byte $08
tempmode        .byte $20
procmode        .byte $40

; see CG
outtype         .byte $82,3,$84,realt
                .byte 3,3,$84,realt
                .byte $84,$84,$84,realt
                .byte realt,realt,realt,realt


;======================================
;   GetTemps()
;======================================
gettemps        .proc
                ldx #args+16
                ldy #7
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

        .if ramzap
                inc sargs,X
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
;   LoadI(,,offset)
;======================================
loadi           .proc
                lda (stack),Y
                sta arg15

                tax
                dey

                lda (stack),Y
                sta arg14

                rts
                .endproc


;======================================
;   LdCdZ(,,stkoff)
;======================================
ldcdz           .proc
                lda #0

                .endproc

                ;[fall-through]


;======================================
;   LoadCd(cdoff,,stkoff)
;======================================
loadcd          .proc
                clc
                adc (stack),Y
                sta qcode

                iny
                lda #0
                adc (stack),Y
                sta qcode+1

                rts
                .endproc


;======================================
;   SaveCd(,,offset)
;======================================
savecd          .proc
                lda qcode
                ldx qcode+1
savstk          sta (stack),Y

                txa
                iny
                sta (stack),Y

                rts
                .endproc


;======================================
;   RelOp()
;======================================
relop           .proc
                lda arg6
                bpl _XIT

                inc arg8

_XIT            rts
                .endproc


;======================================
;   ChkZero()
;======================================
chkzero         .proc
                lda arg3
                bne _XIT

                lda arg1
                bpl _XIT                ; not const

                cmp #vart
                bcs _XIT

                ldy #1
                lda (stack),Y

_XIT            rts
                .endproc


;======================================
;   OpCd1()
;======================================
opcd1           .proc
                ldx arg8
                lda cgopscd+1,X

                rts
                .endproc


;======================================
;   StkAddr(,,offset)
;======================================
stkaddr         .proc
                lda (stack),Y
                tax

                iny
                lda (stack),Y
                tay

                rts
                .endproc


;======================================
;   StkP(,,offset)
;======================================
stkp            .proc
                jsr stkaddr

                lda #1
                jmp gprop

                .endproc


;======================================
;   StkPZ(,,offset)
;======================================
stkpz           .proc
                lda #0

                .endproc

                ;[fall-through]


;======================================
;    StkPS(,,offset)
;======================================
stkps           .proc
                sta arg12

                .endproc

                ;[fall-through]


;======================================
;    StkProp(,,offset)
;======================================
stkprop         .proc
                jsr stkp

                clc
                adc arg12

                tax
                iny

                lda (props),Y
                adc #0
                tay

                rts
                .endproc


;======================================
;   JSRTable(,index)
;======================================
jsrtable        .proc
;           .if RAMzap
;               ldy LTab+1,X
;               lda LTab,X
;           .else
                ldy lsh+1,X
                lda lsh,X
;           .endif

                tax
                lda #$20                ; JSR opcode

                jmp push3

                .endproc


;======================================
;   Push0()
;======================================
push0           .proc
                sty arg13

                ldy qcode
                sty arg14
                ldy qcode+1
                sty arg15

                ldy #0

                rts
                .endproc


;======================================
;   PushTrue(op)
;======================================
pushtrue        .proc
                jsr push1

                ldy #10
                jsr savecd

                lda #0                  ; no other true branches
                sta arg9

                .endproc

                ;[fall-through]


;======================================
;   Push1(op)
;======================================
push1           .proc
                jsr push0

                sta (arg14),Y
                beq insrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Insrt1(op)
;======================================
insrt1          .proc
                ldy #1
                jsr addcdsp

_ENTRY1         iny
                tya

                jmp codeincr

                .endproc


;======================================
;   STempH()
;======================================
stemph          .proc
                inc arg9

                ldy #12
                bne stempl._ENTRY1      ; [unc]

                .endproc


;======================================
;   STempL()
;======================================
stempl          .proc
                ldy #10

_ENTRY1         jsr savecd

                lda arg9
                tax
                and #$FE                ; set to low address
                sta arg9

                lda #$85                ; STA addr

                .endproc

                ;[fall-through]


;======================================
;   Push2(op,,data)
;======================================
push2           .proc
                jsr push0

                sta (arg14),Y
                beq insrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Insrt2(op,data)
;======================================
insrt2          .proc
                ldy #2
                jsr addcdsp

_ENTRY1         txa
                iny
                sta (arg14),Y
                bne insrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Push3(op,data16)
;======================================
push3           .proc
                jsr push0

                sta (arg14),Y
                beq insrt3._ENTRY2

                .endproc

                ;[fall-through]


;======================================
;   Insrt3(op,data16)
;======================================
insrt3          .proc
                sty arg13

                ldy #3
_ENTRY1         jsr addcdsp

_ENTRY2         txa
                ldx arg13
                iny
                sta (arg14),Y
                bne insrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   AddCdSp()
;--------------------------------------
;   AddCdSp(,,size) add qcode space
;   does NOT change qcode or codeOff
;======================================
addcdsp         .proc
                pha

                clc
                tya
                adc arg14
                sta arg10

                lda #0
                adc arg15
                sta arg11

                sec
                lda qcode
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
