
; SPDX-FileName: ampl.cgu.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   LoadY() value in arg12
;======================================
LoadY           .proc
                lda cury
                cmp arg12
                beq _XIT

                jsr Push0

                cmp #1
                bne _1

                lda #$88                ; DEY
_next1          jsr Insrt1

                jmp _3

_1              cmp #0
                bne _2

                lda #$C8                ; INY
                bne _next1

_2              lda #$A0
                ldx arg12
                jsr Insrt2              ; LDY #0 or #1

_3              lda arg12
                sta cury

                ldy arg13
_XIT            rts
                .endproc


;======================================
;   TrashY()
;======================================
TrashY          .proc
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
LoadX           .proc

                lda (stack),y
                iny
                bit tempmode
                bne _1

                bit cnstmode
                beq _3

;   var to load
                jsr StkProp

                beq _2

                lda #$AE                ; LDX addr16
                jmp Push3

_1              lda (stack),y
                tax
                dec temps-args,x

_2              lda #$A6                ; LDX addr
_XIT1           jmp Push2

_3              lda (stack),y

;               tax
;               lda #$A2                ; LDX data
;               bne _XIT1

                sta arg12

                pla
                pla
                pla
                tay
                bra ophigh._opv

_optype         and #$20
                beq ophigh._operr       ; con. exp.

                jsr StkAddr

                lda arg12
                beq ophigh._4

                inx
                bne ophigh._4

                iny
                jmp ophigh._4


;======================================
;   Load1L
;======================================
Load1L          lda #$A1                ; LDA op

                ;[fall-through]


;======================================
;   Op1L(op)
;======================================
Op1L            pha

                lda arg2
                ldy #8
oplow           ldx #0
ophigh          stx arg12

; NOTE:  the order of following
; comparisons is important!

                tax
                bpl LoadX._optype

                bit procmode
                bne _5

                bit arrmode
                bne _6                  ; array

                bit tempmode
                bne _13                 ; temp

                bit cnstmode
                beq _9                  ; constant

;   var if we get here
_opv            jsr StkProp

                beq _14                 ; page zero var

;   16 bit address
_4              pla
                ora #$0C                ; addr16

                jmp Push3

;   proc
_5              inc arg12               ; skip JMP byte
                and #8
                beq _opv

_operr          jmp conderr             ; cond. exp.

;   array
_6              bit cnstmode
                bne _7

                jsr LoadY

                ; lda arg7
                ; and #$F7
                ; sta arg7 ; flag Y reg used

                lda #0
                sta arg12

                lda #$10                ; (addr),Y
_next1          sta arg10

                lda (stack),y
                clc
                adc arg12
                cmp #args
                bcc _11

                cmp #args+16
                bcs _11

                tax
                dec temps-args,x        ; free temp
                bra _12

_7              tya                     ; small array
                pha

                iny
                iny

                jsr LoadX

                pla
                tay
                jsr StkProp
                beq _8                  ; page zero

                pla
                ora #$1C                ; addr16,X

                jmp Push3

_8              pla
                ora #$14                ; addr,X

                jmp Push2

;   constant
_9              lda #$08                ; data
                sta arg10

                lda arg12
                beq _10

                iny

_10             lda (stack),y
_11             tax
_12             pla
                ora arg10               ; op mode
                jmp Push2

;   temp
_13             lda #$04                ; addr
                bra _next1

;   zero-page variable
_14             pla
                ora #$04                ; addr
                jmp Push2

                .endproc


;======================================
;   Load2L()
;======================================
Load2L          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op2L(op)
;======================================
Op2L            .proc
                pha

                lda arg1
                ldy #1
                jmp LoadX.oplow

                .endproc


;======================================
;   Load1H()
;======================================
Load1H          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op1H(op)
;======================================
Op1H            .proc
                ldx arg4
                beq Op2H._ophz

                pha
                lda arg2
                ldy #8
_ENTRY1         ldx #1
                jmp LoadX.ophigh

                .endproc


;======================================
;   Load2H()
;======================================
Load2H          .proc
                lda #$A1                ; LDA op

                .endproc

                ;[fall-through]


;======================================
;   Op2H(op)
;======================================
Op2H            .proc
                ldx arg3
                beq _ophz

                pha
                lda arg1
                ldy #1
                bne Op1H._ENTRY1

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
outtype         .byte $82,3,$84,tokREAL_t
                .byte 3,3,$84,tokREAL_t
                .byte $84,$84,$84,tokREAL_t
                .byte tokREAL_t,tokREAL_t,tokREAL_t,tokREAL_t


;======================================
;   GetTemps()
;======================================
GetTemps        .proc
                ldx #args+16
                ldy #7
_next1          dex
                dex
                dey
                bmi _err                ; exp. too complex

                lda temps-args,x
                bne _next1

                inc temps-args,x

                lda arg5                ; see if byte temp
                beq _1                  ;   yes

                inc temps-args+1,x

        .if ramzap
                inc SArgs,x
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
LoadI           .proc
                lda (stack),y
                sta arg15

                tax
                dey

                lda (stack),y
                sta arg14

                rts
                .endproc


;======================================
;   LdCdZ(,,stkoff)
;======================================
LdCdZ           .proc
                lda #0

                .endproc

                ;[fall-through]


;======================================
;   LoadCd(cdoff,,stkoff)
;======================================
LoadCd          .proc
                clc
                adc (stack),y
                sta qcode

                iny
                lda #0
                adc (stack),y
                sta qcode+1

                rts
                .endproc


;======================================
;   SaveCd(,,offset)
;======================================
SaveCd          .proc
                lda qcode
                ldx qcode+1
_saveStack      sta (stack),y

                txa
                iny
                sta (stack),y

                rts
                .endproc


;======================================
;   RelOp()
;======================================
RelOp           .proc
                lda arg6
                bpl _XIT

                inc arg8

_XIT            rts
                .endproc


;======================================
;   ChkZero()
;======================================
ChkZero         .proc
                lda arg3
                bne _XIT

                lda arg1
                bpl _XIT                ; not const

                cmp #tokVAR_t
                bcs _XIT

                ldy #1
                lda (stack),y

_XIT            rts
                .endproc


;======================================
;   OpCd1()
;======================================
OpCd1           .proc
                ldx arg8
                lda cgopscd+1,x

                rts
                .endproc


;======================================
;   StkAddr(,,offset)
;======================================
StkAddr         .proc
                lda (stack),y
                tax

                iny
                lda (stack),y
                tay

                rts
                .endproc


;======================================
;   StkP(,,offset)
;======================================
StkP            .proc
                jsr StkAddr

                lda #1
                jmp gprop

                .endproc


;======================================
;   StkPZ(,,offset)
;======================================
StkPZ           .proc
                lda #0

                .endproc

                ;[fall-through]


;======================================
;    StkPS(,,offset)
;======================================
StkPS           .proc
                sta arg12

                .endproc

                ;[fall-through]


;======================================
;    StkProp(,,offset)
;======================================
StkProp         .proc
                jsr StkP

                clc
                adc arg12

                tax
                iny

                lda (props),y
                adc #0
                tay

                rts
                .endproc


;======================================
;   JSRTable(,index)
;======================================
JSRTable        .proc
;           .if RAMzap
;               ldy LTab+1,X
;               lda LTab,X
;           .else
                ldy jt_lsh+1,x
                lda jt_lsh,x
;           .endif

                tax
                lda #$20                ; JSR opcode

                jmp Push3

                .endproc


;======================================
;   Push0()
;======================================
Push0           .proc
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
PushTrue        .proc
                jsr Push1

                ldy #10
                jsr SaveCd

                lda #0                  ; no other true branches
                sta arg9

                .endproc

                ;[fall-through]


;======================================
;   Push1(op)
;======================================
Push1           .proc
                jsr Push0

                sta (arg14),y
                beq Insrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Insrt1(op)
;======================================
Insrt1          .proc
                ldy #1
                jsr AddCdSp

_ENTRY1         iny
                tya

                jmp codeincr

                .endproc


;======================================
;   STempH()
;======================================
STempH          .proc
                inc arg9

                ldy #12
                bra STempL._ENTRY1

                .endproc


;======================================
;   STempL()
;======================================
STempL          .proc
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
Push2           .proc
                jsr Push0

                sta (arg14),y
                beq Insrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Insrt2(op,data)
;======================================
Insrt2          .proc
                ldy #2
                jsr AddCdSp

_ENTRY1         txa
                iny
                sta (arg14),y
                bne Insrt1._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   Push3(op,data16)
;======================================
Push3           .proc
                jsr Push0

                sta (arg14),y
                beq Insrt3._ENTRY2

                .endproc

                ;[fall-through]


;======================================
;   Insrt3(op,data16)
;======================================
Insrt3          .proc
                sty arg13

                ldy #3
_ENTRY1         jsr AddCdSp

_ENTRY2         txa
                ldx arg13
                iny
                sta (arg14),y
                bne Insrt2._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   AddCdSp()
;--------------------------------------
; AddCdSp(,,size) add qcode space
; does NOT change qcode or codeOff
;======================================
AddCdSp         .proc
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

_next1          lda (arg14),y
                sta (arg10),y

                dey
                bne _next1

                lda (arg14),y
                sta (arg10),y

_1              pla
                sta (arg14),y

                rts
                .endproc
