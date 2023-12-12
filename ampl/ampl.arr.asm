
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.arr.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   ArrRef()
;======================================
arrref          .proc
                ldx nxttoken
                cpx #lparen
                beq arrconst._2

                cpx #uparrow
                beq arrconst._2

arrvar          ldy #vart+cardt         ; no index!
                sty token
                cmp #arrayt+8
                bcc arrconst._XIT1

arrconst        jsr procref._ENTRY1

_XIT1           jmp pushst

_next1          ldy #0
                lda (stack),Y
                cmp #arrayt+8
                bcs _1                  ; small array

                iny
                jsr stkp

                cpx #0
                bne _1

;   page zero pointer
                ldy #1
                sta (stack),Y
                dey
                lda (stack),Y
                ora #$B0                ; temp array mode
                sta (stack),Y

                rts

_1              jsr zerost
                bne _3                  ; [unc]

_2              jsr pushnext

                cmp #uparrow
                beq _next1

                jsr getexp

                cmp #rparen
                bne arrerr

                ldx op
                bne arrerr

_3              ldy #7
                lda (stack),Y
arra0           pha

                lda #vart+cardt
                sta (stack),Y

                lda #plusid
                jsr genops

                pla
                cmp #arrayt+8
                bcs arrerr._small

                and #7
                tax
                ora #$B0                ; temp array mode
                sta arg7

                ldy arg1
                cpy #constt+strt
                ldy #1                  ; clear Z flag if we branch
                bcs _4

                lda (stack),Y
                iny
                ora (stack),Y
_4              sta fr1
                beq _5                  ; pointer

                ldy vartype-1,X
                beq arrerr._XIT2

;               cpy #3
;               beq _ARReal

;   integer or cardinal

_5              jsr gettemps

                lda #$A1                ; LDA
                ldx fr1
                beq _6

                jsr load2l

                lda #$0A                ; ASL A
                ldx #$08                ; PHP
                ldy #$18                ; CLC
                jsr push3

                lda #$61                ; ADC
        .if ramzap
                sta (arg8),Y
        .else
                nop
                nop
        .endif

_6              jsr loadx.op1l
                jsr stempl

                lda #$A1                ; LDA
                ldx fr1
                beq _7

                jsr load2h

                lda #$2A                ; ROL A
                ldx #$28                ; PLP, restore carry
                jsr push2

                lda #$61                ; ADC
_7              jsr op1h
                jmp cgadd._ENTRY2

arrerr          ldy #arrer              ; bad array ref
                jmp splerr

_XIT2           jmp codegen._ENTRY1

;   small arrary

_small          ldy #7
                sta (stack),Y           ; restore correct type

                lda arg1
                bpl arrerr              ; can't index with bool.

                bit arrmode
                bne arrerr              ; can't index with array

                ldy #10
                sta (stack),Y

                ldy #2
                jsr loadi

                ldy #11
                jsr savecd.savstk

                jmp popst

                .endproc
