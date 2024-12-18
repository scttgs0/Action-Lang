
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.array.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   ArrRef()
;======================================
ArrRef          .proc
                ldx nxttoken
                cpx #tokLParen
                beq arrconst._2

                cpx #tokUpArrow
                beq arrconst._2

arrvar          ldy #tokVAR_t+tokCARD_t ; no index!
                sty token
                cmp #tokARRAY_t+8
                bcc arrconst._XIT1

arrconst        jsr procref._ENTRY1

_XIT1           jmp pushst

_next1          ldy #0
                lda (stack),Y
                cmp #tokARRAY_t+8
                bcs _1                  ; small array

                iny
                jsr StkP

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
                bra _3

_2              jsr pushnext

                cmp #tokUpArrow
                beq _next1

                jsr getexp

                cmp #tokRParen
                bne arrerr

                ldx zpAllocOP
                bne arrerr

_3              ldy #7
                lda (stack),Y
arra0           pha

                lda #tokVAR_t+tokCARD_t
                sta (stack),Y

                lda #tokPLUS
                jsr genops

                pla
                cmp #tokARRAY_t+8
                bcs arrerr._small

                and #7
                tax
                ora #$B0                ; temp array mode
                sta arg7

                ldy arg1
                cpy #tokCONST_t+tokSTR_t
                ldy #1                  ; clear Z flag if we branch
                bcs _4

                lda (stack),Y
                iny
                ora (stack),Y
_4              sta FR1
                beq _5                  ; pointer

                ldy vartype-1,X
                beq arrerr._XIT2

                ; cpy #3
                ; beq _ARReal

;   integer or cardinal

_5              jsr GetTemps

                lda #$A1                ; LDA
                ldx FR1
                beq _6

                jsr Load2L

                lda #$0A                ; ASL A
                ldx #$08                ; PHP
                ldy #$18                ; CLC
                jsr Push3

                lda #$61                ; ADC
            .if ZAPRAM
                sta (arg8),Y
            .else
                nop
                nop
            .endif

_6              jsr LoadX.Op1L
                jsr STempL

                lda #$A1                ; LDA
                ldx FR1
                beq _7

                jsr Load2H

                lda #$2A                ; ROL A
                ldx #$28                ; PLP, restore carry
                jsr Push2

                lda #$61                ; ADC
_7              jsr Op1H
                jmp cgadd._ENTRY2

arrerr          ldy #arrayERR           ; bad array ref
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
                jsr LoadI

                ldy #11
                jsr SaveCd._saveStack

                jmp popst

                .endproc