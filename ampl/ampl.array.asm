
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.array.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


array           .namespace

;======================================
; Ref()
;======================================
Ref             .proc
                ldx nxttoken
                cpx #tokLParen
                beq arrconst._2

                cpx #tokUpArrow
                beq arrconst._2

arrvar          ldy #tokVAR_t+tokCARD_t ; no index!
                sty token
                cmp #tokARRAY_t+8
                bcc arrconst._XIT1

arrconst        jsr compiler.ProcRef._ENTRY1

_XIT1           jmp compiler.PushST

_next1          ldy #$00
                lda (stack),Y
                cmp #tokARRAY_t+8
                bcs _1                  ; small array

                iny
                jsr ampl.cgu.StkP

                cpx #$00
                bne _1

;   page zero pointer
                ldy #$01
                sta (stack),Y

                dey
                lda (stack),Y
                ora #$B0                ; temp array mode
                sta (stack),Y

                rts

_1              jsr compiler.ZeroST
                bne _3                  ; [unc]

_2              jsr compiler.PushNext

                cmp #tokUpArrow
                beq _next1

                jsr compiler.GetExp

                cmp #tokRParen
                bne arrerr

                ldx zpAllocOP
                bne arrerr

_3              ldy #$07
                lda (stack),Y
arra0           pha

                lda #tokVAR_t+tokCARD_t
                sta (stack),Y

                lda #tokPLUS
                jsr compiler.GenOps

                pla
                cmp #tokARRAY_t+8
                bcs arrerr._small

                and #$07
                tax
                ora #$B0                ; temp array mode
                sta arg7

                ldy arg1
                cpy #tokCONST_t+tokSTR_t
                ldy #$01                ; clear Z flag if we branch
                bcs _4

                lda (stack),Y
                iny
                ora (stack),Y
_4              sta FR1
                beq _5                  ; pointer

                ldy compiler.vartype-1,X
                beq arrerr._XIT2

                ; cpy #$03
                ; beq _ARReal

;   integer or cardinal

_5              jsr ampl.cgu.GetTemps

                lda #$A1                ; LDA
                ldx FR1
                beq _6

                jsr ampl.cgu.Load2L

                lda #$0A                ; ASL A
                ldx #$08                ; PHP
                ldy #$18                ; CLC
                jsr ampl.cgu.Push3

                lda #$61                ; ADC
            .if ZAPRAM
                sta (arg8),Y
            .else
                nop
                nop
            .endif

_6              jsr ampl.cgu.Op1L
                jsr ampl.cgu.STempL

                lda #$A1                ; LDA
                ldx FR1
                beq _7

                jsr ampl.cgu.Load2H

                lda #$2A                ; ROL A
                ldx #$28                ; PLP, restore carry
                jsr ampl.cgu.Push2

                lda #$61                ; ADC
_7              jsr ampl.cgu.Op1H
                jmp compiler.CGAdd._ENTRY2

arrerr          ldy #arrayERR           ; bad array ref
                jmp bankSPLErr

_XIT2           jmp compiler.CodeGen._ENTRY1

;   small arrary
_small          ldy #$07
                sta (stack),Y           ; restore correct type

                lda arg1
                bpl arrerr              ; can't index with bool.

                bit ampl.cgu.modeArr
                bne arrerr              ; can't index with array

                ldy #$0A
                sta (stack),Y

                ldy #$02
                jsr ampl.cgu.LoadI

                ldy #$0B
                jsr ampl.cgu.SaveCd._ToStack

                jmp compiler.PopST

                .endproc

                .endnamespace
