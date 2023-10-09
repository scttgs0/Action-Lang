
; SPDX-FileName: ampl.arr.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   ArrRef()
;======================================
ArrRef          .proc
                ldx nxttoken
                cpx #tokLParen
                beq arrconst._arr0

                cpx #tokUpArrow
                beq arrconst._arr0

arrvar          ldy #tokVAR_t+tokCARD_t ; no index!
                sty token
                cmp #tokARRAY_t+8
                bcc arrconst._arr1

arrconst        jsr procref.stconst

_arr1           jmp pushst

_arptr          ldy #0
                lda (stack),y
                cmp #tokARRAY_t+8
                bcs _arpt1              ; small array

                iny
                jsr StkP

                cpx #0
                bne _arpt1

;   page zero pointer
                ldy #1
                sta (stack),y

                dey
                lda (stack),y
                ora #$B0                ; temp array mode
                sta (stack),y

                rts

_arpt1          jsr zerost
                bra _ar0

_arr0           jsr pushnext

                cmp #tokUpArrow
                beq _arptr

                jsr getexp

                cmp #tokRParen
                bne arrerr

                ldx op
                bne arrerr

_ar0            ldy #7
                lda (stack),y
arra0           pha

                lda #tokVAR_t+tokCARD_t
                sta (stack),y

                lda #tokPLUS
                jsr genops

                pla
                cmp #tokARRAY_t+8
                bcs arrerr._arsmall

                and #7
                tax
                ora #$B0                ; temp array mode
                sta arg7

                ldy arg1
                cpy #tokCONST_t+tokSTR_t
                ldy #1                  ; clear Z flag if we branch
                bcs _ar1

                lda (stack),y
                iny
                ora (stack),y
_ar1            sta FR1
                beq _arint              ; pointer

                ldy vartype-1,x
                beq arrerr._arbyte

                ; cpy #3
                ; beq _ARReal

;   integer or cardinal

_arint          jsr GetTemps

                lda #$A1                ; LDA
                ldx FR1
                beq _ari1

                jsr Load2L

                lda #$0A                ; ASL A
                ldx #$08                ; PHP
                ldy #$18                ; CLC
                jsr Push3

                lda #$61                ; ADC
        .if ramzap
                sta (arg8),y
        .else
                nop
                nop
        .endif

_ari1           jsr LoadX.Op1L
                jsr STempL

                lda #$A1                ; LDA
                ldx FR1
                beq _ari2

                jsr Load2H

                lda #$2A                ; ROL A
                ldx #$28                ; PLP, restore carry
                jsr Push2

                lda #$61                ; ADC
_ari2           jsr Op1H
                jmp cgadd.cgadd2

arrerr          ldy #arrayERR           ; bad array ref
                jmp splerr

_arbyte         jmp codegen.cg1

_arsmall        ldy #7
                sta (stack),y           ; restore correct type

                lda arg1
                bpl arrerr              ; can't index with bool.

                bit arrmode
                bne arrerr              ; can't index with array

                ldy #10
                sta (stack),y

                ldy #2
                jsr LoadI

                ldy #11
                jsr SaveCd._saveStack

                jmp popst

                .endproc
