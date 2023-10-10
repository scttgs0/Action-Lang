
; SPDX-FileName: edit.tab.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Tab()
;======================================
Tab_             .proc
                jsr SetSpacing
                jsr TabLocation._ENTRY1

_next1          ;!!lda TABMAP,x
                beq _2

                and _onbit,y
                beq _1

;   found tab setting
                sty arg0

                txa
                asl
                asl
                asl
                ora arg0

                jmp Back._ENTRY1        ; do the tab

_1              iny
                cpy #8
                bmi _next1

_2              ldy #0
                inx
                cpx #15
                bmi _next1

                rts

;--------------------------------------

_onbit          .byte $80,$40,$20,$10
                .byte $08,$04,$02,$01
                .byte $00
_offbit         .byte $7F,$BF,$DF,$EF
                .byte $F7,$FB,$FD,$FE
                .byte $FF

                .endproc


;======================================
;   SetTab()
;======================================
SetTab          .proc
                jsr TabLocation

                ;!!lda TABMAP,x
                ora Tab_._onbit,y
                ;!!sta TABMAP,x

                rts
                .endproc


;======================================
;   ClearTab()
;======================================
ClearTab        .proc
                jsr TabLocation

                ;!!lda TABMAP,x
                and Tab_._offbit,y
                ;!!sta TABMAP,x

                rts
                .endproc


;======================================
;   TabLocation()
;======================================
TabLocation     .proc
                jsr SetSpacing

                sec
                sbc #1

_ENTRY1         tay
                lsr
                lsr
                lsr

                tax
                tya
                and #7

                tay
                cpx #15
                bmi _XIT

                ldy #8

_XIT            rts
                .endproc
