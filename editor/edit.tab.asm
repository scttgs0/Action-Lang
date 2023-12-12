
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.tab.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   Tab()
;======================================
tab             .proc
                jsr setsp
                jsr tabloc._ENTRY1

_next1          lda TABMAP,X
                beq _2

                and _onbit,Y
                beq _1

;   found tab setting
                sty arg0

                txa
                asl
                asl
                asl
                ora arg0

                jmp back._ENTRY1          ; do the tab

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
;
;======================================
settab          .proc
                jsr tabloc

                lda TABMAP,X
                ora tab._onbit,Y
                sta TABMAP,X

                rts
                .endproc


;======================================
;
;======================================
clrtab          .proc
                jsr tabloc

                lda TABMAP,X
                and tab._offbit,Y
                sta TABMAP,X

                rts
                .endproc


;======================================
;
;======================================
tabloc          .proc
                jsr setsp

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
