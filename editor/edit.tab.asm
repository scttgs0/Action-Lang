
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
                jsr tabloc._tabpos

_t1             lda TABMAP,X
                beq _t3

                and _onbit,Y
                beq _t2

    ; found tab setting
                sty arg0
                txa
                asl
                asl
                asl
                ora arg0
                jmp back.back0          ; do the tab

_t2             iny
                cpy #8
                bmi _t1

_t3             ldy #0
                inx
                cpx #15
                bmi _t1

                rts

;--------------------------------------

_onbit          .byte $80,$40,$20,$10,8,4,2,1,0
_offbit         .byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe,$ff

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
tabloc         .proc
                jsr setsp

                sec
                sbc #1
_tabpos         tay
                lsr
                lsr
                lsr
                tax
                tya
                and #7
                tay
                cpx #15
                bmi _tp1

                ldy #8
_tp1            rts
                .endproc
