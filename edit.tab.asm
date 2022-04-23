;======================================
;   FILE: edit.tab.asm
;======================================

; Copyright 1983 by Action Computer Services
; All rights reserved.

; last modified March 9, 1983


;    Tab()
;    -----
tab             .proc
                jsr setsp
                jsr tabloc._tabpos

_t1             lda tabmap,x
                beq _t3
                and _onbit,y
                beq _t2
    ; found tab setting
                sty arg0
                txa
                asl a
                asl a
                asl a
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

_onbit          .byte $80,$40,$20,$10,8,4,2,1,0
_offbit         .byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe,$ff
                .endproc


settab          .proc
                jsr tabloc
                lda tabmap,x
                ora tab._onbit,y
                sta tabmap,x
                rts
                .endproc


clrtab          .proc
                jsr tabloc
                lda tabmap,x
                and tab._offbit,y
                sta tabmap,x
                rts
                .endproc


tabloc         .proc
                jsr setsp
                sec
                sbc #1
_tabpos         tay
                lsr a
                lsr a
                lsr a
                tax
                tya
                and #7
                tay
                cpx #15
                bmi _tp1
                ldy #8
_tp1            rts
                .endproc
