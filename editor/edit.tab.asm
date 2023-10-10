
;======================================
;   FILE: edit.tab.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


;======================================
;   Tab()
;======================================
tab             .proc
                jsr setsp
                jsr tabloc._ENTRY1

_next1          lda TABMAP,x
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

                lda TABMAP,x
                ora tab._onbit,y
                sta TABMAP,x

                rts
                .endproc


;======================================
;
;======================================
clrtab          .proc
                jsr tabloc

                lda TABMAP,x
                and tab._offbit,y
                sta TABMAP,x

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
