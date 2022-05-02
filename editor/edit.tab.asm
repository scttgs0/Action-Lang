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
Tab_             .proc
                jsr SetSpacing
                jsr TabLocation._tabpos

_t1             lda TABMAP,x
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
                jmp Back.back0          ; do the tab

_t2             iny
                cpy #8
                bmi _t1

_t3             ldy #0
                inx
                cpx #15
                bmi _t1

                rts

;--------------------------------------

_onbit          .byte 128,64,32,16,8,4,2,1,0
_offbit         .byte $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE,$FF
                .endproc


;======================================
;
;======================================
SetTab          .proc
                jsr TabLocation

                lda TABMAP,x
                ora Tab_._onbit,y
                sta TABMAP,x
                rts
                .endproc


;======================================
;
;======================================
ClearTab        .proc
                jsr TabLocation

                lda TABMAP,x
                and Tab_._offbit,y
                sta TABMAP,x
                rts
                .endproc


;======================================
;
;======================================
TabLocation     .proc
                jsr SetSpacing

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
