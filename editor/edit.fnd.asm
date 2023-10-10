
;======================================
;   FILE: edit.fnd.asm
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
;   Find()
;======================================
find            .proc
                jsr setsp
                jsr savewd

                lda lastch
                cmp #$F8
                beq _ENTRY2

                lda #<findmsg
                ldx #>findmsg

_ENTRY1         ldy #>findbuf
                sty arg3

                ldy #<findbuf
                jsr cmdstr

                lda #$F8
                sta curch

_ENTRY2         lda findbuf
                beq _3

_next1          ldy #0
                lda (buf),y
                tay
                iny
                sty arg0

_next2          ldy sp
                iny
                cpy arg0
                bcs _1

                sty sp
                ldx #0

_next3          lda (buf),y
                inx
                cmp findbuf,x
                bne _next2

                iny
                cpx findbuf
                beq found

                cpy arg0
                bcc _next3

_1              jsr nextdwn
                beq _2

                jsr ldbuf

                lda #0
                sta sp
                beq _next1

_2              sta curch

                jsr rstcur
                jsr ldbuf

                lda #<notfnd
                ldx #>notfnd
                jsr cmdmsg

                lda #0
_3              sta curch

                rts
                .endproc


;======================================
;
;======================================

found           .proc
                jsr ctrln

                ldy sp
                dey
                tya
                jsr back._ENTRY1

                lda #$FE

                rts
                .endproc


;--------------------------------------
;--------------------------------------

notfnd          .text 9,"not found"

findmsg         .text 6,"Find? "
