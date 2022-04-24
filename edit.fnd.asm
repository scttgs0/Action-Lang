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
                cmp #$f8
                beq find2

                lda #<findmsg
                ldx #>findmsg
find1           ldy #>findbuf
                sty arg3
                ldy #<findbuf
                jsr cmdstr

                lda #$f8
                sta curch

find2           lda findbuf
                beq _f7

_f2             ldy #0
                lda (buf),y
                tay
                iny
                sty arg0

_f3             ldy sp
                iny
                cpy arg0
                bcs _f5

                sty sp
                ldx #0

_f4             lda (buf),y
                inx
                cmp findbuf,x
                bne _f3

                iny
                cpx findbuf
                beq found

                cpy arg0
                bcc _f4

_f5             jsr nextdwn
                beq _f6

                jsr ldbuf

                lda #0
                sta sp
                beq _f2

_f6             sta curch
                jsr rstcur
                jsr ldbuf

                lda #<notfnd
                ldx #>notfnd
                jsr cmdmsg

                lda #0
_f7             sta curch
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
                jsr back.back0

                lda #$fe
                rts
                .endproc

;--------------------------------------
;--------------------------------------

notfnd          .text 9,"not found"

findmsg         .text 6,"Find? "
