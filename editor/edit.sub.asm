
;======================================
;   FILE: edit.sub.asm
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
;   Subst()
;======================================
subst           .proc
                jsr setsp
                jsr savewd

                lda lastch
                cmp #$7D
                beq _2

                pha

                lda #<submsg
                ldx #>submsg

                ldy #>subbuf
                sty arg3
                ldy #<subbuf

                jsr cmdstr

                pla

;   check for ESC key
                ldx subbuf
                bne _1

                ldx subbuf+1
                cpx #$1B
                beq _XIT1

_1              cmp #$F8
                beq _3                  ; string already found

                lda #<formsg
                ldx #>formsg
                jsr find._ENTRY1
                bne _3

_XIT1           rts

_2              jsr find._ENTRY2
                beq _XIT1

_3              lda #$7D
                sta curch
                sta dirtyf              ; flag line as dirty

                sec
                lda subbuf
                sbc findbuf             ; get delta size
                sta arg3

                php                     ; save status for test below
                bcs _4

                lda #1
                sbc arg3                ; negate delta size

_4              clc
                adc buf
                sta arg0

                lda #0
                tay
                adc buf+1
                sta arg1

                lda (buf),y
                plp
                bcc _5                  ; need to remove space
                beq _6                  ; same size

;   need to add space
                tay
_next1          lda (buf),y
                sta (arg0),y

                dey
                cpy sp
                bcs _next1
                bcc _6                  ; [unc]

_5              sta arg2

                ldy sp
                dey

_next2          iny
                lda (arg0),y
                sta (buf),y

                cpy arg2
                bcc _next2

_6              ldy sp
                ldx #0
                beq _7

_next3          inx
                lda subbuf,x
                sta (buf),y

                iny
_7              cpx subbuf
                bne _next3

                clc
                ldy #0
                lda (buf),y
                adc arg3
                sta (buf),y

                jmp rfrshbuf

                .endproc

;--------------------------------------

submsg          .text 12,"Substitute? "

formsg          .text 5,"for? "
