
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
;   Substitute()
;======================================
Substitute      .proc
                jsr SetSpacing
                jsr SaveWindow

                lda lastch
                cmp #$7d
                beq _s2

                pha
                lda #<submsg
                ldx #>submsg
                ldy #>subbuf
                sty arg3
                ldy #<subbuf
                jsr CommandString

                pla

    ; check for ESC key
                ldx subbuf
                bne _s0

                ldx subbuf+1
                cpx #$1b
                beq _s1

_s0             cmp #$f8
                beq _s3                 ; string already found

                lda #<formsg
                ldx #>formsg
                jsr Find.find1
                bne _s3

_s1             rts

_s2             jsr Find.find2
                beq _s1

_s3             lda #$7d
                sta curch
                sta isDirty             ; flag line as dirty
                sec
                lda subbuf
                sbc findbuf             ; get delta size
                sta arg3
                php                     ; save status for test below
                bcs _s4

                lda #1
                sbc arg3                ; negate delta size
_s4             clc
                adc buf
                sta arg0
                lda #0
                tay
                adc buf+1
                sta arg1
                lda (buf),y
                plp
                bcc _s6                 ; need to remove space
                beq _s8                 ; same size

    ; need to add space
                tay
_s5             lda (buf),y
                sta (arg0),y
                dey
                cpy sp
                bcs _s5
                bcc _s8

_s6             sta arg2
                ldy sp
                dey
_s7             iny
                lda (arg0),y
                sta (buf),y
                cpy arg2
                bcc _s7

_s8             ldy sp
                ldx #0
                beq _s10

_s9             inx
                lda subbuf,x
                sta (buf),y
                iny
_s10            cpx subbuf
                bne _s9

                clc
                ldy #0
                lda (buf),y
                adc arg3
                sta (buf),y
                jmp RefreshBuf

                .endproc

;--------------------------------------
;--------------------------------------

submsg          .text 12,"Substitute? "

formsg          .text 5,"for? "
