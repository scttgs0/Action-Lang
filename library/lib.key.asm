;======================================
;   FILE: lib.key.asm
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
;
;======================================
lgetkey         .proc
    ; Get next key in buffer
                clc                     ; blink cursor
                lda rtclok+2
                adc #14
                tax
_bc1            lda $02FC ;!! CH_                 ; key down?
                eor #$ff
                bne _gk0

                cpx rtclok+2
                bpl _bc1

                ldy #0
                lda (oldadr),y
                eor #$80
                sta (oldadr),y
                jmp lgetkey

_gk0            ldy #0
                lda oldchr
                eor #$80
                sta (oldadr),y          ; restore cursor
                ldx $022B ;!! SRTIMR              ; faster repeat
                cpx #$0c
                bcs _gk5

                cpx #4
                bcc _gk2

                ldx #3
_gk1            stx $022B ;!! SRTIMR
_gk2            lda $02FC ;!! CH_
                cmp #$c0
                bcc _gk3                ; not Ctrl-Shft

_cskey          jsr click
                bmi _gk4                ; uncond.

_gk3            and #$3f
                cmp #$3c                ; caps key
                beq _caps

                cmp #$27                ; Atari key
                beq _atari

_gkey           ldx #$70
                lda #7                  ; GETCHR
                sta brkkey              ; ignore BREAK key
                jsr putch.putch2

_gk4            ldx $022B ;!! SRTIMR
                cpx #10
                bcs _gkret

                ldx #3
                stx $022B ;!! SRTIMR
_gkret          sta curch
                rts

_gk5            ldx #20
                bne _gk1

_caps           lda $02FC ;!! CH_
                and #$c0
                sta $02BE ;!! SHFLOC
_caps1          jsr click
                bmi lgetkey

_atari          lda $02B6 ;!! INVFLG
                eor #$80
                sta $02B6 ;!! INVFLG
                jmp _caps1

                .endproc


;======================================
;   Click() click the keyboard
;======================================
click           .proc
                ldx #$7f
_click1         stx $D01F ;!! CONSOL
                stx $D40A ;!! WSYNC
                dex
                bpl _click1

                stx $02FC ;!! CH_
                rts
                .endproc
