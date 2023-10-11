
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
;   Get next key in buffer
;======================================
lgetkey         .proc
                clc                     ; blink cursor
                lda rtclok+2
                adc #14

                tax
_next1          lda CH_                 ; key down?
                eor #$FF
                bne _1

                cpx rtclok+2
                bpl _next1

                ldy #0
                lda (oldadr),y
                eor #$80
                sta (oldadr),y

                jmp lgetkey

_1              ldy #0
                lda oldchr
                eor #$80
                sta (oldadr),y          ; restore cursor

                ldx SRTIMR              ; faster repeat
                cpx #$0C
                bcs _6

                cpx #4
                bcc _2

                ldx #3
_next2          stx SRTIMR

_2              lda CH_
                cmp #$C0
                bcc _3                  ; not Ctrl-Shft

                jsr click
                bmi _4                  ; [unc]

_3              and #$3F
                cmp #$3C                ; caps key
                beq _7

                cmp #$27                ; Atari key
                beq _8

                ldx #$70
                lda #7                  ; GETCHR
                sta brkkey              ; ignore BREAK key

                jsr putch._ENTRY2

_4              ldx SRTIMR
                cpx #10
                bcs _5

                ldx #3
                stx SRTIMR

_5              sta curch

                rts

_6              ldx #20
                bne _next2

_7              lda CH_
                and #$C0                ; isolate control (128) and uppercase (64)
                sta SHFLOC

_next3          jsr click
                bmi lgetkey

_8              lda INVFLG
                eor #$80
                sta INVFLG

                jmp _next3

                .endproc


;======================================
;   Click() click the keyboard
;======================================
click           .proc
                ldx #$7F
_next1          stx CONSOL
                stx WSYNC

                dex
                bpl _next1

                stx CH_

                rts
                .endproc
