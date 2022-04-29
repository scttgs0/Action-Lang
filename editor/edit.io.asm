;======================================
;   FILE: edit.io.asm
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
;   GetStr(prompt, str, invert)
;======================================
getstr          .proc
                jsr dspstr
_gs1            jsr getkey

                tax
                cpx #$7e
                beq _gs3                ; backspace

                cpx #$7d
                beq _gs3                ; clear

_gs1a           ldy #0
                clc
                lda (arg12),y
                adc #1

                cpx #$1b                ; ESC
                beq _gs1b

                cpx #eol
                beq _gs2

                cpy arg3                ; first char?
                beq _gs3                ; yes, clear line

                stx arg3
                ldx colcrs
                cpx rmargin
                bcs _gs1                ; don't go off screen

                sta (arg12),y
                tay
                lda arg3
                sta (arg12),y
                eor arg2
                jsr scrch

                jmp _gs1

_gs1b           lda #0
                sta curch
                sta (arg12),y
                iny
                tya
_gs2            tay
                txa                     ; EOL
                sta (arg12),y
                rts

_gs3            stx arg3
_gs4            ldy #0
                lda (arg12),y
                beq _gs5
                sec
                sbc #1
                sta (arg12),y

                jsr scrlft

                lda #$20
                eor arg2
                jsr scrch
                jsr scrlft

                ldx arg3
                cpx #$7e
                bne _gs4
                beq _gs1

_gs5            cpx #$7d
                beq _gs1
                bne _gs1a
                .endproc


;======================================
;   FRead()
;======================================
fread           .proc
                lda #0
                sta inbuf
                lda #<rdmsg
                ldx #>rdmsg
                ldy #4
                jsr fopen

_frd1           lda #1
                jsr rdbuf
                bmi _fr3

                jsr instb

                lda allocerr
                beq _frd1

                ldy #22                 ; file too big
                bne _fr4

_fr3            cpy #$88                ; EOF
                beq _fr5

_fr4            jsr syserr
_fr5            jsr fwrite._fw2

                jmp ctrln

;--------------------------------------

rdmsg           .text 6,"Read? "
                .endproc


;======================================
;   FWrite()
;======================================
fwrite          .proc
                lda #<wrtmsg
                ldx #>wrtmsg
                ldy #8
                jsr fopen

                jsr chkcur._ldtop
                beq _fw3

_fw1            jsr ldbuf

    ; INC $03_02C8 ;!! COLOR4 ; let user know we're here
                nop
                nop
                nop

                lda #1
                jsr wrtbuf
                bmi _fw3

                jsr nextdwn
                bne _fw1

                lda #0
                sta dirty
_fw2            lda #1
                jsr close
                jsr rstcur

                jmp dspon

_fw3            jsr syserr

                jmp _fw2

;--------------------------------------

wrtmsg          .text 7,"Write? "
                .endproc


;======================================
;   FOpen(prompt, mode)
;======================================
fopen           .proc
                sta arg10
                stx arg11
                sty opmode

        ; jsr ClnLn ; in SaveWd
                jsr savewd
                jsr rstcsr

                ldy #<inbuf
                lda #>inbuf
                sta arg3
                lda arg10
                ldx arg11
                jsr cmdstr

                lda #1
                jsr close

                ldy inbuf
                beq _fo7

                ldx opmode
                lda #':'
                cmp inbuf+2
                beq _fo2

                cmp inbuf+3
                beq _fo2

                iny
                iny
                sty inbuf
_fo1            lda inbuf,y
                sta inbuf+2,y
                dey
                bne _fo1

                lda #':'
                sta inbuf+2
                bra _fo3

_fo2            lda inbuf+1
                cmp #'?'                ; read directory?
                bne _fo4                ; no

                ldx #6
_fo3            lda #'D'
                sta inbuf+1

_fo4            stx arg3
                jsr dspoff

                lda #1
                sta arg4                ; clear high bit for cassette
                ldx #<inbuf
                ldy #>inbuf
                jsr open
                bmi _fo6

                lda arg3                ; see if directory
                eor #6
                bne _fo5

                sta inbuf               ; clear inbuf
_fo5            rts

_fo6            pla
                pla                     ; pop return
                jmp syserr

_fo7            pla
                pla
                rts
                .endproc


;======================================
;
;======================================
initkeys        .proc
                lda #7
                jsr close

                lda #4
                sta arg3                ; read only
                lda #7
                ldx #<keybd
                ldy #>keybd
                jmp open

;--------------------------------------

keybd           .text 2,"K:"
                .endproc


;======================================
;
;======================================
gotkey          .proc
    ; Test if key in buffer
                lda $03_02FC ;!! CH_                 ; key down?
                eor #$ff
                rts
                .endproc
