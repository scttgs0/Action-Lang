
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.io.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   GetStr(prompt, str, invert)
;======================================
getstr          .proc
                jsr dspstr
_next1          jsr getkey

                tax
                cpx #$7E
                beq _3                  ; backspace

                cpx #$7D
                beq _3                  ; clear

_next2          ldy #0
                clc
                lda (arg12),Y
                adc #1

                cpx #$1B                ; ESC
                beq _1

                cpx #eol
                beq _2

                cpy arg3                ; first char?
                beq _3                  ; yes, clear line

                stx arg3

                ldx colcrs
                cpx rmargin
                bcs _next1              ; don't go off screen

                sta (arg12),Y

                tay
                lda arg3
                sta (arg12),Y

                eor arg2
                jsr scrch

                jmp _next1

_1              lda #0
                sta curch
                sta (arg12),Y

                iny
                tya

_2              tay
                txa                     ; EOL
                sta (arg12),Y

                rts

_3              stx arg3

_next3          ldy #0
                lda (arg12),Y
                beq _4

                sec
                sbc #1
                sta (arg12),Y

                jsr scrlft

                lda #$20
                eor arg2

                jsr scrch
                jsr scrlft

                ldx arg3
                cpx #$7E
                bne _next3
                beq _next1              ; [unc]

_4              cpx #$7D
                beq _next1
                bne _next2              ; [unc]

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

_next1          lda #1
                jsr rdbuf
                bmi _1

                jsr instb

                lda allocerr
                beq _next1

                ldy #22                 ; file too big
                bne _2

_1              cpy #$88                ; EOF
                beq _3

_2              jsr syserr
_3              jsr fwrite._ENTRY1

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

                jsr chkcur._ENTRY1
                beq _1

_next1          jsr ldbuf

;               inc COLOR4              ; let user know we're here

                nop
                nop
                nop

                lda #1
                jsr wrtbuf
                bmi _1

                jsr nextdwn
                bne _next1

                lda #0
                sta dirty

_ENTRY1         lda #1
                jsr close
                jsr rstcur

                jmp dspon

_1              jsr syserr

                jmp _ENTRY1

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

;               jsr ClnLn               ; in SaveWd
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
                beq _5

                ldx opmode
                lda #':'
                cmp inbuf+2
                beq _1

                cmp inbuf+3
                beq _1

                iny
                iny
                sty inbuf

_next1          lda inbuf,Y
                sta inbuf+2,Y

                dey
                bne _next1

                lda #':'
                sta inbuf+2
                bne _2                  ; [unc]

_1              lda inbuf+1
                cmp #'?'                ; read directory?
                bne _3                  ;   no

                ldx #6
_2              lda #'D'
                sta inbuf+1

_3              stx arg3
                jsr dspoff

                lda #1
                sta arg4                ; clear high bit for cassette

                ldx #<inbuf
                ldy #>inbuf
                jsr open
                bmi _4

                lda arg3                ; see if directory
                eor #6
                bne _XIT

                sta inbuf               ; clear inbuf

_XIT            rts

_4              pla
                pla                     ; pop return

                jmp syserr

_5              pla
                pla

                rts
                .endproc


;======================================
;   InitKeys()
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
; Test if key in buffer
;======================================
gotkey          .proc
                lda CH_                 ; key down?
                eor #$FF

                rts
                .endproc
