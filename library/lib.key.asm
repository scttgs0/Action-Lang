
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.key.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


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
                lda (oldadr),Y
                eor #$80
                sta (oldadr),Y

                jmp lgetkey

_1              ldy #0
                lda oldchr
                eor #$80
                sta (oldadr),Y          ; restore cursor

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
