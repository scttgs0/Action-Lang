
; SPDX-FileName: lib.key.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Get next key in buffer
;======================================
libKeyGetKey    .proc
                clc                     ; blink cursor
                ;!!lda RTCLOK+2
                adc #14

                tax
_waitForKey     lda KEYCHAR             ; key down?
                eor #$FF                ; flip the bits
                bne _faster

                ;!!cpx RTCLOK+2
                bpl _waitForKey

                ldy #0
                lda (OLDADR),Y
                eor #$80
                sta (OLDADR),Y

                jmp libKeyGetKey

_1              ldy #0
                lda OLDCHR
                eor #$80
                sta (OLDADR),Y          ; restore cursor

_faster         ;!!ldx SRTIMR           ; faster repeat
                cpx #$0C
                bcs _6

                cpx #4
                bcc _2

                ldx #3
_next2          ;!!stx SRTIMR

_2              lda KEYCHAR             ; last key pressed
                cmp #$C0                ; Ctrl-Shft?
                bcc _3                  ;   no

                jsr libKeyClick
                bra _4

_3              and #$3F
                cmp #$3C                ; caps key
                beq _7

                cmp #$27                ; Atari key
                beq _8

                ldx #$70
                lda #7                  ; GETCHR
                sta BRKKEY              ; ignore BREAK key

                jsr putch._ENTRY2

_4              ;!!ldx SRTIMR
                cpx #10
                bcs _5

                ldx #3
                ;!!stx SRTIMR

_5              sta curch

                rts

_6              ldx #20
                bne _next2

_7              lda KEYCHAR             ; last key pressed
                and #$C0                ; isolate control (128) and uppercase (64)
                ;!!sta SHFLOK

_next3          jsr libKeyClick
                bmi libKeyGetKey

_8              ;!!lda INVFLG
                eor #$80
                ;!!sta INVFLG

                jmp _next3

                .endproc


;======================================
;   Click() click the keyboard
;======================================
libKeyClick     .proc
                ldx #$7F
_next1          stx CONSOL
                ;!!stx WSYNC

                dex
                bpl _next1

                stx KEYCHAR             ; reset ($FF)

                rts
                .endproc
