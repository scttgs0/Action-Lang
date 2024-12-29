
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.key.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


key             .namespace

;======================================
; get next key in buffer
;======================================
Get             .proc
_next1          clc                     ; blink cursor
                lda JIFFYCLOCK
                adc #$0E

                tax
_waitForKey     lda KEYCHAR             ; key down?
                eor #$FF                ; flip the bits
                bne _1

                cpx JIFFYCLOCK
                bpl _waitForKey

;   unnecessary code
                ;!! ldy #$00
                ;!! lda (OLDADR),Y
                ;!! eor #$80
                ;!! sta (OLDADR),Y

                jmp _next1

;   unnecessary code
_1              ;!! ldy #$00
                ;!! lda OLDCHR
                ;!! eor #$80
                ;!! sta (OLDADR),Y          ; restore cursor

_faster         ;!!ldx SRTIMR           ; faster repeat
                ldx #$0A    ; HACK:
                cpx #$0C
                bcs _6

                cpx #$04
                bcc _2

                ldx #$03
_next2          ;!!stx SRTIMR

_2              lda KEYCHAR             ; last key pressed
                cmp #$C0                ; Ctrl-Shft?
                bcc _3                  ;   no

                jsr Click
                bra _4

_3              and #$3F
                cmp #$3C                ; caps key
                beq _7

                cmp #$27                ; Atari key
                beq _8

                ldx #$70
                lda #$07                ; GETCHR
                sta BRKKEY              ; ignore BREAK key

                jsr screenPutCh._ENTRY2

_4              ;!!ldx SRTIMR
                ldx #$0A    ; HACK:
                cpx #$0A
                bcs _5

                ldx #$03
                ;!!stx SRTIMR

_5              sta curCH

                rts

_6              ldx #$14
                bne _next2

_7              lda KEYCHAR             ; last key pressed
                and #$C0                ; isolate control (128) and uppercase (64)
                ;!!sta SHFLOK

_next3          jsr Click
                bmi _next1

_8              ;!!lda INVFLG
                lda #$00    ; HACK:
                eor #$80
                ;!!sta INVFLG

                jmp _next3

                .endproc


;======================================
; Click()
;--------------------------------------
; click the keyboard
;======================================
Click           .proc
                ldx #$7F
_next1          stx CONSOL
                ;!!stx WSYNC

                dex
                bpl _next1

                stx KEYCHAR             ; reset ($FF)

                rts
                .endproc

                .endnamespace
