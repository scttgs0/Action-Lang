
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
                lda rtclok+2
                adc #$0E

                tax
_waitForKey     lda CH_                 ; key down?
                eor #$FF                ; flip the bits
                bne _1

                cpx rtclok+2
                bpl _waitForKey

                ldy #$00
                lda (OLDADR),Y
                eor #$80
                sta (OLDADR),Y

                jmp _next1

_1              ldy #$00
                lda OLDCHR
                eor #$80
                sta (OLDADR),Y          ; restore cursor

_faster         ldx SRTIMR              ; faster repeat
                cpx #$0C
                bcs _6

                cpx #$04
                bcc _2

                ldx #$03
_next2          stx SRTIMR

_2              lda CH_
                cmp #$C0                ; Ctrl-Shft?
                bcc _3                  ;   no

                jsr Click
                bmi _4                  ; [unc]

_3              and #$3F
                cmp #$3C                ; caps key
                beq _7

                cmp #$27                ; Atari key
                beq _8

                ldx #$70
                lda #$07                ; GETCHR
                sta BRKKEY              ; ignore BREAK key

                jsr screen.PutCh._ENTRY2

_4              ldx SRTIMR
                cpx #$0A
                bcs _5

                ldx #$03
                stx SRTIMR

_5              sta curCH

                rts

_6              ldx #$14
                bne _next2

_7              lda CH_
                and #$C0                ; isolate control (128) and uppercase (64)
                sta SHFLOC

_next3          jsr Click
                bmi _next1

_8              lda INVFLG
                eor #$80
                sta INVFLG

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
                stx WSYNC

                dex
                bpl _next1

                stx CH_

                rts
                .endproc

                .endnamespace
