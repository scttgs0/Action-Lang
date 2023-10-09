
; SPDX-FileName: lib.key.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;
;======================================
libKeyGetKey    .proc
;   get next key in buffer
_waitForKey     ;!!lda CH_                 ; key down?
                eor #$FF
                bne _gk0

                bra _waitForKey

_gk0            ;!!ldx SRTIMR              ; faster repeat
                cpx #$0C
                bcs _gk5

                cpx #4
                bcc _gk2

                ldx #3
_gk1            ;!!stx SRTIMR
_gk2            ;!!lda CH_
                cmp #$C0
                bcc _gk3                ; not Ctrl-Shft

_cskey          jsr libKeyClick
                bra _gk4

_gk3            and #$3F
                cmp #$3C                ; caps key
                beq _caps

                cmp #$27                ; Atari key
                beq _atari

_gkey           ldx #$70
                lda #7                  ; GETCHR
                sta BRKKEY              ; ignore BREAK key
                jsr putch.putch2

_gk4            ;!!ldx SRTIMR
                cpx #10
                bcs _gkret

                ldx #3
                ;!!stx SRTIMR
_gkret          sta curch
                rts

_gk5            ldx #20
                bne _gk1

_caps           ;!!lda CH_
                and #$C0                ; isolate control (128) and uppercase (64)
                ;!!sta SHFLOK
_caps1          jsr libKeyClick
                bmi libKeyGetKey

_atari          ;!!lda INVFLG
                eor #$80
                ;!!sta INVFLG
                jmp _caps1

                .endproc


;======================================
;   Click() click the keyboard
;======================================
libKeyClick     .proc
                ldx #$7F
_click1         stx CONSOL
                ;!!stx WSYNC
                dex
                bpl _click1

                ;!!stx CH_
                rts
                .endproc
