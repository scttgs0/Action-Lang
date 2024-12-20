
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: screen.mac.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;
;======================================
screenInit      .proc
;   80x60 text mode
                .frsGraphics mcTextOn,mcVideoMode240
                stz DINDEX              ; text mode

                ; lda #$00
                ; jsr ioClose           ; close #$00, sets X to 0

                ; lda #$0C
                ; sta arg3
                ; lda #$00
                ; ldx #<_data
                ; ldy #>_data
                ; jmp ioOpen

;--------------------------------------

; _data         .ptext "E:"
;               .byte $9B
                .endproc


;======================================
; screenCh(char)
;--------------------------------------
; outputs char to screen
; Char passed in A reg
; Control characters are ignored
;======================================
screenCh        .proc
                tay
                lda #$00
_ENTRY1         ldx #$01
                bne screenPutCh._ENTRY1 ; [unc]

                .endproc


;======================================
; screenPutCh(char)
;--------------------------------------
; outputs char to screen.
; Char passed in A reg.
; Processes control characters.
;======================================
screenPutCh     .proc
                tay
                lda #$00
                tax
_ENTRY1         ;!!stx DSPFLG

                asl
                asl
                asl
                asl

                tax
                lda #$0B                ; PUTCHR
_ENTRY2         sta IOCB0+ICCOM,X

                lda #$00
                sta IOCB0+ICBLL,X
                sta IOCB0+ICBLH,X

                tya
                jmp CIOV
                .endproc


;======================================
; screenCursorUp()
;--------------------------------------
; Move cursor up one
;======================================
screenCursorUp  .proc
                lda #$1C
                bra screenPutCh

                .endproc


;======================================
; screenCursorDown()
;--------------------------------------
; Move cursor down one
;======================================
screenCursorDown .proc
                lda #$1D
                bra screenPutCh

                .endproc


;======================================
; screenBell()
;--------------------------------------
; Bell Char
;======================================
screenBell      .proc
                lda #$FD
                bra screenPutCh

                .endproc


;======================================
; screenCursorLeft()
;--------------------------------------
; Move cursor left one
;======================================
screenCursorLeft .proc
                lda #$1E
                bra screenPutCh

                .endproc


;======================================
; screenCursorRight()
;--------------------------------------
; Move cursor right one
;======================================
screenCursorRight .proc
                lda #$1F
                bra screenPutCh

                .endproc
