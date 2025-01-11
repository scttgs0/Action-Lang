
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: screen.mac.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


screen          .namespace

;======================================
;
;======================================
Init            .proc
                lda #$00
                jsr mainio.Close        ; close #0, sets X to 0

                .mbv #$0C,arg3

                lda #$00
                ldx #<_data
                ldy #>_data

                jmp mainio.Open

;--------------------------------------

_data           .ptext "E:"
                .byte $9B

                .endproc


;======================================
; PutChar(char)
;--------------------------------------
; outputs char to screen
; Char passed in A reg
; Control characters are ignored
;======================================
PutChar         .proc
                tay
                lda #$00
_ENTRY1         ldx #$01
                bne PutCh._ENTRY1 ; [unc]

                .endproc


;======================================
; PutCh(char)
;--------------------------------------
; outputs char to screen.
; Char passed in A reg.
; Processes control characters.
;======================================
PutCh           .proc
                tay
                lda #$00
                tax
_ENTRY1         stx DSPFLG

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
; CursorUp()
;--------------------------------------
; Move cursor up one
;======================================
CursorUp        .proc
                lda #$1C
                bne PutCh               ; [unc]

                .endproc


;======================================
; CursorDown()
;--------------------------------------
; Move cursor down one
;======================================
CursorDown      .proc
                lda #$1D
                bne PutCh               ; [unc]

                .endproc


;======================================
; Bell()
;--------------------------------------
; Bell Char
;======================================
Bell            .proc
                lda #$FD
                bne PutCh               ; [unc]

                .endproc


;======================================
; CursorLeft()
;--------------------------------------
; Move cursor left one
;======================================
CursorLeft      .proc
                lda #$1E
                bne PutCh               ; [unc]

                .endproc


;======================================
; CursorRight()
;--------------------------------------
; Move cursor right one
;======================================
CursorRight     .proc
                lda #$1F
                bne PutCh               ; [unc]

                .endproc

                .endnamespace
