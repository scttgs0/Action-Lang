
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: spl.err.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; coreSPLErr(,,error)
;--------------------------------------
; Scanner/Parser/Lexeme error
;======================================
coreSPLErr      .proc
                lda top+1
                beq _1

;   set pointer to error
                ldx currentWindow
                lda spln
                sta w1+WSP,X

                lda curln
                sta w1+WCUR,X
                lda curln+1
                sta w1+WCUR+1,X

_1              jsr ioSystemError
                jsr ioPutEOL
                jsr ioPrintBuffer

                lda #$00
                ldx #<msgSysErr
                ldy #>msgSysErr
                jsr ioOutput

                lda #$00
                sta INITAD+1

                ldx #<numbuf
                ldy #>numbuf
                jsr ioPrint

                jmp bankEmLoop

                .endproc
