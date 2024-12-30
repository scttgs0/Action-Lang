
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
                sta win1Base+WSP,X

                lda curln
                sta win1Base+WCUR,X
                lda curln+1
                sta win1Base+WCUR+1,X

_1              jsr mainio.SystemError
                jsr mainio.PutEOL
                jsr mainio.PrintBuffer

                lda #$00
                ldx #<mainio.msgSysErr
                ldy #>mainio.msgSysErr
                jsr mainio.Output

                lda #$00
                sta INITAD+1

                ldx #<numbuf
                ldy #>numbuf
                jsr mainio.Print

                jmp mainbank.EMLoop

                .endproc
