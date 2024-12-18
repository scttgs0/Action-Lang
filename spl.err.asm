
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: spl.err.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   SPLErr(,,error)
;======================================
lsplerr         .proc
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

_1              jsr SystemError
                jsr PutEOL
                jsr PrintBuffer

                lda #0
                ldx #<msgSysErr
                ldy #>msgSysErr
                jsr Output

                lda #0
                sta INITAD+1

                ldx #<numbuf
                ldy #>numbuf
                jsr Print

                jmp emloop

                .endproc
