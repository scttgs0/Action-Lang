
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: spl.err.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   SPLErr(,,error)
;======================================
lsplerr         .proc
                lda top+1
                beq spler1

    ; set pointer to error
                ldx curwdw
                lda spln
                sta w1+wsp,X
                lda curln
                sta w1+wcur,X
                lda curln+1
                sta w1+wcur+1,X
spler1          jsr syserr
                jsr puteol
                jsr printbuf

                lda #0
                ldx #<sermsg
                ldy #>sermsg
                jsr output

                lda #0
                sta INITAD+1
                ldx #<numbuf
                ldy #>numbuf
                jsr print

                jmp emloop

                .endproc
