
; SPDX-FileName: spl.err.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   SPLErr(,,error)
;======================================
lsplerr         .proc
                lda top+1
                beq _1

;   set pointer to error
                ldx currentWindow
                lda spln
                sta w1+wsp,x

                lda curln
                sta w1+wcur,x
                lda curln+1
                sta w1+wcur+1,x

_1              jsr syserr
                jsr puteol
                jsr printbuf

                lda #0
                ldx #<sermsg
                ldy #>sermsg
                jsr output

                lda #0
                ;!!sta INITAD+1

                ldx #<numbuf
                ldy #>numbuf
                jsr print

                jmp emloop

                .endproc
