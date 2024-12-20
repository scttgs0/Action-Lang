
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: screen.mac.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;
;======================================
scrinit         .proc
                lda #0
                jsr close               ; close #0, sets X to 0

                lda #$0C
                sta arg3

                lda #0
                ldx #<_data
                ldy #>_data

                jmp open

;--------------------------------------

_data           .text 2,"E:",$9B

                .endproc


;======================================
;     ScrCh(char)
;    ------------
; outputs char to screen.  Char passed in A reg.
; Control characters are ignored.
;======================================
scrch           .proc
                tay
                lda #0
_ENTRY1         ldx #1
                bne putch._ENTRY1       ; [unc]

                .endproc


;======================================
;    PutCh(char)
;    -----------
; outputs char to screen.  Char passed in A reg.
; Processes control characters.
;======================================
putch           .proc
                tay
                lda #0
                tax
_ENTRY1         stx DSPFLG

                asl
                asl
                asl
                asl

                tax
                lda #$0B                ; PUTCHR
_ENTRY2         sta IOCB0+ICCOM,X

                lda #0
                sta IOCB0+ICBLL,X
                sta IOCB0+ICBLH,X

                tya
                jmp CIOV

                .endproc


;======================================
;   ScrUp() - Move cursor up one
;======================================
scrup           .proc
                lda #$1C
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrDwn() - Move cursor down one
;======================================
scrdwn          .proc
                lda #$1D
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrBell() - Bell Char
;======================================
scrbell         .proc
                lda #$FD
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrLft() - Move cursor left one
;======================================
scrlft          .proc
                lda #$1E
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrRt() - Move cursor right one
;======================================
scrrt           .proc
                lda #$1F
                bne putch               ; [unc]

                .endproc
