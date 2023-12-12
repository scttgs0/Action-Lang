
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.ini.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;
;======================================
minit           .proc                   ; initialize memory
                lda MEMLO
                sta afbase
                lda MEMLO+1
                sta afbase+1

                lda #0
                tay
                sta (afbase),Y

                iny
                sta (afbase),Y

                sec
                lda MEMTOP
                sbc afbase

                iny
                sta (afbase),Y

                lda MEMTOP+1
                sbc afbase+1

                iny
                sta (afbase),Y

                lda #0                  ; allocate 2 pages of
                ldx #2                  ; spare memory
                jsr allocate

                lda afcur
                sta sparem
                ldx afcur+1
                stx sparem+1

                rts
                .endproc


;======================================
; Initialize window
;======================================
zerow           .proc
                lda #0
                ldx #15

_next1          dex                     ; zero page0 window table
                sta sp,X
                bne _next1

                sta dirtyf
                sta inbuf

                tay
                sta (buf),Y

                rts
                .endproc


;======================================
; Initialize secondary window
;======================================
w2init          .proc
                jsr ctrln

                lda wsize
                sta nlines
                sta cmdln

                jsr savworld

                lda #w2-w1
                sta numwd
                sta curwdw

                jsr zerow

                ldy wsize
                iny
                sty ytop

                sec
                lda #23
                sbc wsize
                sta nlines

                bne einit._ENTRY2       ; [unc]

                .endproc


;======================================
; Initialize the Editor
;======================================
einit           .proc
                jsr minit

                lda #0
                ldx #1
                jsr allocate            ; get edit buffer

                lda afcur
                sta buf
                ldx afcur+1
                stx buf+1

                lda #$40
                sta chcvt

                lda #<delbuf
                sta delbuf
                sta delbuf+4
                lda #>delbuf
                sta delbuf+1
                sta delbuf+5

;   initialize window
                jsr zerow

_ENTRY1         lda #23                 ; rowcount
                sta nlines
                sta cmdln

                lda #0
                sta curwdw
                sta ytop

_ENTRY2         jsr ctrln

_ENTRY3         lda #<editc
                ldx #>editc

                jmp cmdmsg

;--------------------------------------

editc           .text 19,"ACTION! (c)1983 ACS"
                .endproc
