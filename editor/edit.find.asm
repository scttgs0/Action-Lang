
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.find.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   Find()
;======================================
Find            .proc
                jsr SetSpacing
                jsr SaveWindow

                lda lastch
                cmp #$F8
                beq _ENTRY2

                lda #<findmsg
                ldx #>findmsg

_ENTRY1         ldy #>findbuf
                sty arg3

                ldy #<findbuf
                jsr CommandString

                lda #$F8
                sta curch

_ENTRY2         lda findbuf
                beq _3

_next1          ldy #0
                lda (buf),Y
                tay
                iny
                sty arg0

_next2          ldy sp
                iny
                cpy arg0
                bcs _1

                sty sp
                ldx #0

_next3          lda (buf),Y
                inx
                cmp findbuf,X
                bne _next2

                iny
                cpx findbuf
                beq Found

                cpy arg0
                bcc _next3

_1              jsr nextdwn
                beq _2

                jsr ldbuf

                lda #0
                sta sp
                beq _next1

_2              sta curch

                jsr rstcur
                jsr ldbuf

                lda #<notfnd
                ldx #>notfnd
                jsr CommandMsg

                lda #0
_3              sta curch

                rts
                .endproc


;======================================
;   Found()
;======================================

Found           .proc
                jsr CenterLine

                ldy sp
                dey
                tya
                jsr Back._ENTRY1

                lda #$FE

                rts
                .endproc


;--------------------------------------
;--------------------------------------

notfnd          .ptext "not found"

findmsg         .ptext "Find? "
