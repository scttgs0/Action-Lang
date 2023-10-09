
; SPDX-FileName: edit.fnd.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Find()
;======================================
Find            .proc
                jsr SetSpacing
                jsr SaveWindow

                lda lastch
                cmp #$F8
                beq find2

                lda #<findmsg
                ldx #>findmsg
find1           ldy #>findbuf
                sty arg3
                ldy #<findbuf
                jsr CommandString

                lda #$F8
                sta curch

find2           lda findbuf
                beq _f7

_f2             ldy #0
                lda (buf),y
                tay
                iny
                sty arg0

_f3             ldy sp
                iny
                cpy arg0
                bcs _f5

                sty sp
                ldx #0

_f4             lda (buf),y
                inx
                cmp findbuf,x
                bne _f3

                iny
                cpx findbuf
                beq Found

                cpy arg0
                bcc _f4

_f5             jsr nextdwn
                beq _f6

                jsr ldbuf

                lda #0
                sta sp
                beq _f2

_f6             sta curch
                jsr rstcur
                jsr ldbuf

                lda #<notfnd
                ldx #>notfnd
                jsr CommandMsg

                lda #0
_f7             sta curch
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
                jsr Back.back0

                lda #$FE
                rts
                .endproc

;--------------------------------------
;--------------------------------------

notfnd          .text 9,"not found"

findmsg         .text 6,"Find? "
