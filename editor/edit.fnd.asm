
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.fnd.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   Find()
;======================================
find            .proc
                jsr setsp
                jsr savewd

                lda lastch
                cmp #$F8
                beq _ENTRY2

                lda #<findmsg
                ldx #>findmsg

_ENTRY1         ldy #>findbuf
                sty arg3

                ldy #<findbuf
                jsr cmdstr

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
                beq found

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
                jsr cmdmsg

                lda #0
_3              sta curch

                rts
                .endproc


;======================================
;
;======================================

found           .proc
                jsr ctrln

                ldy sp
                dey
                tya
                jsr back._ENTRY1

                lda #$FE

                rts
                .endproc


;--------------------------------------
;--------------------------------------

notfnd          .text 9,"not found"

findmsg         .text 6,"Find? "
