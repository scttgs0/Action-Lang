
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.find.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


find            .namespace

;======================================
; Find()
;======================================
Find            .proc
                jsr editor.command.SetSpacing
                jsr editor.display.SaveWindow

                lda lastCH
                cmp #$F8
                beq _ENTRY2

                lda #<msgFIND
                ldx #>msgFIND

_ENTRY1         ldy #>findbuf
                sty arg3
                ldy #<findbuf
                jsr editor.window.CommandString

                lda #$F8
                sta curCH

_ENTRY2         lda findbuf
                beq _3

_next1          ldy #$00
                lda (buf),Y
                tay
                iny
                sty arg0

_next2          ldy sp
                iny
                cpy arg0
                bcs _1

                sty sp
                ldx #$00

_next3          lda (buf),Y
                inx
                cmp findbuf,X
                bne _next2

                iny
                cpx findbuf
                beq Found

                cpy arg0
                bcc _next3

_1              jsr mscNextDown
                beq _2

                jsr ioLoadBuffer

                lda #$00
                sta sp
                beq _next1

_2              sta curCH

                jsr ioResetCursor
                jsr ioLoadBuffer

                lda #<msgNOTFOUND
                ldx #>msgNOTFOUND
                jsr editor.display.CommandMsg

                lda #$00
_3              sta curCH

                rts
                .endproc


;======================================
;   Found()
;======================================
Found           .proc
                jsr editor.display.CenterLine

                ldy sp
                dey
                tya
                jsr editor.command.Back._ENTRY1

                lda #$FE

                rts
                .endproc


;--------------------------------------
;--------------------------------------

msgNOTFOUND     .ptext "not found"
msgFIND         .ptext "Find? "

                .endnamespace
