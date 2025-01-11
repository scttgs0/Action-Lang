
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.display.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


display         .namespace

;======================================
; CommandMsg(message)
;--------------------------------------
; on entry:
;   X:A         message
;======================================
CommandMsg      .proc
                sta arg0

                jsr mainio.CmdColumn

                .mbv #$00,arg3

                lda arg0                ; message_LO
                ldy #$80                ; char used for clearing (inverse space)
                jsr mainio.PutStr
                jmp mainio.RestoreColumn

                .endproc


;======================================
; CleanLine()
;======================================
CleanLine       .proc
                jsr mainio.ChkCursor

                lda isDirty
                beq _XIT

                sta dirty

                .mbv #$00,isDirty

                jsr editor.memory.DeleteCurrentLine
                jsr editor.memory.InsertByte

_XIT            jmp mainio.ChkCursor

                .endproc


;======================================
; SaveWindow()
;======================================
SaveWindow      .proc
                jsr CleanLine

_ENTRY1         clc
                lda #$0E
                tax
                adc currentWindow

                tay
_next1          lda sp,X
                sta win1Base,Y

                dey
                dex
                bpl _next1

                rts
                .endproc


;======================================
; RestoreWindow()
;--------------------------------------
; restore window
;======================================
RestoreWindow   .proc
                clc
                lda #$0E
                tax
                adc currentWindow

                tay
_next1          lda win1Base,Y
                sta sp,X

                dey
                dex
                bpl _next1

_XIT            rts
                .endproc


;======================================
; EndLine()
;======================================
EndLine         .proc
                jsr CleanLine

                .mwa bot,cur

                .endproc

                ;[fall-through]


;======================================
; CenterLine()
;--------------------------------------
; center line
;======================================
CenterLine      .proc
                .mbv #$00,temps

                jsr CleanLine
                beq _1

                jsr mainmsc.NextUp
                beq _1

                inc temps

                jsr mainmsc.NextUp
                beq _1

                inc temps

_1              jsr NewPage

_next1          lda temps
                beq RestoreWindow._XIT

                jsr editor.command.ScrollDown

                dec temps

                jmp _next1

                .endproc


;======================================
; TopLine()
;======================================
TopLine         .proc
                jsr CleanLine
                jsr mainio.ChkCursor._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; NewPage()
;======================================
NewPage         .proc
                lda #$00
                sta lnum

_ENTRY1         sta choff

                jsr mainio.RestoreCursorChar ; for command line

                .mba LMARGN,COLCRS

                .endproc

                ;[fall-through]


;======================================
; Refresh()
;======================================
Refresh         .proc
                clc
                lda ytop
                adc lnum
                sta ROWCRS

                jsr mainio.SaveColumn
                jsr SaveWindow

                inc ROWCRS

                jsr mainmsc.NextDown

                sta arg9

                clc
                lda nlines
                sbc lnum
                sta arg10
                beq _2

_next1          ldy #$00
                .mba indent,arg3

                ldx arg9
                beq _3

                jsr mainmsc.CurStr

_next2          jsr mainio.PutStr

                lda arg9
                bne _1

                tay
                sta (arg0),Y

_1              inc ROWCRS

                jsr mainmsc.NextDown
                sta arg9

                dec arg10
                bne _next1

_2              jsr mainio.ResetCursor
                jsr mainio.RestoreColumn
                jmp editor.chr.RefreshBuf

_3              lda #<editor.cartridge.zero
                ldx #>editor.cartridge.zero

                bne _next2                ; [unc]

                .endproc

                .endnamespace
