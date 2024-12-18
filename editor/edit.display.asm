
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.display.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   CommandMsg(message)
;======================================
CommandMsg      .proc
                sta arg0

                jsr CmdColumn

                lda #0
                sta arg3

                lda arg0
                ldy #$80
                jsr PutStr

                jmp ResetColumn

                .endproc


;======================================
;   CleanLine()
;======================================
CleanLine       .proc
                jsr ChkCursor

                lda isDirty
                beq _XIT

                sta dirty

                lda #0
                sta isDirty

                jsr DeleteCurrentLine
                jsr InsertByte

_XIT            jmp ChkCursor

                .endproc


;======================================
;   SaveWindow()
;======================================
SaveWindow      .proc
                jsr CleanLine

_ENTRY1         clc
                lda #14
                tax
                adc currentWindow

                tay
_next1          lda sp,X
                sta w1,Y

                dey
                dex
                bpl _next1

                rts
                .endproc


;======================================
;   RestoreWindow() restore window
;======================================
RestoreWindow   .proc
                clc
                lda #14
                tax
                adc currentWindow

                tay
_next1          lda w1,Y
                sta sp,X

                dey
                dex
                bpl _next1

_XIT            rts
                .endproc


;======================================
;   EndLine()
;======================================
EndLine         .proc
                jsr CleanLine

                lda bot
                sta cur
                lda bot+1
                sta cur+1

                .endproc

                ;[fall-through]


;======================================
;   CenterLine() center line
;======================================
CenterLine      .proc
                lda #0
                sta temps

                jsr CleanLine
                beq _1

                jsr nextup
                beq _1

                inc temps

                jsr nextup
                beq _1

                inc temps

_1              jsr NewPage

_next1          lda temps
                beq RestoreWindow._XIT

                jsr ScrollDown

                dec temps

                jmp _next1

                .endproc


;======================================
;   TopLine()
;======================================
TopLine         .proc
                jsr CleanLine
                jsr ChkCursor._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   NewPage()
;======================================
NewPage         .proc
                lda #0
                sta lnum

_ENTRY1         sta choff

                jsr RestoreCursorChar              ; for command line

                lda LMARGN
                sta COLCRS

                .endproc

                ;[fall-through]


;======================================
;   Refresh()
;======================================
Refresh         .proc
                clc
                lda ytop
                adc lnum
                sta ROWCRS

                jsr SaveColumn
                jsr SaveWindow

                inc ROWCRS

                jsr nextdwn

                sta arg9

                clc
                lda nlines
                sbc lnum
                sta arg10
                beq _2

_next1          ldy #0
                lda indent
                sta arg3

                ldx arg9
                beq _3

                jsr curstr

_next2          jsr PutStr

                lda arg9
                bne _1

                tay
                sta (arg0),Y

_1              inc ROWCRS

                jsr nextdwn

                sta arg9

                dec arg10
                bne _next1

_2              jsr ResetCursor
                jsr ResetColumn

                jmp RefreshBuf

_3              lda #<zero
                ldx #>zero

                bra _next2

                .endproc
