
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.window.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


window          .namespace

;======================================
; Window1()
;======================================
Window1         .proc
                lda currentWindow
                beq SaveWorld._XIT

                lda #$00
                pha

                .endproc

                ;[fall-through]


;======================================
; SwapWindows()
;======================================
SwapWindows     .proc
                jsr SaveWorld

                pla
                jmp RestoreWorld

                .endproc


;======================================
; Window2()
;======================================
Window2         .proc
                lda currentWindow
                bne SaveWorld._XIT

                lda numwd
                bne _1

                jmp editor.init.Window2

_1              lda #w2-w1
                pha

                bra SwapWindows

                .endproc


;======================================
; SaveWorld()
;======================================
SaveWorld       .proc
                jsr editor.display.CleanLine
                jsr ioSaveColumn
                jsr ioRestoreCursorChar
                jsr editor.command.SetSpacing
                jmp editor.display.SaveWindow

_XIT            rts
                .endproc


;======================================
; Clear()
;======================================
Clear           .proc
                jsr jt_alarm

                lda #<Delete.msgClear
                ldx #>Delete.msgClear
                jsr YesNo
                bne SaveWorld._XIT

_ENTRY1         jsr editor.display.CleanLine

                lda dirty
                beq _1

;               jsr Alarm

                lda #<Delete.msgDirty
                ldx #>Delete.msgDirty
                jsr YesNo
                bne SaveWorld._XIT

_1              jsr editor.tag.FreeTags            ; get rid of tags

                lda bot
                ldx bot+1

_next1          jsr editor.memory.DeleteLine
                bne _next1

                stx cur+1
                stx dirty
                stx isDirty
                stx inbuf

                jmp editor.display.NewPage

                .endproc


;======================================
; RestoreWorld(window)
;======================================
RestoreWorld    .proc
                sta currentWindow

                jsr editor.display.RestoreWindow
                jsr ioLoadBuffer
                jmp ioResetColumn

                .endproc


;======================================
; Delete()
;======================================
Delete          .proc
                lda numwd
                beq SaveWorld._XIT

                jsr jt_alarm

                lda #<msgDelete
                ldx #>msgDelete
                jsr YesNo
                bne SaveWorld._XIT

                jsr Clear._ENTRY1

                lda dirty
                bne SaveWorld._XIT

                ldy #$00
                sty numwd

                cpy currentWindow
                bne _1

                ldy #w2-w1
_1              sty currentWindow

                jsr editor.display.RestoreWindow
                jmp editor.init.EditorInit._ENTRY1

;--------------------------------------

msgClear        .ptext "CLEAR? "
msgDelete       .ptext "Delete window? "
msgDirty        .ptext "Not saved, Delete? "

                .endproc


;======================================
; GetTemp(msg)
;======================================
GetTemp         .proc
                ldy #$00
_ENTRY1         sty tempbuf

                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf

                .endproc

                ;[fall-through]


;======================================
; CommandString(msg, buf)
;======================================
CommandString   .proc
                sta arg0
                sty arg2

                jsr ioCmdColumn

                lda #$80
                sta arg4

                lda arg0
                ldy arg2

                jsr editor.io.GetString
                jsr ioRestoreCursorChar
                jmp ioResetColumn

                .endproc


;======================================
; YesNo(msg)
;======================================
YesNo           .proc
                jsr GetTemp

                ldy tempbuf
                bne _1

                iny

                rts

_1              lda tempbuf+1
                ora #$20
                cmp #'y'

                rts
                .endproc

                .endnamespace
