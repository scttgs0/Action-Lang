
; SPDX-FileName: edit.window.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Wind1()
;======================================
Window1         .proc
                lda currentWindow
                beq SaveWorld._XIT

                lda #0
                pha

                .endproc

                ;[fall-through]


;======================================
;   SwapWd()
;======================================
SwapWindows     .proc
                jsr SaveWorld

                pla
                jmp RestoreWorld

                .endproc


;======================================
;   Wind2()
;======================================
Window2         .proc
                lda currentWindow
                bne SaveWorld._XIT

                lda numwd
                bne _1

                jmp Window2Init

_1              lda #w2-w1
                pha

                bra SwapWindows

                .endproc


;======================================
;   SaveWorld()
;======================================
SaveWorld       .proc
                jsr CleanLine
                jsr savecol
                jsr rstcsr
                jsr SetSpacing

                jmp SaveWindow

_XIT            rts
                .endproc


;======================================
;   Clear()
;======================================
Clear_           .proc
                jsr jt_alarm

                lda #<DeleteWindow.clearmsg
                ldx #>DeleteWindow.clearmsg
                jsr YesNo
                bne SaveWorld._XIT

_ENTRY1         jsr CleanLine

                lda dirty
                beq _1

;               jsr Alarm

                lda #<DeleteWindow.dirtymsg
                ldx #>DeleteWindow.dirtymsg
                jsr YesNo
                bne SaveWorld._XIT

_1              jsr FreeTags            ; get rid of tags

                lda bot
                ldx bot+1

_next1          jsr DeleteLine
                bne _next1

                stx cur+1
                stx dirty
                stx isDirty
                stx inbuf

                jmp NewPage

                .endproc


;======================================
;   RestoreWorld(window)
;======================================
RestoreWorld    .proc
                sta currentWindow

                jsr RestoreWindow
                jsr ldbuf

                jmp rstcol

                .endproc


;======================================
;   DeleteWindow()
;======================================
DeleteWindow    .proc
                lda numwd
                beq SaveWorld._XIT

                jsr jt_alarm

                lda #<delmsg
                ldx #>delmsg
                jsr YesNo
                bne SaveWorld._XIT

                jsr Clear_._ENTRY1

                lda dirty
                bne SaveWorld._XIT

                ldy #0
                sty numwd

                cpy currentWindow
                bne _1

                ldy #w2-w1
_1              sty currentWindow

                jsr RestoreWindow

                jmp EditorInit._ENTRY1

;--------------------------------------

clearmsg        .ptext "CLEAR? "
delmsg          .ptext "Delete window? "
dirtymsg        .ptext "Not saved, Delete? "

                .endproc


;======================================
;   GetTemp(msg)
;======================================
GetTemp         .proc
                ldy #0
_ENTRY1         sty tempbuf

                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf

                .endproc

                ;[fall-through]


;======================================
;   CmdStr(msg, buf)
;======================================
CommandString   .proc
                sta arg0
                sty arg2

                jsr cmdcol

                lda #$80
                sta arg4

                lda arg0
                ldy arg2

                jsr GetString
                jsr rstcsr

                jmp rstcol

                .endproc


;======================================
;   YesNo(msg)
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
