
; SPDX-FileName: edit.wnd.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Wind1()
;======================================
Window1         .proc
                lda currentWindow
                beq SaveWorld.wdret

                lda #0
                pha
                .endproc


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
                bne SaveWorld.wdret

                lda numwd
                bne _w2

                jmp Window2Init

_w2             lda #w2-w1
                pha
                bra SwapWindows

                .endproc


;======================================
;   SaveWorld()
;======================================
SaveWorld       .proc
                jsr CleanLine
                jsr savecol
                jsr SetSpacing

                jmp SaveWindow

wdret           rts
                .endproc


;======================================
;   Clear()
;======================================
Clear_           .proc
                jsr jt_alarm

                lda #<DeleteWindow.clearmsg
                ldx #>DeleteWindow.clearmsg
                jsr YesNo
                bne SaveWorld.wdret

clr0            jsr CleanLine

                lda dirty
                beq clr1

;               jsr Alarm
                lda #<DeleteWindow.dirtymsg
                ldx #>DeleteWindow.dirtymsg
                jsr YesNo
                bne SaveWorld.wdret

clr1            jsr FreeTags            ; get rid of tags

                lda bot
                ldx bot+1
_clr2           jsr DeleteLine
                bne _clr2

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
                beq SaveWorld.wdret

                jsr jt_alarm

                lda #<delmsg
                ldx #>delmsg
                jsr YesNo
                bne SaveWorld.wdret

_dw1            jsr Clear_.clr0

                lda dirty
                bne SaveWorld.wdret

                ldy #0
                sty numwd
                cpy currentWindow
                bne delwd2

                ldy #w2-w1
delwd2          sty currentWindow
                jsr RestoreWindow

                jmp EditorInit.winit1

;--------------------------------------

clearmsg        .text 7,"CLEAR? "
delmsg          .text 15,"Delete window? "
dirtymsg        .text 19,"Not saved, Delete? "
                .endproc


;======================================
;   GetTemp(msg)
;======================================
GetTemp         .proc
                ldy #0
gett1           sty tempbuf
                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf
                .endproc


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

                jmp rstcol

                .endproc


;======================================
;   YesNo(msg)
;======================================
YesNo           .proc
                jsr GetTemp

                ldy tempbuf
                bne _yn1

                iny
                rts

_yn1            lda tempbuf+1
                ora #$20
                cmp #'y'
                rts
                .endproc
