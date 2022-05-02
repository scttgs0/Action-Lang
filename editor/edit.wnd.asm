;======================================
;   FILE: edit.wnd.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


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
                jsr rstcsr
                jsr SetSpacing

                jmp SaveWindow

wdret           rts
                .endproc


;======================================
;   Clear()
;======================================
Clear_           .proc
                jsr alarm

                lda #<DeleteWindow.clearmsg
                ldx #>DeleteWindow.clearmsg
                jsr YesNo
                bne SaveWorld.wdret

clr0            jsr CleanLine

                lda dirty
                beq clr1

    ; JSR Alarm
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

                jsr alarm

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
                jsr rstcsr

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
