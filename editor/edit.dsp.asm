;======================================
;   FILE: edit.dsp.asm
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
;   CommandMsg(message)
;======================================
CommandMsg      .proc
                sta arg0
                jsr cmdcol

                lda #0
                sta arg3
                lda arg0
                ldy #$80
                jsr putstr

                jmp rstcol

                .endproc


;======================================
;   CleanLine()
;======================================
CleanLine       .proc
                jsr chkcur

                lda isDirty
                beq _cll1

                sta dirty
                lda #0
                sta isDirty

                jsr DeleteCurrentLine
                jsr InsertByte

_cll1           jmp chkcur

                .endproc


;======================================
;   SaveWindow()
;======================================
SaveWindow      .proc
                jsr CleanLine

savwd1          clc
                lda #14
                tax
                adc currentWindow
                tay
_sw1            lda sp,x
                sta w1,y
                dey
                dex
                bpl _sw1

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
_rw1            lda w1,y
                sta sp,x
                dey
                dex
                bpl _rw1

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
                beq _cl0

                jsr nextup
                beq _cl0

                inc temps
                jsr nextup
                beq _cl0

                inc temps
_cl0            jsr NewPage

_cl1            lda temps
                beq RestoreWindow._XIT

                jsr ScrollDown

                dec temps
                jmp _cl1

                .endproc


;======================================
;   TopLine()
;======================================
TopLine         .proc
                jsr CleanLine
                jsr chkcur._ldtop

                .endproc

                ;[fall-through]


;======================================
;   NewPage()
;======================================
NewPage         .proc
                lda #0
                sta lnum
npage1          sta choff
                jsr rstcsr              ; for command line

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
                jsr savecol
                jsr SaveWindow

                inc ROWCRS
                jsr nextdwn

                sta arg9
                clc
                lda nlines
                sbc lnum
                sta arg10
                beq _cl4

_cl1            ldy #0
                lda indent
                sta arg3
                ldx arg9
                beq _cl5

                jsr curstr

_cl2            jsr putstr

                lda arg9
                bne _cl3

                tay
                sta (arg0),y
_cl3            inc ROWCRS
                jsr nextdwn

                sta arg9
                dec arg10
                bne _cl1

_cl4            jsr rstcur
                jsr rstcol

                jmp RefreshBuf

_cl5            lda #<zero
                ldx #>zero
                bra _cl2

                .endproc
