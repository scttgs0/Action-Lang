;======================================
;   FILE: edit.cmd.asm
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
;   Front()
;======================================
Front           .proc
                sec
                lda #0
                sbc indent
                sta choff
                jsr dspbuf

                lda LMARGN
                jmp rstcol+6

                .endproc


;======================================
;   Back()
;======================================
Back            .proc
                ldy #0
                lda (buf),y
back0           pha
                clc
                adc LMARGN
                sec
                sbc RMARGN
                bcs _back1

                lda #1
_back1          sbc indent
                sta choff
                jsr dspbuf

                sec
                pla
                sbc indent
                sec
                sbc choff
                clc
                adc LMARGN
                jmp rstcol+6

                .endproc


;======================================
;   PageUp()
;======================================
PageUp          .proc
                sec
                lda lnum
                sbc #2
                ldy #1
                bne PageContent

                .endproc


;======================================
;   PageDown()
;======================================
PageDown        .proc
                ldy #5
                sec
                lda #2
                sbc lnum
                .endproc


;--------------------------------------
;
;--------------------------------------
PageContent     .proc
                clc
                adc nlines
                sta arg14
                dec arg14
                beq _page2

                sty arg13
                jsr CleanLine

_page1          ldy arg13
                jsr next

                dec arg14
                bne _page1

_page2          jmp CenterLine

                .endproc


;======================================
;   Paste()
;======================================
Paste           .proc
                jsr DeleteTop
                beq _XIT

                stx dirty
                jsr CleanLine
                jsr nextup

                sta cur+1               ; tricky, fake out top
                jsr SaveWindow.savwd1
                jsr DeleteTop
_p1             jsr strptr
                jsr ldbuf.ldbuf1
                jsr InsertByte

                lda allocerr
                bne _p2                 ; check for out of memory

                jsr DeleteNext
                bne _p1

_p2             jsr rstcur

                ldy currentWindow
                lda w1+wcur+1,y
                beq _p3

                jsr nextdwn

_p3             lda #0
                jmp NewPage.npage1

_XIT            rts
                .endproc


;======================================
;   old IndentL()
;======================================
IndentLeft      .proc
                lda indent
                beq ScrollInit._XIT

                dec indent
                jmp CenterLine

                .endproc


;======================================
;   old IndentR()
;======================================
IndentRight     .proc
                lda indent
                bmi ScrollInit._XIT

                inc indent
                jmp CenterLine

                .endproc


;======================================
;   InsrtT() insert/replace toggle
;======================================
InsertToggle    .proc
                lda #<_rmsg
                ldx #>_rmsg
                inc insert
                beq _it1

                lda #$ff
                sta insert
                lda #<_imsg
                ldx #>_imsg
_it1            jmp CommandMsg

;--------------------------------------

_imsg           .text 6,"INSERT"
_rmsg           .text 7,"REPLACE"
                .endproc


;======================================
; Intialize Scrolling
;======================================
ScrollInit      .proc
                sty arg13
                jsr CleanLine
                beq _siret

                ldy arg13
                jsr next
                beq _siret              ; EOF

                lda COLCRS
                sta x

    ; lda choff
    ; beq _SI1

                lda #0
                sta choff
                jsr dspbuf

_si1            jmp ldbuf

_siret          pla
                pla
_XIT            rts
                .endproc


;======================================
;   ScrollUp()
;======================================
ScrollUp        .proc
                ldy #1
                jsr ScrollInit

                dec lnum
                bmi _su2

                jmp scrup

_su2            inc lnum
                lda ytop
                sta y
                jsr BottomLine

                lda nlines
                jsr MoveDown
                jsr rstcol

                jmp RefreshBuf

                .endproc


;======================================
;   ScrollDown()
;======================================
ScrollDown      .proc
                ldy #5
                jsr ScrollInit

                ldx lnum
                inx
                cpx nlines
                beq _sd2

                stx lnum
                jmp scrdwn

_sd2            jsr BottomLine

                stx y
                lda nlines
                ldx ytop
                jsr MoveUp

                jsr rstcol
                jsr dspbuf

                jmp rstcol

                .endproc


;======================================
;   BottomLine()
;======================================
BottomLine      .proc
                clc
                lda ytop
                adc nlines
                tax
                dex
_XIT            rts
                .endproc


;======================================
;   CheckColumn()
;======================================
CheckColumn     .proc
                jsr SetSpacing

                ldy #0
                lda (buf),y
                cmp sp
                bcs _XIT

                jsr Back
                jsr SetSpacing

                clc
_XIT            rts
                .endproc


;======================================
;   ScrollLeft()
;======================================
ScrollLeft      .proc
                jsr CheckColumn

                lda LMARGN
                cmp COLCRS
                bcc _sl1

                clc
                lda choff
                adc indent
                beq CheckColumn._XIT

                dec choff
                jsr dspbuf
                jsr scrrt

_sl1            jmp scrlft

                .endproc


;======================================
;   ScrlRt()
;======================================
ScrollRight     .proc
                jsr CheckColumn
                bcc CheckColumn._XIT

                lda COLCRS
                cmp RMARGN
                bcc _sr2

                inc choff
                jsr dspbuf
                jsr scrlft

_sr2            jmp scrrt

                .endproc


;======================================
;   SetSpacing()
;======================================
SetSpacing      .proc
                sec
                lda indent
                adc choff
                clc
                adc COLCRS
                sec
                sbc LMARGN
                sta sp
                rts
                .endproc


;======================================
;   MoveDown(cnt, row)
;======================================
MoveDown        .proc
                ldy #+0-40     ; rowSize
                sty arg5
                ldy #$ff
                bne MoveContent

                .endproc


;======================================
;   MoveUp(cnt, row)
;======================================
MoveUp          .proc
                ldy #40     ; rowSize
                sty arg5
                ldy #0
                .endproc


;--------------------------------------
;
;--------------------------------------
MoveContent     .proc
                sty arg6
                sta arg4
                stx ROWCRS
                jsr rstcsr
                jsr DisplayLocation    ; get display address

                ldx arg4
                dex
_mu1            lda arg0
                sta arg2
                clc
                adc arg5
                sta arg0
                lda arg1
                sta arg3
                adc arg6
                sta arg1

                ldy #39
_mu2            lda (arg0),y
                sta (arg2),y
                dey
                bpl _mu2

                dex
                bne _mu1

                rts
                .endproc
