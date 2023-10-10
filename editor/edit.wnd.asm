
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
wind1           .proc
                lda curwdw
                beq savworld._XIT

                lda #0
                pha

                .endproc

                ;[fall-through]


;======================================
;   SwapWd()
;======================================
swapwd          .proc
                jsr savworld

                pla
                jmp rstworld

                .endproc


;======================================
;   Wind2()
;======================================
wind2           .proc
                lda curwdw
                bne savworld._XIT

                lda numwd
                bne _1

                jmp w2init

_1              lda #w2-w1
                pha

                bne swapwd              ; [unc]

                .endproc


;======================================
;   SavWorld()
;======================================
savworld        .proc
                jsr clnln
                jsr savecol
                jsr rstcsr
                jsr setsp

                jmp savewd

_XIT            rts
                .endproc


;======================================
;   Clear()
;======================================
clear           .proc
                jsr alarm

                lda #<delwd.clearmsg
                ldx #>delwd.clearmsg
                jsr yesno
                bne savworld._XIT

_ENTRY1         jsr clnln

                lda dirty
                beq _1

;               jsr Alarm

                lda #<delwd.dirtymsg
                ldx #>delwd.dirtymsg
                jsr yesno
                bne savworld._XIT

_1              jsr freetags            ; get rid of tags

                lda bot
                ldx bot+1

_next1          jsr delln
                bne _next1

                stx cur+1
                stx dirty
                stx dirtyf
                stx inbuf

                jmp newpage

                .endproc


;======================================
;   RstWorld(window)
;======================================
rstworld        .proc
                sta curwdw

                jsr rstwd
                jsr ldbuf

                jmp rstcol

                .endproc


;======================================
;   DelWd()
;======================================
delwd           .proc
                lda numwd
                beq savworld._XIT

                jsr alarm

                lda #<delmsg
                ldx #>delmsg
                jsr yesno
                bne savworld._XIT

                jsr clear._ENTRY1

                lda dirty
                bne savworld._XIT

                ldy #0
                sty numwd

                cpy curwdw
                bne _1

                ldy #w2-w1
_1              sty curwdw

                jsr rstwd

                jmp einit._ENTRY1

;--------------------------------------

clearmsg        .text 7,"CLEAR? "
delmsg          .text 15,"Delete window? "
dirtymsg        .text 19,"Not saved, Delete? "

                .endproc


;======================================
;   GetTemp(msg)
;======================================
gettemp         .proc
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
cmdstr          .proc
                sta arg0
                sty arg2

                jsr cmdcol

                lda #$80
                sta arg4

                lda arg0
                ldy arg2

                jsr getstr
                jsr rstcsr

                jmp rstcol

                .endproc


;======================================
;   YesNo(msg)
;======================================
yesno           .proc
                jsr gettemp

                ldy tempbuf
                bne _1

                iny

                rts

_1              lda tempbuf+1
                ora #$20
                cmp #'y'

                rts
                .endproc
