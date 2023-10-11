
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
front           .proc
                sec
                lda #0
                sbc indent
                sta choff

                jsr dspbuf

                lda lmargin
                jmp rstcol+6

                .endproc


;======================================
;   Back()
;======================================
back            .proc
                ldy #0
                lda (buf),y
_ENTRY1         pha

                clc
                adc lmargin

                sec
                sbc rmargin
                bcs _1

                lda #1
_1              sbc indent
                sta choff

                jsr dspbuf

                sec
                pla
                sbc indent

                sec
                sbc choff

                clc
                adc lmargin

                jmp rstcol+6

                .endproc


;======================================
;   PgUp()
;======================================
pgup            .proc
                sec
                lda lnum
                sbc #2

                ldy #1
                bne page                ; [unc]

                .endproc


;======================================
;   PgDwn()
;======================================
pgdwn           .proc
                ldy #5

                sec
                lda #2
                sbc lnum

                .endproc

                ;[fall-through]


;======================================
;
;======================================
page            .proc
                clc
                adc nlines
                sta arg14

                dec arg14
                beq _XIT

                sty arg13
                jsr clnln

_next1          ldy arg13
                jsr next

                dec arg14
                bne _next1

_XIT            jmp ctrln

                .endproc


;======================================
;   Paste()
;======================================
paste           .proc
                jsr deltop
                beq _XIT

                stx dirty

                jsr clnln
                jsr nextup

                sta cur+1               ; tricky, fake out top

                jsr savewd._ENTRY1
                jsr deltop

_next1          jsr strptr
                jsr ldbuf._ENTRY1
                jsr instb

                lda allocerr
                bne _1                  ; check for out of memory

                jsr delnext
                bne _next1

_1              jsr rstcur

                ldy curwdw
                lda w1+wcur+1,y
                beq _2

                jsr nextdwn

_2              lda #0
                jmp newpage._ENTRY1

_XIT            rts
                .endproc


;======================================
;   old IndentL()
;======================================
indntl          .proc
                lda indent
                beq scrlinit._XIT

                dec indent

                jmp ctrln

                .endproc


;======================================
;   old IndentR()
;======================================
indntr          .proc
                lda indent
                bmi scrlinit._XIT

                inc indent

                jmp ctrln

                .endproc


;======================================
;   InsrtT() insert/replace toggle
;======================================
insrtt          .proc            ; was InsertT
                lda #<_rmsg
                ldx #>_rmsg
                inc insert
                beq _XIT

                lda #$FF
                sta insert

                lda #<_imsg
                ldx #>_imsg

_XIT            jmp cmdmsg

;--------------------------------------

_imsg           .text 6,"INSERT"
_rmsg           .text 7,"REPLACE"

                .endproc


;======================================
;
;======================================
scrlinit        .proc
                sty arg13

                jsr clnln
                beq _1

                ldy arg13
                jsr next
                beq _1                  ; EOF

                lda colcrs
                sta x

                ; lda choff
                ; beq _SI1

                lda #0
                sta choff

                jsr dspbuf

                jmp ldbuf

_1              pla
                pla

_XIT            rts
                .endproc


;======================================
;   ScrlUp()
;======================================
scrlup          .proc
                ldy #1
                jsr scrlinit

                dec lnum
                bmi _1

                jmp scrup

_1              inc lnum

                lda ytop
                sta y

                jsr botln

                lda nlines
                jsr movedwn
                jsr rstcol

                jmp rfrshbuf

                .endproc


;======================================
;   ScrlDwn()
;======================================
scrldwn         .proc
                ldy #5
                jsr scrlinit

                ldx lnum
                inx
                cpx nlines
                beq _1

                stx lnum

                jmp scrdwn

_1              jsr botln

                stx y

                lda nlines
                ldx ytop
                jsr moveup

                jsr rstcol
                jsr dspbuf

                jmp rstcol

                .endproc


;======================================
;   BotLn()
;======================================
botln           .proc
                clc
                lda ytop
                adc nlines

                tax
                dex

_XIT            rts
                .endproc


;======================================
;   ChkCol()
;======================================
chkcol          .proc
                jsr setsp

                ldy #0
                lda (buf),y
                cmp sp
                bcs _XIT

                jsr back
                jsr setsp

                clc
_XIT            rts
                .endproc


;======================================
;   ScrlLft()
;======================================
scrllft         .proc
                jsr chkcol

                lda lmargin
                cmp colcrs
                bcc _XIT

                clc
                lda choff
                adc indent
                beq chkcol._XIT

                dec choff

                jsr dspbuf
                jsr scrrt

_XIT            jmp scrlft

                .endproc


;======================================
;   ScrlRt()
;======================================
scrlrt          .proc
                jsr chkcol
                bcc chkcol._XIT

                lda colcrs
                cmp rmargin
                bcc _XIT

                inc choff

                jsr dspbuf
                jsr scrlft

_XIT            jmp scrrt

                .endproc


;======================================
;   SetSp()
;======================================
setsp           .proc
                sec
                lda indent
                adc choff

                clc
                adc colcrs

                sec
                sbc lmargin
                sta sp

                rts
                .endproc


;======================================
;   MoveDwn(cnt, row)
;======================================
movedwn         .proc
                ldy #+0-40              ; rowSize
                sty arg5

                ldy #$FF
                bne move                ; [unc]

                .endproc


;======================================
;   MoveUp(cnt, row)
;======================================
moveup          .proc
                ldy #40                 ; rowSize
                sty arg5

                ldy #0

                .endproc

                ;[fall-through]


;======================================
;
;======================================
move            .proc
                sty arg6                ; save registers
                sta arg4

                stx rowcrs
                jsr rstcsr
                jsr dsploc              ; get display address

                ldx arg4
                dex

_next1          lda arg0
                sta arg2

                clc
                adc arg5
                sta arg0

                lda arg1
                sta arg3

                adc arg6
                sta arg1

                ldy #39
_next2          lda (arg0),y
                sta (arg2),y

                dey
                bpl _next2

                dex
                bne _next1

                rts
                .endproc
