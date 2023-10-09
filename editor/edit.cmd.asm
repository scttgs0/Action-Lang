
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
back0           pha
                clc
                adc lmargin
                sec
                sbc rmargin
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
                bne page

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


;======================================
;
;======================================
page            .proc
                clc
                adc nlines
                sta arg14
                dec arg14
                beq _page2

                sty arg13
                jsr clnln

_page1          ldy arg13
                jsr next

                dec arg14
                bne _page1

_page2          jmp ctrln

                .endproc


;======================================
;   Paste()
;======================================
paste           .proc
                jsr deltop
                beq _pret

                stx dirty
                jsr clnln
                jsr nextup

                sta cur+1               ; tricky, fake out top
                jsr savewd.savwd1
                jsr deltop
_p1             jsr strptr
                jsr ldbuf.ldbuf1
                jsr instb

                lda allocerr
                bne _p2                 ; check for out of memory

                jsr delnext
                bne _p1

_p2             jsr rstcur

                ldy curwdw
                lda w1+wcur+1,y
                beq _p3

                jsr nextdwn

_p3             lda #0
                jmp newpage.npage1

_pret           rts
                .endproc


;======================================
;   old IndentL()
;======================================
indntl          .proc
                lda indent
                beq scrlinit.putrtn

                dec indent
                jmp ctrln

                .endproc


;======================================
;   old IndentR()
;======================================
indntr          .proc
                lda indent
                bmi scrlinit.putrtn

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
                beq _it1

                lda #$FF
                sta insert
                lda #<_imsg
                ldx #>_imsg
_it1            jmp cmdmsg

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
                beq _siret

                ldy arg13
                jsr next
                beq _siret              ; EOF

                lda colcrs
                sta x

    ; LDA choff
    ; BEQ _SI1

                lda #0
                sta choff
                jsr dspbuf

_si1            jmp ldbuf

_siret          pla
                pla
putrtn          rts
                .endproc


;======================================
;   ScrlUp()
;======================================
scrlup          .proc
                ldy #1
                jsr scrlinit

                dec lnum
                bmi _su2

                jmp scrup

_su2            inc lnum
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
                beq _sd2

                stx lnum
                jmp scrdwn

_sd2            jsr botln

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
escape          rts
                .endproc


;======================================
;   ChkCol()
;======================================
chkcol          .proc
                jsr setsp

                ldy #0
                lda (buf),y
                cmp sp
                bcs chkc1

                jsr back
                jsr setsp

                clc
chkc1           rts
                .endproc


;======================================
;   ScrlLft()
;======================================
scrllft         .proc
                jsr chkcol

                lda lmargin
                cmp colcrs
                bcc _sl1

                clc
                lda choff
                adc indent
                beq chkcol.chkc1

                dec choff
                jsr dspbuf
                jsr scrrt

_sl1            jmp scrlft

                .endproc


;======================================
;   ScrlRt()
;======================================
scrlrt          .proc
                jsr chkcol
                bcc chkcol.chkc1

                lda colcrs
                cmp rmargin
                bcc _sr2

                inc choff
                jsr dspbuf
                jsr scrlft

_sr2            jmp scrrt

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
                ldy #+0-40
                sty arg5
                ldy #$FF
                bne move

                .endproc


;======================================
;   MoveUp(cnt, row)
;======================================
moveup          .proc
                ldy #40
                sty arg5
                ldy #0
                .endproc


;======================================
;
;======================================
move            .proc
                sty arg6
                sta arg4
                stx rowcrs
                jsr rstcsr
                jsr dsploc              ; get display address

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
