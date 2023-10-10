
;======================================
;   FILE: edit.chr.asm
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
;   InsrtCh() char in curCh
;======================================
insrtch         .proc
                jsr setsp

                ldy #0
                lda (buf),y
                cmp linemax
                bcc _1                  ; test line too long

                jsr scrbell

                ldy #0
                lda (buf),y
_1              cmp sp
                bcs _2

                sta arg0

                lda sp
                sta (buf),y

                ldy arg0
                lda #$20                ; pad with spaces
_next1          iny
                sta (buf),y

                cpy sp
                bcc _next1

_next2          ldy sp
                lda curch
                sta (buf),y

                lda #$FF
                sta dirtyf

                jsr dspbuf

                jmp scrlrt

_2              ldx insert
                beq _next2

;   move buf right one char
                adc #0                  ; really 1, carry set
                sta (buf),y

                tay
_next3          dey

                lda (buf),y
                iny
                sta (buf),y

                dey
                cpy sp
                bne _next3
                beq _next2              ;[unc]

                .endproc


;======================================
;   InsrtSp()
;======================================
insrtsp         .proc
                lda insert
                pha

                lda #$20
                sta insert
                sta curch

                jsr insrtch

                pla
                sta insert

                jmp scrlft

                .endproc


;======================================
;   Insrt()
;======================================
insrt           .proc
                jsr clnln
                jsr nextup

                sta cur+1               ; tricky

                jsr _ENTRY1

                lda #0
                jmp newpage._ENTRY1

_ENTRY1         lda #0
                tay
_ENTRY2         sta (buf),y

                iny
                sty dirty

                jmp instb

                .endproc


;======================================
; handle pad if any
;======================================
csret           .proc
                jsr insrtsp
                jsr delch

                ldy #0
                lda (buf),y
                pha

                jsr setsp

                sta dirtyf              ; always non-zero

                sec
                sbc #1
                sta (buf),y

                jsr clnln

                pla
                sta arg1

                inc arg1
                lda #0
                sta arg0
                beq _1

_next1          lda (buf),y
                inc arg0
                ldy arg0
                sta (buf),y

                inc sp
_1              ldy sp
                cpy arg1
                bcc _next1

                ldy #0
                lda arg0
                jsr insrt._ENTRY2
                jsr nextup
                jsr refresh

                jmp return._ENTRY1

                .endproc


;======================================
;   Return()
;======================================
return          .proc
                ldx insert
                bne csret

                jsr chkdwn
                bne _ENTRY1

                jsr insrt._ENTRY1
                jsr nextup
                jsr ldbuf

_ENTRY1         jsr scrldwn

_XIT            jmp front

                .endproc


;======================================
;   Delete()
;======================================
delete          .proc
                jsr clnln

                lda delbuf
                ldx delbuf+1
                stx dirty

                ldy lastch
                cpy #$9C
                beq _1

                jsr delfree

_1              sta arg3
                stx arg4

                jsr instbuf
                jsr chkdwn              ; last line ?
                bne _2                  ;   no, delete it

                tay
                sta (buf),y

                iny
                sty dirtyf
                bne return._XIT         ; [unc]

_2              jsr delcur
                beq _3

                jsr nextdwn
_3              jsr chkcur

                lda #0

                jmp newpage._ENTRY1

                .endproc


;======================================
;   DelTop()
;======================================
deltop          .proc
                lda delbuf+4
                ldx delbuf+5
                sta delnxt
                stx delnxt+1

                .endproc

                ;[fall-through]


;======================================
;   DelEnd(ptr)
;======================================
delend          .proc
                cmp #<delbuf
                bne _XIT

                cpx #>delbuf

_XIT            rts
                .endproc


;======================================
;   DelFree(bot)
;======================================
delfree         .proc
                jsr delend
                beq delend._XIT

                jsr delln
                bne delfree

                .endproc


;======================================
;   DelNext()
;======================================
delnext         .proc
                ldy #5
                lda (delnxt),y
                tax

                dey
                lda (delnxt),y
                sta delnxt
                stx delnxt+1

                jmp delend

                .endproc


;======================================
;
;======================================
undo            .proc
                jsr ldbuf

                jmp front

                .endproc


;======================================
;   DeleteCh()
;======================================
delch           .proc
                jsr chkcol
                bcc chkdwn._XIT

                ldy #0
                lda (buf),y
                sta dirtyf

                sec
                sbc #1
                sta (buf),y

                ldy sp
_next1          iny
                lda (buf),y

                dey
                sta (buf),y

                iny
                cpy dirtyf
                bcc _next1               ; really checking for =

                .endproc


;======================================
;   RfrshBuf()
;======================================
rfrshbuf        .proc
                jsr dspbuf

                jmp rstcol.lftrt

                .endproc


;======================================
;   ChkDwn()
;======================================
chkdwn          .proc
                jsr clnln
                beq _XIT

                ldy #5
                lda (cur),y

_XIT            rts
                .endproc


;======================================
;   BackSp()
;======================================
backsp          .proc
                jsr setsp

                cmp #2
                bcc chkdwn._XIT

_ENTRY1         jsr scrllft
                jsr setsp

                tay
                lda #$20
                sta (buf),y
                sta dirtyf

                lda insert
                bne delch

                jmp rfrshbuf

                .endproc


;======================================
;
;======================================
csbs            .proc
                jsr setsp

                cmp #2
                bcs backsp._ENTRY1

                jsr chkcur
                beq chkdwn._XIT        ; no lines at all!

                ldy #1
                lda (cur),y
                beq chkdwn._XIT        ; no line to merge with

;   merge
                jsr scrlup
                jsr back
                jsr nextdwn

                sta dirtyf

                jsr curstr

                clc
                ldy #0
                lda (buf),y
                sta arg2

                adc (arg0),y
                sta (buf),y

                lda (arg0),y
                beq _next2

                sta arg3

_next1          iny
                sty arg4

                lda (arg0),y
                inc arg2
                ldy arg2
                sta (buf),y

                ldy arg4
                cpy arg3
                bne _next1

_next2          jsr delcur

                jmp refresh

                .endproc
