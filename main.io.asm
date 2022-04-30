;======================================
;   FILE: main.io.asm
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
;   Open(device, name, mode, opt)
;--------------------------------------
; returns status
;======================================
open            .proc
                stx arg5
                sty arg6
                ldy #3
                bne xiostr

                .endproc


;======================================
;   Print(device, str)
;======================================
print           .proc
                stx arg5
                sty arg6
                ldx #0
                stx arg3
                ldy #$09
                jsr xiostr
                bne print1

                lda #$0b
                sta $03_0342,x ;!! IOCB0+ICCOM,x
                lda #eol
                jmp $03_E456 ;!! CIOV

print1          rts
                .endproc


;======================================
;   Close(device)
;======================================
close           .proc
                ldx #>$03_B000 ;!! ml
                stx arg6                ; note: address must be non-zero to
                                        ; fake out zero check in XIOstr
        .if ramzap
                 sta (arg5),y
        .else
                 nop
                 nop
        .endif

                ldy #$0c
                bne input.input1

                .endproc


;======================================
;   Input(device, str)
;======================================
input           .proc
                sty arg6
                ldy #$05
input1          stx arg5
                ldx #0
                stx arg3
                .endproc

                ;[fall-through]


;======================================
;   XIOstr(device,,cmd,aux1,aux2,str)
;======================================
xiostr          .proc
                asl a
                asl a
                asl a
                asl a
                tax
                tya
                sta $03_0342,x ;!! IOCB0+ICCOM,x       ; command
                lda arg3
                beq _xs1

                sta $03_034A,x ;!! IOCB0+ICAX1,x
                lda arg4
                sta $03_034B,x ;!! IOCB0+ICAX2,x

                lda #0
_xs1            tay
                sta $03_0349,x ;!! IOCB0+ICBLH,x
                lda (arg5),y
                sta $03_0348,x ;!! IOCB0+ICBLL,x       ; size

                beq print.print1        ; return

                clc
                lda arg5
                adc #1
                sta $03_0344,x ;!! IOCB0+ICBAL,x       ; buffer address
                lda arg6
                adc #0
                sta $03_0345,x ;!! IOCB0+ICBAH,x
                jmp $03_E456 ;!! CIOV

                .endproc


;======================================
;   Output(device, str)
;======================================
output          .proc
                sty arg6
                ldy #$0b
                bne input.input1

                .endproc


;======================================
;   DspStr(prompt, str, invert)
;======================================
dspstr          .proc
                sty arg12
                ldy arg3
                sty arg13
                ldy #0
                sty arg3
                ldy arg4
                jsr putstr

                lda arg6                ; PutStr size
                clc
                adc lmargin
                sta colcrs
                jsr scrrt

                ldy #0
                lda (arg12),y
                beq _ds2

                sta arg3
                sty arg4

_ds1            inc arg4
                ldy arg4
                lda (arg12),y
                eor arg2
                jsr scrch

                dec arg3
                bne _ds1

_ds2            rts
                .endproc


;======================================
;   RdBuf(device)
;======================================
rdbuf           .proc
    ; INC $03_02C8 ;!! COLOR4
                nop
                nop
                nop
                ldy #0
                tax
                lda #240
                sta (buf),y
                txa
                ldx buf
                ldy buf+1
inputs          jsr input

                sty arg0
                lda $03_0348,x ;!! IOCB0+ICBLL,x       ; size
                beq _rb1

                sec
                sbc #1
_rb1            ldy #0
                sta (arg5),y
                ldy arg0
                rts
                .endproc


;======================================
;   WrtBuf(device)
;======================================
wrtbuf          .proc
                ldx buf
                ldy buf+1
                jmp print

                .endproc


;======================================
;   RstCur()
;======================================
rstcur          .proc
                ldy currentWindow
                lda w1+wcur,y
                sta cur
                lda w1+wcur+1,y
                sta cur+1
                jmp ldbuf

                .endproc


;======================================
;   SysErr(,,errnum)
;======================================
syserr          .proc
                jsr dspon

                tya
                ldx #0
                jsr ctostr
                jsr cmdcol

                lda #$80
                sta arg4
                lda #>numbuf
                sta arg3
                ldy #<numbuf
                lda #<sermsg
                ldx #>sermsg
                jsr dspstr
                jsr rstcsr
                jsr rstcol

                jmp scrbell

                .endproc

;--------------------------------------
;--------------------------------------

sermsg          .text 7,"Error: "


;======================================
;   CToStr(num) - Cardinal to string
;======================================
ctostr          .proc
                sta fr0
                stx fr0+1
                jsr $03_D9AA ;!! IFP                 ; Cardinal to real

                .endproc


;======================================
;   RToStr() - real in FR0
;======================================
rtostr          ;.proc
                jsr $03_D8E6 ;!! FASC

                ldy #$ff
                ldx #0
_rts1           iny
                inx
                lda (inbuff),y
                sta numbuf,x
                bpl _rts1

                eor #$80
                sta numbuf,x
                stx numbuf
                rts
                ;.endproc


;======================================
;   DspOff()
;======================================
dspoff          .proc
                lda tvdisp
                sta $03_022F ;!! SDMCTL
                sta $03_D400 ;!! DMACTL
                rts
                .endproc


;======================================
;   DspOn()
;======================================
dspon           .proc
                lda #$22
                sta $03_022F ;!! SDMCTL
                sta $03_D400 ;!! DMACTL
                lda bckgrnd             ; background color
                sta $03_02C8 ;!! COLOR4              ; restore background
                rts
                .endproc


;======================================
;   PrintC(num)
;======================================
printc          .proc
                jsr ctostr

pnum            lda device
                ldx #<numbuf
                ldy #>numbuf
                jmp output

                .endproc


;======================================
;   OpenChan(mode)
;======================================
openchan        .proc
                pha
                lda chan
                jsr close

                pla
                sta arg3

    ; check for default device
                lda #':'
                ldy #2
                cmp (nxtaddr),y
                beq _oc2

                iny
                cmp (nxtaddr),y
                beq _oc2

    ; stuff in D: for device
                clc
                lda nxtaddr
                adc #2
                sta fr0
                lda nxtaddr+1
                adc #0
                sta fr0+1

                ldy #0
                lda (nxtaddr),y         ; add 2 to length of string
                adc #2                  ;  so we can insert 'D:'
                sta (nxtaddr),y
                tay
_oc1            lda (nxtaddr),y         ; move string up...
                sta (fr0),y
                dey
                bne _oc1

                iny
                lda #'D'
                sta (nxtaddr),y
                iny
                lda #':'
                sta (nxtaddr),y

_oc2            lda chan
                ldx nxtaddr
                ldy nxtaddr+1
                jsr open
                bpl printbuf

                jmp splerr              ; oopps error in open

                .endproc


;======================================
;   PrintBuf()
;======================================
printbuf        .proc
                lda list
                bne rtocar.htcr1        ; return

                jmp wrtbuf

                .endproc


;======================================
;   HToCar(buf,index)
;======================================
htocar          .proc
                sty cix
                sta arg1
                stx arg2
                lda #0
                sta fr0
                sta fr0+1

_htc1           ldy cix
                lda (arg1),y
                sec
                sbc #'0'
                bmi rtocar.htcrtn

                cmp #10
                bmi _htcok

                cmp #17
                bmi rtocar.htcrtn

                sbc #7
                cmp #16
                bpl rtocar.htcrtn

_htcok          sta arg5
                lda fr0
                ldx fr0+1
                ldy #4
                jsr lsh1

                clc
                adc arg5
                sta fr0
                stx fr0+1
                inc cix
                bra _htc1

                .endproc


;======================================
;   RToCar()
;======================================
rtocar          .proc
                jsr $03_D9D2 ;!! FPI
                bcs rcerr

htcrtn          lda fr0
                ldx fr0+1
                ldy cix
htcr1           rts

rcerr           ldy #conster
                jmp splerr

                .endproc


;======================================
;   SToReal(str, index)
;======================================
storeal         .proc
                sty cix
                sta inbuff
                stx inbuff+1
                jmp $03_D800 ;!! AFP

                .endproc


;======================================
;   PutSp()
;======================================
putsp           .proc
                ldy #$20
                bne putchar

                .endproc


;======================================
;   PutEOL()
;======================================
puteol          .proc
                ldy #eol
                .endproc


;======================================
;   PutChar(,,char)
;======================================
putchar         .proc
                lda device
                jmp scrch.scrchar

                .endproc


;======================================
;   PutStr(str, invert, offset)
;======================================
putstr          .proc
                sta arg6
                stx arg7
                sty arg2

                sec
                adc arg3
                sta arg4
                bcc _ps1

                inx
_ps1            stx arg5
                jsr dsploc
                jsr zapcsr

                ldy #39
                lda arg2
_ps2            sta (arg0),y            ; clear line
                dey
                bpl _ps2

                clc                     ; handle left margin
                lda arg0
                adc lmargin
                sta arg0
                bcc _ps3

                inc arg1
_ps3            iny                     ; sets Y to 0
                clc
                lda (arg6),y
                sbc arg3
                bcc _ps10               ; no chars

                sta arg6
                tay
                lda #0
                sta arg7

                sec
                lda rmargin
                sbc lmargin
                cmp arg6
                beq _ps3a               ; handle EOL char
                bcs _ps4                ; length ok

                sta arg6
                ldy arg6                ; length too long
_ps3a           lda #$80
                sta arg7

_ps4            lda arg2
                eor (arg4),y
                pha
                and #$60
                tax
                pla
                and #$9f
                ora chrConvert,x
                sta (arg0),y
                dey
                bpl _ps4

                ldy arg6
                lda arg7
                bne _ps6

                lda arg2
                bne _ps7                ; no EOL char if inverted

                iny
_ps5            lda eolch
                sta (arg0),y
                jmp _ps7

_ps6            eor (arg0),y
                sta (arg0),y

_ps7            lda arg3
                beq _ps9

_ps8            ldy #0
                lda (arg0),y
                eor #$80
                sta (arg0),y
_ps9            rts

_ps10           lda arg3
                bne _ps8

                tay
                bra _ps5

                .endproc


;======================================
;   CmdCol()
;======================================
cmdcol          .proc
                jsr savecol
                jsr rstcsr

                ldy cmdln
                sty rowcrs
                rts
                .endproc


;======================================
;   SaveCol()
;======================================
savecol         .proc
                lda rowcrs
                sta y
                lda colcrs
                sta x
                rts
                .endproc


;======================================
;   RstCol()
;======================================
rstcol          .proc
                lda y
                sta rowcrs
                lda x
                sta colcrs
                jsr zapcsr
lftrt           jsr scrlft

                jmp scrrt

                .endproc


;======================================
;   ChkCur()
;======================================
chkcur          .proc
                lda cur+1
                bne _ccret

_ldtop          lda top
                sta cur
                lda top+1
                sta cur+1
_ccret          rts
                .endproc


;======================================
;   LdBuf() load buf
;======================================
ldbuf           .proc
                jsr chkcur
                bne _ldb0

                tay
                sta (buf),y
                rts

_ldb0           jsr curstr

ldbuf1          ldy #0
                lda (arg0),y
                sta (buf),y
                tay

_ldb1           lda (arg0),y
                sta (buf),y
                dey
                bne _ldb1

                rts
                .endproc


;======================================
;   DspBuf() - display buffer
;======================================
dspbuf          .proc
                clc
                lda indent
                adc choff
                sta arg3
                ldy #0
                lda buf
                ldx buf+1
                jmp putstr

                .endproc


;======================================
;   DspLoc() get address of display
;======================================
dsploc          .proc
                lda savmsc
                ldx savmsc+1
                ldy rowcrs
                beq _dlocrt

_dloc1          clc
                adc #40     ; rowSize
                bcc _dloc2

                inx
_dloc2          dey
                bne _dloc1

_dlocrt         sta arg0
                stx arg1
                rts
                .endproc


;======================================
;   ZapCsr() get rid of old cursor
;======================================
zapcsr          .proc
                lda #<csrch
                sta oldadr
                lda #>csrch
                sta oldadr+1
                rts
                .endproc


;======================================
;   RstCsr() restore char under Csr
;======================================
rstcsr          .proc
                ldy #0
                lda oldchr
                sta (oldadr),y
                rts
                .endproc
