
; SPDX-FileName: main.io.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   Open(device, name, mode, opt)
;--------------------------------------
; returns status
;======================================
Open            .proc
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

                lda #$0B
                ;!!sta IOCB0+ICCOM,x
                lda #eol
                ;!!jmp CIOV

print1          rts
                .endproc


;======================================
;   Close(device)
;======================================
close           .proc
                ldx #>$B000             ; ml
                stx arg6                ; note: address must be non-zero to
                                        ; fake out zero check in XIOstr
        .if ramzap
                 sta (arg5),y
        .else
                 nop
                 nop
        .endif

                ldy #$0C
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
                asl                     ; *16
                asl
                asl
                asl
                tax
                tya
                ;!!sta IOCB0+ICCOM,x       ; command
                lda arg3
                beq _xs1

                ;!!sta IOCB0+ICAX1,x
                lda arg4
                ;!!sta IOCB0+ICAX2,x

                lda #0
_xs1            tay
                ;!!sta IOCB0+ICBLH,x
                lda (arg5),y
                ;!!sta IOCB0+ICBLL,x       ; size

                beq print.print1        ; return

                clc
                lda arg5
                adc #1
                ;!!sta IOCB0+ICBAL,x       ; buffer address
                lda arg6
                adc #0
                ;!!sta IOCB0+ICBAH,x
                ;!!jmp CIOV

                .endproc


;======================================
;   Output(device, str)
;======================================
output          .proc
                sty arg6
                ldy #$0B
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
                adc LMARGN
                sta COLCRS
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
;   ReadBuffer(device)
;======================================
ReadBuffer      .proc
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
                ;!!lda IOCB0+ICBLL,x       ; size
                beq _1

                sec
                sbc #1
_1              ldy #0
                sta (arg5),y
                ldy arg0
                rts
                .endproc


;======================================
;   WrtBuf(device)
;======================================
WriteBuffer     .proc
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
                sta FR0
                stx FR0+1
                ;!!jsr IFP                 ; Cardinal to real

                .endproc


;======================================
;   RToStr() - real in FR0
;======================================
rtostr          ;.proc
                ;!!jsr FASC

                ldy #$FF
                ldx #0
_rts1           iny
                inx
                lda (INBUFF),y
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
                lda jt_tvdisp
                ;!!sta SDMCTL
                ;!!sta DMACTL
                rts
                .endproc


;======================================
;   DspOn()
;======================================
dspon           .proc
                lda #$22
                ;!!sta SDMCTL
                ;!!sta DMACTL
                lda bckgrnd             ; background color
                ;!!sta COLOR4              ; restore background
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
                lda Channel
                jsr close

                pla
                sta arg3

;   check for default device
                lda #':'
                ldy #2
                cmp (nxtaddr),y
                beq _oc2

                iny
                cmp (nxtaddr),y
                beq _oc2

;   stuff in D: for device
                clc
                lda nxtaddr
                adc #2
                sta FR0
                lda nxtaddr+1
                adc #0
                sta FR0+1

                ldy #0
                lda (nxtaddr),y         ; add 2 to length of string
                adc #2                  ;  so we can insert 'D:'
                sta (nxtaddr),y
                tay
_oc1            lda (nxtaddr),y         ; move string up...
                sta (FR0),y
                dey
                bne _oc1

                iny
                lda #'D'
                sta (nxtaddr),y
                iny
                lda #':'
                sta (nxtaddr),y

_oc2            lda Channel
                ldx nxtaddr
                ldy nxtaddr+1
                jsr Open
                bpl printbuf

                jmp splerr              ; oops, error in Open

                .endproc


;======================================
;   PrintBuf()
;======================================
printbuf        .proc
                lda list
                bne rtocar.htcr1        ; return

                jmp WriteBuffer

                .endproc


;======================================
;   HToCar(buf,index)
;======================================
htocar          .proc
                sty CIX
                sta arg1
                stx arg2
                lda #0
                sta FR0
                sta FR0+1

_htc1           ldy CIX
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
                lda FR0
                ldx FR0+1
                ldy #4
                jsr lsh1

                clc
                adc arg5
                sta FR0
                stx FR0+1
                inc CIX
                bra _htc1

                .endproc


;======================================
;   RToCar()
;======================================
rtocar          .proc
                ;!!jsr FPI
                bcs rcerr

htcrtn          lda FR0
                ldx FR0+1
                ldy CIX
htcr1           rts

rcerr           ldy #constERR
                jmp splerr

                .endproc


;======================================
;   SToReal(str, index)
;======================================
storeal         .proc
                sty CIX
                sta INBUFF
                stx INBUFF+1
                ;!!jmp AFP

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
                jsr DisplayLocation
                jsr zapcsr

                ldy #39
                lda arg2
_ps2            sta (arg0),y            ; clear line
                dey
                bpl _ps2

                clc                     ; handle left margin
                lda arg0
                adc LMARGN
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
                lda RMARGN
                sbc LMARGN
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
                and #$9F
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
_ps5            lda eol
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
                sty ROWCRS
                rts
                .endproc


;======================================
;   SaveCol()
;======================================
savecol         .proc
                lda ROWCRS
                sta y
                lda COLCRS
                sta x
                rts
                .endproc


;======================================
;   RstCol()
;======================================
rstcol          .proc
                lda y
                sta ROWCRS
                lda x
                sta COLCRS
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
;   DisplayLocation() get address of display
;======================================
DisplayLocation .proc
                lda #<CS_TEXT_MEM_PTR
                ldx #>CS_TEXT_MEM_PTR
                ldy ROWCRS
                beq _dlocrt

_dloc1          clc
                adc #CharResX           ; TODO: fragile
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
                ;sta OLDADR      ; TODO:
                lda #>csrch
                ;sta OLDADR+1
                rts
                .endproc


;======================================
;   RstCsr() restore char under Csr
;======================================
rstcsr          .proc
                ldy #0
                lda OLDCHR
                ;sta (OLDADR),y
                rts
                .endproc
