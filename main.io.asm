
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.io.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   Open(device, name, mode, opt)
;--------------------------------------
; returns status
;======================================
Open            .proc
                stx arg5
                sty arg6

                ldy #3
                bne xiostr              ; [unc]

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
                bne _XIT

                lda #$0B
                ;!!sta IOCB0+ICCOM,X

                lda #EOL
                ;!!jmp CIOV

_XIT            rts
                .endproc


;======================================
;   Close(device)
;======================================
close           .proc
                ldx #>$B000      ;; ml
                stx arg6                ; note: address must be non-zero to
                                        ; fake out zero check in XIOstr
            .if ZAPRAM
                 sta (arg5),Y
            .else
                 nop
                 nop
            .endif

                ldy #$0C
                bne input._ENTRY1       ; [unc]

                .endproc


;======================================
;   Input(device, str)
;======================================
input           .proc
                sty arg6

                ldy #$05
_ENTRY1         stx arg5

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
                ;!!sta IOCB0+ICCOM,X       ; command

                lda arg3
                beq _1

                ;!!sta IOCB0+ICAX1,X

                lda arg4
                ;!!sta IOCB0+ICAX2,X

                lda #0
_1              tay
                ;!!sta IOCB0+ICBLH,X
                lda (arg5),Y
                ;!!sta IOCB0+ICBLL,X       ; size

                beq print._XIT          ; return

                clc
                lda arg5
                adc #1
                ;!!sta IOCB0+ICBAL,X       ; buffer address

                lda arg6
                adc #0
                ;!!sta IOCB0+ICBAH,X

                ;!!jmp CIOV

                .endproc


;======================================
;   Output(device, str)
;======================================
output          .proc
                sty arg6

                ldy #$0B
                bne input._ENTRY1       ; [unc]

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
                lda (arg12),Y
                beq _XIT

                sta arg3
                sty arg4

_next1          inc arg4
                ldy arg4
                lda (arg12),Y
                eor arg2
                jsr scrch

                dec arg3
                bne _next1

_XIT            rts
                .endproc


;======================================
;   ReadBuffer(device)
;======================================
ReadBuffer      .proc
;               inc COLOR4
                nop
                nop
                nop

                ldy #0
                tax
                lda #240
                sta (buf),Y

                txa
                ldx buf
                ldy buf+1
inputs          jsr input

                sty arg0

                ;!!lda IOCB0+ICBLL,X    ; size
                beq _1

                sec
                sbc #1
_1              ldy #0
                sta (arg5),Y

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
                lda w1+WCUR,Y
                sta cur
                lda w1+WCUR+1,Y
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

sermsg          .ptext "Error: "


;======================================
;   CToStr(num) - Cardinal to string
;======================================
ctostr          .proc
                sta FR0
                stx FR0+1

                ;!!jsr IFP                 ; Cardinal to real

                .endproc

                ;[fall-through]


;======================================
;   RToStr() - real in FR0
;======================================
rtostr          ;.proc
                ;!!jsr FASC

                ldy #$FF
                ldx #0
_next1          iny
                inx

                lda (INBUFF),Y
                sta numbuf,X
                bpl _next1

                eor #$80
                sta numbuf,X
                stx numbuf

                rts
                ;.endproc

                ;[fall-through]


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
                cmp (nxtaddr),Y
                beq _1

                iny
                cmp (nxtaddr),Y
                beq _1

;   stuff in D: for device
                clc
                lda nxtaddr
                adc #2
                sta FR0
                lda nxtaddr+1
                adc #0
                sta FR0+1

                ldy #0
                lda (nxtaddr),Y         ; add 2 to length of string
                adc #2                  ;  so we can insert 'D:'
                sta (nxtaddr),Y

                tay
_next1          lda (nxtaddr),Y         ; move string up...
                sta (FR0),Y

                dey
                bne _next1

                iny
                lda #'D'
                sta (nxtaddr),Y

                iny
                lda #':'
                sta (nxtaddr),Y

_1              lda Channel
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
                bne rtocar._XIT         ; return

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

_next1          ldy CIX
                lda (arg1),Y
                sec
                sbc #'0'
                bmi rtocar._ENTRY1

                cmp #10
                bmi _1

                cmp #17
                bmi rtocar._ENTRY1

                sbc #7
                cmp #16
                bpl rtocar._ENTRY1

_1              sta arg5

                lda FR0
                ldx FR0+1
                ldy #4
                jsr lsh1

                clc
                adc arg5
                sta FR0
                stx FR0+1

                inc CIX
                bne _next1

                .endproc


;======================================
;   RToCar()
;======================================
rtocar          .proc
                ;!!jsr FPI
                bcs _err

_ENTRY1         lda FR0
                ldx FR0+1
                ldy CIX

_XIT            rts

_err            ldy #constERR
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
                bne putchar             ; [unc]

                .endproc


;======================================
;   PutEOL()
;======================================
puteol          .proc
                ldy #EOL

                .endproc

                ;[fall-through]


;======================================
;   PutChar(,,char)
;======================================
putchar         .proc
                lda device
                jmp scrch._ENTRY1

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
                bcc _1

                inx

_1              stx arg5

                jsr DisplayLocation
                jsr zapcsr

                ldy #39
                lda arg2
_next1          sta (arg0),Y            ; clear line

                dey
                bpl _next1

                clc                     ; handle left margin
                lda arg0
                adc LMARGN
                sta arg0
                bcc _2

                inc arg1

_2              iny                     ; sets Y to 0

                clc
                lda (arg6),Y
                sbc arg3
                bcc _6                  ; no chars

                sta arg6

                tay
                lda #0
                sta arg7

                sec
                lda RMARGN
                sbc LMARGN
                cmp arg6
                beq _3                  ; handle EOL char
                bcs _next2                ; length ok

                sta arg6

                ldy arg6                ; length too long
_3              lda #$80
                sta arg7

_next2          lda arg2
                eor (arg4),Y
                pha

                and #$60
                tax

                pla
                and #$9F
                ora chrConvert,X
                sta (arg0),Y

                dey
                bpl _next2

                ldy arg6
                lda arg7
                bne _4

                lda arg2
                bne _5                  ; no EOL char if inverted

                iny
_next3          lda EOL
                sta (arg0),Y

                jmp _5

_4              eor (arg0),Y
                sta (arg0),Y

_5              lda arg3
                beq _XIT1

_next4          ldy #0
                lda (arg0),Y
                eor #$80
                sta (arg0),Y

_XIT1           rts

_6              lda arg3
                bne _next4

                tay
                beq _next3              ; [unc]

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
                sta y__

                lda COLCRS
                sta x__

                rts
                .endproc


;======================================
;   RstCol()
;======================================
rstcol          .proc
                lda y__
                sta ROWCRS

                lda x__
                sta COLCRS

                jsr zapcsr
_ENTRY1         jsr scrlft

                jmp scrrt

                .endproc


;======================================
;   ChkCur()
;======================================
chkcur          .proc
                lda cur+1
                bne _XIT

_ENTRY1         lda top
                sta cur
                lda top+1
                sta cur+1

_XIT            rts
                .endproc


;======================================
;   LdBuf() load buf
;======================================
ldbuf           .proc
                jsr chkcur
                bne _1

                tay
                sta (buf),Y

                rts

_1              jsr curstr

_ENTRY1         ldy #0
                lda (arg0),Y
                sta (buf),Y

                tay
_next1          lda (arg0),Y
                sta (buf),Y

                dey
                bne _next1

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
                beq _2

_next1          clc
                adc #CharResX           ; TODO: fragile
                bcc _1

                inx

_1              dey
                bne _next1

_2              sta arg0
                stx arg1

                rts
                .endproc


;======================================
;   ZapCsr() get rid of old cursor
;======================================
zapcsr          .proc
                lda #<CSRCH
                sta OLDADR
                lda #>CSRCH
                sta OLDADR+1

                rts
                .endproc


;======================================
;   RstCsr() restore char under Csr
;======================================
rstcsr          .proc
                ldy #0
                lda OLDCHR
                sta (OLDADR),Y

                rts
                .endproc
