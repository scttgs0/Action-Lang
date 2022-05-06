;======================================
;   FILE: lib.io.asm
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
;PROC ChkErr=*(BYTE result, block, errCode)
; checks for error return from CIO
; Sets EOF(block) to true on error
; does not call Error if EOF error ($88)
; see Hardware manual for CIO details
;======================================
libIOChkErr     .proc
                bpl _ce2

                cpy #$88                ; EOF
                beq _ce1

                tya
                cpy #$80                ; break key
                beq libIOBreak1

                jmp error

_ce1            txa
                lsr a
                lsr a
                lsr a
                lsr a
                tax
                tya
                sta eof,x
_ce2
        .if ramzap
                dec libIOChkErr-$10,x
        .else
                nop
                nop
                nop
        .endif
                rts
                .endproc


;======================================
;   Break1(error)
;======================================
libIOBreak1     .proc
                ldx #1
                stx BRKKEY
                pha
                jsr libMscBreak

                pla
                tay
pfe             rts
                .endproc


;======================================
;PROC PrintF(STRING f, CARD a1, a2, a3, a4, a5)
;--------------------------------------
; outputs a1-a5 to default device
; using format f.  Any non '%' char
; in f is output directly.  '%' char
; is interpreted as follows:
;    %S - output next arg as string
;    %I - output next arg as signed number
;    %U - output next arg as unsigned number
;    %C - output next arg as CHAR
;    %H - output next arg as HEX
;    %% - output '%' char
;    %E - output EOL
; any other char after % is treated
; the same as %U.
;======================================
libIOPrintF     .proc
                sta addr
                stx addr+1
                sty temps
                ldy #0
                lda (addr),y
                sta token
                inc token
                ldx #13
_pf1            lda args+2,x
                sta temps,x
                dex
                bne _pf1

                stx lsttoken
                stx op
_pf2            inc op
                ldy op
                cpy token
                bcs libIOBreak1.pfe

                lda (addr),y
                cmp #'%'
                bne _pf3

                inc op
                iny
                lda (addr),y
                cmp #'%'
                beq _pf3

                cmp #'E'
                bne _pf4

                lda #eol
_pf3            jsr libIOPut

                jmp _pf2
;
_pf4            ldy lsttoken
                inc lsttoken
                inc lsttoken
                sta args
                lda temps,y
                ldx temps+1,y
                ldy args
                cpy #'C'
                beq _pf3

                cpy #'S'
                bne _pf5

                jsr libIOPrint
                jmp _pf2

_pf5            cpy #'I'
                bne _pf6

                jsr libIOPrintI
                jmp _pf2

_pf6            cpy #'H'
                bne _pf7

                jsr prth
                jmp _pf2

_pf7            jsr libIOPrintC
                jmp _pf2
                .endproc


;======================================
;PROC Open(BYTE dev, STRING fileSpec, BYTE mode, aux2)
; opens fileSpec and assigns it to IOCB dev
;======================================
libIOOpen       .proc
                pha
                stx arg1
                sty arg2
                tay
                lda #0
                sta eof,y
                tay
                lda (arg1),y
                sta (buf),y
                tay
                iny
                lda #eol
                bra _op2

_op1            lda (arg1),y
_op2            sta (buf),y
                dey
                bne _op1

                pla
                ldx buf
                ldy buf+1
                jsr Open

                jmp libIOChkErr

                .endproc


;======================================
;PROC PrintE(STRING str)
; outputs str to default IOCB with EOL
;======================================
libIOPrintE     .proc
                stx arg1
                tax
                ldy arg1
                lda device

    ; falls into PrintDE
                .endproc


;======================================
;PROC PrintDE(BYTE dev, STRING str)
; outputs str to IOCB dev appended with an EOL
;======================================
libIOPrintDE    .proc
                jsr print

                jmp libIOChkErr

                .endproc


;======================================
;PROC Close(BYTE dev)
; closes IOCB dev
;======================================
libIOClose      .proc
                jsr close

                jmp libIOChkErr

                .endproc


;======================================
;PROC Print(STRING str)
; outputs str to default IOCB
;======================================
libIOPrint      .proc
                stx arg1
                tax
                ldy arg1
                lda device
                .endproc

                ;[fall-through]


;======================================
;PROC PrintD(BYTE dev, STRING str)
; outputs str to IOCB dev
;======================================
libIOPrintD     .proc
                jsr output

                jmp libIOChkErr

                .endproc


;======================================
;PROC InputS(STRING str)
; same as InputSD, but uses default IOCB
;======================================
libIOInputS     .proc
                stx arg2
                tax
                ldy arg2
                lda device
                .endproc

                ;[fall-through]


;======================================
;PROC InputSD(BYTE dev, STRING str)
; see Input, size set to 255
;======================================
libIOInputSD    .proc
                pha
                lda #255
                sta arg3
                pla
                .endproc

                ;[fall-through]


;======================================
;PROC InputMD(BYTE dev, STRING str, BYTE max)
; see Input, size set to max
;======================================
libIOInputMD    .proc
                pha
                stx arg1
                sty arg2
                ldy #0
                lda arg3
                sta (arg1),y
                pla
                ldy arg2
                .endproc

                ;[fall-through]


;======================================
;PROC InputD(BYTE dev, STRING str)
; inputs str from IOCB dev
; first byte must be set to maximum size
; on return, first byte set to size of string input
;======================================
libIOInputD     .proc
                jsr ReadBuffer.inputs

                jmp libIOChkErr

                .endproc


;======================================
;BYTE FUNC GetD(BYTE dev)
; inputs character from IOCB dev
;======================================
libIOGetD       .proc
                ldx #$07
ccio            stx arg4
                asl a
                asl a
                asl a
                asl a
                tax
                lda arg4
                sta IOCB0+ICCOM,x
                lda #0
                sta IOCB0+ICBLL,x
                sta IOCB0+ICBLH,x
                tya
                jsr CIOV

                sta args
                jmp libIOChkErr

                .endproc


;======================================
;PROC PutE()
; output EOL do default IOCB
;======================================
libIOPutE       .proc
                lda #eol
                .endproc

                ;[fall-through]


;======================================
;PROC Put(CHAR ch)
; outputs ch to default IOCB
;======================================
libIOPut        .proc
                tax
                lda device
                .endproc

                ;[fall-through]


;======================================
;PROC PutD(BYTE dev, CHAR ch)
; outputs ch to IOCB dev
;======================================
libIOPutD       .proc
                stx arg1
                ldy arg1
putd1           ldx #$0b
                jmp libIOGetD.ccio

                .endproc


;======================================
;PROC PutDE(BYTE dev)
; outputs EOL to IOCD dev
;======================================
libIOPutDE      .proc
                ldy #eol
                bra libIOPutD.putd1

                .endproc


;======================================
;PROC XIOstr(BYTE dev, fill, cmd, aux1, aux2, STRING str)
;--------------------------------------
; see Hardware manual for CIO details
; performs system CIO call where:
;   ICCOM = cmd
;   ICBL = str(0)
;   ICBA = str+1
;   ICAX1 = aux1
;   ICAX2 = aux2
; CIO is not called if str(0)=0
; ICAX1 and ICAX2 are not set if aux1=0
;======================================
libIOXIO        .proc
                jsr xiostr

                jmp libIOChkErr

                .endproc


;======================================
;PROC PrintB(BYTE num)
; outputs byte num to default IOCB
;======================================
libIOPrintB     .proc
                ldx #0
                .endproc


;======================================
;PROC PrintC(CARD num)
; outputs cardinal num to default IOCB
;======================================
libIOPrintC     .proc
                jsr printc

                jmp libIOChkErr

                .endproc


;======================================
;PROC PrintBE(BYTE num)
; same as PrintB except EOL appended
;======================================
libIOPrintBE    .proc
                ldx #0
                .endproc


;======================================
;PROC PrintCE(CARD num)
; same as PrintC except EOL appended
;======================================
libIOPrintCE    .proc
                jsr libIOPrintC

                jmp libIOPutE

                .endproc


;======================================
;PROC PrintBD(BYTE dev, BYTE num)
; output byte num to IOCB dev
;======================================
libIOPrintBD    .proc
                ldy #0
                .endproc


;======================================
;PROC PrintCD(BYTE dev, CARD num)
; output cardinal num to IOCB dev
;======================================
libIOPrintCD    .proc
                sta arg0
                txa
                sty arg2
                ldx arg2
                jsr ctostr

                lda arg0
                jsr printc.pnum+2

                jmp libIOChkErr

                .endproc


;======================================
;PROC PrintBDE(BYTE dev, BYTE num)
; output num to IOCB dev with EOL
;======================================
libIOPrintBDE   .proc
                ldy #0
                .endproc


;======================================
;PROC PrintCDE(BYTE dev, CARD num)
; output num to IOCB dev with EOL
;======================================
libIOPrintCDE   .proc
                jsr libIOPrintCD

                lda arg0
                jmp libIOPutDE

                .endproc


;======================================
;PROC PrintI(INT num)
; outputs integer num to default IOCB
;======================================
libIOPrintI     .proc
                stx arg2
                tax
                ldy arg2
                lda device
                .endproc

                ;[fall-through]


;======================================
;PROC PrintID(BYTE dev, INT num)
; outputs integer num to IOCB dev
;======================================
libIOPrintID    .proc
                cpy #0
                bpl libIOPrintCD

                pha
                stx arg1
                sty arg2
                ldy #'-'
                jsr libIOPutD.putd1

                sec
                lda #0
                sbc arg1
                tax
                lda #0
                sbc arg2
                tay
                pla
                jmp libIOPrintCD

                .endproc


;======================================
;PROC PrintIE(INT num)
; same as PrintI with EOL
;======================================
libIOPrintIE    .proc
                jsr libIOPrintI

                jmp libIOPutE

                .endproc


;======================================
;PROC PrintIDE(BYTE dev, INT num)
; same as PrintID with EOL
;======================================
libIOPrintIDE   .proc
                jsr libIOPrintID

                lda arg0
                jmp libIOPutDE

                .endproc


;======================================
;PROC StrB(BYTE n, STRING s)
; convert number to string
;======================================
libIOStrB       .proc
                stx arg2
                sty arg3
                ldx #0
                ldy arg2
                .endproc

                ;[fall-through]


;======================================
;PROC StrC(CARD n, STRING s)
; convert number to string
;======================================
libIOStrC       .proc
                sty arg2
                jsr ctostr

                iny
_strc1          lda numbuf,y
                sta (arg2),y
                dey
                bpl _strc1

                rts
                .endproc


;======================================
;PROC StrI(INT n, STRING s)
; convert number to string
;======================================
libIOStrI       .proc
                cpx #0
                bpl libIOStrC

                sta arg0
                stx arg1
                sty arg2
                sec
                lda #0
                sbc arg0
                tay
                lda #0
                sbc arg1
                tax
                tya
                jsr ctostr

                inx
                txa
                tay
_s1             lda numbuf-1,y
                sta (arg2),y
                dey
                bne _s1

                txa
                sta (arg2),y
                iny
                lda #'-'
                sta (arg2),y
                rts
                .endproc


;======================================
;BYTE FUNC InputB()
;CARD FUNC InputC()
;INT FUNC InputI()
; input number from default IOCB
; number must be terminated with EOL
;======================================
libIOInputB
libIOInputC
libIOInputI
                lda    device

                ;[fall-through]


;======================================
;BYTE FUNC InputBD()
;CARD FUNC InputCD()
;INT FUNC InputID(BYTE dev)
; same as InputI, but from IOCB dev
;======================================
libIOInputBD
libIOInputCD
libIOInputID
                ldx #19
                stx numbuf
                ldx #<numbuf
                ldy #>numbuf
                jsr libIOInputD

                lda #<numbuf
                ldx #>numbuf

                ;[fall-through]


;======================================
; BYTE FUNC ValB(STRING s)
; INT FUNC ValI(STRING s)
; CARD FUNC ValC(STRING s)
; returns numeric value of s
;======================================
libIOValB
libIOValI
libIOValC
                sta arg4
                stx arg5
                ldy #0
                sty arg0
                sty arg1
                sty arg2
                lda (arg4),y
                sta arg3
                inc arg3
                lda #32
                iny
_i1             cmp (arg4),y
                bne _i2

                iny
                cpy arg3
                bmi _i1

_i2             lda (arg4),y
                cmp #'-'
                bne _i3

                sta arg2
                iny
_i3             cpy arg3
                bpl _i6

_i4             lda (arg4),y
                cmp #'0'
                bmi _i6

                cmp #':'                ; '9'+1
                bpl _i6

                sec
                sbc #'0'
                tax
    ; arg01*10
                lda arg1
                pha
                lda arg0
                asl a
                rol arg1
                asl a
                rol arg1
                clc
                adc arg0
                sta arg0
                pla
                adc arg1
                sta arg1
                asl arg0
                rol arg1
                clc
                txa
                adc arg0                ; add in digit
                sta arg0
                bcc _i5

                inc arg1
_i5             iny
                cpy arg3
                bmi _i4

_i6             lda arg2
                beq _i7

                sec
                lda #0
                sbc arg0
                sta arg0
                lda #0
                sbc arg1
                sta arg1
_i7             rts


;======================================
;PROC Note(BYTE dev, CARD POINTER sector, BYTE POINTER offset)
; returns disk sector and offset in that
; sector of next byte to be read or
; written to IOCB dev
; example:  Note(1, @sect, @pos)
; see Hardware manual
;======================================
libIONote       .proc
                stx arg1
                sty arg2
                asl a
                asl a
                asl a
                asl a
                tax
                lda #$26                ; NOTE
                sta IOCB0+ICCOM,x
                jsr CIOV
                jsr libIOChkErr

                ldy #0
                lda IOCB0+ICAX5,x       ; offset
                sta (arg3),y
                lda IOCB0+ICAX3,x       ; low byte of sector
                sta (arg1),y
                lda IOCB0+ICAX4,x       ; high byte of sector
                iny
                sta (arg1),y
                rts
                .endproc


;======================================
;PROC Point(BYTE dev, CARD sector, BYTE offset)
; Sets next byte to be read or written
; to be byte offset of sector.    File
; must be open for update (mode=12)
; see Hardware manual
;======================================
libIOPoint      .proc
                stx arg1
                asl a
                asl a
                asl a
                asl a
                tax
                tya                     ; sector+1
                sta IOCB0+ICAX4,x
                lda arg1                ; sector
                sta IOCB0+ICAX3,x
                lda arg3                ; offset
                sta IOCB0+ICAX5,x
                lda #$25                ; POINT
                sta IOCB0+ICCOM,x
                jsr CIOV

                jmp libIOChkErr

                .endproc
