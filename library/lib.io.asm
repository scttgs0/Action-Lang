
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.io.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


io              .namespace

;======================================
; PROC ChkErr=*(BYTE result, block, errCode)
;--------------------------------------
; checks for error return from CIO
; Sets EOF(block) to true on error
; does not call Error if EOF error ($88)
; see Hardware manual for CIO details
;======================================
ChkErr          .proc
                bpl _2

                cpy #$88                ; EOF
                beq _1

                tya
                cpy #$80                ; break key
                beq Break1

                jmp jt_vecError

_1              txa
                lsr
                lsr
                lsr
                lsr

                tax
                tya
                sta eof,X
_2
            .if ZAPRAM
                dec ChkErr-$10,X
            .else
                nop
                nop
                nop
            .endif

                rts
                .endproc


;======================================
; Break1(error)
;======================================
Break1          .proc
                ldx #$01
                stx BRKKEY

                pha

                jsr lib.msc.Break

                pla
                tay

_XIT            rts
                .endproc


;======================================
; PROC PrintF(STRING f, CARD a1, a2, a3, a4, a5)
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
PrintF          .proc
                sta addr
                stx addr+1
                sty temps

                ldy #$00
                lda (addr),Y
                sta token

                inc token

                ldx #$0D
_next1          lda args+2,X
                sta temps,X

                dex
                bne _next1

                stx zpAllocPrevToken
                stx zpAllocOP

_next2          inc zpAllocOP
                ldy zpAllocOP
                cpy token
                bcs Break1._XIT

                lda (addr),Y
                cmp #'%'
                bne _next3

                inc zpAllocOP

                iny
                lda (addr),Y
                cmp #'%'
                beq _next3

                cmp #'E'
                bne _1

                lda #EOL
_next3          jsr Put
                jmp _next2

_1              ldy zpAllocPrevToken
                inc zpAllocPrevToken
                inc zpAllocPrevToken
                sta args

                lda temps,Y
                ldx temps+1,Y
                ldy args
                cpy #'C'
                beq _next3

                cpy #'S'
                bne _2

                jsr Print
                jmp _next2

_2              cpy #'I'
                bne _3

                jsr PrintI
                jmp _next2

_3              cpy #'H'
                bne _4

                jsr bankPrintH

                jmp _next2

_4              jsr PrintC

                jmp _next2

                .endproc


;======================================
; PROC Open(BYTE dev, STRING fileSpec, BYTE mode, aux2)
;--------------------------------------
; opens fileSpec and assigns it to IOCB dev
;======================================
Open            .proc
                pha

                stx arg1
                sty arg2

                tay
                lda #$00
                sta eof,Y

                tay
                lda (arg1),Y
                sta (buf),Y

                tay
                iny

                lda #EOL
                bra _1

_next1          lda (arg1),Y
_1              sta (buf),Y

                dey
                bne _next1

                pla
                ldx buf
                ldy buf+1

                jsr mainio.Open
                jmp ChkErr

                .endproc


;======================================
; PROC PrintE(STRING str)
;--------------------------------------
; outputs str to default IOCB with EOL
;======================================
PrintE          .proc
                stx arg1

                tax
                ldy arg1
                lda device

                .endproc

                ;[fall-through]


;======================================
; PROC PrintDE(BYTE dev, STRING str)
;--------------------------------------
; outputs str to IOCB dev appended with an EOL
;======================================
PrintDE         .proc
                jsr mainio.Print
                jmp ChkErr

                .endproc


;======================================
; PROC Close(BYTE dev)
;--------------------------------------
; closes IOCB dev
;======================================
Close           .proc
                jsr mainio.Close
                jmp ChkErr

                .endproc


;======================================
; PROC Print(STRING str)
;--------------------------------------
; outputs str to default IOCB
;======================================
Print           .proc
                stx arg1

                tax
                ldy arg1
                lda device

                .endproc

                ;[fall-through]


;======================================
; PROC PrintD(BYTE dev, STRING str)
;--------------------------------------
; outputs str to IOCB dev
;======================================
PrintD          .proc
                jsr mainio.Output
                jmp ChkErr

                .endproc


;======================================
; PROC InputS(STRING str)
;--------------------------------------
; same as InputSD, but uses default IOCB
;======================================
InputS          .proc
                stx arg2

                tax
                ldy arg2
                lda device

                .endproc

                ;[fall-through]


;======================================
; PROC InputSD(BYTE dev, STRING str)
;--------------------------------------
; see Input, size set to 255
;======================================
InputSD         .proc
                pha

                lda #255
                sta arg3

                pla

                .endproc

                ;[fall-through]


;======================================
; PROC InputMD(BYTE dev, STRING str, BYTE max)
;--------------------------------------
; see Input, size set to max
;======================================
InputMD         .proc
                pha

                stx arg1
                sty arg2

                ldy #$00
                lda arg3
                sta (arg1),Y

                pla
                ldy arg2

                .endproc

                ;[fall-through]


;======================================
; PROC InputD(BYTE dev, STRING str)
;--------------------------------------
; inputs str from IOCB dev
; first byte must be set to maximum size
; on return, first byte set to size of
; string input
;======================================
InputD          .proc
                jsr mainio.ReadBuffer.inputs
                jmp ChkErr

                .endproc


;======================================
; BYTE FUNC GetD(BYTE dev)
;--------------------------------------
; inputs character from IOCB dev
;======================================
GetD            .proc
                ldx #$07                ; GET

_ENTRY1         stx arg4

                asl
                asl
                asl
                asl
                tax

                lda arg4
                sta IOCB0+ICCOM,X

                lda #$00
                sta IOCB0+ICBLL,X
                sta IOCB0+ICBLH,X

                tya
                jsr CIOV
                sta args

                jmp ChkErr

                .endproc


;======================================
; PROC PutE()
;--------------------------------------
; output EOL do default IOCB
;======================================
PutE            .proc
                lda #EOL

                .endproc

                ;[fall-through]


;======================================
; PROC Put(CHAR ch)
;--------------------------------------
; outputs ch to default IOCB
;======================================
Put             .proc
                tax
                lda device

                .endproc

                ;[fall-through]


;======================================
; PROC PutD(BYTE dev, CHAR ch)
;--------------------------------------
; outputs ch to IOCB dev
;======================================
PutD            .proc
                stx arg1

                ldy arg1
_ENTRY1         ldx #$0B                ; PUTCHR
                jmp GetD._ENTRY1

                .endproc


;======================================
; PROC PutDE(BYTE dev)
;--------------------------------------
; outputs EOL to IOCD dev
;======================================
PutDE           .proc
                ldy #EOL
                bra PutD._ENTRY1

                .endproc


;======================================
; PROC XIOstr(BYTE dev, fill, cmd, aux1, aux2, STRING str)
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
XIO             .proc
                jsr mainio.XioStr
                jmp ChkErr

                .endproc


;======================================
; PROC PrintB(BYTE num)
;--------------------------------------
; outputs byte num to default IOCB
;======================================
PrintB          .proc
                ldx #$00

                .endproc

                ;[fall-through]


;======================================
; PROC PrintC(CARD num)
;--------------------------------------
; outputs cardinal num to default IOCB
;======================================
PrintC          .proc
                jsr mainio.PrintCard
                jmp ChkErr

                .endproc


;======================================
; PROC PrintBE(BYTE num)
;--------------------------------------
; same as PrintB except EOL appended
;======================================
PrintBE         .proc
                ldx #$00

                .endproc

                ;[fall-through]


;======================================
; PROC PrintCE(CARD num)
;--------------------------------------
; same as PrintC except EOL appended
;======================================
PrintCE         .proc
                jsr PrintC
                jmp PutE

                .endproc


;======================================
; PROC PrintBD(BYTE dev, BYTE num)
;--------------------------------------
; output byte num to IOCB dev
;======================================
PrintBD         .proc
                ldy #$00

                .endproc

                ;[fall-through]


;======================================
; PROC PrintCD(BYTE dev, CARD num)
;--------------------------------------
; output cardinal num to IOCB dev
;======================================
PrintCD         .proc
                sta arg0

                txa
                sty arg2
                ldx arg2

                jsr mainio.CardToStr

                lda arg0
                jsr mainio.PrintCard.pnum+2
                jmp ChkErr

                .endproc


;======================================
; PROC PrintBDE(BYTE dev, BYTE num)
;--------------------------------------
; output num to IOCB dev with EOL
;======================================
PrintBDE        .proc
                ldy #$00

                .endproc

                ;[fall-through]


;======================================
; PROC PrintCDE(BYTE dev, CARD num)
;--------------------------------------
; output num to IOCB dev with EOL
;======================================
PrintCDE        .proc
                jsr PrintCD

                lda arg0
                jmp PutDE

                .endproc


;======================================
; PROC PrintI(INT num)
;--------------------------------------
; outputs integer num to default IOCB
;======================================
PrintI          .proc
                stx arg2

                tax
                ldy arg2
                lda device

                .endproc

                ;[fall-through]


;======================================
; PROC PrintID(BYTE dev, INT num)
;--------------------------------------
; outputs integer num to IOCB dev
;======================================
PrintID         .proc
                cpy #$00
                bpl PrintCD

                pha

                stx arg1
                sty arg2

                ldy #'-'
                jsr PutD._ENTRY1

                sec
                lda #$00
                sbc arg1

                tax
                lda #$00
                sbc arg2

                tay
                pla

                jmp PrintCD

                .endproc


;======================================
; PROC PrintIE(INT num)
;--------------------------------------
; same as PrintI with EOL
;======================================
PrintIE         .proc
                jsr PrintI
                jmp PutE

                .endproc


;======================================
; PROC PrintIDE(BYTE dev, INT num)
;--------------------------------------
; same as PrintID with EOL
;======================================
PrintIDE        .proc
                jsr PrintID

                lda arg0
                jmp PutDE

                .endproc


;======================================
; PROC StrB(BYTE n, STRING s)
;--------------------------------------
; convert number to string
;======================================
StrB            .proc
                stx arg2
                sty arg3

                ldx #$00
                ldy arg2

                .endproc

                ;[fall-through]


;======================================
; PROC StrC(CARD n, STRING s)
;--------------------------------------
; convert number to string
;======================================
StrC            .proc
                sty arg2

                jsr mainio.CardToStr

                iny
_next1          lda numbuf,Y
                sta (arg2),Y

                dey
                bpl _next1

                rts
                .endproc


;======================================
; PROC StrI(INT n, STRING s)
;--------------------------------------
; convert number to string
;======================================
StrI            .proc
                cpx #$00
                bpl StrC

                sta arg0
                stx arg1
                sty arg2

                sec
                lda #$00
                sbc arg0

                tay
                lda #$00
                sbc arg1

                tax
                tya

                jsr mainio.CardToStr

                inx
                txa
                tay

_next1          lda numbuf-1,Y
                sta (arg2),Y

                dey
                bne _next1

                txa
                sta (arg2),Y

                iny
                lda #'-'
                sta (arg2),Y

                rts
                .endproc


;======================================
; BYTE FUNC InputB()
; CARD FUNC InputC()
; INT FUNC InputI()
;--------------------------------------
; input number from default IOCB
; number must be terminated with EOL
;======================================
InputB
InputC
InputI          lda device

                ;[fall-through]


;======================================
; BYTE FUNC InputBD()
; CARD FUNC InputCD()
; INT FUNC InputID(BYTE dev)
;--------------------------------------
; same as InputI, but from IOCB dev
;======================================
InputBD
InputCD
InputID         ldx #$13
                stx numbuf

                ldx #<numbuf
                ldy #>numbuf
                jsr InputD

                lda #<numbuf
                ldx #>numbuf

                ;[fall-through]


;======================================
; BYTE FUNC ValB(STRING s)
; INT FUNC ValI(STRING s)
; CARD FUNC ValC(STRING s)
;--------------------------------------
; returns numeric value of s
;======================================
ValB
ValI
ValC            sta arg4
                stx arg5

                ldy #$00
                sty arg0
                sty arg1
                sty arg2

                lda (arg4),Y
                sta arg3

                inc arg3

                lda #$20
                iny
_next1          cmp (arg4),Y
                bne _1

                iny
                cpy arg3
                bmi _next1

_1              lda (arg4),Y
                cmp #'-'
                bne _2

                sta arg2
                iny

_2              cpy arg3
                bpl _4

_next2          lda (arg4),Y
                cmp #'0'
                bmi _4

                cmp #':'                ; '9'+1
                bpl _4

                sec
                sbc #'0'

;   arg01*10
                tax
                lda arg1
                pha

                lda arg0
                asl
                rol arg1
                asl
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
                bcc _3

                inc arg1

_3              iny
                cpy arg3
                bmi _next2

_4              lda arg2
                beq _XIT

                sec
                lda #$00
                sbc arg0
                sta arg0

                lda #$00
                sbc arg1
                sta arg1

_XIT            rts


;======================================
; PROC Note(BYTE dev, CARD POINTER sector, BYTE POINTER offset)
;--------------------------------------
; returns disk sector and offset in that
; sector of next byte to be read or
; written to IOCB dev
; example:  Note(1, @sect, @pos)
; see Hardware manual
;======================================
Note            .proc
                stx arg1
                sty arg2

                asl
                asl
                asl
                asl

                tax
                lda #$26                ; NOTE
                sta IOCB0+ICCOM,X

                jsr CIOV
                jsr ChkErr

                ldy #$00
                lda IOCB0+ICAX5,X       ; offset
                sta (arg3),Y

                lda IOCB0+ICAX3,X       ; low byte of sector
                sta (arg1),Y
                lda IOCB0+ICAX4,X       ; high byte of sector
                iny
                sta (arg1),Y

                rts
                .endproc


;======================================
; PROC Point(BYTE dev, CARD sector, BYTE offset)
;--------------------------------------
; Sets next byte to be read or written
; to be byte offset of sector.    File
; must be open for update (mode=12)
; see Hardware manual
;======================================
Point           .proc
                stx arg1

                asl
                asl
                asl
                asl

                tax
                tya                     ; sector+1
                sta IOCB0+ICAX4,X
                lda arg1                ; sector
                sta IOCB0+ICAX3,X

                lda arg3                ; offset
                sta IOCB0+ICAX5,X

                lda #$25                ; POINT
                sta IOCB0+ICCOM,X

                jsr CIOV
                jmp ChkErr

                .endproc

                .endnamespace
