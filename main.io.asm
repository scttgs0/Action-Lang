
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.io.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; Open(device, name, mode, opt)
;--------------------------------------
; returns status
;======================================
Open            .proc
                stx arg5
                sty arg6

                ldy #3
                bne XioStr              ; [unc]

                .endproc


;======================================
; Print(device, str)
;======================================
Print           .proc
                stx arg5
                sty arg6

                ldx #0
                stx arg3

                ldy #$09
                jsr XioStr
                bne _XIT

                lda #$0B
                ;!!sta IOCB0+ICCOM,X

                lda #EOL
                ;!!jmp CIOV

_XIT            rts
                .endproc


;======================================
; Close(device)
;======================================
Close           .proc
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
                bne Input._ENTRY1       ; [unc]

                .endproc


;======================================
; Input(device, str)
;======================================
Input           .proc
                sty arg6

                ldy #$05
_ENTRY1         stx arg5

                ldx #0
                stx arg3

                .endproc

                ;[fall-through]


;======================================
; XioStr(device,,cmd,aux1,aux2,str)
;======================================
XioStr          .proc
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

                beq Print._XIT          ; return

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
; Output(device, str)
;======================================
Output          .proc
                sty arg6

                ldy #$0B
                bne Input._ENTRY1       ; [unc]

                .endproc


;======================================
; DisplayStr(prompt, str, invert)
;======================================
DisplayStr      .proc
                sty arg12

                ldy arg3
                sty arg13

                ldy #0
                sty arg3

                ldy arg4
                jsr PutStr

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
; ReadBuffer(device)
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
inputs          jsr Input

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
; WriteBuffer(device)
;======================================
WriteBuffer     .proc
                ldx buf
                ldy buf+1
                jmp Print

                .endproc


;======================================
; ResetCursor()
;======================================
ResetCursor     .proc
                ldy currentWindow
                lda w1+WCUR,Y
                sta cur
                lda w1+WCUR+1,Y
                sta cur+1

                jmp LoadBuffer

                .endproc


;======================================
; SystemError(,,errnum)
;======================================
SystemError     .proc
                jsr DisplayOn

                tya
                ldx #0
                jsr CardToStr
                jsr CmdColumn

                lda #$80
                sta arg4
                lda #>numbuf
                sta arg3
                ldy #<numbuf

                lda #<msgSysErr
                ldx #>msgSysErr
                jsr DisplayStr
                jsr RestoreCursorChar
                jsr ResetColumn

                jmp scrbell

                .endproc


;--------------------------------------
;--------------------------------------

msgSysErr       .ptext "Error: "


;======================================
; CardToStr(num) - Cardinal to string
;======================================
CardToStr       .proc
                sta FR0
                stx FR0+1

                ;!!jsr IFP                 ; Cardinal to real

                .endproc

                ;[fall-through]


;======================================
; RealToStr() - real in FR0
;======================================
RealToStr       ;.proc
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
; DisplayOff()
;======================================
DisplayOff      .proc
                lda jt_tvdisp
                ;!!sta SDMCTL
                ;!!sta DMACTL

                rts
                .endproc


;======================================
; DisplayOn()
;======================================
DisplayOn       .proc
                lda #$22
                ;!!sta SDMCTL
                ;!!sta DMACTL

                lda bckgrnd             ; background color
                ;!!sta COLOR4              ; restore background

                rts
                .endproc


;======================================
; PrintCard(num)
;======================================
PrintCard       .proc
                jsr CardToStr

pnum            lda device
                ldx #<numbuf
                ldy #>numbuf

                jmp Output

                .endproc


;======================================
; OpenChannel(mode)
;======================================
OpenChannel     .proc
                pha

                lda Channel
                jsr Close

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
                bpl PrintBuffer

                jmp splerr              ; oops, error in Open

                .endproc


;======================================
; PrintBuffer()
;======================================
PrintBuffer     .proc
                lda list
                bne RealToCard._XIT         ; return

                jmp WriteBuffer

                .endproc


;======================================
; HexToCard(buf,index)
;======================================
HexToCard       .proc
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
                bmi RealToCard._ENTRY1

                cmp #10
                bmi _1

                cmp #17
                bmi RealToCard._ENTRY1

                sbc #7
                cmp #16
                bpl RealToCard._ENTRY1

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
; RealToCard()
;======================================
RealToCard      .proc
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
; StrToReal(str, index)
;======================================
StrToReal       .proc
                sty CIX
                sta INBUFF
                stx INBUFF+1

                ;!!jmp AFP

                .endproc


;======================================
; PutSpace()
;======================================
PutSpace        .proc
                ldy #$20
                bne PutChar             ; [unc]

                .endproc


;======================================
; PutEOL()
;======================================
PutEOL          .proc
                ldy #EOL

                .endproc

                ;[fall-through]


;======================================
; PutChar(,,char)
;======================================
PutChar         .proc
                lda device
                jmp scrch._ENTRY1

                .endproc


;======================================
; PutStr(str, invert, offset)
;======================================
PutStr          .proc
                sta arg6
                stx arg7
                sty arg2

                sec
                adc arg3
                sta arg4
                bcc _1

                inx

_1              stx arg5

                jsr GetDisplayAddr
                jsr ZapCursor

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
; Command column???
;======================================
CmdColumn       .proc
                jsr SaveColumn
                jsr RestoreCursorChar

                ldy cmdln
                sty ROWCRS

                rts
                .endproc


;======================================
; Preserve column
;======================================
SaveColumn      .proc
                lda ROWCRS
                sta y__

                lda COLCRS
                sta x__

                rts
                .endproc


;======================================
; Reset column
;======================================
ResetColumn     .proc
                lda y__
                sta ROWCRS

                lda x__
                sta COLCRS

                jsr ZapCursor
_ENTRY1         jsr scrlft

                jmp scrrt

                .endproc


;======================================
; Check cursor bounds
;======================================
ChkCursor       .proc
                lda cur+1
                bne _XIT

_ENTRY1         lda top
                sta cur
                lda top+1
                sta cur+1

_XIT            rts
                .endproc


;======================================
; Load buffer
;======================================
LoadBuffer      .proc
                jsr ChkCursor
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
;   Display content from buffer
;======================================
DisplayBuffer   .proc
                clc
                lda indent
                adc choff
                sta arg3

                ldy #0
                lda buf
                ldx buf+1

                jmp PutStr

                .endproc


;======================================
; Get address of the display
;======================================
GetDisplayAddr  .proc
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
; Get rid of the old cursor
;======================================
ZapCursor       .proc
                lda #<CSRCH
                sta OLDADR
                lda #>CSRCH
                sta OLDADR+1

                rts
                .endproc


;======================================
; Restore char under cursor
;======================================
RestoreCursorChar .proc
                ldy #0
                lda OLDCHR
                sta (OLDADR),Y

                rts
                .endproc
