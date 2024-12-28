
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.io.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; ioOpen(device, name, mode, opt)
;--------------------------------------
; returns status
;======================================
ioOpen          .proc
                stx arg5
                sty arg6

                ldy #$03                ; OPEN
                bne ioXioStr            ; [unc]

                .endproc


;======================================
; ioPrint(device, str)
;======================================
ioPrint         .proc
                stx arg5
                sty arg6

                ldx #$00
                stx arg3

                ldy #$09                ; PUTTEXT
                jsr ioXioStr
                bne _XIT

                lda #$0B                ; PUTCHR
                sta IOCB0+ICCOM,X

                lda #EOL
                jmp CIOV

_XIT            rts
                .endproc


;======================================
; ioClose(device)
;======================================
ioClose         .proc
                ldx #>ml
                stx arg6                ; note: address must be non-zero to
                                        ; fake out zero check in XIOstr
            .if ZAPRAM
                sta (arg5),Y
            .else
                nop
                nop
            .endif

                ldy #$0C
                bne ioInput._ENTRY1     ; [unc]

                .endproc


;======================================
; ioInput(device, str)
;======================================
ioInput         .proc
                sty arg6

                ldy #$05
_ENTRY1         stx arg5

                ldx #$00
                stx arg3

                .endproc

                ;[fall-through]


;======================================
; ioXioStr(device,,cmd,aux1,aux2,str)
;======================================
ioXioStr        .proc
                asl                     ; *16
                asl
                asl
                asl

                tax
                tya
                sta IOCB0+ICCOM,X       ; command

                lda arg3
                beq _1

                sta IOCB0+ICAX1,X

                lda arg4
                sta IOCB0+ICAX2,X

                lda #$00
_1              tay
                sta IOCB0+ICBLH,X
                lda (arg5),Y
                sta IOCB0+ICBLL,X       ; size

                beq ioPrint._XIT        ; return

                clc
                lda arg5
                adc #$01
                sta IOCB0+ICBAL,X       ; buffer address

                lda arg6
                adc #$00
                sta IOCB0+ICBAH,X

                jmp CIOV

                .endproc


;======================================
; ioOutput(device, str)
;======================================
ioOutput        .proc
                sty arg6

                ldy #$0B
                bne ioInput._ENTRY1     ; [unc]

                .endproc


;======================================
; ioDisplayStr(prompt, str, invert)
;--------------------------------------
; on entry:
;   X:A         message
;   Y
;   arg4        char used for clearing
;======================================
ioDisplayStr    .proc
                sty arg12

                ldy arg3
                sty arg13

                ldy #$00
                sty arg3

                ldy arg4                ; char used for clearing
                jsr ioPutStr

                lda arg6                ; PutStr size
                clc
                adc LMARGN
                sta COLCRS

                jsr screenCursorRight

                ldy #$00
                lda (arg12),Y
                beq _XIT

                sta arg3
                sty arg4

_next1          inc arg4
                ldy arg4
                lda (arg12),Y
                eor arg2
                jsr screenCh

                dec arg3
                bne _next1

_XIT            rts
                .endproc


;======================================
; ioReadBuffer(device)
;======================================
ioReadBuffer    .proc
;               inc COLOR4
                nop
                nop
                nop

                ldy #$00
                tax
                lda #240
                sta (buf),Y

                txa
                ldx buf
                ldy buf+1
inputs          jsr ioInput

                sty arg0

                lda IOCB0+ICBLL,X       ; size
                beq _1

                sec
                sbc #$01
_1              ldy #$00
                sta (arg5),Y

                ldy arg0

                rts
                .endproc


;======================================
; ioWriteBuffer(device)
;======================================
ioWriteBuffer   .proc
                ldx buf
                ldy buf+1
                jmp ioPrint

                .endproc


;======================================
; ioResetCursor()
;======================================
ioResetCursor   .proc
                ldy currentWindow
                lda win1Base+WCUR,Y
                sta cur
                lda win1Base+WCUR+1,Y
                sta cur+1

                jmp ioLoadBuffer

                .endproc


;======================================
; ioSystemError(,,errnum)
;======================================
ioSystemError   .proc
                jsr ioDisplayOn

                tya
                ldx #$00
                jsr ioCardToStr
                jsr ioCmdColumn

                lda #$80
                sta arg4
                lda #>numbuf
                sta arg3
                ldy #<numbuf

                lda #<msgSysErr
                ldx #>msgSysErr
                jsr ioDisplayStr
                jsr ioRestoreCursorChar
                jsr ioResetColumn

                jmp screenBell

                .endproc


;--------------------------------------
;--------------------------------------

msgSysErr       .ptext "Error: "


;======================================
; ioCardToStr(num)
;--------------------------------------
; Cardinal to string
;======================================
ioCardToStr     .proc
                sta FR0
                stx FR0+1

                jsr IFP                 ; Cardinal to real

                .endproc

                ;[fall-through]


;======================================
; ioRealToStr()
;--------------------------------------
; real in FR0
;======================================
ioRealToStr     ;.proc
                jsr FASC

                ldy #$FF
                ldx #$00
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
; ioDisplayOff()
;======================================
ioDisplayOff    .proc
                lda jt_tvdisp
                sta SDMCTL
                sta DMACTL

                rts
                .endproc


;======================================
; ioDisplayOn()
;======================================
ioDisplayOn     .proc
                lda #$22
                sta SDMCTL
                sta DMACTL

                lda bckgrnd             ; background color
                sta COLOR4              ; restore background

                rts
                .endproc


;======================================
; ioPrintCard(num)
;======================================
ioPrintCard     .proc
                jsr ioCardToStr

pnum            lda device
                ldx #<numbuf
                ldy #>numbuf

                jmp ioOutput

                .endproc


;======================================
; ioOpenChannel(mode)
;======================================
ioOpenChannel   .proc
                pha

                lda ioChnnl
                jsr ioClose

                pla
                sta arg3

;   check for default device
                lda #':'
                ldy #$02
                cmp (nxtaddr),Y
                beq _1

                iny
                cmp (nxtaddr),Y
                beq _1

;   stuff in D: for device
                clc
                lda nxtaddr
                adc #$02
                sta FR0
                lda nxtaddr+1
                adc #$00
                sta FR0+1

                ldy #$00
                lda (nxtaddr),Y         ; add 2 to length of string
                adc #$02                ;  so we can insert 'D:'
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

_1              lda ioChnnl
                ldx nxtaddr
                ldy nxtaddr+1
                jsr ioOpen
                bpl ioPrintBuffer

                jmp bankSPLErr          ; oops, error in Open

                .endproc


;======================================
; ioPrintBuffer()
;======================================
ioPrintBuffer   .proc
                lda isListing
                bne ioRealToCard._XIT   ; return
                jmp ioWriteBuffer

                .endproc


;======================================
; ioHexToCard(buf,index)
;======================================
ioHexToCard     .proc
                sty CIX
                sta arg1
                stx arg2

                lda #$00
                sta FR0
                sta FR0+1

_next1          ldy CIX
                lda (arg1),Y
                sec
                sbc #'0'
                bmi ioRealToCard._ENTRY1

                cmp #$0A
                bmi _1

                cmp #$11
                bmi ioRealToCard._ENTRY1

                sbc #$07
                cmp #$10
                bpl ioRealToCard._ENTRY1

_1              sta arg5

                lda FR0
                ldx FR0+1
                ldy #$04
                jsr mscLShift

                clc
                adc arg5
                sta FR0
                stx FR0+1

                inc CIX
                bne _next1

                .endproc


;======================================
; ioRealToCard()
;======================================
ioRealToCard    .proc
                jsr FPI
                bcs _err

_ENTRY1         lda FR0
                ldx FR0+1
                ldy CIX

_XIT            rts

_err            ldy #constERR
                jmp bankSPLErr

                .endproc


;======================================
; ioStrToReal(str, index)
;======================================
ioStrToReal     .proc
                sty CIX
                sta INBUFF
                stx INBUFF+1

                jmp AFP

                .endproc


;======================================
; ioPutSpace()
;======================================
ioPutSpace      .proc
                ldy #' '
                bne ioPutChar           ; [unc]

                .endproc


;======================================
; ioPutEOL()
;======================================
ioPutEOL        .proc
                ldy #EOL

                .endproc

                ;[fall-through]


;======================================
; ioPutChar(,,char)
;======================================
ioPutChar       .proc
                lda device
                jmp screenCh._ENTRY1

                .endproc


;======================================
; ioPutStr(str, invert, offset)
;--------------------------------------
; on entry:
;   X:A         message
;   Y           char used for clearing
;======================================
ioPutStr        .proc
_DEST_LO        = arg0
_DEST_HI        = arg1
_clearChar      = arg2
_idxSRC         = arg3
_SRC_LO         = arg4
_SRC_HI         = arg5
_message_LO     = arg6
_message_HI     = arg7
;---

                sta _message_LO
                stx _message_HI
                sty _clearChar

;   calculate the SRC address
                ;;lda _message_LO
                sec                     ; intentional???
                adc _idxSRC
                sta _SRC_LO
                bcc _1

                inx                     ; message_HI++

_1              stx _SRC_HI

                jsr ioGetDisplayAddr
                jsr ioZapCursor

                ldy #39
                lda _clearChar
_next1          sta (_DEST_LO),Y        ; clear line

                dey
                bpl _next1

;   adjust DEST address by margin
                clc                     ; handle left margin
                lda _DEST_LO
                adc LMARGN
                sta _DEST_LO
                bcc _2

                inc _DEST_HI

_2              iny                     ; sets Y to 0

;   fetch the message length
                clc
                lda (_message_LO),Y
                sbc _idxSRC
                bcc _6                  ; no chars

                sta _message_LO

                tay
                lda #$00
                sta _message_HI

;   message extended beyond right margin?
                sec
                lda RMARGN
                sbc LMARGN
                cmp _message_LO
                beq _3                  ; handle EOL char
                bcs _next2              ; length ok

                sta _message_LO

                ldy _message_LO         ; length too long
_3              lda #$80
                sta _message_HI

_next2          lda _clearChar
                eor (_SRC_LO),Y
                pha

                and #$60
                tax

                pla
                and #$9F
                ora chrConvert,X
                sta (_DEST_LO),Y

                dey
                bpl _next2

                ldy _message_LO
                lda _message_HI
                bne _4

                lda _clearChar
                bne _5                  ; no EOL char if inverted

                iny
_next3          lda jt_eolch
                sta (_DEST_LO),Y

                jmp _5

_4              eor (_DEST_LO),Y
                sta (_DEST_LO),Y

_5              lda _idxSRC
                beq _XIT1

_next4          ldy #$00
                lda (_DEST_LO),Y
                eor #$80
                sta (_DEST_LO),Y

_XIT1           rts

_6              lda arg3
                bne _next4

                tay
                beq _next3              ; [unc]

                .endproc


;======================================
; Command column???
;======================================
ioCmdColumn     .proc
                jsr ioSaveColumn
                jsr ioRestoreCursorChar

                ldy cmdln
                sty ROWCRS

                rts
                .endproc


;======================================
; Preserve column
;======================================
ioSaveColumn    .proc
                lda ROWCRS
                sta y__

                lda COLCRS
                sta x__

                rts
                .endproc


;======================================
; Reset column
;======================================
ioResetColumn   .proc
                lda y__
                sta ROWCRS

                lda x__
                sta COLCRS

                jsr ioZapCursor
_ENTRY1         jsr screenCursorLeft
                jmp screenCursorRight

                .endproc


;======================================
; Check cursor bounds
;======================================
ioChkCursor     .proc
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
ioLoadBuffer    .proc
                jsr ioChkCursor
                bne _1

                tay
                sta (buf),Y

                rts

_1              jsr mscCurStr

_ENTRY1         ldy #$00
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
ioDisplayBuffer .proc
                clc
                lda indent
                adc choff
                sta arg3

                ldy #$00                ; char used for clearing (space)
                lda buf                 ; X:A = message
                ldx buf+1
                jmp ioPutStr

                .endproc


;======================================
; Get address of the display
;======================================
ioGetDisplayAddr .proc
                lda SAVMSC
                ldx SAVMSC+1
                ldy ROWCRS
                beq _2

_next1          clc
                adc #40
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
ioZapCursor     .proc
                lda #<CSRCH
                sta OLDADR
                lda #>CSRCH
                sta OLDADR+1

                rts
                .endproc


;======================================
; Restore char under cursor
;======================================
ioRestoreCursorChar .proc
                ldy #$00
                lda OLDCHR
                sta (OLDADR),Y

                rts
                .endproc
