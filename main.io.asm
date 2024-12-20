
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

                ldy #$03
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

                ldy #$09
                jsr ioXioStr
                bne _XIT

                lda #$0B
                sta IOCB0+ICCOM,X

                lda #EOL
                jmp CIOV

_XIT            rts
                .endproc


;======================================
; ioClose(device)
;======================================
ioClose         .proc
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
                bne ioInput._ENTRY1       ; [unc]

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
;======================================
ioDisplayStr    .proc
                sty arg12

                ldy arg3
                sty arg13

                ldy #$00
                sty arg3

                ldy arg4
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
                lda w1+WCUR,Y
                sta cur
                lda w1+WCUR+1,Y
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

                ;!!jsr IFP              ; Cardinal to real

                .endproc

                ;[fall-through]


;======================================
; ioRealToStr()
;--------------------------------------
; real in FR0
;======================================
ioRealToStr       ;.proc
                ;!!jsr FASC

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
                ;!!sta SDMCTL
                ;!!sta DMACTL

                rts
                .endproc


;======================================
; ioDisplayOn()
;======================================
ioDisplayOn     .proc
                lda #$22
                ;!!sta SDMCTL
                ;!!sta DMACTL

                lda bckgrnd             ; background color
                ;!!sta COLOR4           ; restore background

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

                lda Channel
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

_1              lda Channel
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
                lda list
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
                ;!!jsr FPI
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

                ;!!jmp AFP
                rts     ;!! HACK:
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
;======================================
ioPutStr        .proc
                sta arg6
                stx arg7
                sty arg2

                sec
                adc arg3
                sta arg4
                bcc _1

                inx

_1              stx arg5

                jsr ioGetDisplayAddr
                jsr ioZapCursor

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
                lda #$00
                sta arg7

                sec
                lda RMARGN
                sbc LMARGN
                cmp arg6
                beq _3                  ; handle EOL char
                bcs _next2              ; length ok

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

_next4          ldy #$00
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

                ldy #$00
                lda buf
                ldx buf+1

                jmp ioPutStr

                .endproc


;======================================
; Get address of the display
;======================================
ioGetDisplayAddr .proc
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
