
; SPDX-FileName: lib.msc.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;misc            .proc


;======================================
;BYTE FUNC Rand(BYTE range)
; returns random number between 0 and
; range-1.  If range=0, then a random
; number between 0 and 255 is returned
;======================================
libMscRand      .proc
                .frsRandomByteX
                cmp #0
                beq _rand1

                stx afcur
                ldx #0
                stx afcur+1
                jsr MultI

_rand1          stx args
                rts
                .endproc


;======================================
;PROC Sound(BYTE v, p, d, vol)
; set voice to specified pitch, distortion,
; and volume.  Assumes volume low  16.
;======================================
libMscSound     .proc
                asl
                sty arg2
                tay
                cmp #7
                bmi _snd1

                ldy #100
                jsr jt_error

_snd1           txa
                ;!!sta AUDF1,y
                lda arg2
                asl
                asl
                asl
                asl
                ora arg3
                ;!!sta AUDC1,y
                rts
                .endproc


;======================================
;PROC SndRst()
; reset sound channels
;======================================
libMscSndRst    .proc
                ;!!lda SSKCTL
                and #$ef                ; turn off two tone bit
                ;!!sta SSKCTL
                ;!!sta SKCTL
                lda #0
                ldx #8
_sr1            ;!!sta AUDF1,x             ; zero sound regs
                dex
                bpl _sr1

                rts
                .endproc


;======================================
;BYTE FUNC Paddle(BYTE port)
; returns paddle value of port
; Assumes port low  8.
; see LIB.ST
;Paddle tax
;                lda POT0,x
;                sta args
;                rts


;BYTE FUNC PTrig(BYTE port)
; returns zero if trigger of paddle
; port is depressed.  Assumes port<8
;======================================
libMscPTrig     .proc
                ldx #0
                cmp #4
                bmi _pt1

                inx
                and #3
_pt1            tay
                ;!!lda PORTA,x
                and _pt2,y
                sta args
                rts

;--------------------------------------

_pt2            .byte $04,$08,$40,$80
                .endproc


;======================================
;BYTE FUNC Stick(BYTE port)
; returns current value of joystick
; controller port.  Assumes port<4
;======================================
libMscStick     .proc
                ldx #0
                cmp #2
                bmi _stk1

                inx
                and #1
_stk1           tay
                ;!!lda PORTA,x
                dey
                bne _stk2

                lsr
                lsr
                lsr
                lsr
_stk2           and #$0f
                sta args
                rts
                .endproc


;======================================
;BYTE FUNC STrig(BYTE port)
; returns zero if trigger of joystick
; port is depressed.  Assumes port<4
;
; see LIB.ST
;STrig tax
;                ;!!lda TRIG0,x
;                sta args
;                rts
;======================================


;======================================
;BYTE FUNC Peek(CARD address)
; returns value stored at address
;======================================
libMscPeek      .proc
                .endproc

                ;[fall-through]


;======================================
;CARD FUNC PeekC(CARD address)
; returns value stored at address
;======================================
libMscPeekC     .proc
                sta arg2
                stx arg3
                ldy #0
                lda (arg2),y
                sta args
                iny
                lda (arg2),y
                sta args+1
                rts
                .endproc


;======================================
;PROC Poke(CARD address, BYTE value)
; store byte or char value at address
; (single byte store)
;======================================
libMscPoke      .proc
                sta arg0
                stx arg1
                tya
                ldy #0
                sta (arg0),y
                rts
                .endproc


;======================================
;PROC PokeC(CARD address, value)
; store cardinal or integer value at
; address (2 byte store)
;======================================
libMscPokeC     .proc
                jsr libMscPoke

                iny
                lda arg3
                sta (arg0),y
                rts
                .endproc


;======================================
;PROC Zero(BYTE POINTER address, CARD size)
; set memory bytes starting at address
; up to (but not including) address+size
; to zero.  Note this modifies size
; bytes of memory.
;======================================
libMscZero      .proc
                pha
                lda #0
                sta arg4
                pla
                .endproc

                ;[fall-through]


;======================================
;PROC SetBlock(BYTE POINTER address, CARD size, BYTE value)
; set memory bytes starting at address
; up to (but not including) address+size
; to value.  Note this modifies size
; bytes of memory.
;======================================
libMscSetBlock  .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda arg4
                ldx arg3
                beq _sb3

_sb1            sta (arg0),y
                iny
                bne _sb1

                inc arg1
                dec arg3
                bne _sb1
                beq _sb3

_sb2            sta (arg0),y
                iny
_sb3            cpy arg2
                bne _sb2

                rts
                .endproc


;======================================
;PROC MoveBlock(BYTE POINTER dest, src, CARD size)
; moves size bytes from src through
; src+size-1 to dest through dest+size-1.
; If dest>src and dest<=src+size-1 then
; transfer will not work properly!
;======================================
libMscMoveBlock .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda arg5
                beq _mb4

_mb2            lda (arg2),y
                sta (arg0),y
                iny
                bne _mb2

                inc arg1
                inc arg3
                dec arg5
                bne _mb2
                beq _mb4

_mb3            lda (arg2),y
                sta (arg0),y
                iny
_mb4            cpy arg4
                bne _mb3

                rts
                .endproc


;======================================
;PROC Break()
; returns to Monitor after saving
; stack pointer in procSP
;======================================
libMscBreak     .proc
                tsx
                stx procsp
                ldy #brkERR
                tya
                jmp jt_error

                .endproc


;======================================
;   Call Trace handler
;======================================
libMscCTrace    .proc
;   name passed following JSR
                clc
                pla
                adc #1
                sta arg10
                pla
                adc #0
                sta arg11

;   address of name now in arg10-11
;   ok, let's print the name
                lda arg10
                ldx arg11
                jsr libIOPrint

                lda #'('
                jsr libIOPut

;   now get addr of args
                sec
                lda arg10
                ldy #0
                sty arg15
                adc (arg10),y
                sta arg10
                bcc _ct1

                inc arg11
_ct1            lda (arg10),y
                sta arg12
                iny
                lda (arg10),y
                sta arg13
;   get number of args
                iny
                lda (arg10),y
                sta arg9
                sty arg14
                beq _ct7                ; no args

_ct2            inc arg14
                ldy arg14
                lda (arg10),y
                bmi _ct4                ; byte

                cmp #tokCARD_t
                inc arg15
                ldy arg15
                lda (arg12),y
                tax
                dey
                bcs _ct5                ; cardinal

;   integer
                lda (arg12),y
                jsr libIOPrintI

                jmp _ct6

_ct4            ldx #0
                ldy arg15
_ct5            lda (arg12),y
                jsr libIOPrintC

_ct6            inc arg15
                dec arg9
                beq _ct7                ; all done

                lda #','
                jsr libIOPut

                jmp _ct2

;   setup return
_ct7            clc
                lda arg10
                adc arg14
                tax
                lda arg11
                adc #0
                pha
                txa
                pha
                lda #')'
                jsr libIOPut

                jmp libIOPutE

                .endproc

                ;.endproc
