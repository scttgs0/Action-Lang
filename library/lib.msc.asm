
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.msc.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


msc             .namespace

;======================================
; BYTE FUNC Rand(BYTE range)
;--------------------------------------
; returns random number between 0 and
; range-1.  If range=0, then a random
; number between 0 and 255 is returned
;======================================
Rand            .proc
                ldx RANDOM
                cmp #$00
                beq _1

                stx zpAllocCurrent

                ldx #$00
                stx zpAllocCurrent+1

                jsr ampl.math.MultI

_1              stx args

                rts
                .endproc


;======================================
; PROC Sound(BYTE v, p, d, vol)
;--------------------------------------
; set voice to specified pitch, distortion,
; and volume.  Assumes volume low  16.
;======================================
Sound           .proc
                asl
                sty arg2

                tay
                cmp #$07
                bmi _1

                ldy #100
                jsr jt_vecError

_1              txa
                sta AUDF1,Y

                lda arg2
                asl
                asl
                asl
                asl
                ora arg3
                sta AUDC1,Y

                rts
                .endproc


;======================================
; PROC SndRst()
;--------------------------------------
; reset sound channels
;======================================
SndRst          .proc
                lda SSKCTL
                and #$EF                ; turn off two tone bit
                sta SSKCTL
                sta SKCTL

                lda #$00
                ldx #$08
_next1          sta AUDF1,X             ; zero sound regs

                dex
                bpl _next1

                rts
                .endproc


;======================================
; BYTE FUNC Paddle(BYTE port)
;--------------------------------------
; returns paddle value of port
; Assumes port low 8.
; see LIB.ST
;======================================
; Paddle         tax
;                lda POT0,X
;                sta args
;                rts


;======================================
; BYTE FUNC PTrig(BYTE port)
;--------------------------------------
; returns zero if trigger of paddle
; port is depressed.  Assumes port<8
;======================================
PTrig           .proc
                ldx #$00
                cmp #$04
                bmi _1

                inx
                and #$03

_1              tay
                lda PORTA,X
                and _data1,Y
                sta args

                rts

;--------------------------------------

_data1          .byte $04,$08,$40,$80

                .endproc


;======================================
; BYTE FUNC Stick(BYTE port)
;--------------------------------------
; returns current value of joystick
; controller port.  Assumes port<4
;======================================
Stick           .proc
                ldx #$00
                cmp #$02
                bmi _1

                inx
                and #$01

_1              tay
                lda PORTA,X

                dey
                bne _2

                lsr
                lsr
                lsr
                lsr

_2              and #$0F
                sta args

                rts
                .endproc


;======================================
; BYTE FUNC STrig(BYTE port)
;--------------------------------------
; returns zero if trigger of joystick
; port is depressed.  Assumes port<4
;
; see LIB.ST
;======================================
;STrig           tax
;                ;!!lda TRIG0,X
;                sta args
;                rts


;======================================
; BYTE FUNC Peek(CARD address)
;--------------------------------------
; returns value stored at address
;======================================
Peek
                ;[fall-through]


;======================================
; CARD FUNC PeekC(CARD address)
;--------------------------------------
; returns value stored at address
;======================================
PeekC           .proc
                sta arg2
                stx arg3

                ldy #$00
                lda (arg2),Y
                sta args

                iny
                lda (arg2),Y
                sta args+1

                rts
                .endproc


;======================================
; PROC Poke(CARD address, BYTE value)
;--------------------------------------
; store byte or char value at address
; (single byte store)
;======================================
Poke            .proc
                sta arg0
                stx arg1

                tya
                ldy #$00
                sta (arg0),Y

                rts
                .endproc


;======================================
; PROC PokeC(CARD address, value)
;--------------------------------------
; store cardinal or integer value at
; address (2 byte store)
;======================================
PokeC           .proc
                jsr Poke

                iny
                lda arg3
                sta (arg0),Y

                rts
                .endproc


;======================================
; PROC Zero(BYTE POINTER address, CARD size)
;--------------------------------------
; set memory bytes starting at address
; up to (but not including) address+size
; to zero.  Note this modifies size
; bytes of memory.
;======================================
Zero            .proc
                pha

                .mbv #$00,arg4

                pla
                .endproc

                ;[fall-through]


;======================================
; PROC SetBlock(BYTE POINTER address, CARD size, BYTE value)
;--------------------------------------
; set memory bytes starting at address
; up to (but not including) address+size
; to value.  Note this modifies size
; bytes of memory.
;======================================
SetBlock        .proc
                sta arg0
                stx arg1
                sty arg2

                ldy #$00
                lda arg4
                ldx arg3
                beq _1

_next1          sta (arg0),Y

                iny
                bne _next1

                inc arg1
                dec arg3
                bne _next1
                beq _1                  ; [unc]

_next2          sta (arg0),Y

                iny
_1              cpy arg2
                bne _next2

                rts
                .endproc


;======================================
; PROC MoveBlock(BYTE POINTER dest, src, CARD size)
;--------------------------------------
; moves size bytes from src through
; src+size-1 to dest through dest+size-1.
; If dest>src and dest<=src+size-1 then
; transfer will not work properly!
;======================================
MoveBlock       .proc
                sta arg0
                stx arg1
                sty arg2

                ldy #$00
                lda arg5
                beq _1

_next1          lda (arg2),Y
                sta (arg0),Y

                iny
                bne _next1

                inc arg1
                inc arg3
                dec arg5
                bne _next1
                beq _1                  ; [unc]

_next2          lda (arg2),Y
                sta (arg0),Y

                iny
_1              cpy arg4
                bne _next2

                rts
                .endproc


;======================================
; PROC Break()
;--------------------------------------
; returns to Monitor after saving
; stack pointer in procSP
;======================================
Break           .proc
                tsx
                stx procSP

                ldy #brkERR
                tya

                jmp jt_vecError

                .endproc


;======================================
; Call Trace handler
;======================================
CTrace          .proc
;   name passed following JSR
                clc
                pla
                adc #$01
                sta arg10

                pla
                adc #$00
                sta arg11

;   address of name now in arg10-11
;   ok, let's print the name
                lda arg10
                ldx arg11
                jsr lib.io.Print

                lda #'('
                jsr lib.io.Put

;   now get addr of args
                sec
                lda arg10

                ldy #$00
                sty arg15

                adc (arg10),Y
                sta arg10
                bcc _1

                inc arg11

_1              lda (arg10),Y
                sta arg12

                iny
                lda (arg10),Y
                sta arg13

;   get number of args
                iny
                lda (arg10),Y
                sta arg9

                sty arg14
                beq _5                  ; no args

_next1          inc arg14
                ldy arg14
                lda (arg10),Y
                bmi _2                  ; byte

                cmp #tokCARD_t

                inc arg15
                ldy arg15
                lda (arg12),Y

                tax
                dey
                bcs _3                  ; cardinal

;   integer
                lda (arg12),Y
                jsr lib.io.PrintI
                jmp _4

_2              ldx #$00
                ldy arg15
_3              lda (arg12),Y
                jsr lib.io.PrintC

_4              inc arg15
                dec arg9
                beq _5                  ; all done

                lda #','
                jsr lib.io.Put
                jmp _next1

;   setup return
_5              clc
                lda arg10
                adc arg14
                tax

                lda arg11
                adc #$00
                pha

                txa
                pha

                lda #')'
                jsr lib.io.Put
                jmp lib.io.PutE

                .endproc

                .endnamespace
