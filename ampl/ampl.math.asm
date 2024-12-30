
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.math.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


math            .namespace

params         .block
_a             = zpAllocLast+1
_b             = zpAllocLast
_c             = zpAllocCurrent+1
_d             = zpAllocCurrent

_rl            = zpAllocSize
_rh            = zpAllocSize+1

_t1            = addr
_t2            = addr+1

_sign          = token
                .endblock


;======================================
; MultI(op1, op2)
;--------------------------------------
; op2 is in c & d
;  r = ab * cd
;  r = (a*d + c*b)*2^8 + b*d
;======================================
MultI           .proc
                jsr SMOps

                ldx params._b
                beq _2

                stx params._t1
                ldx params._d
                beq _2

                dex
                stx params._t2

                ldx #$08
_next1          asl                     ; b*d, 16-bit result
                rol params._rh
                asl params._t1
                bcc _1

                adc params._t2
                bcc _1

                inc params._rh

_1              dex
                bne _next1

_2              sta params._rl

                lda params._b
                ldx params._c
                jsr MulB                ; b*c, 8-bit result

                lda params._a
                ldx params._d
                jsr MulB                ; a*d, 8-bit result

_setsign        ldy params._sign
                bpl _XIT

            .if ZAPRAM
                sta MulB,X
            .else
                nop
                nop
                nop
            .endif

_setsign2       sta params._rl
                stx params._rh

                sec
                lda #$00
                sbc params._rl
                tay
                lda #$00
                sbc params._rh
                tax
                tya

_XIT            rts
                .endproc


;======================================
; MulB()
;======================================
MulB            .proc
                beq _2

                dex
                stx params._t2
                tax
                beq _2

                stx params._t1

                lda #$00
                ldx #$08
_next1          asl
                asl params._t1
                bcc _1

                adc params._t2

_1              dex
                bne _next1

                clc
                adc params._rh
                sta params._rh

_2              lda params._rl
                ldx params._rh

                rts
                .endproc


;======================================
;
;======================================
SMOps           .proc
                stx params._sign
                cpx #$00                ; check signs
                bpl _1

                jsr MultI._setsign2

_1              sta params._b
                stx params._a

                lda params._c
                bpl _2

                tax
                eor params._sign
                sta params._sign

                lda params._d
                jsr MultI._setsign2

                sta params._d
                stx params._c

_2              lda #$00
                sta params._rh

                rts
                .endproc


;======================================
; DivC(op1, op2)
;======================================
DivC            .proc
                jsr SMOps

;   see MultC above
                lda params._c
                beq _small

; - - - - - - - - - - - - - - - - - - -

_large          ldx #$08
_next1          rol params._b
                rol params._a
                rol params._rh

                sec
                lda params._a
                sbc params._d

                tay
                lda params._rh
                sbc params._c
                bcc _1                ; overflow, don't subtract

                sta params._rh
                sty params._a

_1              dex
                bne _next1

                lda params._b
                rol
                ldx #$00
                ldy params._a
                sty params._rl          ; save low byte of REM

                jmp MultI._setsign

; - - - - - - - - - - - - - - - - - - -

_small          ldx #$10
_next2          rol params._b
                rol params._a
                rol
                bcs _2                  ; keep track of shift output

                cmp params._d
                bcc _3                  ; overflow, don't subtract

_2              sbc params._d
                sec                     ; for carry out in ROL A above

_3              dex
                bne _next2

                rol params._b
                rol params._a
                sta params._rl

                lda params._b
                ldx params._a

                jmp MultI._setsign

                .endproc


;======================================
;
;======================================
RemL            .proc
                jsr DivC

                lda params._rl
                ldx params._rh

                rts
                .endproc


;======================================
; RShift(val, cnt)
;======================================
RShift          .proc
                ldy params._d
                beq _XIT

                stx params._c
_next1          lsr params._c
                ror

                dey
                bne _next1

                ldx params._c

_XIT            rts
                .endproc


;======================================
; SArgs()
;======================================
SArgs           .proc                   ; saves args for call
                sta arg0
                stx arg1
                sty arg2

                clc
                pla
                sta zpAllocCurrent

                adc #$03                ; jump over data
                tay

                pla
                sta zpAllocCurrent+1

                adc #$00
                pha

                tya
                pha

                ldy #$01
                lda (zpAllocCurrent),Y  ; local address
                sta zpAllocLast

                iny
                lda (zpAllocCurrent),Y
                sta zpAllocLast+1

                iny
                lda (zpAllocCurrent),Y  ; # of bytes
                tay

_next1          lda args,Y
                sta (zpAllocLast),Y

                dey
                bpl _next1

;   check for break key
                lda BRKKEY
                bne _XIT

                inc BRKKEY
                jmp lib.msc.Break

_XIT            rts
                .endproc

                ;.endproc

;--------------------------------------
; IToReal(int) -> FR0
;--------------------------------------
;IToReal        stx _sign

;               jsr _SetSign

;               sta FR0
;               stx FR0+1

;               jsr IFP

;:FSign         lda _sign
;               bpl _Rem1

;               jsr FMOVE
;               jsr ZFR0

;               jmp FSUB


;--------------------------------------
; RToInt() real in FR0
;--------------------------------------
;RToInt         lda FR0
;               sta _sign

;               jsr _FSign
;               jsr FPI

;               lda FR0
;               lda FR0+1

;               jmp _SetSign

                .endnamespace
