
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.mth.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


math            .block
_a              = aflast+1
_b              = aflast
_c              = afcur+1
_d              = afcur

_rl             = afsize
_rh             = afsize+1

_t1             = addr
_t2             = addr+1

_sign           = token
                .endblock


;======================================
;   MultI(op1, op2)
;--------------------------------------
; op2 is in c & d
;  r = ab * cd
;  r = (a*d + c*b)*2^8 + b*d
;======================================
multi           .proc
                jsr smops

                ldx math._b
                beq _2

                stx math._t1

                ldx math._d
                beq _2

                dex
                stx math._t2

                ldx #8
_next1          asl a                   ; b*d, 16 bit result
                rol math._rh
                asl math._t1
                bcc _1

                adc math._t2
                bcc _1

                inc math._rh

_1              dex
                bne _next1

_2              sta math._rl

                lda math._b
                ldx math._c
                jsr mulb                ; b*c, 8 bit result

                lda math._a
                ldx math._d
                jsr mulb                ; a*d, 8 bit result

_setsign        ldy math._sign
                bpl _XIT

        .if ramzap
                sta mulb,X
        .else
                nop
                nop
                nop
        .endif

_ss1            sta math._rl
                stx math._rh

                sec
                lda #0
                sbc math._rl

                tay
                lda #0
                sbc math._rh
                tax
                tya

_XIT            rts
                .endproc


;======================================
;
;======================================
mulb            .proc
                beq _2

                dex
                stx math._t2

                tax
                beq _2

                stx math._t1

                lda #0
                ldx #8
_next1          asl a
                asl math._t1
                bcc _1

                adc math._t2

_1              dex
                bne _next1

                clc
                adc math._rh
                sta math._rh

_2              lda math._rl
                ldx math._rh

                rts
                .endproc


;======================================
;
;======================================
smops           .proc
                stx math._sign
                cpx #0                  ; check signs
                bpl _1

                jsr multi._ss1

_1              sta math._b
                stx math._a

                lda math._c
                bpl _2

                tax
                eor math._sign
                sta math._sign

                lda math._d
                jsr multi._ss1

                sta math._d
                stx math._c

_2              lda #0
                sta math._rh

                rts
                .endproc


;======================================
;   DivC(op1, op2)
;======================================
divi            .proc
                jsr smops

;   see MultC above
                lda math._c
                beq _small

;   large
                ldx #8
_next1          rol math._b
                rol math._a
                rol math._rh

                sec
                lda math._a
                sbc math._d

                tay
                lda math._rh
                sbc math._c
                bcc _1                  ; overflow, don't subtract

                sta math._rh
                sty math._a

_1              dex
                bne _next1

                lda math._b
                rol a

                ldx #0
                ldy math._a
                sty math._rl            ; save low byte of REM

                jmp multi._setsign

_small          ldx #16
_next2          rol math._b
                rol math._a
                rol a
                bcs _2                  ; keep track of shift output

                cmp math._d
                bcc _3                ; overflow, don't subtract

_2              sbc math._d
                sec                     ; for carry out in ROL A above

_3              dex
                bne _next2

                rol math._b
                rol math._a
                sta math._rl

                lda math._b
                ldx math._a

                jmp multi._setsign

                .endproc


;======================================
;
;======================================
remi            .proc
                jsr divi

                lda math._rl
                ldx math._rh

                rts
                .endproc


;======================================
;   RShift(val, cnt)
;======================================
rshift          .proc
                ldy math._d
                beq _XIT

                stx math._c

_next1          lsr math._c
                ror a

                dey
                bne _next1

                ldx math._c

_XIT            rts
                .endproc


;======================================
;
;======================================
sargs           .proc                   ; saves args for call
                sta arg0
                stx arg1
                sty arg2

                clc
                pla
                sta afcur

                adc #3                  ; jump over data
                tay

                pla
                sta afcur+1

                adc #0
                pha

                tya
                pha

                ldy #1
                lda (afcur),Y           ; local address
                sta aflast

                iny
                lda (afcur),Y
                sta aflast+1

                iny
                lda (afcur),Y           ; # of bytes
                tay

_next1          lda args,Y
                sta (aflast),Y

                dey
                bpl _next1

;   check for break key
                lda brkkey
                bne _XIT

                inc brkkey

                jmp break

_XIT            rts
                .endproc


;--------------------------------------
;   IToReal(int) -> FR0
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
;   RToInt() real in FR0
;--------------------------------------
;RToInt         lda FR0
;               sta _sign

;               jsr _FSign
;               jsr FPI

;               lda FR0
;               lda FR0+1

;               jmp _SetSign
