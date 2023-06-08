
; SPDX-FileName: ampl.pf.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   PF()
;======================================
ld1             ldy #0
                lda (stack),y
                cmp #tokARRAY_t
                bcs pf._c5a

                inc abt-args,x
                ldy #7
                lda (stack),y
                cmp #tokTEMP_t+tokBYTE_t
                beq pf._c5b

                dec abt+1-args,x
                cpx #args+2
                bcc pf._c5b

                jsr genops.gops
                jsr Load2H

                lda #$81                ; STA
                jsr Op1H

                jmp pf._c5b

pf              lda #0                  ; load arg types flag
                jsr GetArgs
                jsr pushst
                jsr GetNext

                ldx #args
                stx argbytes
                ldx nxttoken
                cpx #tokRParen
                bne _c4

                jsr GetNext
                bra _c7

_c4             ldx numargs
                ldy #tokTEMP_t+tokBYTE_t
                lda argtypes-1,x
                ldx argbytes
                stx abt+3
                cmp #$7f
                bcs _c5                 ; one byte arg

                sta temps-args+1,x
                inc argbytes
                iny
_c5             sta temps-args,x
                inc argbytes
                txa
                jsr storst
                jsr getexp

                dec numargs
                bmi callerr

                ldx abt+3
                cpx #args+3
                bcc ld1

_c5a            jsr cgassign

_c5b            lda token
                cmp #tokComma
                beq _c4

                cmp #tokRParen
                bne callerr

_c6             lda argbytes
                cmp #args+3
                bcs _c8

                cmp #args+2
                bcs _c9

                cmp #args+1
                bcs _c10

_c7             jsr TrashY

                ldy #1
                jsr StkAddr

                lda #$20                ; JSR
                jmp Push3

_c8             ldx #args+2
                jsr callerr._push

_c9             ldx #args+1
                jsr callerr._push

_c10            ldx #args
                jsr callerr._push

                jmp _c7

callerr         jmp Segment.argerror

_push           lda abt-args,x
                bne _p1

                lda _ops-args,x
                ora #$04
                jmp Push2

_p1             stx arg0
                jsr genops.gops

                ldx arg0
                lda _ops-args,x

; all of this for LDX # and LDY #
; can't use OpXX for these instr.

                cpx #args
                beq _p4                 ; LDA instr.

                ldy arg1
                bpl _p3a                ; record element

                cpy #tokVAR_t
                ldy abt-args,x
                bcs _p3                 ; not const.

                pha
                sty arg0
                ldy #2
                jsr LoadI

                ldy arg0
                bmi _p2

                tax
                pla
                jsr Push2               ; low byte of const

                jmp cgassign.cga1

_p2             pla
_p2a            jmp Push2               ; high byte

_p3a            ldy abt-args,x
_p3             bpl _p4

                ldx arg3
                beq _p2a

                jmp Op2H

_p4             jsr Op2L

                jmp cgassign.cga1

;--------------------------------------
;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY
