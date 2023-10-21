
; SPDX-FileName: ampl.pf.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;
;======================================
ld1             .proc
                ldy #0
                lda (stack),Y
                cmp #tokARRAY_t
                bcs pf._ENTRY1

                inc abt-args,X

                ldy #7
                lda (stack),Y
                cmp #tokTEMP_t+tokBYTE_t
                beq pf._ENTRY2

                dec abt+1-args,X

                cpx #args+2
                bcc pf._ENTRY2

                jsr genops._ENTRY1
                jsr Load2H

                lda #$81                ; STA
                jsr Op1H

                jmp pf._ENTRY2

                .endproc


;======================================
;   PF()
;======================================
pf              .proc
                lda #0                  ; load arg types flag
                jsr GetArgs
                jsr pushst
                jsr GetNext

                ldx #args
                stx argbytes

                ldx nxttoken
                cpx #tokRParen
                bne _next1

                jsr GetNext

                bra _next2

_next1          ldx numargs
                ldy #tokTEMP_t+tokBYTE_t
                lda argtypes-1,X

                ldx argbytes
                stx abt+3
                cmp #$7F
                bcs _1                 ; one byte arg

                sta temps-args+1,X

                inc argbytes
                iny
_1              sta temps-args,X

                inc argbytes

                txa
                jsr storst
                jsr getexp

                dec numargs
                bmi _err

                ldx abt+3
                cpx #args+3
                bcc ld1

_ENTRY1         jsr cgassign

_ENTRY2         lda token
                cmp #tokComma
                beq _next1

                cmp #tokRParen
                bne _err

                lda argbytes
                cmp #args+3
                bcs _2

                cmp #args+2
                bcs _3

                cmp #args+1
                bcs _4

_next2          jsr TrashY

                ldy #1
                jsr StkAddr

                lda #$20                ; JSR
                jmp Push3

_2              ldx #args+2
                jsr _push

_3              ldx #args+1
                jsr _push

_4              ldx #args
                jsr _push

                jmp _next2

_err            jmp Segment._argerr


;======================================
;
;======================================
_push           lda abt-args,X
                bne _5

                lda _ops-args,X
                ora #$04

                jmp Push2

_5              stx arg0
                jsr genops._ENTRY1

                ldx arg0
                lda _ops-args,X

; all of this for LDX # and LDY #
; can't use OpXX for these instr.

                cpx #args
                beq _9                  ; LDA instr.

                ldy arg1
                bpl _7                  ; record element

                cpy #tokVAR_t
                ldy abt-args,X
                bcs _8                  ; not const.

                pha
                sty arg0

                ldy #2
                jsr LoadI

                ldy arg0
                bmi _6

                tax
                pla
                jsr Push2               ; low byte of const

                jmp cgassign._ENTRY5

_6              pla
_XIT1           jmp Push2               ; high byte

_7              ldy abt-args,X
_8              bpl _9

                ldx arg3
                beq _XIT1

                jmp Op2H

_9              jsr Op2L

                jmp cgassign._ENTRY5

;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY

                .endproc
