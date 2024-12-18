
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.pf.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;
;======================================
ld1             .proc
                ldy #0
                lda (stack),Y
                cmp #arrayt
                bcs pf._c5a

                inc abt-args,X

                ldy #7
                lda (stack),Y
                cmp #tempt+bytet
                beq pf._c5b

                dec abt+1-args,X

                cpx #args+2
                bcc pf._c5b

                jsr genops.gops
                jsr load2h

                lda #$81                ; STA
                jsr op1h

                jmp pf._c5b

                .endproc


;======================================
; PF()
;======================================
pf              .proc
                lda #0                  ; load arg types flag
                jsr getargs
                jsr pushst
                jsr getnext

                ldx #args
                stx argbytes

                ldx nxttoken
                cpx #rparen
                bne _c4

                jsr getnext

                bne _c7                 ; [unc]

_c4             ldx numargs
                ldy #tempt+bytet
                lda argtypes-1,X

                ldx argbytes
                stx abt+3
                cmp #$7f
                bcs _c5                 ; one byte arg

                sta temps-args+1,X

                inc argbytes
                iny
_c5             sta temps-args,X

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
                cmp #comma
                beq _c4

                cmp #rparen
                bne callerr

_c6             lda argbytes
                cmp #args+3
                bcs _c8

                cmp #args+2
                bcs _c9

                cmp #args+1
                bcs _c10

_c7             jsr trashy

                ldy #1
                jsr stkaddr

                lda #$20                ; JSR
                jmp push3

_c8             ldx #args+2
                jsr callerr._push

_c9             ldx #args+1
                jsr callerr._push

_c10            ldx #args
                jsr callerr._push

                jmp _c7

callerr         jmp segment.argerr


;======================================
;
;======================================
_push           lda abt-args,X
                bne _p1

                lda _ops-args,X
                ora #$04

                jmp push2

_p1             stx arg0
                jsr genops.gops

                ldx arg0
                lda _ops-args,X

; all of this for LDX # and LDY #
; can't use OpXX for these instr.

                cpx #args
                beq _p4                 ; LDA instr.

                ldy arg1
                bpl _p3a                ; record element

                cpy #vart
                ldy abt-args,X
                bcs _p3                 ; not const.

                pha
                sty arg0

                ldy #2
                jsr loadi

                ldy arg0
                bmi _p2

                tax
                pla
                jsr push2               ; low byte of const

                jmp cgassign.cga1

_p2             pla
_p2a            jmp push2               ; high byte

_p3a            ldy abt-args,X
_p3             bpl _p4

                ldx arg3
                beq _p2a

                jmp op2h

_p4             jsr op2l

                jmp cgassign.cga1

;--------------------------------------
;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY

                .endproc
