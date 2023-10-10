
;======================================
;   FILE: ampl.pf.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


;======================================
;
;======================================
ld1             .proc
                ldy #0
                lda (stack),y
                cmp #arrayt
                bcs pf._ENTRY1

                inc abt-args,x

                ldy #7
                lda (stack),y
                cmp #tempt+bytet
                beq pf._ENTRY2

                dec abt+1-args,x

                cpx #args+2
                bcc pf._ENTRY2

                jsr genops.gops
                jsr load2h

                lda #$81                ; STA
                jsr op1h

                jmp pf._ENTRY2

                .endproc


;======================================
;   PF()
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
                bne _next1

                jsr getnext

                bne _next2              ; [unc]

_next1          ldx numargs
                ldy #tempt+bytet
                lda argtypes-1,x

                ldx argbytes
                stx abt+3
                cmp #$7F
                bcs _1                  ; one byte arg

                sta temps-args+1,x

                inc argbytes
                iny
_1              sta temps-args,x

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
                cmp #comma
                beq _next1

                cmp #rparen
                bne _err

                lda argbytes
                cmp #args+3
                bcs _2

                cmp #args+2
                bcs _3

                cmp #args+1
                bcs _4

_next2          jsr trashy

                ldy #1
                jsr stkaddr

                lda #$20                ; JSR
                jmp push3

_2              ldx #args+2
                jsr _push

_3              ldx #args+1
                jsr _push

_4              ldx #args
                jsr _push

                jmp _next2

_err            jmp segment._argerr


;======================================
;
;======================================
_push           lda abt-args,x
                bne _5

                lda _ops-args,x
                ora #$04

                jmp push2

_5              stx arg0
                jsr genops.gops

                ldx arg0
                lda _ops-args,x

;   all of this for LDX # and LDY #
;   can't use OpXX for these instr.

                cpx #args
                beq _9                  ; LDA instr.

                ldy arg1
                bpl _7                  ; record element

                cpy #vart
                ldy abt-args,x
                bcs _8                  ; not const.

                pha
                sty arg0

                ldy #2
                jsr loadi

                ldy arg0
                bmi _6

                tax
                pla
                jsr push2               ; low byte of const

                jmp cgassign.cga1

_6              pla
_XIT1           jmp push2               ; high byte

_7              ldy abt-args,x
_8              bpl _9

                ldx arg3
                beq _XIT1

                jmp op2h

_9              jsr op2l

                jmp cgassign.cga1

;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY

                .endproc
