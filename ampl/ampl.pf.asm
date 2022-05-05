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
;   PF()
;======================================
ld1             ldy #0
                lda (stack),y
                cmp #arrayt
                bcs pf._c5a

                inc abt-(args-DPBASE),x
                ldy #7
                lda (stack),y
                cmp #tempt+bytet
                beq pf._c5b

                dec abt+1-(args-DPBASE),x
                cpx #(args-DPBASE)+2
                bcc pf._c5b

                jsr genops.gops
                jsr load2h

                lda #$81                ; STA
                jsr op1h

                jmp pf._c5b

pf              lda #0                  ; load arg types flag
                jsr GetArgs
                jsr pushst
                jsr getnext

                ldx #(args-DPBASE)
                stx argbytes
                ldx nxttoken
                cpx #rparen
                bne _c4

                jsr getnext
                bra _c7

_c4             ldx numargs
                ldy #tempt+bytet
                lda argtypes-1,x
                ldx argbytes
                stx abt+3
                cmp #$7f
                bcs _c5                 ; one byte arg

                sta temps-(args-DPBASE)+1,x
                inc argbytes
                iny
_c5             sta temps-(args-DPBASE),x
                inc argbytes
                txa
                jsr storst
                jsr getexp

                dec numargs
                bmi callerr

                ldx abt+3
                cpx #(args-DPBASE)+3
                bcc ld1

_c5a            jsr cgassign

_c5b            lda token
                cmp #comma
                beq _c4

                cmp #rparen
                bne callerr

_c6             lda argbytes
                cmp #(args-DPBASE)+3
                bcs _c8

                cmp #(args-DPBASE)+2
                bcs _c9

                cmp #(args-DPBASE)+1
                bcs _c10

_c7             jsr trashy

                ldy #1
                jsr stkaddr

                lda #$20                ; JSR
                jmp push3

_c8             ldx #(args-DPBASE)+2
                jsr callerr._push

_c9             ldx #(args-DPBASE)+1
                jsr callerr._push

_c10            ldx #(args-DPBASE)
                jsr callerr._push

                jmp _c7

callerr         jmp segment.argerr

_push           lda abt-(args-DPBASE),x
                bne _p1

                lda _ops-(args-DPBASE),x
                ora #$04
                jmp push2

_p1             stx arg0
                jsr genops.gops

                ldx arg0
                lda _ops-(args-DPBASE),x

; all of this for LDX # and LDY #
; can't use OpXX for these instr.

                cpx #(args-DPBASE)
                beq _p4                 ; LDA instr.

                ldy arg1
                bpl _p3a                ; record element

                cpy #vart
                ldy abt-(args-DPBASE),x
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

_p3a            ldy abt-(args-DPBASE),x
_p3             bpl _p4

                ldx arg3
                beq _p2a

                jmp op2h

_p4             jsr op2l

                jmp cgassign.cga1

;--------------------------------------
;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY
