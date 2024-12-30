
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.pf.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


pf              .namespace

;======================================
;
;======================================
Load1           .proc
                ldy #$00
                lda (stack),Y
                cmp #tokARRAY_t
                bcs ProcFunc._ENTRY1

                inc abt-args,X

                ldy #$07
                lda (stack),Y
                cmp #tokTEMP_t+tokBYTE_t
                beq ProcFunc._ENTRY2

                dec abt+1-args,X

                cpx #args+2
                bcc ProcFunc._ENTRY2

                jsr compiler.GenOps._ENTRY1
                jsr ampl.cgu.Load2H

                lda #$81                ; STA
                jsr ampl.cgu.Op1H

                jmp ProcFunc._ENTRY2

                .endproc


;======================================
; PF()
;======================================
ProcFunc        .proc
                lda #$00                ; load arg types flag
                jsr mainbank.GetArgs
                jsr compiler.PushST
                jsr compiler.lexicon.GetNext

                ldx #args
                stx argbytes

                ldx nxttoken
                cpx #tokRParen
                bne _next1

                jsr compiler.lexicon.GetNext

                bne _next2              ; [unc]

_next1          ldx numargs
                ldy #tokTEMP_t+tokBYTE_t
                lda argtypes-1,X

                ldx argbytes
                stx abt+3
                cmp #$7F
                bcs _1                  ; one byte arg

                sta temps-args+1,X

                inc argbytes
                iny
_1              sta temps-args,X

                inc argbytes

                txa
                jsr compiler.StoreST
                jsr compiler.GetExp

                dec numargs
                bmi _err

                ldx abt+3
                cpx #args+3
                bcc Load1

_ENTRY1         jsr compiler.CGAssign

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

_next2          jsr ampl.cgu.TrashY

                ldy #$01
                jsr ampl.cgu.StkAddr

                lda #$20                ; JSR
                jmp ampl.cgu.Push3

_2              ldx #args+2
                jsr _push

_3              ldx #args+1
                jsr _push

_4              ldx #args
                jsr _push

                jmp _next2

_err            jmp ampl.Segment._argerr


; = = = = = = = = = = = = = = = = = = =
;
; = = = = = = = = = = = = = = = = = = =
_push           lda abt-args,X
                bne _5

                lda _ops-args,X
                ora #$04

                jmp ampl.cgu.Push2

_5              stx arg0
                jsr compiler.GenOps._ENTRY1

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

                ldy #$02
                jsr ampl.cgu.LoadI

                ldy arg0
                bmi _6

                tax
                pla
                jsr ampl.cgu.Push2      ; low byte of const

                jmp compiler.CGAssign._ENTRY5

_6              pla
_XIT1           jmp ampl.cgu.Push2      ; high byte

_7              ldy abt-args,X
_8              bpl _9

                ldx arg3
                beq _XIT1

                jmp ampl.cgu.Op2H

_9              jsr ampl.cgu.Op2L

                jmp compiler.CGAssign._ENTRY5

;--------------------------------------

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY

                .endproc

                .endnamespace
