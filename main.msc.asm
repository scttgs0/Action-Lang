
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.msc.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; mscLShift(val, cnt)
;======================================
mscLShift       .proc
_a              = zpAllocLast+1
_b              = zpAllocLast
_c              = zpAllocCurrent+1
_d              = zpAllocCurrent
_rl             = zpAllocSize
_rh             = zpAllocSize+1
_t1             = addr
_t2             = addr+1
_sign           = token

                sty _d
_lshift         ldy _d
                beq _lshret

                stx _c
_lsh1           asl
                rol _c
                dey
                bne _lsh1

                ldx _c
_lshret         rts
                .endproc


;======================================
; mscNextUp()
;======================================
mscNextUp       .proc
                ldy #$01
                bra mscNext

                .endproc


;======================================
; mscNextDown()
;======================================
mscNextDown     .proc
                ldy #$05

                .endproc

                ;[fall-through]


;======================================
; mscNext(,,dir)
;======================================
mscNext         .proc
                jsr ChkCursor
                beq _XIT

                lda (cur),Y
                beq _XIT

                tax
                dey
                lda (cur),Y
                sta cur
                txa
                sta cur+1

_XIT            rts
                .endproc


;======================================
; mscCurStr()
;======================================
mscCurStr       .proc
                lda cur
                ldx cur+1

                .endproc

                ;[fall-through]


;======================================
; mscStrPtr()
;======================================
mscStrPtr       .proc
                clc
                adc #$06
                sta arg0
                bcc _1

                inx

_1              stx arg1

                rts
                .endproc


;======================================
; mscMNum()
;======================================
mscMNum         .proc
                lda #$00
                sta zpAllocSize
                sta zpAllocSize+1

_next1          lda nxttoken
                cmp #tokMULT
                beq _4                  ; QCODE reference

                cmp #tokLBracket
                beq _5

                cmp #tokRECORD
                beq _1

                cmp #tokTYPE_t
                beq _1

                cmp #tokTYPE_t+8
                beq _1

                cmp #tokQuote
                beq _6

                cmp #tokUNDEC
                beq _9
                bcs _1

                jsr mscGetConst
                bcc _next3

_1              lda #$01
                jsr mscNextProp
_next2          jsr mscResetProp

_next3          clc
                adc zpAllocSize
                sta zpAllocSize

                txa
                adc zpAllocSize+1
_next4          sta zpAllocSize+1

                jsr NextChar

                cmp #'+'
                bne _2

                jsr GetNext
                bra _next1

_2              ldy #$00
                cmp #'^'
                bne _3

                lda (zpAllocSize),Y
                tax
                iny

                lda (zpAllocSize),Y
                stx zpAllocSize

                jmp _next4

_3              dec choff               ; put back character
                lda zpAllocSize
                ldx zpAllocSize+1
;               ldy #$00

                rts

_4              jsr mscGetCodeOffset    ; QCODE reference
                jmp _next3

_5              jsr mscGetCodeOffset    ; table reference

                pha
                txa
                pha
                bne _8                  ; [unc]

_6              jsr mscCopyStr          ; string ref
                jmp _next3

_next5          lda nxttoken            ; body of table
                cmp #tokRBracket
                beq _XIT

                jsr mscGetConst

                ldy #$00
                jsr mscStoreVar

                lda zpAllocOP
                beq _7                  ; byte?

                iny                     ; no, word
_7              tya
                jsr mscCodeIncr
_8              jsr GetNext
                bra _next5

_9              lda #$01
                jsr mscNextProp

                tax
                iny
                lda (zpAllocProps),Y
                beq _varerr

                tay
                ;!!sta bank+lbank

                lda #$01
                jsr mscGProp
                jsr bankRestoreBank

                jmp _next2

_varerr         ldy #varERR
_adrerr         jmp bankSplErr

_XIT            pla                     ; end of table
                tax
                pla
                jmp _next3

                .endproc


;======================================
; mscGetConst(token)
;======================================
mscGetConst     .proc
                ldy #constERR
                cmp #$81
                bcc mscMNum._adrerr

                cmp #tokCONST_t+tokSTR_t
                bcs mscMNum._adrerr

                lda nxtaddr
                ldx nxtaddr+1

                rts
                .endproc


;======================================
;
;======================================
mscCopyStr      .proc
                jsr mscGetCodeOffset

                pha
                txa
                pha

                ldy #$00
                lda (nxtaddr),Y         ; size
                sta (QCODE),Y

                tax
                tay
_next1          lda (nxtaddr),Y
                sta (QCODE),Y

                dey
                bne _next1

                inx
                txa
                bne _1

                inc QCODE+1

_1              jsr mscCodeIncr

                inc choff               ; get rid of end quote

                pla
                tax
                pla

                rts
                .endproc


;======================================
; mscGetCodeOffset()
;======================================
mscGetCodeOffset .proc
                clc
                lda QCODE
                adc codeoff
                pha

                lda QCODE+1
                adc codeoff+1

                tax
                pla
                rts
                .endproc


;======================================
; mscStoreVar(low, high, index)
;======================================
mscStoreVar     .proc
                sta (QCODE),Y

                iny
                txa
                sta (QCODE),Y

                rts
                .endproc


;======================================
;
;======================================
mscLookup       .proc
                sty arg2

            .if ZAPRAM
                sta (arg1),Y            ; zap RAM if any
            .else
                nop
                nop
            .endif

                stx arg1

                tax
                ldy #$02
                lda (arg1),Y

                tay
                txa
_next1          cmp (arg1),Y
                beq _1

                dey
                dey
                dey
                cpy #$02
                bne _next1

_1              dey

                lda (arg1),Y
                sta arg4

                dey
                lda (arg1),Y
                sta arg3

                jmp (arg3)

                .endproc


;======================================
; mscAlphaNum(char)
;======================================
mscAlphaNum     .proc
                jsr mscAlpha
                bne _XIT

_num            cmp #'0'
                bmi _1

                cmp #':'
                bmi _XIT

_1              ldx #$00
_XIT            rts
                .endproc


;======================================
; mscAlpha(char)
;======================================
mscAlpha        .proc
                pha

                ora #$20
                tax

                pla
                cpx #'a'
                bmi _1

                cpx #$7B
                bmi _XIT

_1              ldx #$00
_XIT            rts
                .endproc


;======================================
; mscSTIncr(size)
;======================================
mscSTIncr       .proc
                clc
                adc symtab
                sta symtab
                bcc _1

                inc symtab+1

_1              lda stmax
                cmp symtab+1
                bcs mscAlpha._XIT       ; return

                ldy #61                 ; out of symbol table space

                jmp bankSplErr

                .endproc


;======================================
; mscCodeIncr(size)
;======================================
mscCodeIncr     .proc
                clc
                adc QCODE
                sta QCODE
                bcc _1

                inc QCODE+1

_1              lda stbase
                cmp QCODE+1
                bcs mscAlpha._XIT       ; return

cderr           ;!!sta bank+ebank

                jsr SPLsetup            ; reset compiler

                ldy #qcodeERR           ; out of QCODE space
                jmp bankSplErr

                .endproc


;======================================
; mscNextProp(offset)
;======================================
mscNextProp     .proc
                ldx zpAllocProps
                stx zpAllocLast
                ldx zpAllocProps+1
                stx zpAllocLast+1

                ldx nxtaddr
                ldy nxtaddr+1
                bne mscGProp

                .endproc

                ;[fall-through]


;======================================
; mscCProp(offset)
;======================================
mscCProp        .proc
                ldx curproc
                ldy curproc+1
                bne mscGProp

                .endproc

                ;[fall-through]


;======================================
; mscGetProp(offset)
;======================================
mscGetProp      .proc
                ldx addr
                ldy addr+1

                .endproc

                ;[fall-through]


;======================================
; mscGProp(offset, addr)
;======================================
mscGProp        .proc
                stx zpAllocProps
                sty zpAllocProps+1

                ldx zpAllocProps+1
                clc
                adc zpAllocProps
                bcc _1

                inx

_1              sec
                ldy #$00
                adc (zpAllocProps),Y
                sta zpAllocProps
                bcc _2

                inx

_2              stx zpAllocProps+1

                iny
                lda (zpAllocProps),Y

                tax
                dey
                lda (zpAllocProps),Y

                rts
                .endproc


;======================================
;
;======================================
mscResetProp    .proc
                ldy zpAllocLast
                sty zpAllocProps
                ldy zpAllocLast+1
                sty zpAllocProps+1

                rts
                .endproc


;======================================
; mscJSRIndirect(addr)
;======================================
mscJSRIndirect  .proc
                sta ADRESS
                stx ADRESS+1
                jmp (ADRESS)

                .endproc
