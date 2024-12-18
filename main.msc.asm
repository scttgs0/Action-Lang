
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.msc.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   LShift(val, cnt)
;======================================
lsh1            .proc
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
;   NextUp()
;======================================
nextup          .proc
                ldy #1
                bra next

                .endproc


;======================================
;   NextDwn()
;======================================
nextdwn         .proc
                ldy #5

                .endproc

                ;[fall-through]


;======================================
;   Next(,,dir)
;======================================
next            .proc
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
;   CurStr()
;======================================
curstr          .proc
                lda cur
                ldx cur+1

                .endproc

                ;[fall-through]


;======================================
;   StrPtr()
;======================================
strptr          .proc
                clc
                adc #6
                sta arg0
                bcc _1

                inx

_1              stx arg1

                rts
                .endproc


;======================================
;   MNum()
;======================================
mnum            .proc
                lda #0
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

                jsr getconst
                bcc _next3

_1              lda #1
                jsr nxtprop
_next2          jsr rstp

_next3          clc
                adc zpAllocSize
                sta zpAllocSize

                txa
                adc zpAllocSize+1
_next4          sta zpAllocSize+1

                jsr nextchar

                cmp #'+'
                bne _2

                jsr GetNext
                bra _next1

_2              ldy #0
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
;               ldy #0

                rts

_4              jsr getcdoff            ; QCODE reference
                jmp _next3

_5              jsr getcdoff            ; table reference

                pha
                txa
                pha
                bne _8                  ; [unc]

_6              jsr copystr             ; string ref
                jmp _next3

_next5          lda nxttoken            ; body of table
                cmp #tokRBracket
                beq _XIT

                jsr getconst

                ldy #0
                jsr storevar

                lda zpAllocOP
                beq _7                  ; byte?

                iny                     ; no, word
_7              tya
                jsr codeincr
_8              jsr GetNext
                bra _next5

_9              lda #1
                jsr nxtprop

                tax
                iny
                lda (zpAllocProps),Y
                beq _varerr

                tay
                ;!!sta bank+lbank

                lda #1
                jsr gprop
                jsr RestoreBank

                jmp _next2

_varerr         ldy #varERR
_adrerr         jmp splerr

_XIT            pla                     ; end of table
                tax
                pla
                jmp _next3

                .endproc


;======================================
;   GetConst(token)
;======================================
getconst        .proc
                ldy #constERR
                cmp #$81
                bcc mnum._adrerr

                cmp #tokCONST_t+tokSTR_t
                bcs mnum._adrerr

                lda nxtaddr
                ldx nxtaddr+1

                rts
                .endproc


;======================================
;
;======================================
copystr         .proc
                jsr getcdoff

                pha
                txa
                pha

                ldy #0
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

_1              jsr codeincr

                inc choff               ; get rid of end quote

                pla
                tax
                pla

                rts
                .endproc


;======================================
;   GetCdOff()
;======================================
getcdoff        .proc
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
;   StoreVar(low, high, index)
;======================================
storevar        .proc
                sta (QCODE),Y

                iny
                txa
                sta (QCODE),Y

                rts
                .endproc


;======================================
;
;======================================
lookup          .proc
                sty arg2

            .if ZAPRAM
                sta (arg1),Y            ; zap RAM if any
            .else
                nop
                nop
            .endif

                stx arg1

                tax
                ldy #2
                lda (arg1),Y

                tay
                txa
_next1          cmp (arg1),Y
                beq _1

                dey
                dey
                dey
                cpy #2
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
;   AlphaNum(char)
;======================================
alphanum        .proc
                jsr alpha
                bne _XIT

_num            cmp #'0'
                bmi _1

                cmp #':'
                bmi _XIT

_1              ldx #0
_XIT            rts
                .endproc


;======================================
;   Alpha(char)
;======================================
alpha           .proc
                pha

                ora #$20
                tax

                pla
                cpx #'a'
                bmi _1

                cpx #$7B
                bmi _XIT

_1              ldx #0
_XIT            rts
                .endproc


;======================================
;   STIncr(size)
;======================================
stincr          .proc
                clc
                adc symtab
                sta symtab
                bcc _1

                inc symtab+1

_1              lda stmax
                cmp symtab+1
                bcs alpha._XIT          ; return

                ldy #61                 ; out of symbol table space

                jmp splerr

                .endproc


;======================================
;   CodeIncr(size)
;======================================
codeincr        .proc
                clc
                adc QCODE
                sta QCODE
                bcc _1

                inc QCODE+1

_1              lda stbase
                cmp QCODE+1
                bcs alpha._XIT          ; return

cderr           ;!!sta bank+ebank

                jsr SPLsetup            ; reset compiler

                ldy #qcodeERR           ; out of QCODE space
                jmp splerr

                .endproc


;======================================
;   NxtProp(offset)
;======================================
nxtprop         .proc
                ldx zpAllocProps
                stx zpAllocLast
                ldx zpAllocProps+1
                stx zpAllocLast+1

                ldx nxtaddr
                ldy nxtaddr+1
                bne gprop

                .endproc

                ;[fall-through]


;======================================
;   CProp(offset)
;======================================
cprop           .proc
                ldx curproc
                ldy curproc+1
                bne gprop

                .endproc

                ;[fall-through]


;======================================
;   GetProp(offset)
;======================================
getprop         .proc
                ldx addr
                ldy addr+1

                .endproc

                ;[fall-through]


;======================================
;   GProp(offset, addr)
;======================================
gprop           .proc
                stx zpAllocProps
                sty zpAllocProps+1

                ldx zpAllocProps+1
                clc
                adc zpAllocProps
                bcc _1

                inx

_1              sec
                ldy #0
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
rstp            .proc
                ldy zpAllocLast
                sty zpAllocProps
                ldy zpAllocLast+1
                sty zpAllocProps+1

                rts
                .endproc


;======================================
;   JSRInd(addr)
;======================================
jsrind          .proc
                sta ADRESS
                stx ADRESS+1
                jmp (ADRESS)

                .endproc
