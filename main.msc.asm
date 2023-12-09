
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.msc.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   LShift(val, cnt)
;======================================
lsh1            .proc
_a              = aflast+1
_b              = aflast
_c              = afcur+1
_d              = afcur
_rl             = afsize
_rh             = afsize+1
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
                jsr chkcur
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
                sta afsize
                sta afsize+1

_next1          lda nxttoken
                cmp #tokMULT
                beq _4                  ; qcode reference

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
                adc afsize
                sta afsize

                txa
                adc afsize+1
_next4          sta afsize+1

                jsr nextchar

                cmp #'+'
                bne _2

                jsr GetNext
                bra _next1

_2              ldy #0
                cmp #'^'
                bne _3

                lda (afsize),Y
                tax
                iny

                lda (afsize),Y
                stx afsize

                jmp _next4

_3              dec choff               ; put back character
                lda afsize
                ldx afsize+1
;               ldy #0

                rts

_4              jsr getcdoff            ; qcode reference
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

                lda op
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
                lda (props),Y
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
                sta (qcode),Y

                tax
                tay
_next1          lda (nxtaddr),Y
                sta (qcode),Y

                dey
                bne _next1

                inx
                txa
                bne _1

                inc qcode+1

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
                lda qcode
                adc codeoff
                pha

                lda qcode+1
                adc codeoff+1

                tax
                pla
                rts
                .endproc


;======================================
;   StoreVar(low, high, index)
;======================================
storevar        .proc
                sta (qcode),Y

                iny
                txa
                sta (qcode),Y

                rts
                .endproc


;======================================
;
;======================================
lookup          .proc
                sty arg2

        .if ramzap
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
                adc qcode
                sta qcode
                bcc _1

                inc qcode+1

_1              lda stbase
                cmp qcode+1
                bcs alpha._XIT          ; return

cderr           ;!!sta bank+ebank

                jsr SPLsetup            ; reset compiler

                ldy #qcodeERR           ; out of qcode space
                jmp splerr

                .endproc


;======================================
;   NxtProp(offset)
;======================================
nxtprop         .proc
                ldx props
                stx aflast
                ldx props+1
                stx aflast+1

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
                stx props
                sty props+1

                ldx props+1
                clc
                adc props
                bcc _1

                inx

_1              sec
                ldy #0
                adc (props),Y
                sta props
                bcc _2

                inx

_2              stx props+1

                iny
                lda (props),Y

                tax
                dey
                lda (props),Y

                rts
                .endproc


;======================================
;
;======================================
rstp            .proc
                ldy aflast
                sty props
                ldy aflast+1
                sty props+1

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
