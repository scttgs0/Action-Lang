
;======================================
;   FILE: main.msc.asm
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
                bne next                ; [unc]

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

                lda (cur),y
                beq _XIT

                tax
                dey
                lda (cur),y
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
                cmp #timesid
                beq _4                  ; qcode reference

                cmp #lbrack
                beq _5

                cmp #record
                beq _1

                cmp #typet
                beq _1

                cmp #typet+8
                beq _1

                cmp #quote
                beq _6

                cmp #undec
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

                jsr getnext
                bne _next1              ; [unc]

_2              ldy #0
                cmp #'^'
                bne _3

                lda (afsize),y
                tax
                iny

                lda (afsize),y
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
                cmp #rbrack
                beq _XIT

                jsr getconst

                ldy #0
                jsr storevar

                lda op
                beq _7                  ; byte?

                iny                     ; no, word
_7              tya
                jsr codeincr
_8              jsr getnext
                bne _next5              ; [unc]

_9              lda #1
                jsr nxtprop

                tax
                iny
                lda (props),y
                beq _varerr

                tay
                sta bank+lbank

                lda #1
                jsr gprop
                jsr rstbank

                jmp _next2

_varerr         ldy #varer
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
                ldy #conster
                cmp #$81
                bcc mnum._adrerr

                cmp #constt+strt
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
                lda (nxtaddr),y         ; size
                sta (qcode),y

                tax
                tay
_next1          lda (nxtaddr),y
                sta (qcode),y

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
                sta (qcode),y

                iny
                txa
                sta (qcode),y

                rts
                .endproc


;======================================
;
;======================================
lookup          .proc
                sty arg2

        .if ramzap
                sta (arg1),y            ; zap RAM if any
        .else
                nop
                nop
        .endif

                stx arg1

                tax
                ldy #2
                lda (arg1),y

                tay
                txa
_next1          cmp (arg1),y
                beq _1

                dey
                dey
                dey
                cpy #2
                bne _next1

_1              dey

                lda (arg1),y
                sta arg4

                dey
                lda (arg1),y
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

cderr           sta bank+ebank

                jsr splsetup            ; reset compiler

                ldy #cder               ; out of qcode space
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
                adc (props),y
                sta props
                bcc _2

                inx

_2              stx props+1

                iny
                lda (props),y

                tax
                dey
                lda (props),y

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
                sta adress
                stx adress+1
                jmp (adress)

                .endproc
