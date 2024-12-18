
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
                bne next                ; [unc]

                .endproc


;======================================
;   NextDwn()
;======================================
nextdwn         .proc
                ldy #5
                .endproc


;======================================
;   Next(,,dir)
;======================================
next            .proc
                jsr chkcur
                beq next1

                lda (cur),Y
                beq next1

                tax
                dey
                lda (cur),Y
                sta cur
                txa
                sta cur+1
next1           rts
                .endproc


;======================================
;   CurStr()
;======================================
curstr          .proc
                lda cur
                ldx cur+1
                .endproc


;======================================
;   StrPtr()
;======================================
strptr          .proc
                clc
                adc #6
                sta arg0
                bcc _sp1

                inx
_sp1            stx arg1
                rts
                .endproc


;======================================
;   MNum()
;======================================
mnum            .proc
                lda #0
                sta afsize
                sta afsize+1

_mn1            lda nxttoken
                cmp #timesid
                beq _mn4                ; qcode reference

                cmp #lbrack
                beq _mn5

                cmp #record
                beq _mnvar

                cmp #typet
                beq _mnvar

                cmp #typet+8
                beq _mnvar

                cmp #quote
                beq _mnstr

                cmp #undec
                beq _mnundec
                bcs _mnvar

                jsr getconst
                bcc _mn2

_mnvar          lda #1
                jsr nxtprop
_mnv1           jsr rstp

_mn2            clc
                adc afsize
                sta afsize
                txa
                adc afsize+1
_mn2a           sta afsize+1
                jsr nextchar

                cmp #'+'
                bne _mn2b

                jsr getnext
                bne _mn1                ; [unc]

_mn2b           ldy #0
                cmp #'^'
                bne _mn3

                lda (afsize),Y
                tax
                iny
                lda (afsize),Y
                stx afsize
                jmp _mn2a

_mn3            dec choff               ; put back character
                lda afsize
                ldx afsize+1
;    LDY #0
                rts

_mn4            jsr getcdoff            ; qcode reference

                jmp _mn2

_mn5            jsr getcdoff            ; table reference

                pha
                txa
                pha
                bne _mn8                ; [unc]

_mnstr          jsr copystr             ; string ref

                jmp _mn2

_mn6            lda nxttoken            ; body of table
                cmp #rbrack
                beq _mn9

                jsr getconst

                ldy #0
                jsr storevar

                lda op
                beq _mn7                ; byte?

                iny                     ; no, word
_mn7            tya
                jsr codeincr
_mn8            jsr getnext
                bne _mn6                ; [unc]

_mnundec        lda #1
                jsr nxtprop

                tax
                iny
                lda (props),Y
                beq _varerr

                tay
                sta bank+lbank
                lda #1
                jsr gprop
                jsr rstbank

                jmp _mnv1

_varerr         ldy #varer
_adrerr         jmp splerr

_mn9            pla                     ; end of table
                tax
                pla
                jmp _mn2

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
                lda (nxtaddr),Y         ; size
                sta (qcode),Y
                tax
                tay
_cs1            lda (nxtaddr),Y
                sta (qcode),Y
                dey
                bne _cs1

                inx
                txa
                bne _cs2

                inc qcode+1
_cs2            jsr codeincr

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

lu1             stx arg1
                tax
                ldy #2
                lda (arg1),Y
                tay
                txa
_fml1           cmp (arg1),Y
                beq _fmjmp

                dey
                dey
                dey
                cpy #2
                bne _fml1

_fmjmp          dey
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
                bne _anum2

_num            cmp #'0'
                bmi _anum1

                cmp #':'
                bmi _anum2
_anum1          ldx #0
_anum2          rts
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
                bmi _alpha1

                cpx #$7b
                bmi _alpha2

_alpha1         ldx #0
_alpha2         rts
                .endproc


;======================================
;   STIncr(size)
;======================================
stincr          .proc
                clc
                adc symtab
                sta symtab
                bcc _s1

                inc symtab+1
_s1             lda stmax
                cmp symtab+1
                bcs alpha._alpha2       ; return

                ldy #61                 ; out of s.t. space
                jmp splerr

                .endproc


;======================================
;   CodeIncr(size)
;======================================
codeincr        .proc
                clc
                adc qcode
                sta qcode
                bcc _c1

                inc qcode+1
_c1             lda stbase
                cmp qcode+1
                bcs alpha._alpha2       ; return

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


;======================================
;   CProp(offset)
;======================================
cprop           .proc
                ldx curproc
                ldy curproc+1
                bne gprop

                .endproc


;======================================
;   GetProp(offset)
;======================================
getprop         .proc
                ldx addr
                ldy addr+1
                .endproc


;======================================
;   GProp(offset, addr)
;======================================
gprop           .proc
                stx props
                sty props+1
                ldx props+1
                clc
                adc props
                bcc _gp1

                inx
_gp1            sec
                ldy #0
                adc (props),Y
                sta props
                bcc _gp2

                inx
_gp2            stx props+1
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
                sta adress
                stx adress+1
                jmp (adress)

                .endproc
