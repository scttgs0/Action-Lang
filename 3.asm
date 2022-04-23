;===========================
;         SCREEN.MAC
;===========================
; Copyright 1982 by Clinton W Parker
; All Rights Reserved
; last modified October 18, 1982
;
; This file is part of Action!.
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

scrinit         .proc
                lda #0
                jsr close               ; close #0, sets X to 0
                lda #$0c
                sta arg3
                lda #0
                ldx #<scred
                ldy #>scred
                jmp open

scred           .text 2,"E:",$9b
                .endproc


;     ScrCh(char)
;    ------------
; outputs char to screen.  Char passed in A reg.
; Control characters are ignored.

scrch           .proc
                tay
                lda #0
scrchar         ldx #1
                bne putch.putch1
                .endproc


;    PutCh(char)
;    -----------
; outputs char to screen.  Char passed in A reg.
; Processes control characters.

putch           .proc
                tay
                lda #0
                tax
putch1          stx $02fe               ; DSPFLG
                asl a
                asl a
                asl a
                asl a
                tax
                lda #$0b                ; PUTCHR
putch2          sta $0342,x             ; ICCOM
                lda #0
                sta $0348,x             ; ICBLL
                sta $0349,x             ; ICBLH
                tya
                jmp $e456               ; CIOV
                .endproc


;    ScrUp() - Move cursor up one
;    ----------------------------
scrup           .proc
                lda #$1c
                bne putch
                .endproc


;    ScrDwn() - Move cursor down one
;    -------------------------------
scrdwn          .proc
                lda #$1d
                bne putch
                .endproc


;    ScrBell() - Bell Char
;    ---------------------
scrbell         .proc
                lda #$fd
                bne putch
                .endproc


;    ScrLft() - Move cursor left one
;    -------------------------------
scrlft          .proc
                lda #$1e
                bne putch
                .endproc


;    ScrRt() - Move cursor right one
;    -------------------------------
scrrt           .proc
                lda #$1f
                bne putch
                .endproc


;=============================
;    Compiler tokens
;=============================

plusid          = 1
minusid         = 2
timesid         = 3
divideid        = 4
orid            = 5
andid           = 6
equalid         = 7
neid            = 8
grid            = 9
geid            = 10
lsid            = 11
leid            = 12
remid           = 13
xorid           = 14
lshid           = 15
rshid           = 16
uminusid        = 17
atid            = 18

scolon          = 21
squote          = 22
period          = 23
rparen          = 24
lparen          = 25
comma           = 26
defid           = 27
digit           = 28+$80
hex             = 29
quote           = 30

char            = 32
byte            = 33
int             = 34
card            = 35
string          = 36
real            = 37
define          = 38
record          = 39

array           = 64
func            = 65
proc            = 66
get             = 67
set             = 68
pointer         = 69
typeid          = 70

ifid            = 80
whileid         = 81
retid           = 82
exitid          = 83
forid           = 84
caseid          = 85
codeid          = 86
modid           = 87
untilid         = 88

lbrack          = 91
rbrack          = 93
uparrow         = 94

then            = 96
else            = 97
do              = 98
fi              = 99
od              = 100
to              = 101
step            = 102
of              = 103
esac            = 104
edoc            = 105
elseif          = 106
downto          = 107

typet           = $70                   ; 112

eofid           = 127

constt          = $80
vart            = $88
arrayt          = $90
tempt           = $a8
funct           = $c0
condt           = $48

;    types
;    -----
chart           = 1
bytet           = 2
intt            = 3
cardt           = 4
strt            = 5
realt           = 6

undec           = $88


;========================
;    Error codes
;========================

; 0 - out of system memory
strer           = 1                     ; missing " at end
dfner           = 2                     ; nested defines
ster            = 3                     ; too many qglobal st
; 4 - too many local st entries
seter           = 5                     ; SET syntax error
dcler           = 6                     ; declaration error
arger           = 7                     ; bad argument list
varer           = 8                     ; var not declared
conster         = 9                     ; not int const
asser           = 10                    ; bad assignment
ender           = 11                    ; unknown error
thener          = 12                    ; missing THEN
fier            = 13                    ; missing FI
cder            = 14                    ; out of qcode space
doer            = 15                    ; missing DO
toer            = 16                    ; missing TO
exper           = 17                    ; bad expression
parer           = 18                    ; unmatched ()s
oder            = 19                    ; missing OD
alcer           = 20                    ; can't alloc mem.
arrer           = 21                    ; bad array ref.
; 22 - file too large on input
cnder           = 23                    ; illegal cond. exp.
forer           = 24                    ; illegal FOR stmt
exiter          = 25                    ; no loop for EXIT
nster           = 26                    ; nesting level too deep
typer           = 27                    ; illegal type ref.
reter           = 28                    ; illegal RETURN stmt.
;61 - out of st space
brker           = $80                   ; Break key depressed


;============================================
;    Compiler lexicon - get tokens
;============================================

;    GetNext()
;    ---------
getnext         .proc
                lda spnxt
                ldx curnxt
                ldy curnxt+1
                stx curln
                sty curln+1
                sta spln

                lda token
                sta lsttoken
                ldx nxtaddr
                ldy nxtaddr+1
                lda nxttoken
                stx addr
                sty addr+1
                sta token
getnloop
                jsr nextchar
getnl0          cmp #eofid
                beq getnr1

                cmp #'!'
                bcc getnloop
    ; save line index for debugging
                ldx choff
                stx spnxt

                cmp #'A'
                bcs _getnl1
                tay
                lda lexchars-33,y
                beq getnloop
                bpl getnr1
                and #$7f
                bne _getnr0              ; uncond.

_getnl1         jsr alpha
                bne _getnid

                cmp #'['
                beq getnr1
                cmp #'^'
                beq getnr1
                cmp #']'
                beq getnr1
                bne getnloop            ; uncond.

_getnid         jsr getname
                bmi getnr1

_getnr0         sta nxttoken
                ldx #<lexcmd
                ldy #>lexcmd
                jmp lookup

getnr1          sta nxttoken

;GetNr2 LDA $D0
; BEQ GetNr3
; JSR PrintTok

getnr2          ldx addr
                ldy addr+1
ismt            lda token
                rts
                .endproc


;    LexCom()
;    --------
lexcom          .proc
                jsr nextline
                bne getnext.getnl0      ; uncond.
                .endproc


;    LexDig()
;    --------
lexdig          .proc
                lda #constt+intt
                sta nxttoken
                jsr lexbuf              ; get buf ptr
                jsr storeal
ldig1           jsr nextchar            ; cardinal?
                jsr alphanum._num
                bne ldig1
                cmp #'.'
                beq ldig4
                cmp #'E'
                beq ldig4
                dec choff
                jsr rtocar
                bcc ldig3

cnsterr         ldy #conster
                jmp splerr

ldig2           dey
                sty choff
ldig3           sta nxtaddr
                stx nxtaddr+1
                cpx #0
                bne getnext.getnr2
                lda #constt+bytet
                bne getnext.getnr1

ldig4           lda #constt+realt
                sta nxttoken
                ldy cix
                ldx #$ff                ; for SET cmd
                bne ldig2
                .endproc


;    LexChr()
;    --------
lexchr          .proc
                jsr nextchar
                sta nxtaddr
                lda #constt+chart
                bne getnext.getnr1
                .endproc


;    LexNE()
;    -------
lexne           .proc
                jsr nextchar
                cmp #'>'
                bne lexeq.leq1
                lda #neid
                bne getnext.getnr1      ; uncond.
                .endproc


;    LexEq()
;    -------
lexeq           .proc
                jsr nextchar
leq1            cmp #'='
                bne putback
                inc nxttoken
                bne getnext.getnr2      ; uncond.
                .endproc


;    LexHex()
;    --------
lexhex          .proc
                lda #constt+cardt
                sta nxttoken
                inc choff
                jsr lexbuf
                jsr htocar
                bne lexdig.ldig2        ; uncond.
                .endproc


;    PutBack returns character to buf
;    --------------------------------
putback         .proc
                dec choff
pback           jmp getnext.getnr2
                .endproc


;    LexPF()
;    -------
lexpf           .proc
                lda qglobal
                beq putback.pback
                lda #0
                sta qglobal

                lda gbase               ; restore qglobal base
                sta symtab
                lda gbase+1
                sta symtab+1
                bne putback.pback       ; uncond.
                .endproc


;    LexStr()
;    --------
lexstr          .proc
                lda token
                cmp #quote
                beq putback.pback       ; zap local st
                lda #0
                sta arg9
_lstr1          jsr nextchar
                inc arg9
                beq _lstr3              ; string too long
                cmp #'"'
                beq _lstr4
_lstr2          ldy arg9
                sta (symtab),y
                lda chan
                bpl _lstr1              ; if not EOF
_lstr3          ldy #strer
                jmp splerr

_lstr4          jsr nextchar
                cmp #'"'
                beq _lstr2              ; " in string
                                        ; end of string
                ldy arg9
                lda #eol
                sta (symtab),y
                dey
                tya
                ldy #0
                sta (symtab),y          ; save size
                lda symtab
                ldx symtab+1
                ldy choff
                dey
                jmp lexdig.ldig2
                .endproc


;    NextChar()
;    ----------
nextchar        .proc
                ldy defflg
                bne lexdef

nxtch0          ldy choff
                cpy sp
                bcc nextline._nxtch1
                .endproc


;    NextLine()
;    ----------
nextline        .proc
                lda chan
                beq _nln1
                bmi _nln4               ; eof

                jsr rdbuf
                bpl _nln2

                cpy #$88                ; EOF
                beq _nln0
                jmp splerr

_nln0           dec chan
                bne nextline

_nln1           ldy top+1
                beq _nln0               ; set eof, tricky qcode
                jsr ldbuf
                lda cur
                sta curnxt
                ldx cur+1
                stx curnxt+1
                jsr nextdwn
                bne _nln2
;    LDA #0
                sta top+1

_nln2           lda list
                beq _nln3               ; don't list
                lda device
                jsr wrtbuf
_nln3           ldy #0
                sty choff
                lda (buf),y
                tay
                iny
                sty sp
                lda #eol
                sta (buf),y
                ldy #0

_nxtch1         iny
                lda (buf),y
                sty choff
                rts

_nln4           lda #eofid
                rts
                .endproc


lexdef          .proc
                ldy #0
                lda (delnxt),y
                inc choff
                cmp choff
                bcs ldef1
                lda defflg
                sta choff
                sty defflg
                bcc nextchar.nxtch0     ; uncond.

ldef1           ldy choff
                lda (delnxt),y
                rts
                .endproc


;    LexGet()
;    --------
lexget          .proc
                jsr getnext.getnloop
lget            lda #0
                sta defflg
                inc chan
                lda #4
                jsr openchan
                jsr nextline
                jmp getnext.getnl0
                .endproc


;    LexSet()
;    --------
lexset          .proc
                jsr getadr
                sta arg11
                stx arg12

                jsr getnext.getnloop
                lda nxttoken
                cmp #equalid
                bne lseterr

                jsr getadr
                ldy #0
                sta (arg11),y
                txa
                beq lset1
                iny
                sta (arg11),y
lset1           jmp getnext.getnloop

lseterr         ldy #seter
                jmp splerr

getadr          jsr getnext.getnloop
                jmp mnum
                .endproc


;    LexExpand()
;    -----------
lexexpand       .proc
                lda defflg
                beq lexp
                ldy #dfner
                jmp splerr

lexp            lda #3
                jsr nxtprop
                lda props
                ldx props+1
                jsr rstp
                ldy choff
lexp1           sta delnxt
                stx delnxt+1
                sty defflg
                lda #0
                sta choff
                jmp getnext.getnloop
                .endproc


;    LexBuf()
;    --------
lexbuf          .proc
                ldy choff
                lda defflg
                beq _lbuf
                lda delnxt
                ldx delnxt+1
                rts
_lbuf           lda buf
                ldx buf+1
                rts
                .endproc


lexcmd          .word getnext.getnr2
                .byte 41
                .word lexdig
                .byte digit-$80
                .word lexhex
                .byte hex
                .word lexeq
                .byte grid
                .word lexne
                .byte lsid
                .word lexexpand
                .byte defid
                .word lexcom
                .byte scolon
                .word lexchr
                .byte squote
                .word lexpf
                .byte proc
                .word lexpf
                .byte func
                .word lexpf
                .byte modid
                .word lexstr
                .byte quote
                .word lexget
                .byte get
                .word lexset
                .byte set

lexchars
                .byte xorid             ; !
                .byte quote+$80         ; "
                .byte neid              ; #
                .byte hex+$80           ; $
                .byte orid              ; %
                .byte andid             ; &
                .byte squote+$80        ; '
                .byte lparen
                .byte rparen
                .byte timesid
                .byte plusid
                .byte comma
                .byte minusid
                .byte period            ; .
                .byte divideid+$80      ; /
                .byte digit,digit,digit,digit
                .byte digit,digit,digit,digit
                .byte digit,digit       ; 0 thru 9
                .byte 0                 ; :
                .byte scolon+$80        ; ;
                .byte lsid+$80          ; low
                .byte equalid           ; =
                .byte grid+$80          ; high
                .byte 126               ; ?
                .byte atid              ; @

;PrintTok LDA token
; LDX #0
; JSR PrintC
; JSR PutSp
; LDA addr
; LDX addr+1
; JSR PrintH
; JSR PutSp
; LDA nxtToken
; LDX #0
; JSR PrintC
; JSR PutSp
; LDA nxtAddr
; LDX nxtAddr+1
; JSR PrintH
; JMP PutEOL


;========================
;         MAIN.MSC
;=========================

;    LShift(val, cnt)
;    ----------------
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
_lsh1           asl a
                rol _c
                dey
                bne _lsh1
                ldx _c
_lshret         rts
                .endproc


;    NextUp()
;    --------
nextup          .proc
                ldy #1
                bne next                ; uncond.
                .endproc


;    NextDwn()
;    ---------
nextdwn         .proc
                ldy #5
                .endproc


;    Next(,,dir)
;    -----------
next            .proc
                jsr chkcur
                beq next1
                lda (cur),y
                beq next1
                tax
                dey
                lda (cur),y
                sta cur
                txa
                sta cur+1
next1           rts
                .endproc


;    CurStr()
;    --------
curstr          .proc
                lda cur
                ldx cur+1
                .endproc


;    StrPtr()
;    --------
strptr          .proc
                clc
                adc #6
                sta arg0
                bcc _sp1
                inx
_sp1            stx arg1
                rts
                .endproc


;    MNum()
;    ------
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
                bne _mn1                ; uncond.

_mn2b           ldy #0
                cmp #'^'
                bne _mn3
                lda (afsize),y
                tax
                iny
                lda (afsize),y
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
                bne _mn8                ; uncond.

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
                bne _mn6                ; uncond.

_mnundec        lda #1
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
                jmp _mnv1

_varerr         ldy #varer
_adrerr         jmp splerr

_mn9            pla                     ; end of table
                tax
                pla
                jmp _mn2
                .endproc


;    GetConst(token)
;    ---------------
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
_cs1            lda (nxtaddr),y
                sta (qcode),y
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


;    GetCdOff()
;    ----------
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


;    StoreVar(low, high, index)
;    --------------------------
storevar        .proc
                sta (qcode),y
                iny
                txa
                sta (qcode),y
                rts
                .endproc


lookup          .proc
                sty arg2
        .if ramzap
                sta (arg1),y            ; zap RAM if any
        .else
                nop
                nop
        .endif

lu1             stx arg1
                tax
                ldy #2
                lda (arg1),y
                tay
                txa
_fml1           cmp (arg1),y
                beq _fmjmp
                dey
                dey
                dey
                cpy #2
                bne _fml1

_fmjmp          dey
                lda (arg1),y
                sta arg4
                dey
                lda (arg1),y
                sta arg3
                jmp (arg3)
                .endproc


;    AlphaNum(char)
;    --------------
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


;    Alpha(char)
;    -----------
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


;    STIncr(size)
;    ------------
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


;    CodeIncr(size)
;    --------------
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


;    NxtProp(offset)
;    ---------------
nxtprop         .proc
                ldx props
                stx aflast
                ldx props+1
                stx aflast+1
                ldx nxtaddr
                ldy nxtaddr+1
                bne gprop
                .endproc


;    CProp(offset)
;    -------------
cprop           .proc
                ldx curproc
                ldy curproc+1
                bne gprop
                .endproc


;    GetProp(offset)
;    ---------------
getprop         .proc
                ldx addr
                ldy addr+1
                .endproc


;    GProp(offset, addr)
;    -------------------
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
                adc (props),y
                sta props
                bcc _gp2
                inx
_gp2            stx props+1
                iny
                lda (props),y
                tax
                dey
                lda (props),y
                rts
                .endproc


rstp            .proc
                ldy aflast
                sty props
                ldy aflast+1
                sty props+1
                rts
                .endproc


;    JSRInd(addr)
;    ------------
jsrind          .proc
                sta adress
                stx adress+1
                jmp (adress)
                .endproc


;=======================
;    MAIN.BNK
;=======================

en0             .text 5,"Error",$c0
                .word error
                .byte 3,138,138,138

en1             .text 3,"EOF",$9a
                .word eof

en2             .text 5,"color",$8a
                .word $02fd

en3             .text 4,"LIST",$8a
                .word list

en4             .text 6,"device",$8a
                .word device

en5             .text 5,"TRACE",$8a
                .word trace


;    CStrt()
;    -------
cstart          .proc
                ldy #ebank
                sty curbank
                sty bank+ebank
                jmp start
                .endproc


;    GetName(char)
;    -------------
getname         .proc
                sta bank+lbank
                jsr lgetname
                .endproc


;    RstBank()
;    ---------
rstbank         .proc
                php
                pha
                tya
                ldy curbank
rbank1          sta bank,y
                tay
                pla
                plp
init            rts
                .endproc


;    Run(address)
;    ------------
run             .proc                   ; reset Error routine
                ldy #<splerr
                sty error+1
                ldy #>splerr
                sty error+2
                jsr lproceed
                jsr jsrind
                jmp editbank
                .endproc


;    Compile()
;    ---------
compile         .proc
                ldy #cbank
                sty curbank
                sty bank+cbank
                jsr ccompile
                .endproc


;    EditBank()
;    ----------
editbank        .proc
                php
                pha
                tya
                ldy #ebank
                sty curbank
                jmp rstbank.rbank1
                .endproc


;    GetAlias()
;    ----------
getalias        .proc
                lda #1
                jsr getprop
                cpx #0
                beq _gal1
                sta addr
                stx addr+1
                sta bank+lbank
                lda #0
                jsr getprop
                sta token
                jmp rstbank

_gal1           jmp mnum._varerr
                .endproc


;    GNlocal()
;    ---------
gnlocal         .proc
                sta bank+lbank
                jsr lgetname.lgnlocal
                jmp rstbank
                .endproc


;    CStmtList()
;    -----------
cstmtlst        .proc
                ldy #cbank
                sty curbank
                sta bank+cbank
                jsr stmtlist
                jmp editbank
                .endproc


mgett1          .proc
                jsr editbank
                jsr gettemp.gett1
                .endproc


;    LProceed()
;    ----------
lproceed        .proc
                ldy #lbank
                sty curbank
                sty bank+lbank
                rts
                .endproc



options         .proc
                jsr lproceed
                jsr setopts
                jmp editbank
                .endproc


getkey          .proc
                sta bank+lbank
                jsr lgetkey
                jmp rstbank
                .endproc


splerr          .proc
                sta bank+lbank
                jmp lsplerr
                .endproc


emloop          .proc
                jsr editbank
                jmp monitor._mloop
                .endproc


getargs         .proc
                pha                     ; save arg type load flag
                sty bank+lbank
                lda #1
                jsr getprop
                sta addr
                stx addr+1

                pla
                bne _ga2                ; don't load arg types

                sta abt                 ; A=0
                sta abt+1               ; flag as temp args
                sta abt+2               ; (default)

                ldy #2
                lda (props),y
                sta numargs
                beq _ga2
                tax
                cpx #9
                bcs _ga2
_ga1            iny
                lda (props),y
                dex
                sta argtypes,x          ; args inverted
                bne _ga1
_ga2            jmp rstbank
                .endproc


prth            .proc                   ; call only from LBANK!
                sty bank+ebank
                jsr printh
                sty bank+lbank
                jmp chkerr
                .endproc


; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;------------------------------------
dret            .proc                   ; Dret()
                jsr lproceed
                jmp (dosvec)
                .endproc
