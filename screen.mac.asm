
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: screen.mac.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;
;======================================
scrinit         .proc
                lda #0
                jsr close               ; close #0, sets X to 0

                lda #$0c
                sta arg3
                lda #0
                ldx #<scred
                ldy #>scred
                jmp open

;--------------------------------------

scred           .text 2,"E:",$9b
                .endproc


;======================================
;     ScrCh(char)
;    ------------
; outputs char to screen.  Char passed in A reg.
; Control characters are ignored.
;======================================
scrch           .proc
                tay
                lda #0
scrchar         ldx #1
                bne putch.putch1

                .endproc


;======================================
;    PutCh(char)
;    -----------
; outputs char to screen.  Char passed in A reg.
; Processes control characters.
;======================================
putch           .proc
                tay
                lda #0
                tax
putch1          stx DSPFLG
                asl a
                asl a
                asl a
                asl a
                tax
                lda #$0b                ; PUTCHR
putch2          sta IOCB0+ICCOM,x
                lda #0
                sta IOCB0+ICBLL,x
                sta IOCB0+ICBLH,x
                tya
                jmp CIOV

                .endproc


;======================================
;   ScrUp() - Move cursor up one
;======================================
scrup           .proc
                lda #$1c
                bne putch

                .endproc


;======================================
;   ScrDwn() - Move cursor down one
;======================================
scrdwn          .proc
                lda #$1d
                bne putch

                .endproc


;======================================
;   ScrBell() - Bell Char
;======================================
scrbell         .proc
                lda #$fd
                bne putch

                .endproc


;======================================
;   ScrLft() - Move cursor left one
;======================================
scrlft          .proc
                lda #$1e
                bne putch

                .endproc


;======================================
;   ScrRt() - Move cursor right one
;======================================
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


;======================================
;    Compiler lexicon - get tokens
;======================================

;======================================
;   GetNext()
;======================================
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


;======================================
;   LexCom()
;======================================
lexcom          .proc
                jsr nextline
                bne getnext.getnl0      ; uncond.

                .endproc


;======================================
;   LexDig()
;======================================
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


;======================================
;   LexChr()
;======================================
lexchr          .proc
                jsr nextchar

                sta nxtaddr
                lda #constt+chart
                bne getnext.getnr1

                .endproc


;======================================
;   LexNE()
;======================================
lexne           .proc
                jsr nextchar

                cmp #'>'
                bne lexeq.leq1

                lda #neid
                bne getnext.getnr1      ; uncond.

                .endproc


;======================================
;   LexEq()
;======================================
lexeq           .proc
                jsr nextchar

leq1            cmp #'='
                bne putback

                inc nxttoken
                bne getnext.getnr2      ; uncond.

                .endproc


;======================================
;   LexHex()
;======================================
lexhex          .proc
                lda #constt+cardt
                sta nxttoken
                inc choff
                jsr lexbuf
                jsr htocar
                bne lexdig.ldig2        ; uncond.

                .endproc


;======================================
;   PutBack returns character to buf
;======================================
putback         .proc
                dec choff
pback           jmp getnext.getnr2

                .endproc


;======================================
;   LexPF()
;======================================
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


;======================================
;   LexStr()
;======================================
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


;======================================
;   NextChar()
;======================================
nextchar        .proc
                ldy defflg
                bne lexdef

nxtch0          ldy choff
                cpy sp
                bcc nextline._nxtch1

                .endproc


;======================================
;   NextLine()
;======================================
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


;======================================
;
;======================================
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


;======================================
;   LexGet()
;======================================
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


;======================================
;   LexSet()
;======================================
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


;======================================
;   LexExpand()
;======================================
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


;======================================
;   LexBuf()
;======================================
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

;--------------------------------------
;--------------------------------------

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
