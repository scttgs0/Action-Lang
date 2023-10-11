
;======================================
;   FILE: screen.mac.asm
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
scrinit         .proc
                lda #0
                jsr close               ; close #0, sets X to 0

                lda #$0C
                sta arg3

                lda #0
                ldx #<_data
                ldy #>_data

                jmp open

;--------------------------------------

_data           .text 2,"E:",$9B

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
_ENTRY1         ldx #1
                bne putch._ENTRY1       ; [unc]

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
_ENTRY1         stx DSPFLG

                asl
                asl
                asl
                asl

                tax
                lda #$0B                ; PUTCHR
_ENTRY2         sta IOCB0+ICCOM,x

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
                lda #$1C
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrDwn() - Move cursor down one
;======================================
scrdwn          .proc
                lda #$1D
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrBell() - Bell Char
;======================================
scrbell         .proc
                lda #$FD
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrLft() - Move cursor left one
;======================================
scrlft          .proc
                lda #$1E
                bne putch               ; [unc]

                .endproc


;======================================
;   ScrRt() - Move cursor right one
;======================================
scrrt           .proc
                lda #$1F
                bne putch               ; [unc]

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
tempt           = $A8
funct           = $C0
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

_ENTRY1         jsr nextchar

_ENTRY2         cmp #eofid
                beq _ENTRY3

                cmp #'!'
                bcc _ENTRY1

;   save line index for debugging
                ldx choff
                stx spnxt

                cmp #'A'
                bcs _1

                tay
                lda lexchars-33,y
                beq _ENTRY1
                bpl _ENTRY3

                and #$7F
                bne _3                  ; [unc]

_1              jsr alpha
                bne _2

                cmp #'['
                beq _ENTRY3

                cmp #'^'
                beq _ENTRY3

                cmp #']'
                beq _ENTRY3
                bne _ENTRY1             ; [unc]

_2              jsr getname
                bmi _ENTRY3

_3              sta nxttoken

                ldx #<lexcmd
                ldy #>lexcmd
                jmp lookup

_ENTRY3         sta nxttoken

;GetNr2         lda $D0
;               beq GetNr3
;               jsr PrintTok

_ENTRY4         ldx addr
                ldy addr+1

_ENTRY5         lda token

                rts
                .endproc


;======================================
;   LexCom()
;======================================
lexcom          .proc
                jsr nextline
                bne getnext._ENTRY2      ; [unc]

                .endproc


;======================================
;   LexDig()
;======================================
lexdig          .proc
                lda #constt+intt
                sta nxttoken

                jsr lexbuf              ; get buf ptr
                jsr storeal

_next1          jsr nextchar            ; cardinal?
                jsr alphanum._num
                bne _next1

                cmp #'.'
                beq _2

                cmp #'E'
                beq _2

                dec choff

                jsr rtocar
                bcc _1

_err            ldy #conster
                jmp splerr

_ENTRY1         dey
                sty choff

_1              sta nxtaddr
                stx nxtaddr+1

                cpx #0
                bne getnext._ENTRY4

                lda #constt+bytet
                bne getnext._ENTRY3

_2              lda #constt+realt
                sta nxttoken

                ldy cix
                ldx #$FF                ; for SET cmd
                bne _ENTRY1               ; [unc]

                .endproc


;======================================
;   LexChr()
;======================================
lexchr          .proc
                jsr nextchar

                sta nxtaddr

                lda #constt+chart
                bne getnext._ENTRY3      ; [unc]

                .endproc


;======================================
;   LexNE()
;======================================
lexne           .proc
                jsr nextchar

                cmp #'>'
                bne lexeq._ENTRY1

                lda #neid
                bne getnext._ENTRY3     ; [unc]

                .endproc


;======================================
;   LexEq()
;======================================
lexeq           .proc
                jsr nextchar

_ENTRY1         cmp #'='
                bne putback

                inc nxttoken
                bne getnext._ENTRY4     ; [unc]

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
                bne lexdig._ENTRY1      ; [unc]

                .endproc


;======================================
;   PutBack returns character to buf
;======================================
putback         .proc
                dec choff

_ENTRY1         jmp getnext._ENTRY4

                .endproc


;======================================
;   LexPF()
;======================================
lexpf           .proc
                lda qglobal
                beq putback._ENTRY1

                lda #0
                sta qglobal

                lda gbase               ; restore qglobal base
                sta symtab
                lda gbase+1
                sta symtab+1

                bne putback._ENTRY1     ; [unc]

                .endproc


;======================================
;   LexStr()
;======================================
lexstr          .proc
                lda token
                cmp #quote
                beq putback._ENTRY1       ; zap local st

                lda #0
                sta arg9

_next1          jsr nextchar

                inc arg9
                beq _1                  ; string too long

                cmp #'"'
                beq _2

_next2          ldy arg9
                sta (symtab),y

                lda chan
                bpl _next1              ; if not EOF

_1              ldy #strer
                jmp splerr

_2              jsr nextchar

                cmp #'"'
                beq _next2              ; " in string
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
                jmp lexdig._ENTRY1

                .endproc


;======================================
;   NextChar()
;======================================
nextchar        .proc
                ldy defflg
                bne lexdef

_ENTRY1         ldy choff
                cpy sp
                bcc nextline._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   NextLine()
;======================================
nextline        .proc
                lda chan
                beq _1
                bmi _4                  ; eof

                jsr rdbuf
                bpl _2

                cpy #$88                ; EOF
                beq _next1

                jmp splerr

_next1          dec chan
                bne nextline

_1              ldy top+1
                beq _next1              ; set eof, tricky qcode

                jsr ldbuf

                lda cur
                sta curnxt
                ldx cur+1
                stx curnxt+1

                jsr nextdwn
                bne _2

;               lda #0
                sta top+1

_2              lda list
                beq _3                  ; don't list

                lda device
                jsr wrtbuf

_3              ldy #0
                sty choff
                lda (buf),y

                tay
                iny
                sty sp

                lda #eol
                sta (buf),y

                ldy #0
_ENTRY1         iny
                lda (buf),y
                sty choff

                rts

_4              lda #eofid

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
                bcs _1

                lda defflg
                sta choff
                sty defflg
                bcc nextchar._ENTRY1    ; [unc]

_1              ldy choff
                lda (delnxt),y

                rts
                .endproc


;======================================
;   LexGet()
;======================================
lexget          .proc
                jsr getnext._ENTRY1

_ENTRY1         lda #0
                sta defflg

                inc chan

                lda #4
                jsr openchan
                jsr nextline

                jmp getnext._ENTRY2

                .endproc


;======================================
;   LexSet()
;======================================
lexset          .proc
                jsr _1

                sta arg11
                stx arg12

                jsr getnext._ENTRY1

                lda nxttoken
                cmp #equalid
                bne _err

                jsr _1

                ldy #0
                sta (arg11),y

                txa
                beq _XIT1

                iny
                sta (arg11),y

_XIT1           jmp getnext._ENTRY1

_err            ldy #seter
                jmp splerr

_1              jsr getnext._ENTRY1
                jmp mnum

                .endproc


;======================================
;   LexExpand()
;======================================
lexexpand       .proc
                lda defflg
                beq _1

                ldy #dfner
                jmp splerr

_1              lda #3
                jsr nxtprop

                lda props
                ldx props+1
                jsr rstp

                ldy choff
_ENTRY1         sta delnxt

                stx delnxt+1
                sty defflg

                lda #0
                sta choff

                jmp getnext._ENTRY1

                .endproc


;======================================
;   LexBuf()
;======================================
lexbuf          .proc
                ldy choff
                lda defflg
                beq _1

                lda delnxt
                ldx delnxt+1

                rts

_1              lda buf
                ldx buf+1

                rts
                .endproc

;--------------------------------------
;--------------------------------------

lexcmd          .word getnext._ENTRY4
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

lexchars        .byte xorid             ; !
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

;PrintTok       lda token
;               ldx #0
;               jsr PrintC
;               jsr PutSp
;               lda addr
;               ldx addr+1
;               jsr PrintH
;               jsr PutSp
;               lda nxtToken
;               ldx #0
;               jsr PrintC
;               jsr PutSp
;               lda nxtAddr
;               ldx nxtAddr+1
;               jsr PrintH
;               jmp PutEOL
