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
                .frsGraphics mcTextOn,mcVideoMode640    ; 80x60 text mode
                stz DINDEX              ; text mode

                ; lda #0
                ; jsr close             ; close #0, sets X to 0

                ; lda #$0c
                ; sta arg3
                ; lda #0
                ; ldx #<scred
                ; ldy #>scred
                ; jmp Open

;--------------------------------------

; scred           .text 2,"E:",$9b
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

tokPLUS         = 1
tokMINUS        = 2
tokMULT         = 3
tokDIVD         = 4
tokOR           = 5
tokAND          = 6
tokEQU          = 7
tokNOTEQU       = 8
tokGRTR         = 9
tokGRTREQU      = 10
tokLESS         = 11
tokLESSEQU      = 12
tokREM          = 13
tokXOR          = 14
tokLSH          = 15
tokRSH          = 16
tokUMINUS       = 17
tokAT           = 18

tokSColon       = 21
tokSQuote       = 22
tokPeriod       = 23
tokRParen       = 24
tokLParen       = 25
tokComma        = 26
tokDef          = 27
tokDigit        = 28+$80
tokHex          = 29
tokQuote        = 30

tokCHAR         = 32
tokBYTE         = 33
tokINT          = 34
tokCARD         = 35
tokSTRING       = 36
tokREAL         = 37
tokDEFINE       = 38
tokRECORD       = 39

tokARRAY        = 64
tokFUNC         = 65
tokPROC         = 66
tokGET          = 67
tokSET          = 68
tokPOINTER      = 69
tokTYPE         = 70

tokIF           = 80
tokWHILE        = 81
tokRET          = 82
tokEXIT         = 83
tokFOR          = 84
tokCASE         = 85
tokCode         = 86
tokMOD          = 87
tokUNTIL        = 88

tokLBracket     = 91
tokRBracket     = 93
tokUpArrow      = 94

tokTHEN         = 96
tokELSE         = 97
tokDO           = 98
tokFI           = 99
tokOD           = 100
tokTO           = 101
tokSTEP         = 102
tokOF           = 103
tokESAC         = 104
tokEDOC         = 105
tokELSEIF       = 106
tokDOWNTO       = 107

tokTYPE_t       = $70                   ; 112

tokEOF          = 127

tokCONST_t      = $80
tokVAR_t        = $88
tokARRAY_t      = $90
tokTEMP_t       = $A8
tokFUNC_t       = $C0
tokCOND_t       = $48

;    types
;    -----
tokCHAR_t       = 1
tokBYTE_t       = 2
tokINT_t        = 3
tokCARD_t       = 4
tokSTR_t        = 5
tokREAL_t       = 6

tokUNDEC        = $88


;========================
;    Error codes
;========================

; 0 - out of system memory
strERR          = 1                     ; missing " at end
dfnERR          = 2                     ; nested defines
symtblERR           = 3                 ; too many qglobal st
; 4 - too many local st entries
setERR          = 5                     ; SET syntax error
declERR          = 6                    ; declaration error
argERR          = 7                     ; bad argument list
varERR          = 8                     ; var not declared
constERR        = 9                     ; not int const
assgnERR        = 10                    ; bad assignment
endERR          = 11                    ; unknown error
thenERR         = 12                    ; missing THEN
fiERR           = 13                    ; missing FI
qcodeERR           = 14                 ; out of qcode space
doERR           = 15                    ; missing DO
toERR           = 16                    ; missing TO
exprERR          = 17                   ; bad expression
parenthERR        = 18                  ; unmatched ()s
odERR           = 19                    ; missing OD
allocateERR     = 20                    ; can't alloc mem.
arrayERR          = 21                  ; bad array ref.
; 22 - file too large on input
condtERR         = 23                   ; illegal cond. exp.
forERR          = 24                    ; illegal FOR stmt
exitERR         = 25                    ; no loop for EXIT
nestERR         = 26                    ; nesting level too deep
typERR          = 27                    ; illegal type ref.
retrnERR          = 28                  ; illegal RETURN stmt.
;61 - out of st space
brkERR          = $80                   ; Break key depressed


;======================================
;    Compiler lexicon - get tokens
;======================================

;======================================
;   GetNext()
;======================================
GetNext         .proc
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

getnl0          cmp #tokEOF
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
                bra _getnr0

_getnl1         jsr alpha
                bne _getnid

                cmp #'['
                beq getnr1

                cmp #'^'
                beq getnr1

                cmp #']'
                beq getnr1
                bra getnloop

_getnid         jsr GetName
                bmi getnr1

_getnr0         sta nxttoken
                ldx #<lexcmd
                ldy #>lexcmd
                jmp lookup

getnr1          sta nxttoken

;GetNr2 lda $D0
;       beq GetNr3
;       jsr PrintTok

getnr2          ldx addr
                ldy addr+1
ismt            lda token
                rts
                .endproc


;======================================
;   LexCom()
;======================================
LexCom          .proc
                jsr NextLine
                bra GetNext.getnl0

                .endproc


;======================================
;   LexDig()
;======================================
LexDig          .proc
                lda #tokCONST_t+tokINT_t
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

cnsterr         ldy #constERR
                jmp splerr

ldig2           dey
                sty choff
ldig3           sta nxtaddr
                stx nxtaddr+1
                cpx #0
                bne GetNext.getnr2

                lda #tokCONST_t+tokBYTE_t
                bne GetNext.getnr1

ldig4           lda #tokCONST_t+tokREAL_t
                sta nxttoken
                ldy CIX
                ldx #$ff                ; for SET cmd
                bne ldig2

                .endproc


;======================================
;   LexChr()
;======================================
LexChr          .proc
                jsr nextchar

                sta nxtaddr
                lda #tokCONST_t+tokCHAR_t
                bne GetNext.getnr1

                .endproc


;======================================
;   LexNE()
;======================================
LexNE           .proc
                jsr nextchar

                cmp #'>'
                bne LexEq.leq1

                lda #tokNOTEQU
                bra GetNext.getnr1

                .endproc


;======================================
;   LexEq()
;======================================
LexEq           .proc
                jsr nextchar

leq1            cmp #'='
                bne putback

                inc nxttoken
                bra GetNext.getnr2

                .endproc


;======================================
;   LexHex()
;======================================
LexHex          .proc
                lda #tokCONST_t+tokCARD_t
                sta nxttoken
                inc choff
                jsr lexbuf
                jsr htocar
                bra LexDig.ldig2

                .endproc


;======================================
;   PutBack returns character to buf
;======================================
putback         .proc
                dec choff
pback           jmp GetNext.getnr2

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
                bra putback.pback

                .endproc


;======================================
;   LexStr()
;======================================
lexstr          .proc
                lda token
                cmp #tokQuote
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
                lda Channel
                bpl _lstr1              ; if not EOF

_lstr3          ldy #strERR
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
                jmp LexDig.ldig2

                .endproc


;======================================
;   NextChar()
;======================================
nextchar        .proc
                ldy defflg
                bne lexdef

nxtch0          ldy choff
                cpy sp
                bcc NextLine._nxtch1

                .endproc

                ;[fall-through]


;======================================
;   NextLine()
;======================================
NextLine        .proc
                lda Channel
                beq _1
                bmi _4                  ; eof

                jsr ReadBuffer
                bpl _2

                cpy #$88                ; EOF
                beq _next1
                jmp splerr

_next1          dec Channel
                bne NextLine

_1              ldy top+1
                beq _next1              ; set eof, tricky qcode

                jsr ldbuf

                lda cur
                sta curnxt
                ldx cur+1
                stx curnxt+1
                jsr nextdwn
                bne _2

        ;    lda #0
                sta top+1

_2              lda list
                beq _3                  ; don't list

                lda device
                jsr WriteBuffer

_3              ldy #0
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

_4              lda #tokEOF
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
                bra nextchar.nxtch0

ldef1           ldy choff
                lda (delnxt),y
                rts
                .endproc


;======================================
;   LexGet()
;======================================
lexget          .proc
                jsr GetNext.getnloop

lget            lda #0
                sta defflg
                inc Channel
                lda #4
                jsr openchan
                jsr NextLine

                jmp GetNext.getnl0

                .endproc


;======================================
;   LexSet()
;======================================
lexset          .proc
                jsr getadr

                sta arg11
                stx arg12

                jsr GetNext.getnloop

                lda nxttoken
                cmp #tokEQU
                bne lseterr

                jsr getadr

                ldy #0
                sta (arg11),y
                txa
                beq lset1

                iny
                sta (arg11),y
lset1           jmp GetNext.getnloop

lseterr         ldy #setERR
                jmp splerr

getadr          jsr GetNext.getnloop

                jmp mnum

                .endproc


;======================================
;   LexExpand()
;======================================
lexexpand       .proc
                lda defflg
                beq lexp

                ldy #dfnERR
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
                jmp GetNext.getnloop

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

lexcmd          .addr GetNext.getnr2
                .byte 41
                .addr LexDig
                .byte tokDigit-$80
                .addr LexHex
                .byte tokHex
                .addr LexEq
                .byte tokGRTR
                .addr LexNE
                .byte tokLESS
                .addr lexexpand
                .byte tokDef
                .addr LexCom
                .byte tokSColon
                .addr LexChr
                .byte tokSQuote
                .addr lexpf
                .byte tokPROC
                .addr lexpf
                .byte tokFUNC
                .addr lexpf
                .byte tokMOD
                .addr lexstr
                .byte tokQuote
                .addr lexget
                .byte tokGET
                .addr lexset
                .byte tokSET

lexchars
                .byte tokXOR            ; !
                .byte tokQuote+$80      ; "
                .byte tokNOTEQU         ; #
                .byte tokHex+$80        ; $
                .byte tokOR             ; %
                .byte tokAND            ; &
                .byte tokSQuote+$80     ; '
                .byte tokLParen
                .byte tokRParen
                .byte tokMULT
                .byte tokPLUS
                .byte tokComma
                .byte tokMINUS
                .byte tokPeriod         ; .
                .byte tokDIVD+$80       ; /
                .byte tokDigit,tokDigit,tokDigit,tokDigit
                .byte tokDigit,tokDigit,tokDigit,tokDigit
                .byte tokDigit,tokDigit ; 0 thru 9
                .byte 0                 ; :
                .byte tokSColon+$80     ; ;
                .byte tokLESS+$80       ; low
                .byte tokEQU            ; =
                .byte tokGRTR+$80       ; high
                .byte 126               ; ?
                .byte tokAT             ; @

;PrintTok lda token
;         ldx #0
;         jsr PrintC
;         jsr PutSp
;         lda addr
;         ldx addr+1
;         jsr PrintH
;         jsr PutSp
;         lda nxtToken
;         ldx #0
;         jsr PrintC
;         jsr PutSp
;         lda nxtAddr
;         ldx nxtAddr+1
;         jsr PrintH
;         jmp PutEOL
