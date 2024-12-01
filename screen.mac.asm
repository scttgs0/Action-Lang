
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: screen.mac.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;
;======================================
scrinit         .proc
                .frsGraphics mcTextOn,mcVideoMode240    ; 40x30 text mode
                stz DINDEX              ; text mode

                ; lda #0
                ; jsr close             ; close #0, sets X to 0

                ; lda #$0C
                ; sta arg3
                ; lda #0
                ; ldx #<_data
                ; ldy #>_data
                ; jmp Open

;--------------------------------------

; _data         .ptext "E:"
;               .byte $9B
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
_ENTRY1         ;!!stx DSPFLG

                asl
                asl
                asl
                asl

                tax
                lda #$0B                ; PUTCHR
_ENTRY2         ;!!sta IOCB0+ICCOM,X

                lda #0
                ;!!sta IOCB0+ICBLL,X
                ;!!sta IOCB0+ICBLH,X

                tya
                ;!!jmp CIOV

                .endproc


;======================================
;   ScrUp() - Move cursor up one
;======================================
scrup           .proc
                lda #$1C
                bra putch

                .endproc


;======================================
;   ScrDwn() - Move cursor down one
;======================================
scrdwn          .proc
                lda #$1D
                bra putch

                .endproc


;======================================
;   ScrBell() - Bell Char
;======================================
scrbell         .proc
                lda #$FD
                bra putch

                .endproc


;======================================
;   ScrLft() - Move cursor left one
;======================================
scrlft          .proc
                lda #$1E
                bra putch

                .endproc


;======================================
;   ScrRt() - Move cursor right one
;======================================
scrrt           .proc
                lda #$1F
                bra putch

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
symtblERR       = 3                     ; too many qglobal st
; 4 - too many local st entries
setERR          = 5                     ; SET syntax error
declERR         = 6                     ; declaration error
argERR          = 7                     ; bad argument list
varERR          = 8                     ; var not declared
constERR        = 9                     ; not int const
assgnERR        = 10                    ; bad assignment
endERR          = 11                    ; unknown error
thenERR         = 12                    ; missing THEN
fiERR           = 13                    ; missing FI
qcodeERR        = 14                    ; out of QCODE space
doERR           = 15                    ; missing DO
toERR           = 16                    ; missing TO
exprERR         = 17                    ; bad expression
parenthERR      = 18                    ; unmatched ()s
odERR           = 19                    ; missing OD
allocateERR     = 20                    ; can't alloc mem.
arrayERR        = 21                    ; bad array ref.
; 22 - file too large on input
condtERR        = 23                    ; illegal cond. exp.
forERR          = 24                    ; illegal FOR stmt
exitERR         = 25                    ; no loop for EXIT
nestERR         = 26                    ; nesting level too deep
typERR          = 27                    ; illegal type ref.
retrnERR        = 28                    ; illegal RETURN stmt.
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

_ENTRY1         jsr nextchar

_ENTRY2         cmp #tokEOF
                beq _ENTRY3

                cmp #'!'
                bcc _ENTRY1

;   save line index for debugging
                ldx choff
                stx spnxt

                cmp #'A'
                bcs _1

                tay
                lda lexchars-33,Y
                beq _ENTRY1
                bpl _ENTRY3

                and #$7F
                bra _3

_1              jsr alpha
                bne _2

                cmp #'['
                beq _ENTRY3

                cmp #'^'
                beq _ENTRY3

                cmp #']'
                beq _ENTRY3
                bra _ENTRY1

_2              jsr GetName
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
LexCom          .proc
                jsr NextLine
                bra GetNext._ENTRY2

                .endproc


;======================================
;   LexDig()
;======================================
LexDig          .proc
                lda #tokCONST_t+tokINT_t
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

_err            ldy #constERR
                jmp splerr

_ENTRY1         dey
                sty choff

_1              sta nxtaddr
                stx nxtaddr+1

                cpx #0
                bne GetNext._ENTRY4

                lda #tokCONST_t+tokBYTE_t
                bne GetNext._ENTRY3

_2              lda #tokCONST_t+tokREAL_t
                sta nxttoken

                ldy CIX
                ldx #$FF                ; for SET cmd
                bra _ENTRY1

                .endproc


;======================================
;   LexChr()
;======================================
LexChr          .proc
                jsr nextchar

                sta nxtaddr

                lda #tokCONST_t+tokCHAR_t
                bra GetNext._ENTRY3

                .endproc


;======================================
;   LexNE()
;======================================
LexNE           .proc
                jsr nextchar

                cmp #'>'
                bne LexEq._ENTRY1

                lda #tokNOTEQU
                bra GetNext._ENTRY3

                .endproc


;======================================
;   LexEq()
;======================================
LexEq           .proc
                jsr nextchar

_ENTRY1         cmp #'='
                bne putback

                inc nxttoken
                bra GetNext._ENTRY4

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
                bra LexDig._ENTRY1

                .endproc


;======================================
;   PutBack returns character to buf
;======================================
putback         .proc
                dec choff

_ENTRY1         jmp GetNext._ENTRY4

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

                bra putback._ENTRY1

                .endproc


;======================================
;   LexStr()
;======================================
lexstr          .proc
                lda token
                cmp #tokQuote
                beq putback._ENTRY1     ; zap local st

                lda #0
                sta arg9

_next1          jsr nextchar

                inc arg9
                beq _1                  ; string too long

                cmp #'"'
                beq _2

_next2          ldy arg9
                sta (symtab),Y

                lda Channel
                bpl _next1              ; if not EOF

_1              ldy #strERR
                jmp splerr

_2              jsr nextchar

                cmp #'"'
                beq _next2              ; " in string
                                        ; end of string

                ldy arg9
                lda #EOL
                sta (symtab),Y

                dey
                tya
                ldy #0
                sta (symtab),Y          ; save size

                lda symtab
                ldx symtab+1
                ldy choff

                dey
                jmp LexDig._ENTRY1

                .endproc


;======================================
;   NextChar()
;======================================
nextchar        .proc
                ldy defflg
                bne lexdef

_ENTRY1         ldy choff
                cpy sp
                bcc NextLine._ENTRY1

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
                beq _next1              ; set eof, tricky QCODE

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
                lda (buf),Y

                tay
                iny
                sty sp

                lda #EOL
                sta (buf),Y

                ldy #0
_ENTRY1         iny
                lda (buf),Y
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
                lda (delnxt),Y

                inc choff
                cmp choff
                bcs _1

                lda defflg
                sta choff
                sty defflg
                bra nextchar._ENTRY1

_1              ldy choff
                lda (delnxt),Y

                rts
                .endproc


;======================================
;   LexGet()
;======================================
lexget          .proc
                jsr GetNext._ENTRY1

_ENTRY1         lda #0
                sta defflg

                inc Channel

                lda #4
                jsr openchan
                jsr NextLine

                jmp GetNext._ENTRY2

                .endproc


;======================================
;   LexSet()
;======================================
lexset          .proc
                jsr _1

                sta arg11
                stx arg12

                jsr GetNext._ENTRY1

                lda nxttoken
                cmp #tokEQU
                bne _err

                jsr _1

                ldy #0
                sta (arg11),Y

                txa
                beq _XIT1

                iny
                sta (arg11),Y

_XIT1           jmp GetNext._ENTRY1

_err            ldy #setERR
                jmp splerr

_1              jsr GetNext._ENTRY1
                jmp mnum

                .endproc


;======================================
;   LexExpand()
;======================================
lexexpand       .proc
                lda defflg
                beq _1

                ldy #dfnERR
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

                jmp GetNext._ENTRY1

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

lexcmd          .addr GetNext._ENTRY4
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
