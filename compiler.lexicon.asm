
;--------------------------------------
;    Compiler tokens
;--------------------------------------

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
;--------------------------------------
tokCHAR_t       = 1
tokBYTE_t       = 2
tokINT_t        = 3
tokCARD_t       = 4
tokSTR_t        = 5
tokREAL_t       = 6

tokUNDEC        = $88


;--------------------------------------
;    Error codes
;--------------------------------------

;   0 - out of system memory
strERR          = 1                     ; missing " at end
dfnERR          = 2                     ; nested defines
symtblERR       = 3                     ; too many qglobal st
;   4 - too many local st entries
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
;   22 - file too large on input
condtERR        = 23                    ; illegal cond. exp.
forERR          = 24                    ; illegal FOR stmt
exitERR         = 25                    ; no loop for EXIT
nestERR         = 26                    ; nesting level too deep
typERR          = 27                    ; illegal type ref.
retrnERR        = 28                    ; illegal RETURN stmt.
;   61 - out of st space
brkERR          = $80                   ; Break key depressed


;--------------------------------------
;    Compiler lexicon - get tokens
;--------------------------------------

;======================================
; LexGetNext()
;======================================
LexGetNext      .proc
                lda spnxt
                ldx curnxt
                ldy curnxt+1
                stx curln
                sty curln+1
                sta spln

                lda token
                sta zpAllocPrevToken

                ldx nxtaddr
                ldy nxtaddr+1
                lda nxttoken
                stx addr
                sty addr+1
                sta token

_ENTRY1         jsr LexNextChar

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
                lda tblLexChars-33,Y
                beq _ENTRY1
                bpl _ENTRY3

                and #$7F
                bra _3

_1              jsr mscAlpha
                bne _2

                cmp #'['
                beq _ENTRY3

                cmp #'^'
                beq _ENTRY3

                cmp #']'
                beq _ENTRY3
                bra _ENTRY1

_2              jsr bankGetName
                bmi _ENTRY3

_3              sta nxttoken

                ldx #<tblLexCmd
                ldy #>tblLexCmd
                jmp mscLookup

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
; LexCom()
;======================================
LexCom          .proc
                jsr LexNextLine
                bra LexGetNext._ENTRY2

                .endproc


;======================================
; LexDig()
;======================================
LexDig          .proc
                lda #tokCONST_t+tokINT_t
                sta nxttoken

                jsr LexBuf              ; get buf ptr
                jsr ioStrToReal

_next1          jsr LexNextChar            ; cardinal?
                jsr mscAlphaNum._num
                bne _next1

                cmp #'.'
                beq _2

                cmp #'E'
                beq _2

                dec choff

                jsr ioRealToCard
                bcc _1

_err            ldy #constERR
                jmp bankSPLErr

_ENTRY1         dey
                sty choff

_1              sta nxtaddr
                stx nxtaddr+1

                cpx #$00
                bne LexGetNext._ENTRY4

                lda #tokCONST_t+tokBYTE_t
                bne LexGetNext._ENTRY3

_2              lda #tokCONST_t+tokREAL_t
                sta nxttoken

                ldy CIX
                ldx #$FF                ; for SET cmd
                bra _ENTRY1

                .endproc


;======================================
; LexChr()
;======================================
LexChr          .proc
                jsr LexNextChar

                sta nxtaddr

                lda #tokCONST_t+tokCHAR_t
                bra LexGetNext._ENTRY3

                .endproc


;======================================
; LexNE()
;======================================
LexNE           .proc
                jsr LexNextChar

                cmp #'>'
                bne LexEQ._ENTRY1

                lda #tokNOTEQU
                bra LexGetNext._ENTRY3

                .endproc


;======================================
; LexEQ()
;======================================
LexEQ           .proc
                jsr LexNextChar

_ENTRY1         cmp #'='
                bne LexPutBack

                inc nxttoken
                bra LexGetNext._ENTRY4

                .endproc


;======================================
; LexHex()
;======================================
LexHex          .proc
                lda #tokCONST_t+tokCARD_t
                sta nxttoken

                inc choff

                jsr LexBuf
                jsr ioHexToCard
                bra LexDig._ENTRY1

                .endproc


;======================================
; LexPutBack returns character to buf
;======================================
LexPutBack      .proc
                dec choff

_ENTRY1         jmp LexGetNext._ENTRY4

                .endproc


;======================================
; LexPF()
;======================================
LexPF           .proc
                lda qglobal
                beq LexPutBack._ENTRY1

                lda #$00
                sta qglobal

                lda gbase               ; restore qglobal base
                sta symtab
                lda gbase+1
                sta symtab+1

                bra LexPutBack._ENTRY1

                .endproc


;======================================
; LexStr()
;======================================
LexStr          .proc
                lda token
                cmp #tokQuote
                beq LexPutBack._ENTRY1  ; zap local st

                lda #$00
                sta arg9

_next1          jsr LexNextChar

                inc arg9
                beq _1                  ; string too long

                cmp #'"'
                beq _2

_next2          ldy arg9
                sta (symtab),Y

                lda Channel
                bpl _next1              ; if not EOF

_1              ldy #strERR
                jmp bankSPLErr

_2              jsr LexNextChar

                cmp #'"'
                beq _next2              ; " in string
                                        ; end of string

                ldy arg9
                lda #EOL
                sta (symtab),Y

                dey
                tya
                ldy #$00
                sta (symtab),Y          ; save size

                lda symtab
                ldx symtab+1
                ldy choff

                dey
                jmp LexDig._ENTRY1

                .endproc


;======================================
; LexNextChar()
;======================================
LexNextChar     .proc
                ldy defflg
                bne LexDef

_ENTRY1         ldy choff
                cpy sp
                bcc LexNextLine._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; LexNextLine()
;======================================
LexNextLine     .proc
                lda Channel
                beq _1
                bmi _4                  ; eof

                jsr ioReadBuffer
                bpl _2

                cpy #$88                ; EOF
                beq _next1

                jmp bankSPLErr

_next1          dec Channel
                bne LexNextLine

_1              ldy top+1
                beq _next1              ; set eof, tricky QCODE

                jsr ioLoadBuffer

                lda cur
                sta curnxt
                ldx cur+1
                stx curnxt+1

                jsr mscNextDown
                bne _2

            ;    lda #$00
                sta top+1

_2              lda list
                beq _3                  ; don't list

                lda device
                jsr ioWriteBuffer

_3              ldy #$00
                sty choff
                lda (buf),Y

                tay
                iny
                sty sp

                lda #EOL
                sta (buf),Y

                ldy #$00
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
LexDef          .proc
                ldy #$00
                lda (delnxt),Y

                inc choff
                cmp choff
                bcs _1

                lda defflg
                sta choff
                sty defflg
                bra LexNextChar._ENTRY1

_1              ldy choff
                lda (delnxt),Y

                rts
                .endproc


;======================================
; LexGet()
;======================================
LexGet          .proc
                jsr LexGetNext._ENTRY1

_ENTRY1         lda #$00
                sta defflg

                inc Channel

                lda #$04
                jsr ioOpenChannel
                jsr LexNextLine

                jmp LexGetNext._ENTRY2

                .endproc


;======================================
; LexSet()
;======================================
LexSet          .proc
                jsr _1

                sta arg11
                stx arg12

                jsr LexGetNext._ENTRY1

                lda nxttoken
                cmp #tokEQU
                bne _err

                jsr _1

                ldy #$00
                sta (arg11),Y

                txa
                beq _XIT1

                iny
                sta (arg11),Y

_XIT1           jmp LexGetNext._ENTRY1

_err            ldy #setERR
                jmp bankSPLErr

_1              jsr LexGetNext._ENTRY1
                jmp mscMNum

                .endproc


;======================================
; LexExpand()
;======================================
LexExpand       .proc
                lda defflg
                beq _1

                ldy #dfnERR
                jmp bankSPLErr

_1              lda #$03
                jsr mscNextProp

                lda zpAllocProps
                ldx zpAllocProps+1
                jsr mscResetProp

                ldy choff
_ENTRY1         sta delnxt

                stx delnxt+1
                sty defflg

                lda #$00
                sta choff

                jmp LexGetNext._ENTRY1

                .endproc


;======================================
; LexBuf()
;======================================
LexBuf          .proc
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

tblLexCmd       .addr LexGetNext._ENTRY4
                .byte 41
                .addr LexDig
                .byte tokDigit-$80
                .addr LexHex
                .byte tokHex
                .addr LexEQ
                .byte tokGRTR
                .addr LexNE
                .byte tokLESS
                .addr LexExpand
                .byte tokDef
                .addr LexCom
                .byte tokSColon
                .addr LexChr
                .byte tokSQuote
                .addr LexPF
                .byte tokPROC
                .addr LexPF
                .byte tokFUNC
                .addr LexPF
                .byte tokMOD
                .addr LexStr
                .byte tokQuote
                .addr LexGet
                .byte tokGET
                .addr LexSet
                .byte tokSET

tblLexChars     .byte tokXOR            ; !
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
;         ldx #$00
;         jsr PrintC
;         jsr PutSp
;         lda addr
;         ldx addr+1
;         jsr monPrintHex
;         jsr PutSp
;         lda nxtToken
;         ldx #$00
;         jsr PrintC
;         jsr PutSp
;         lda nxtAddr
;         ldx nxtAddr+1
;         jsr monPrintHex
;         jmp ioPutEOL
