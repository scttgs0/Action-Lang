
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: compiler.lexicon.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


lexicon        .namespace

;--------------------------------------
;    Compiler lexicon - get tokens
;--------------------------------------

;======================================
; GetNext()
;======================================
GetNext         .proc
                lda spnxt
                ldx curnxt
                ldy curnxt+1
                stx curln
                sty curln+1
                sta spln

                .mba token,zpAllocPrevToken

                ldx nxtaddr
                ldy nxtaddr+1
                lda nextToken
                stx addr
                sty addr+1
                sta token

_ENTRY1         jsr NextChar

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
                bne _3                  ; [unc]

_1              jsr mainmsc.Alpha
                bne _2

                cmp #'['
                beq _ENTRY3

                cmp #'^'
                beq _ENTRY3

                cmp #']'
                beq _ENTRY3
                bne _ENTRY1             ; [unc]

_2              jsr mainbank.GetName
                bmi _ENTRY3

_3              sta nextToken

                ldx #<tblLexCmd
                ldy #>tblLexCmd
                jmp mainmsc.Lookup

_ENTRY3         sta nextToken

;GetNr2         lda $D0
;               beq GetNr3
;               jsr PrintTok

_ENTRY4         ldx addr
                ldy addr+1

_ENTRY5         lda token

                rts
                .endproc


;======================================
; Com()
;======================================
Com             .proc
                jsr NextLine
                bne GetNext._ENTRY2     ; [unc]

                .endproc


;======================================
; Dig()
;======================================
Dig             .proc
                .mba #tokCONST_t+tokINT_t,nextToken

                jsr LexBuf              ; get buf ptr
                jsr mainio.StrToReal

_next1          jsr NextChar            ; cardinal?
                jsr mainmsc.AlphaNum._num
                bne _next1

                cmp #'.'
                beq _2

                cmp #'E'
                beq _2

                dec choff

                jsr mainio.RealToCard
                bcc _1

_err            ldy #constERR
                jmp mainbank.SPLErr

_ENTRY1         dey
                sty choff

_1              sta nxtaddr
                stx nxtaddr+1

                cpx #$00
                bne GetNext._ENTRY4

                lda #tokCONST_t+tokBYTE_t
                bne GetNext._ENTRY3

_2              .mba #tokCONST_t+tokREAL_t,nextToken

                ldy CIX
                ldx #$FF                ; for SET cmd
                bne _ENTRY1             ; [unc]

                .endproc


;======================================
; Chr()
;======================================
Chr             .proc
                jsr NextChar

                sta nxtaddr

                lda #tokCONST_t+tokCHAR_t
                bne GetNext._ENTRY3     ; [unc]

                .endproc


;======================================
; LexNE()
;======================================
LexNE           .proc
                jsr NextChar

                cmp #'>'
                bne LexEQ._ENTRY1

                lda #tokNOTEQU
                bne GetNext._ENTRY3     ; [unc]

                .endproc


;======================================
; LexEQ()
;======================================
LexEQ           .proc
                jsr NextChar

_ENTRY1         cmp #'='
                bne PutBack

                inc nextToken
                bne GetNext._ENTRY4     ; [unc]

                .endproc


;======================================
; Hex()
;======================================
Hex             .proc
                .mba #tokCONST_t+tokCARD_t,nextToken

                inc choff

                jsr LexBuf
                jsr mainio.HexToCard
                bne Dig._ENTRY1         ; [unc]

                .endproc


;======================================
; PutBack returns character to buf
;======================================
PutBack         .proc
                dec choff

_ENTRY1         jmp GetNext._ENTRY4

                .endproc


;======================================
; ProcFunc()
;======================================
ProcFunc        .proc
                lda qglobal
                beq PutBack._ENTRY1

                .mbv #$00,qglobal
                .mwa gbase,symtab       ; restore qglobal base

                bne PutBack._ENTRY1     ; [unc]

                .endproc


;======================================
; Str()
;======================================
Str             .proc
                lda token
                cmp #tokQuote
                beq PutBack._ENTRY1  ; zap local st

                .mbv #$00,arg9

_next1          jsr NextChar

                inc arg9
                beq _1                  ; string too long

                cmp #'"'
                beq _2

_next2          ldy arg9
                sta (symtab),Y

                lda ioChnnl
                bpl _next1              ; if not EOF

_1              ldy #strERR
                jmp mainbank.SPLErr

_2              jsr NextChar

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
                jmp Dig._ENTRY1

                .endproc


;======================================
; NextChar()
;======================================
NextChar        .proc
                ldy defflg
                bne Def

_ENTRY1         ldy choff
                cpy sp
                bcc NextLine._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; NextLine()
;======================================
NextLine        .proc
                lda ioChnnl
                beq _1
                bmi _4                  ; eof

                jsr mainio.ReadBuffer
                bpl _2

                cpy #$88                ; EOF
                beq _next1

                jmp mainbank.SPLErr

_next1          dec ioChnnl
                bne NextLine

_1              ldy top+1
                beq _next1              ; set eof, tricky QCODE

                jsr mainio.LoadBuffer

                lda cur
                sta curnxt
                ldx cur+1
                stx curnxt+1

                jsr mainmsc.NextDown
                bne _2

                ; lda #$00
                sta top+1

_2              lda isListing
                beq _3                  ; don't list

                lda device
                jsr mainio.WriteBuffer

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
Def             .proc
                ldy #$00
                lda (delnxt),Y

                inc choff
                cmp choff
                bcs _1

                .mba defflg,choff
                sty defflg
                bcc NextChar._ENTRY1    ; [unc]

_1              ldy choff
                lda (delnxt),Y

                rts
                .endproc


;======================================
; Get()
;======================================
Get             .proc
                jsr GetNext._ENTRY1

_ENTRY1         .mbv #$00,defflg

                inc ioChnnl

                lda #$04
                jsr mainio.OpenChannel
                jsr NextLine
                jmp GetNext._ENTRY2

                .endproc


;======================================
; Set()
;======================================
Set             .proc
                jsr _1

                sta arg11
                stx arg12

                jsr GetNext._ENTRY1

                lda nextToken
                cmp #tokEQU
                bne _err

                jsr _1

                ldy #$00
                sta (arg11),Y

                txa
                beq _XIT1

                iny
                sta (arg11),Y

_XIT1           jmp GetNext._ENTRY1

_err            ldy #setERR
                jmp mainbank.SPLErr

_1              jsr GetNext._ENTRY1
                jmp mainmsc.MNum

                .endproc


;======================================
; Expand()
;======================================
Expand          .proc
                lda defflg
                beq _1

                ldy #dfnERR
                jmp mainbank.SPLErr

_1              lda #$03
                jsr mainmsc.NextProp

                lda zpAllocProps
                ldx zpAllocProps+1
                jsr mainmsc.ResetProp

                ldy choff
_ENTRY1         sta delnxt

                stx delnxt+1
                sty defflg

                .mbv #$00,choff

                jmp GetNext._ENTRY1

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

tblLexCmd       .addr GetNext._ENTRY4   ; default routine
                .byte $29               ; table size (#entries*3 - 1)

                .addr Dig
                .byte tokDigit-$80
                .addr Hex
                .byte tokHex
                .addr LexEQ
                .byte tokGRTR
                .addr LexNE
                .byte tokLESS
                .addr Expand
                .byte tokDef
                .addr Com
                .byte tokSColon
                .addr Chr
                .byte tokSQuote
                .addr ProcFunc
                .byte tokPROC
                .addr ProcFunc
                .byte tokFUNC
                .addr ProcFunc
                .byte tokMOD
                .addr Str
                .byte tokQuote
                .addr Get
                .byte tokGET
                .addr Set
                .byte tokSET

tblLexChars     .byte tokXOR            ; !
                .byte tokQuote+$80      ; "
                .byte tokNOTEQU         ; #
                .byte tokHex+$80        ; $
                .byte tokOR             ; %
                .byte tokAND            ; &
                .byte tokSQuote+$80     ; '
                .byte tokLeftParen
                .byte tokRightParen
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

                .endnamespace
