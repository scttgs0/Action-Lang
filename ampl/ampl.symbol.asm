
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.symbol.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   STM(table)
;======================================
STM             .proc
                sta arg2
                stx arg3
                sta arg4

                inx
                stx arg5

                ldy arg15
                sty arg13

_next1          lda (arg2),Y
                sta nxtaddr+1
                beq _XIT1

                lda (arg4),Y
                sta nxtaddr

                ldy #0
_next2          lda (nxtaddr),Y
                eor (symtab),Y
                and jt_stmask
                bne _1

                cpy arg14
                iny
                bcc _next2

                lda (nxtaddr),Y         ; matched

_XIT1           rts

_1              inc arg13               ; try next entry
                ldy arg13
                cpy arg15
                bne _next1

                ldy #symtblERR
                lda arg3
                cmp symTblGlobal+1
                beq _XIT2

                iny

_XIT2           jmp splerr

_XIT            jmp (jt_stmradr)

                .endproc


; this normially goes to ISTMres below


;======================================
;   STMres() lookup reserved names
;======================================
iSTMres         .proc
                ldy arg14
                cpy #8
                lda #$FF                ; if name too long!
                bcs _XIT1               ; not reserved name

                iny
                sty arg0

                ldx tblReserveWords-2,Y
_next1          stx arg1

                ldy #1
_next2          lda resw1,X
                bmi _XIT1

                eor (symtab),Y
                and jt_stmask
                bne _1

                inx
                iny
                cpy arg0
                bcc _next2

;   we have a match
                lda resw1,X             ; get token value

_XIT1           rts

_1              clc
                lda arg1
                adc arg0

                tax
                bne _next1              ; try next entry

                .endproc


;======================================
;   GetName(char)
;======================================
lGetName        .proc
                ldy #0
                sta FirstChar           ; indicates a big symbol table is not needed (yet)

                tax                     ; preserve A
                ora #$20
                sta arg15               ; initial hash

                txa                     ; restore A
_next1          iny
                sty arg14
                sta (symtab),Y

                ora #$20
                asl arg15
                adc arg15
                sta arg15

                jsr nextchar

                ldy arg14
                cmp #'_'
                beq _next1

                jsr alphanum
                bne _next1

                tya
                ldy #0
                sta (symtab),Y

                dec choff               ; put character back

                jsr STM._XIT            ; check for res. name
                bpl iSTMres._XIT1       ; return

                lda qglobal
                beq _1

                lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne iSTMres._XIT1       ; return

_1              lda symTblGlobal
                ldx symTblGlobal+1

                ldy FirstChar
                cpy isBigSymTbl
                bpl _2

                lda bigSymTblGlobal
                ldx bigSymTblGlobal+1
_2              jsr STM
                bne iSTMres._XIT1       ; return

                lda qglobal
                beq NewEntry

_ENTRY1         lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne iSTMres._XIT1

        .if ramzap
                inc STM,X
        .else
                nop
                nop
                nop
        .endif
                .endproc


;======================================
;   Make new entry in symbol table
;======================================
NewEntry        .proc
                lda symtab+1
                sta (arg2),Y
                lda symtab
                sta (arg4),Y

                lda #<libst
                ldx #>libst
                jsr STM                 ; lookup shadow name

                lda #tokUNDEC
                ldy arg14
                iny
                sta (symtab),Y

                lda nxtaddr
                iny
                sta (symtab),Y          ; save shadow entry

                lda nxtaddr+1
                iny
                sta (symtab),Y

                lda symtab
                sta nxtaddr
                ldx symtab+1
                stx nxtaddr+1

                iny
                tya
                jsr stincr

                lda #tokUNDEC

                rts
                .endproc


;--------------------------------------
;--------------------------------------

tblReserveWords .byte 0
                .byte resw2-resw1
                .byte resw3-resw1
                .byte resw4-resw1
                .byte resw5-resw1
                .byte resw6-resw1
                .byte resw7-resw1
;
resw1           .byte $ff

resw2           .text "DO",tokDO
                .text "FI",tokFI
    ; .byte "FO",tokESAC
                .text "IF",tokIF
                .text "OD",tokOD
    ; .byte "OF",of
                .text "OR",tokOR
                .text "TO",tokTO
                .byte $ff

resw3           .text "AND",tokAND
                .text "FOR",tokFOR
    ; .byte "GET",get
                .text "INT",tokINT
                .text "LSH",tokLSH
                .text "MOD",tokREM
    ; .byte "NOT",notId
                .text "RSH",tokRSH
                .text "SET",tokSET
                .text "XOR",tokXOR
                .byte $ff

resw4           .text "BYTE",tokBYTE
                .text "CARD",tokCARD
    ; .byte "CASE",caseId
                .text "CHAR",tokCHAR
                .text "ELSE",tokELSE
    ; .byte "ESAC",tokESAC
                .text "EXIT",tokEXIT
                .text "FUNC",tokFUNC
                .text "PROC",tokPROC
    ; .byte "REAL",tokREAL
                .text "STEP",tokSTEP
                .text "THEN",tokTHEN
                .text "TYPE",tokTYPE
                .byte $ff

resw5           .text "ARRAY",tokARRAY
                .text "UNTIL",tokUNTIL
                .text "WHILE",tokWHILE
                .byte $ff

resw6           .text "DEFINE",tokDEFINE
    ; .byte "DOWNTO",tokDOWNTO
                .text "ELSEIF",tokELSEIF
                .text "MODULE",tokMOD
                .text "RETURN",tokRET
                .byte $ff

resw7           .text "INCLUDE",tokGET
                .text "POINTER",tokPOINTER
                .byte $ff
