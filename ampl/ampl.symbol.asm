
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.symbol.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


symbol         .namespace

;======================================
; STM(table)
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

                ldy #$00
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

_XIT2           jmp mainbank.SPLErr

_XIT            jmp (jt_vecStmRAdr)

                .endproc


;   this normally goes to ISTMres below


;======================================
; STMres() lookup reserved names
;======================================
STMres          .proc
                ldy arg14
                cpy #$08
                lda #$FF                ; if name too long!
                bcs _XIT1               ; not reserved name

                iny
                sty arg0

                ldx tblReserveWords-2,Y
_next1          stx arg1

                ldy #$01
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
; GetName(char)
;======================================
GetName         .proc
                ldy #$00
                sta firstChar           ; indicates a big symbol table is not needed (yet)

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

                jsr compiler.lexicon.NextChar

                ldy arg14
                cmp #'_'
                beq _next1

                jsr mscAlphaNum
                bne _next1

                tya
                ldy #$00
                sta (symtab),Y

                dec choff               ; put character back

                jsr STM._XIT            ; check for res. name
                bpl STMres._XIT1        ; return

                lda qglobal
                beq _1

                lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne STMres._XIT1        ; return

_1              lda symTblGlobal
                ldx symTblGlobal+1

                ldy firstChar
                cpy isBigSymTbl
                bpl _2

                lda bigSymTblGlobal
                ldx bigSymTblGlobal+1
_2              jsr STM
                bne STMres._XIT1        ; return

                lda qglobal
                beq NewEntry

_ENTRY1         lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne STMres._XIT1

            .if ZAPRAM
                inc STM,X
            .else
                nop
                nop
                nop
            .endif
                .endproc


;======================================
; Make new entry in symbol table
;======================================
NewEntry        .proc
                lda symtab+1
                sta (arg2),Y
                lda symtab
                sta (arg4),Y

                lda #<lib.str.libst
                ldx #>lib.str.libst
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
                jsr mscSTIncr

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

resw1           .byte $FF

resw2           .text "DO",tokDO
                .text "FI",tokFI
                ; .text "FO",tokESAC
                .text "IF",tokIF
                .text "OD",tokOD
                ; .text "OF",of
                .text "OR",tokOR
                .text "TO",tokTO
                .byte $FF

resw3           .text "AND",tokAND
                .text "FOR",tokFOR
                ; .text "GET",get
                .text "INT",tokINT
                .text "LSH",tokLSH
                .text "MOD",tokREM
                ; .text "NOT",notId
                .text "RSH",tokRSH
                .text "SET",tokSET
                .text "XOR",tokXOR
                .byte $FF

resw4           .text "BYTE",tokBYTE
                .text "CARD",tokCARD
                ; .text "CASE",caseId
                .text "CHAR",tokCHAR
                .text "ELSE",tokELSE
                ; .text "ESAC",tokESAC
                .text "EXIT",tokEXIT
                .text "FUNC",tokFUNC
                .text "PROC",tokPROC
                ; .text "REAL",tokREAL
                .text "STEP",tokSTEP
                .text "THEN",tokTHEN
                .text "TYPE",tokTYPE
                .byte $FF

resw5           .text "ARRAY",tokARRAY
                .text "UNTIL",tokUNTIL
                .text "WHILE",tokWHILE
                .byte $FF

resw6           .text "DEFINE",tokDEFINE
                ; .text "DOWNTO",tokDOWNTO
                .text "ELSEIF",tokELSEIF
                .text "MODULE",tokMOD
                .text "RETURN",tokRET
                .byte $FF

resw7           .text "INCLUDE",tokGET
                .text "POINTER",tokPOINTER
                .byte $FF

                .endnamespace
