;======================================
;   FILE: ampl.sym.asm
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
_stm1           lda (arg2),y
                sta nxtaddr+1
                beq _stm3

                lda (arg4),y
                sta nxtaddr
                ldy #0
_stm2           lda (nxtaddr),y
                eor (symtab),y
                and jt_stmask
                bne _stm4

                cpy arg14
                iny
                bcc _stm2

                lda (nxtaddr),y         ; matched
_stm3           rts

_stm4           inc arg13               ; try next entry
                ldy arg13
                cpy arg15
                bne _stm1

                ldy #symtblERR
                lda arg3
                cmp symTblGlobal+1
                beq _stm5

                iny
_stm5           jmp splerr

stmres          jmp [jt_stmradr]

                .endproc


; this normially goes to ISTMres below

;======================================
;   STMres() lookup reserved names
;======================================
iSTMres         .proc
                ldy arg14
                cpy #8
_stmr0          lda #$ff                ; if name too long!
                bcs _stmr3              ; not reserved name

                iny
                sty arg0
                ldx tblReserveWords-2,y
_stmr1          stx arg1
                ldy #1
_stmr2          lda resw1,x
                bmi _stmr3

                eor (symtab),y
                and jt_stmask
                bne _stmr4

                inx
                iny
                cpy arg0
                bcc _stmr2

    ; we have a match
                lda resw1,x             ; get token value
_stmr3          rts

_stmr4          clc
                lda arg1
                adc arg0
                tax
                bne _stmr1              ; try next entry

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
_gname1         iny
                sty arg14
                sta (symtab),y
                ora #$20
                asl arg15
                adc arg15
                sta arg15
                jsr nextchar

                ldy arg14
                cmp #'_'
                beq _gname1
                jsr alphanum

                bne _gname1

                tya
                ldy #0
                sta (symtab),y
                dec choff               ; put character back
                jsr STM.stmres          ; check for res. name
                bpl iSTMres._stmr3      ; return

                lda qglobal
                beq gnglobal

                lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne iSTMres._stmr3      ; return

gnglobal        lda symTblGlobal
                ldx symTblGlobal+1
                ldy FirstChar
                cpy isBigSymTbl
                bpl _gng1

                lda bigSymTblGlobal
                ldx bigSymTblGlobal+1
_gng1           jsr STM
                bne iSTMres._stmr3      ; return

                lda qglobal
                beq NewEntry

lgnlocal        lda symTblLocal
                ldx symTblLocal+1
                jsr STM
                bne iSTMres._stmr3

        .if ramzap
                inc STM,x
        .else
                nop
                nop
                nop
        .endif
                .endproc


;======================================
;
;======================================
NewEntry        .proc
    ; Make new entry in symbol table
                lda symtab+1
                sta (arg2),y
                lda symtab
                sta (arg4),y
                lda #<libst
                ldx #>libst
                jsr STM                 ; lookup shadow name

                lda #tokUNDEC
                ldy arg14
                iny
                sta (symtab),y
                lda nxtaddr
                iny
                sta (symtab),y          ; save shadow entry
                lda nxtaddr+1
                iny
                sta (symtab),y
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
    ; .BYTE "FO",tokESAC
                .text "IF",tokIF
                .text "OD",tokOD
    ; .BYTE "OF",of
                .text "OR",tokOR
                .text "TO",tokTO
                .byte $ff

resw3           .text "AND",tokAND
                .text "FOR",tokFOR
    ; .BYTE "GET",get
                .text "INT",tokINT
                .text "LSH",tokLSH
                .text "MOD",tokREM
    ; .BYTE "NOT",notId
                .text "RSH",tokRSH
                .text "SET",tokSET
                .text "XOR",tokXOR
                .byte $ff

resw4           .text "BYTE",tokBYTE
                .text "CARD",tokCARD
    ; .BYTE "CASE",caseId
                .text "CHAR",tokCHAR
                .text "ELSE",tokELSE
    ; .BYTE "ESAC",tokESAC
                .text "EXIT",tokEXIT
                .text "FUNC",tokFUNC
                .text "PROC",tokPROC
    ; .BYTE "REAL",tokREAL
                .text "STEP",tokSTEP
                .text "THEN",tokTHEN
                .text "TYPE",tokTYPE
                .byte $ff

resw5           .text "ARRAY",tokARRAY
                .text "UNTIL",tokUNTIL
                .text "WHILE",tokWHILE
                .byte $ff

resw6           .text "DEFINE",tokDEFINE
    ; .BYTE "DOWNTO",tokDOWNTO
                .text "ELSEIF",tokELSEIF
                .text "MODULE",tokMOD
                .text "RETURN",tokRET
                .byte $ff

resw7           .text "INCLUDE",tokGET
                .text "POINTER",tokPOINTER
                .byte $ff
