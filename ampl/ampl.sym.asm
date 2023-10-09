
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
stm             .proc
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
                and stmask
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

                ldy #ster
                lda arg3
                cmp stglobal+1
                beq _stm5

                iny
_stm5           jmp splerr

stmres          jmp (stmradr)

                .endproc


; this normially goes to ISTMres below

;======================================
;   STMres() lookup reserved names
;======================================
istmres         .proc
                ldy arg14
                cpy #8
_stmr0          lda #$FF                ; if name too long!
                bcs _stmr3              ; not reserved name

                iny
                sty arg0
                ldx rwstrt-2,y
_stmr1          stx arg1
                ldy #1
_stmr2          lda resw1,x
                bmi _stmr3

                eor (symtab),y
                and stmask
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
lgetname        .proc
                ldy #0
                sta frstchar
                tax
                ora #$20
                sta arg15               ; initial hash
                txa
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
                jsr stm.stmres          ; check for res. name
                bpl istmres._stmr3      ; return

                lda qglobal
                beq gnglobal

                lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._stmr3      ; return

gnglobal        lda stglobal
                ldx stglobal+1
                ldy frstchar
                cpy bigst
                bpl _gng1

                lda stg2
                ldx stg2+1
_gng1           jsr stm
                bne istmres._stmr3      ; return

                lda qglobal
                beq newentry

lgnlocal        lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._stmr3

        .if ramzap
                inc stm,x
        .else
                nop
                nop
                nop
        .endif
                .endproc


;======================================
;
;======================================
newentry        .proc
    ; Make new entry in symbol table
                lda symtab+1
                sta (arg2),y
                lda symtab
                sta (arg4),y
                lda #<libst
                ldx #>libst
                jsr stm                 ; lookup shadow name

                lda #undec
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

                lda #undec
                rts
                .endproc

;--------------------------------------
;--------------------------------------

rwstrt          .byte 0
                .byte resw2-resw1
                .byte resw3-resw1
                .byte resw4-resw1
                .byte resw5-resw1
                .byte resw6-resw1
                .byte resw7-resw1
;
resw1           .byte $ff

resw2           .text "DO",do
                .text "FI",fi
    ; .BYTE "FO",esac
                .text "IF",ifid
                .text "OD",od
    ; .BYTE "OF",of
                .text "OR",orid
                .text "TO",to
                .byte $ff

resw3           .text "AND",andid
                .text "FOR",forid
    ; .BYTE "GET",get
                .text "INT",int
                .text "LSH",lshid
                .text "MOD",remid
    ; .BYTE "NOT",notId
                .text "RSH",rshid
                .text "SET",set
                .text "XOR",xorid
                .byte $ff

resw4           .text "BYTE",byte
                .text "CARD",card
    ; .BYTE "CASE",caseId
                .text "CHAR",char
                .text "ELSE",else
    ; .BYTE "ESAC",esac
                .text "EXIT",exitid
                .text "FUNC",func
                .text "PROC",proc
    ; .BYTE "REAL",real
                .text "STEP",step
                .text "THEN",then
                .text "TYPE",typeid
                .byte $ff

resw5           .text "ARRAY",array
                .text "UNTIL",untilid
                .text "WHILE",whileid
                .byte $ff

resw6           .text "DEFINE",define
    ; .BYTE "DOWNTO",downto
                .text "ELSEIF",elseif
                .text "MODULE",modid
                .text "RETURN",retid
                .byte $ff

resw7           .text "INCLUDE",get
                .text "POINTER",pointer
                .byte $ff
