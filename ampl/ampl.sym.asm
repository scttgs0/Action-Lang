
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.sym.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


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

_next1          lda (arg2),Y
                sta nxtaddr+1
                beq _XIT1

                lda (arg4),Y
                sta nxtaddr

                ldy #0
_next2          lda (nxtaddr),Y
                eor (symtab),Y
                and stmask
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

                ldy #ster
                lda arg3
                cmp stglobal+1
                beq _XIT2

                iny

_XIT2           jmp splerr

_XIT            jmp (stmradr)

                .endproc


; this normially goes to ISTMres below


;======================================
;   STMres() lookup reserved names
;======================================
istmres         .proc
                ldy arg14
                cpy #8
                lda #$FF                ; if name too long!
                bcs _XIT1               ; not reserved name

                iny
                sty arg0

                ldx rwstrt-2,Y
_next1          stx arg1

                ldy #1
_next2          lda resw1,X
                bmi _XIT1

                eor (symtab),Y
                and stmask
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
lgetname        .proc
                ldy #0
                sta frstchar

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

                jsr stm._XIT            ; check for res. name
                bpl istmres._XIT1       ; return

                lda qglobal
                beq _1

                lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._XIT1       ; return

_1              lda stglobal
                ldx stglobal+1

                ldy frstchar
                cpy bigst
                bpl _2

                lda stg2
                ldx stg2+1
_2              jsr stm
                bne istmres._XIT1       ; return

                lda qglobal
                beq newentry

_ENTRY1         lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._XIT1

        .if ramzap
                inc stm,X
        .else
                nop
                nop
                nop
        .endif
                .endproc


;======================================
;   Make new entry in symbol table
;======================================
newentry        .proc
                lda symtab+1
                sta (arg2),Y
                lda symtab
                sta (arg4),Y

                lda #<libst
                ldx #>libst
                jsr stm                 ; lookup shadow name

                lda #undec
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
    ; .byte "FO",esac
                .text "IF",ifid
                .text "OD",od
    ; .byte "OF",of
                .text "OR",orid
                .text "TO",to
                .byte $ff

resw3           .text "AND",andid
                .text "FOR",forid
    ; .byte "GET",get
                .text "INT",int
                .text "LSH",lshid
                .text "MOD",remid
    ; .byte "NOT",notId
                .text "RSH",rshid
                .text "SET",set
                .text "XOR",xorid
                .byte $ff

resw4           .text "BYTE",byte
                .text "CARD",card
    ; .byte "CASE",caseId
                .text "CHAR",char
                .text "ELSE",else
    ; .byte "ESAC",esac
                .text "EXIT",exitid
                .text "FUNC",func
                .text "PROC",proc
    ; .byte "REAL",real
                .text "STEP",step
                .text "THEN",then
                .text "TYPE",typeid
                .byte $ff

resw5           .text "ARRAY",array
                .text "UNTIL",untilid
                .text "WHILE",whileid
                .byte $ff

resw6           .text "DEFINE",define
    ; .byte "DOWNTO",downto
                .text "ELSEIF",elseif
                .text "MODULE",modid
                .text "RETURN",retid
                .byte $ff

resw7           .text "INCLUDE",get
                .text "POINTER",pointer
                .byte $ff
