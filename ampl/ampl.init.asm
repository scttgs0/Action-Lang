
; SPDX-FileName: ampl.init.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   SPLsetup()
;======================================
SPLsetup        .proc
                lda #0
                tay
                sta sp
                sta Channel
                sta symtab
                sta INITAD+1
                sta (buf),Y
                sta param
                sta qglobal
                sta stackptr
                sta arrayptr+1
                sta whaddr+1
                sta curnxt+1
                sta procsp

;   clear qglobal symbol table
                ldx isBigSymTbl
                beq _next2              ;   no

_next1          sta (bigSymTblGlobal),Y

                iny
                bne _next1

_next2          sta (symTblGlobal),Y

                iny
                bne _next2

;   get last block in heap
                lda afbase
                ldx afbase+1
                sta arg0
                stx arg1

_next3          ldy #1
                lda (arg0),Y
                beq _1

                tax
                dey
                lda (arg0),Y
                sta arg0
                stx arg1

                jmp _next3

_1              clc
                lda arg0
                adc #4
                bcc _2

                inc arg1

_2              sta codebase
                sta qcode

                lda arg1
                sta codebase+1
                sta qcode+1

                lda MEMTOP+1
                sta stmax

                dec stmax
                clc
                sbc SymTblSizePages
                sta stbase
                sta symtab+1

                inc symtab+1
                cmp qcode+1
                bcs _3

;   can't allocate memory
                lda sparem
                sta symtab
                lda sparem+1
                sta symtab+1

_err            ldy #allocateERR
                jmp splerr

_3              lda sparem
                sta frame

                ldx sparem+1
                inx
                inx
                stx frame+1

                lda #<stkbase
                sta stack
                lda #>stkbase
                sta stack+1

                sta cury                ; unknown initial Y value

                rts
                .endproc
