
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.ini.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   SPLsetup()
;======================================
splsetup        .proc
                lda #0
                tay
                sta sp
                sta chan
                sta symtab
                sta INITAD+1
                sta (buf),y
                sta param
                sta qglobal
                sta stackptr
                sta arrayptr+1
                sta whaddr+1
                sta curnxt+1
                sta procsp

    ; clear qglobal s.t.
                ldx bigst               ; big s.t. ?
                beq _spls0              ; no

_s0             sta (stg2),y
                iny
                bne _s0

_spls0          sta (stglobal),y
                iny
                bne _spls0

    ; get last block in heap
                lda afbase
                ldx afbase+1
                sta arg0
                stx arg1
_spls1          ldy #1
                lda (arg0),y
                beq _spls2

                tax
                dey
                lda (arg0),y
                sta arg0
                stx arg1
                jmp _spls1

_spls2          clc
                lda arg0
                adc #4
                bcc _spls3

                inc arg1
_spls3          sta codebase
                sta qcode

                lda arg1
                sta codebase+1
                sta qcode+1

                lda MEMTOP+1
                sta stmax
                dec stmax
                clc
                sbc stsp
                sta stbase
                sta symtab+1
                inc symtab+1

                cmp qcode+1
                bcs _spls4

    ; can't allocate memory
                lda sparem
                sta symtab
                lda sparem+1
                sta symtab+1
_alcerr         ldy #alcer
                jmp splerr

_spls4          lda sparem
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
