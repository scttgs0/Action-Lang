
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.car.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;
;======================================
emjmps          rts                     ; Seg catch all

                .word 0
                .byte ebank             ; curBank
                .byte $df               ; stMask
                jmp splerr              ; Error

                .byte 18                ; wSize
                .byte 120               ; line input max
                .byte $20               ; ChCvt2
                rts                     ; Exp catch all

                .word 0
                rts                     ; Dcl catch all

                .word 0
                rts                     ; CodeGen catch all

                .word 0
                rts                     ; ArrRef Catch all
zero            .word 0
                rts                     ; SPLEnd

                .word 0
                jmp scrbell             ; Alarm

                .byte 0                 ; EOLch (default = space)
ltab            .word lsh1._lshift      ; LSH
                .word rshift
                .word multi
                .word divi
                .word remi
                .word sargs
                .byte $60               ; ChCvt3
                .byte $22               ; tvDisp

                jmp insrtch             ; normal char

                rts                     ; ctrl-shift char

serial          .word $0A00             ; serial number of ROM
                                        ; TODO: to be filled in before burning ROM

                jmp getnext._ENTRY5        ; STM catch all

                rts                     ; illegal monitor cmd

                .byte $86
                .byte $9d
                .word istmres           ; STMrAdr in EDIT.DEF


;======================================
;   Init RTS
;======================================
start           .proc
                jsr initkeys            ; get keyboard

                lda warmst
                beq cold

                lda chcvt3
                cmp #$60                ; make sure RAM initialized
                bne cold

_warm           lda mpc                 ; see where we were
                beq _1

                jmp monitor._ENTRY1     ; monitor

_1              jmp gmerr.punt          ; editor

cold            lda #0
                tay
_next1          sta $0480,Y             ; zero RAM

                dey
                bne _next1

                ldy #$3A
_next2          lda emjmps-1,Y          ; init RAM
                dey
                sta jmps,Y
                bne _next2

;               sty ChCvt1              ; Y=0

                jsr einit               ; init editor

;SPLInit PROC                           ; init compiler RAM

        .if ramzap
                jsr zap4
        .else
                nop
                nop
                nop
        .endif

                ldx #8                  ; 2K id space
                stx stsp

                lda #0
                ldx #4
                ldy bigst               ; big symbol table?
                beq _2                  ;   no

                ldx #6
_2              jsr getmem              ; get hash table

                sta stglobal            ; qglobal hash table
                stx stglobal+1

                ldy bigst               ; big symbol table?
                beq _3                  ;   no

                inx
                inx
                sta stg2                ; big symbol table hash table
                stx stg2+1

_3              inx
                inx
                sta stlocal             ; local hash table
                stx stlocal+1

;               rts

                .endproc
