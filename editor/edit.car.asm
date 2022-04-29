;======================================
;   FILE: edit.car.asm
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
;
;======================================
emjmps          rts                     ; Seg catch all

                .word 0
                .byte $03 ;!! ebank     ; curBank
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
ltab            .addr lsh1._lshift      ; LSH
                .addr rshift
                .addr multi
                .addr divi
                .addr remi
                .addr sargs
                .byte $60               ; ChCvt3
                .byte $22               ; tvDisp

                jmp insrtch             ; normal char
                rts                     ; ctrl-shift char
serial          .word $0A00             ; serial number of ROM
                                        ; TODO: to be filled in before burning ROM

                jmp getnext.ismt        ; STM catch all
                rts                     ; illegal monitor cmd

                .byte $86
                .byte $9d
                .addr istmres           ; STMrAdr in EDIT.DEF


;======================================
;   Init RTS
;======================================
Start           .proc
                jsr initkeys            ; get keyboard

                lda warmst
                beq cold

                lda chcvt3
                cmp #$60                ; make sure RAM initialized
                bne cold

_warm           lda mpc                 ; see where we were
                beq _w1

                jmp monitor._mon1       ; monitor

_w1             jmp gmerr.punt          ; editor

cold            lda #0
                tay
_c0             sta $03_0480,y          ; zero RAM
                dey
                bne _c0

                ldy #$3a
_cold1          lda emjmps-1,y          ; init RAM
                dey
                sta jmps,y
                bne _cold1

    ; sty ChCvt1 ; Y=0
                jsr einit               ; init editor

;SPLInit PROC ; init compiler RAM

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
                ldy bigst               ; big s.t. ?
                beq _si1                ; no

                ldx #6
_si1            jsr getmem              ; get hash table

                sta stglobal            ; qglobal hash table
                stx stglobal+1
                ldy bigst               ; big s.t. ?
                beq _si2                ; no

                inx
                inx
                sta stg2                ; big s.t. hash table
                stx stg2+1
_si2            inx
                inx
                sta stlocal             ; local hash table
                stx stlocal+1
                .endproc
