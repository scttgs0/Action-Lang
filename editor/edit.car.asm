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
                .byte ebank             ; curBank
                .byte $df               ; stMask
                jmp splerr              ; Error

                .byte 18                ; wSize
                .byte 120               ; line input max
                .byte $20               ; chrConvert2
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
                .addr RShift
                .addr MultI
                .addr DivC
                .addr remi
                .addr SArgs
                .byte $60               ; chrConvert3
                .byte $22               ; tvDisp

                jmp InsertChar          ; normal char
                rts                     ; ctrl-shift char
serial          .word $0A00             ; serial number of ROM
                                        ; TODO: to be filled in before burning ROM

                jmp GetNext.ismt        ; STM catch all
                rts                     ; illegal Monitor cmd

                .byte $86
                .byte $9d
                .addr iSTMres           ; STMrAdr in EDIT.DEF


;======================================
;   Init RTS
;======================================
Start           .proc
                jsr InitKeys            ; get keyboard

                lda WARMST
                beq cold

                lda chrConvert3
                cmp #$60                ; make sure RAM initialized
                bne cold

_warm           lda isMonitorLive       ; see where we were
                beq _w1

                jmp Monitor._mon1

_w1             jmp GeneralMemErr.Punt  ; editor

cold            lda #0
                tay
_nextZero       sta $03_0480,y          ; zero RAM
                dey
                bne _nextZero

                ldy #$3a
_nextJmps       lda emjmps-1,y          ; init RAM
                dey
                sta jmps,y
                bne _nextJmps

    ; sty chrConvert1 ; Y=0
                jsr EditorInit          ; init editor

;SPLInit PROC ; init compiler RAM

        .if ramzap
                jsr zap4
        .else
                nop
                nop
                nop
        .endif

                ldx #8                  ; 2K id space
                stx SymTblSizePages

                lda #0
                ldx #4
                ldy isBigSymTbl
                beq _1                  ; no

                ldx #6
_1              jsr GetMemory           ; get hash table

                sta symTblGlobal        ; qglobal hash table
                stx symTblGlobal+1
                ldy isBigSymTbl
                beq _2                  ; no

                inx
                inx
                sta bigSymTblGlobal     ; big symbol table hash table
                stx bigSymTblGlobal+1
_2              inx
                inx
                sta symTblLocal         ; local hash table
                stx symTblLocal+1
                .endproc
