
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.cartridge.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


cartridge      .namespace

;--------------------------------------
;--------------------------------------
;   Editor/Monitor jump table

emjmps          rts                     ; Seg catch all

                .word 0
                ;!!.byte ebank          ; curBank
                .byte $df               ; stMask
                jmp bankSPLErr          ; Error

                .byte 18                ; wSize
                .byte 120               ; line input max
                .byte $20               ; jt_chrConvert2
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
                jmp screenBell          ; Alarm

                .byte 0                 ; EOLch (default = space)

ltab            .addr mscLShift._lshift ; LSH
                .addr mathRShift
                .addr mathMultI
                .addr mathDivC
                .addr mathRemL
                .addr mathSArgs
                .byte $60               ; jt_chrConvert3
                .byte $22               ; tvDisp

                jmp editor.chr.InsertChar   ; normal char

                rts                     ; ctrl-shift char

serial          .word $0A00             ; serial number of ROM
                                        ; TODO: to be filled in before burning ROM

                jmp LexGetNext._ENTRY5  ; STM catch all

                rts                     ; illegal Monitor cmd

                .byte $86
                .byte $9D
                .addr symSTMres         ; STMrAdr in EDIT.DEF


;======================================
; Init RTS
;======================================
START           .proc
                jsr editor.io.InitKeys  ; get keyboard

                lda WARMST
                beq _cold

                lda jt_chrConvert3
                cmp #$60                ; make sure RAM initialized
                bne _cold

; - - - - - - - - - - - - - - - - - - -

_warm           lda isMonitorLive       ; see where we were
                beq _XIT1

                jmp Monitor._ENTRY1

_XIT1           jmp editor.memory.GeneralErr.Punt  ; editor

; - - - - - - - - - - - - - - - - - - -

_cold           lda #$00
                tay
_next1          sta $0480,Y             ; zero RAM

                dey
                bne _next1

                ldy #$3A
_next2          lda emjmps-1,Y          ; init RAM
                dey
                sta jt_jmps,Y
                bne _next2

                lda #<symSTMres
                sta jt_stmradr
                lda #>symSTMres
                sta jt_stmradr+1
                lda #`symSTMres
                sta jt_stmradr+2

                ; sty chrConvert1       ; Y=0

                jsr editor.init.EditorInit  ; init editor

;SPLInit PROC ; init compiler RAM

            .if ZAPRAM
                jsr zap4
            .else
                nop
                nop
                nop
            .endif

                ldx #$08                ; 2K id space
                stx SymTblSizePages

                lda #$00
                ldx #$04
                ldy isBigSymTbl         ; big symbol table?
                beq _2                  ;   no

                ldx #$06
_2              jsr editor.memory.Get   ; get hash table

                sta symTblGlobal        ; qglobal hash table
                stx symTblGlobal+1

                ldy isBigSymTbl         ; big symbol table?
                beq _3                  ;   no

                inx
                inx
                sta bigSymTblGlobal     ; big symbol table hash table
                stx bigSymTblGlobal+1

_3              inx
                inx
                sta symTblLocal         ; local hash table
                stx symTblLocal+1

                .endproc

                .endnamespace
