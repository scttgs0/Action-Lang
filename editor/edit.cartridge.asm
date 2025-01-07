
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

;   clone into jt_jmps [$04C6:04FF]

emjmps
_jt_vecSegEnd   rts                     ; Seg catch all
                .word 0

_jt_curbank     .byte ebank
_jt_stmask      .byte $DF

_jt_vecError    jmp mainbank.SPLErr

_jt_wsize       .byte 18
_jt_linemax     .byte 120               ; line input max
_jt_chrConvert2 .byte $20

_jt_vecExpEnd   rts                     ; Exp catch all
                .word 0

_jt_vecDeclEnd  rts                     ; Dcl catch all
                .word 0

_jt_vecCGenEnd  rts                     ; CodeGen catch all
                .word 0

_jt_vecArrEnd   rts                     ; ampl.array.Ref Catch all
zero            .word 0

_jt_vecSPLEnd   rts
                .word 0

_jt_vecAlarm    jmp screen.Bell         ; Alarm

_jt_eolch       .byte 0                 ; EOLch (default = space)

_jt_lsh
ltab            .addr mainmsc.LShift._lshift
                .addr ampl.math.RShift
                .addr ampl.math.MultI
                .addr ampl.math.DivC
                .addr ampl.math.RemL
                .addr ampl.math.SArgs

_jt_chrConvert3 .byte $60
_jt_tvdisp      .byte $22

_jt_vecDispTb   jmp editor.chr.InsertChar   ; normal char

                rts                     ; ctrl-shift char

serial          .word $0A00             ; serial number of ROM
                                        ; TODO: to be filled in before burning ROM

_jt_vecStmtEnd  jmp compiler.lexicon.GetNext._ENTRY5  ; STM catch all

                rts                     ; illegal monitor cmd
                .byte $86
                .byte $9D

_jt_vecStmRAdr  .addr ampl.symbol.StmtReserved    ; STMrAdr in EDIT.DEF


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

                jmp ampl.monitor.Monitor._ENTRY1

_XIT1           jmp editor.memory.GeneralErr.Punt  ; editor

; - - - - - - - - - - - - - - - - - - -

;   clear RAM [$0480:057F]
_cold           lda #$00
                tay
_next1          sta $0480,Y             ; zero RAM

                dey
                bne _next1

;   build jump table jt_jmps [$B282:B2BC] -> [$04C6:04FF]
                ldy #$3A
_next2          lda emjmps-1,Y          ; init RAM
                dey
                sta jt_jmps,Y
                bne _next2

                ;-- lda #<ampl.symbol.StmtReserved
                ;-- sta jt_vecStmRAdr
                ;-- lda #>ampl.symbol.StmtReserved
                ;-- sta jt_vecStmRAdr+1
                ;-- lda #`ampl.symbol.StmtReserved
                ;-- sta jt_vecStmRAdr+2

                ; sty chrConvert1       ; Y=0

                jsr editor.init.EditorInit  ; init editor

;SPLInit PROC   ; init compiler RAM

            .if ZAPRAM
                jsr editor.main.zapBckSpc
            .else
                nop
                nop
                nop
            .endif

                ldx #$08                ; 2K id space
                stx nSymTblPages

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

                ;[fall-through to edit.main.asm]

                .endnamespace
