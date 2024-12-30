
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: action.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


                .enc "atari-screen-inverse"
                    .cdef " z",$A0
                .enc "none"

;--------------------------------------

                .include "equates/system_f256.equ"
                .include "equates/zeropage.equ"
                .include "equates/action.equ"

                .include "macros/f256_graphic.mac"
                .include "macros/f256_random.mac"
                .include "macros/f256_text.mac"

                .include "equates/editor.equ"


;--------------------------------------
;--------------------------------------
                * = $7000
;--------------------------------------

;   Boot from RAM data block

.if PGZ=0
                .byte $F2,$56           ; signature
                .byte $03               ; slot count
                .byte $03               ; START slot
                .addr BOOT              ; execute address
                .word $0306             ; version       ; TODO: assemble date of latest version!
                .word $0000             ; kernel
                .null 'Action!'         ; binary name
.endif

;--------------------------------------

BOOT            cld

                ldx #$FF                ; initialize the stack
                txs

                jmp INIT


;--------------------------------------
;   Initialization code

INIT            .proc
                clc

                stz LMARGN
                lda #39
                sta RMARGN

                lda #<$7FFF
                sta MEMTOP
                lda #>$7FFF
                sta MEMTOP+1

                lda #<$0700
                sta MEMLO
                lda #>$0700
                sta MEMLO+1

                jsr PrepBanks
                jsr SetFont

                .frsGraphics mcTextOn,mcVideoMode240|mcTextDoubleX|mcTextDoubleY
                stz DINDEX              ; text mode

                jsr ClearScreen

                jsr mainbank.CartStart
                ;[no return]

                .endproc


;======================================
; clone banks into upper memory
;======================================
PrepBanks       .proc
                ; TODO:
                rts
                .endproc


;--------------------------------------
;--------------------------------------

                .include "platform_f256.asm"
                .include "CIO_wedge.asm"

                .align $0100
                .include "atari-screen.inc"


;--------------------------------------
;--------------------------------------
                * = $8000
;--------------------------------------

version         .byte $40               ; TODO: [M.m]    4.0
versionDate     .byte $24,$12,$19       ; TODO: [YYMMDD] assemble date of latest version!



                .include "main.io.asm"

propid          ldx $A0

                .include "screen.mac.asm"
                .include "compiler.inc"

compiler    .namespace
                .include "compiler.lexicon.asm"
            .endnamespace

                .include "main.msc.asm"
                .include "main.bank.asm"


;--------------------------------------
;    ACTION! - Editor Routines

editor      .namespace
                .fill 4,$00
                .include "editor/edit.find.asm"
                .include "editor/edit.substitute.asm"
                .include "editor/edit.tab.asm"
            .endnamespace


;--------------------------------------
;    "ACTION! - Compiler Routines

ampl        .namespace
                .fill 3,$00
                .include "ampl/ampl.segment.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.array.asm"
                .include "ampl/ampl.cgu.asm"
            .endnamespace


;--------------------------------------
;    ACTION! - Symbol Table

                ;.align $1000
            .namespace ampl
                .include "ampl/ampl.math.asm"
                .include "ampl/ampl.symbol.asm"
            .endnamespace

lib         .namespace
                .include "library/lib.key.asm"
            .endnamespace

                .include "spl.err.asm"

            .namespace lib
                .include "library/lib.io.asm"
                .include "library/lib.gr.asm"
                .include "library/lib.msc.asm"
                .include "library/lib.str.asm"
                .include "library/lib.opt.asm"

ampl_copyright  .null " ACTION! (c) 2024 GPL3           Foenix Adaptation           v4.0 Dec 04, 2024 "
            .endnamespace


;--------------------------------------
;    "ACTION! - Compiler

                ;.align $1000

            .namespace compiler
                .include "compiler.main.asm"

comp_copyright  .null "ACTION! (c) 2024 GPL3           Foenix Adaptation"
                .byte $00
            .endnamespace


;--------------------------------------
;    ACTION! 4.0 - Editor

                ;.align $1000

                .include "storage.mac.asm"

            .namespace editor
                .include "editor/edit.memory.asm"
                .include "editor/edit.cartridge.asm"
                .include "editor/edit.main.asm"
                .include "editor/edit.chr.asm"
                .include "editor/edit.init.asm"
                .include "editor/edit.io.asm"
                .include "editor/edit.window.asm"
                .include "editor/edit.display.asm"
                .include "editor/edit.command.asm"
                .include "editor/edit.tag.asm"
            .endnamespace

            .namespace ampl
                .include "ampl/ampl.monitor.asm"
                .include "ampl/ampl.init.asm"
            .endnamespace

edit_copyright  .null "ces"
                .byte $00


;--------------------------------------
;--------------------------------------
                ;* = el+$0fff
;--------------------------------------

                ;.byte $01

editend
                .end
