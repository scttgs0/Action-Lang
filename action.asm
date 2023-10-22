
; SPDX-FileName: action.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 2023, Scott Giese
; SPDX-License-Identifier: GPL-3.0-or-later

                .cpu "65c02"

                .enc "atari-screen-inverse"
                    .cdef " z", $A0
                .enc "none"

;--------------------------------------

                .include "equates/system_f256.equ"
                .include "equates/zeropage.equ"
                .include "equates/action.equ"

                .include "macros/frs_jr_graphic.mac"
                .include "macros/frs_jr_random.mac"
                .include "macros/frs_jr_text.mac"

                .include "equates/editor.equ"


;--------------------------------------
;--------------------------------------
                * = $6000
;--------------------------------------

;   Boot from RAM data block

                .byte $F2,$56           ; signature
                .byte $03               ; slot count
                .byte $03               ; start slot
                .addr BOOT_             ; execute address
                .word $0306             ; version       ; TODO: assemble date of latest version!
                .word $0000             ; kernel
                .null 'Action!'         ; binary name


;--------------------------------------

BOOT_           ldx #$FF                ; initialize the stack
                txs
                jmp INIT


;--------------------------------------
;--------------------------------------
; Initialization code

;--------------------------------------
;--------------------------------------
                * = $7FE0
;--------------------------------------

INIT            clc
                jsr cstart
                jmp Start

;--------------------------------------
;--------------------------------------
                * = $8000
;--------------------------------------

version         .byte $40
date            .byte $07,$05,$23        ; TODO: assemble date of latest version!


                .include "main.io.asm"
propid          ldx arg0
                .include "screen.mac.asm"
                .include "main.msc.asm"
                .include "main.bank.asm"

amplfin

;    ACTION! - Editor Routines
;    ----------------------------------

                .fill 4,$00
                .include "editor/edit.find.asm"
                .include "editor/edit.substitute.asm"
                .include "editor/edit.tab.asm"


;    "ACTION! - Compiler Routines
;    ----------------------------------

                .fill 3,$00
                .include "ampl/ampl.segment.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.array.asm"
                .include "ampl/ampl.cgu.asm"


;    ACTION! - Symbol Table
;    ----------------------------------

                .include "ampl/ampl.math.asm"
                .include "ampl/ampl.symbol.asm"
                .include "library/lib.key.asm"
                .include "spl.err.asm"
                .include "library/lib.io.asm"
                .include "library/lib.gr.asm"
                .include "library/lib.msc.asm"
                .include "library/lib.str.asm"
                .include "library/lib.opt.asm"

cpyright        .null " ACTION! (c) 2023 GPL3           Foenix Adaptation           v3.6 July 05, 2022 "


;    "ACTION! - Compiler
;    -----------------------

main
                .include "compiler.main.asm"

cright          .text "ACTION! (c) 2023 GPL3      Foenix Adaptation",$00,$00


;    ACTION! 3.6 - Editor
;    --------------------

                .include "storage.mac.asm"
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

                .include "ampl/ampl.monitor.asm"
                .include "ampl/ampl.init.asm"

                .include "platform_f256.asm"

                .text "ces",$00,$00

editend
                .end
