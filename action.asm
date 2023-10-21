
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
                .include "main.bnk.asm"

amplfin

;    ACTION! - Editor Routines
;    ----------------------------------

                .fill 4,$00
                .include "editor/edit.fnd.asm"
                .include "editor/edit.sub.asm"
                .include "editor/edit.tab.asm"


;    "ACTION! - Compiler Routines
;    ----------------------------------

                .fill 3,$00
                .include "ampl/ampl.seg.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.arr.asm"
                .include "ampl/ampl.cgu.asm"


;    ACTION! - Symbol Table
;    ----------------------------------

                .include "ampl/ampl.mth.asm"
                .include "ampl/ampl.sym.asm"
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
                .include "comp.main.asm"

cright          .text "ACTION! (c) 2023 GPL3      Foenix Adaptation",$00,$00


;    ACTION! 3.6 - Editor
;    --------------------

                .include "storage.mac.asm"
                .include "editor/edit.mem.asm"
                .include "editor/edit.car.asm"
                .include "editor/edit.man.asm"
                .include "editor/edit.chr.asm"
                .include "editor/edit.ini.asm"
                .include "editor/edit.io.asm"
                .include "editor/edit.wnd.asm"
                .include "editor/edit.dsp.asm"
                .include "editor/edit.cmd.asm"
                .include "editor/edit.tag.asm"

                .include "ampl/ampl.mon.asm"
                .include "ampl/ampl.ini.asm"

                .include "platform_f256.asm"

                .text "ces",$00,$00

editend
                .end
