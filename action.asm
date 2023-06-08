
; SPDX-FileName: action.asm
; SPX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 2023, Scott Giese
; SPDX-License-Identifier: GPL-3.0-or-later


                .cpu "65c02"

;///  Platform  ///////////////////////

                .enc "atari-screen-inverse"
                    .cdef " z", $A0
                .enc "none"

;--------------------------------------

                .include "equates/system_f256jr.equ"
                .include "equates/zeropage.equ"
                .include "equates/action.equ"

                .include "macros/frs_jr_graphic.mac"
                .include "macros/frs_jr_text.mac"

;//////////////////////////////////////

                .include "editor/edit.def.asm"

                * = INIT-8
                .text "PGX"
                .byte $01
                .dword INIT

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
date            .byte $04,$24,$22        ; TODO: assemble date of latest version!


                .include "main.io.asm"
propid          ldx arg0
                .include "screen.mac.asm"
                .include "main.msc.asm"
                .include "main.bnk.asm"

amplfin
;    ACTION! - Editor Routines
;    ------------------------------
                .fill 4,$00
                .include "editor/edit.fnd.asm"
                .include "editor/edit.sub.asm"
                .include "editor/edit.tab.asm"


;    "ACTION! - Compiler Routines
;    ---------------------------------------
                .fill 3,$00
                .include "ampl/ampl.seg.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.arr.asm"
                .include "ampl/ampl.cgu.asm"


;    ACTION! - Symbol Table
;    ------------------
                .include "ampl/ampl.mth.asm"
                .include "ampl/ampl.sym.asm"
                .include "library/lib.key.asm"
                .include "spl.err.asm"
                .include "library/lib.io.asm"
                .include "library/lib.gr.asm"
                .include "library/lib.msc.asm"
                .include "library/lib.str.asm"
                .include "library/lib.opt.asm"

cpyright        .text "ACTION! (c) 2023 GPL3           Foenix Adaptation          v3.6 April 24, 2022",$00


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

                .include "platform_f256jr.asm"

                .text "ces",$00,$00

editend
                .end
