
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

                .include "equates/system_atari8.equ"
                .include "equates/editor.equ"

;--------------------------------------
;--------------------------------------
                * = $8000
                .logical ml
;--------------------------------------

version         .byte $36
date            .byte $01,$17,$84       ; assemble date of latest version!


                .include "main.io.asm"

propid          ldx $A0

                .include "screen.mac.asm"

                .include "compiler.inc"
compiler    .namespace
                .include "compiler.lexicon.asm"
            .endnamespace

                .include "main.msc.asm"
                .include "main.bank.asm"

amplfin

;    ACTION! 3.6 - Editor Routines
;    [EDIT.FND, EDIT.SUB, EDIT.TAB]
;    ------------------------------

editor      .namespace
                .fill 4,$00
                .include "editor/edit.find.asm"
                .include "editor/edit.substitute.asm"
                .include "editor/edit.tab.asm"
            .endnamespace


;    "ACTION! 3.6 - Compiler Routines
;    [AMPL.SEG, AMPL.PF, AMPL.ARR, AMPL.CGU]
;    ---------------------------------------

ampl        .namespace
                .fill 3,$00
                .include "ampl/ampl.segment.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.array.asm"
                .include "ampl/ampl.cgu.asm"
            .endnamespace

                .fill 9,$00

                .addr bankCartStart
                .byte $00,$05           ; boot disk and start cart.
                .addr bankRestore.init


;--------------------------------------
;--------------------------------------
                .endlogical
;--------------------------------------


;    ACTION! 3.6 - S.T.
;    ------------------


;--------------------------------------
;--------------------------------------
                .logical ll
;--------------------------------------

            .namespace ampl
                .include "ampl/ampl.math.asm"
                .include "ampl/ampl.symbol.asm"
            .endnamespace

                .include "library/lib.key.asm"
                .include "spl.err.asm"
                .include "library/lib.io.asm"
                .include "library/lib.gr.asm"
                .include "library/lib.msc.asm"
                .include "library/lib.str.asm"
                .include "library/lib.opt.asm"

cpyright        .null "ACTION! (c)1983 Action Computer Services (ACS)  November 4, 1983  "


;--------------------------------------
;--------------------------------------
                * = ll+$0FFF
;--------------------------------------

                .byte lbank

doc
;--------------------------------------
;--------------------------------------
                .endlogical
;--------------------------------------


;    "ACTION! 3.6 - Compiler
;    -----------------------


;--------------------------------------
;--------------------------------------
                .logical cl
;--------------------------------------

            .namespace compiler
                .include "compiler.main.asm"

comp_copyright  .null "ACTION! (c)1983 Action Computer Services"
                .byte $00
            .endnamespace

;--------------------------------------
;--------------------------------------
                * = cl+$0fff
;--------------------------------------

                .byte cbank


;--------------------------------------
;--------------------------------------
                .endlogical
;--------------------------------------


;    ACTION! 3.6 - Editor
;    --------------------


;--------------------------------------
;--------------------------------------
                .logical el
;--------------------------------------

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
                * = el+$0fff
;--------------------------------------

                .byte $01

editend
;--------------------------------------
;--------------------------------------
                .endlogical
;--------------------------------------

                .end
