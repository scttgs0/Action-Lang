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


                .enc "atari-screen-inverse"
                .cdef " z", $A0
                .enc "none"

;--------------------------------------

                ;.include "equates_system_atari8.asm"
                .include "equates_system_c256.asm"
                .include "editor/edit.def.asm"

                * = $8000
                .logical ml
version         .byte $40
date            .byte $04,$24,$22        ; assemble date of latest version!


                .include "main.io.asm"
propid          ldx $a0
                .include "screen.mac.asm"
                .include "main.msc.asm"
                .include "main.bnk.asm"

amplfin
;    ACTION! - Editor Routines
;    [EDIT.FND, EDIT.SUB, EDIT.TAB]
;    ------------------------------
                .fill 4,$00
                .include "editor/edit.fnd.asm"
                .include "editor/edit.sub.asm"
                .include "editor/edit.tab.asm"


;    "ACTION! - Compiler Routines
;    [AMPL.SEG, AMPL.PF, AMPL.ARR, AMPL.CGU]
;    ---------------------------------------
                .fill 3,$00
                .include "ampl/ampl.seg.asm"
                .include "ampl/ampl.pf.asm"
                .include "ampl/ampl.arr.asm"
                .include "ampl/ampl.cgu.asm"

                .fill 9,$00
                .addr cstart
                .byte $00,$05           ; boot disk and start cart.
                .addr rstbank.init

                .endlogical


;    ACTION! - S.T.
;    ------------------
                .logical ll
                .include "ampl/ampl.mth.asm"
                .include "ampl/ampl.sym.asm"
                .include "library/lib.key.asm"
                .include "spl.err.asm"
                .include "library/lib.io.asm"
                .include "library/lib.gr.asm"
                .include "library/lib.msc.asm"
                .include "library/lib.str.asm"

                .include "library/lib.opt.asm"

cpyright
               ;.text "ACTION! (c)1983 Action Computer Services (ACS)  November 4, 1983  ",$00
                .text "ACTION! (c)2022 GPL3            Foenix version  April 24, 2022    ",$00

                * = ll+$0fff
                .byte lbank
doc
                .endlogical

;    "ACTION! - Compiler
;    -----------------------
                .logical cl
main
                .include "comp.main.asm"

cright         ;.text "ACTION! (c)1983 Action Computer Services",$00,$00
                .text "ACTION! (c)2022 GPL3      Foenix version",$00,$00

                * = cl+$0fff
                .byte cbank

                .endlogical


;    ACTION! 3.6 - Editor
;    --------------------
                .logical el
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

                .text "ces",$00,$00

                * = el+$0fff
                .byte $01

editend
                .endlogical

                .end
