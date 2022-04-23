;$title(ACTION! 3.6 - 11/4/83)

; Copyright 1983 by Clinton W Parker
; All Rights Reserved
; last modified November 4, 1983
;
; This file is part of Action!.
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


                .include "edit.def.asm"

                * = $8000
                .logical ml
version         .byte $36
date            .byte $1,$17,$84        ; assemble date of latest version!


main
                .include "main.io.asm"
propid          ldx $a0
                .include "screen.mac.asm"
                .include "main.msc.asm"
                .include "main.bnk.asm"

amplfin
                ;.fill 15,$00
;    ACTION! 3.6 - Editor Routines
;    [EDIT.FND, EDIT.SUB, EDIT.TAB]
;    ------------------------------
                .fill 4,$00
                ;* = ml+$08d8
                .include "8.asm"

;    "ACTION! 3.6 - Compiler Routines
;    [AMPL.SEG, AMPL.PF, AMPL.ARR, AMPL.CGU]
;    ---------------------------------------
                .fill 3,$00
                ;* = ml+$0a80
                .include "10.asm"

                .fill 9,$00
                .addr cstart
                .byte $00,$05           ; boot disk and start cart.
                .addr rstbank.init

                .endlogical


;    ACTION! 3.6 - S.T.
;    ------------------
                .logical ll
                .include "ampl.mth.asm"
                .include "ampl.sym.asm"
                .include "lib.key.asm"
                .include "spl.err.asm"
                .include "lib.io.asm"
                .include "lib.gr.asm"
                .include "lib.msc.asm"
                .include "lib.str.asm"

                .include "lib.opt.asm"

cpyright
                .text "ACTION! (c)1983 Action Computer Services (ACS)  November 4, 1983  ",$00

                * = ll+$0fff
                .byte lbank
doc
                .endlogical


;    "ACTION! 3.6 - Compiler
;    -----------------------
                .logical cl
                .include "9.asm"

cright          .text "ACTION! (c)1983 Action Computer Services",$00,$00

                * = cl+$0fff
                .byte cbank

                ;* = ml+$0a80
                ;.include "10.asm"

                .endlogical


;    ACTION! 3.6 - Editor
;    --------------------
                .logical el
                .include "6.asm"
                .include "7.asm"
                .fill 149,$00

                * = el+$0fff
                .byte ebank
                ;.fill 2264,$00

                ;* = ml+$08d8
                ;.include "8.asm"

editend
                .endlogical

                .end
