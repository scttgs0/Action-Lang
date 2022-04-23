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


                .include "1.asm"

                * = $8000
                .logical ml
version         .byte $36
date            .byte $1,$17,$84        ; assemble date of latest version!


                .include "2.asm"
propid          ldx $a0
                .include "3.asm"
main
                .fill 1830,$00

                .addr cstart
                .word $0500             ; boot disk and start cart.
                .addr rstbank.init

                .endlogical


;    ACTION! 3.6 - S.T.
;    ------------------
                .logical ll
                .include "4.asm"
                .include "5.asm"

cpyright
                .text "ACTION! (c)1983 Action Computer Services (ACS)  November 4, 1983  ",$00

                * = ll+$0fff
                .byte lbank
doc
                .endlogical

;    ACTION! 3.6 - Editor
;    --------------------
                .logical el
                .include "6.asm"
                .include "7.asm"
                .fill 149,$00

                * = el+$0fff
                .byte ebank
                .fill 2264,$00

                * = ml+$08d8
                .include "8.asm"

editend
                .fill 3,$00
                .endlogical

;    "ACTION! 3.6 - Compiler
;    -----------------------
                * = cl
                .include "9.asm"

cright          .text "ACTION! (c)1983 Action Computer Services"

                * = cl+$0fff
                .byte cbank

                * = ml+$0a80
                .include "10.asm"

amplfin
                .fill 15,$00
                .end
