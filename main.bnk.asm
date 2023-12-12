
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.bnk.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


en0             .text 5,"Error",$C0
                .word error
                .byte 3,138,138,138

en1             .text 3,"EOF",$9A
                .word eof

en2             .text 5,"color",$8A
                .word FILDAT

en3             .text 4,"LIST",$8A
                .word list

en4             .text 6,"device",$8A
                .word device

en5             .text 5,"TRACE",$8A
                .word trace


;======================================
;   CStrt()
;======================================
cstart          .proc
                ldy #ebank
                sty curbank
                sty bank+ebank

                jmp start

                .endproc


;======================================
;   GetName(char)
;======================================
getname         .proc
                sta bank+lbank
                jsr lgetname

                .endproc

                ;[fall-through]


;======================================
;   RstBank()
;======================================
rstbank         .proc
                php
                pha

                tya
                ldy curbank
rbank1          sta bank,Y
                tay

                pla
                plp
init            rts
                .endproc


;======================================
;   Run(address)
;======================================
run             .proc
; reset Error routine
                ldy #<splerr
                sty error+1
                ldy #>splerr
                sty error+2

                jsr lproceed
                jsr jsrind

                jmp editbank

                .endproc


;======================================
;   Compile()
;======================================
compile         .proc
                ldy #cbank
                sty curbank
                sty bank+cbank

                jsr ccompile

                .endproc

                ;[fall-through]


;======================================
;   EditBank()
;======================================
editbank        .proc
                php
                pha

                tya
                ldy #ebank
                sty curbank

                jmp rstbank.rbank1

                .endproc


;======================================
;   GetAlias()
;======================================
getalias        .proc
                lda #1
                jsr getprop

                cpx #0
                beq _XIT

                sta addr
                stx addr+1
                sta bank+lbank

                lda #0
                jsr getprop

                sta token

                jmp rstbank

_XIT            jmp mnum._varerr

                .endproc


;======================================
;   GNlocal()
;======================================
gnlocal         .proc
                sta bank+lbank

                jsr lgetname._ENTRY1

                jmp rstbank

                .endproc


;======================================
;   CStmtList()
;======================================
cstmtlst        .proc
                ldy #cbank
                sty curbank
                sta bank+cbank

                jsr stmtlist

                jmp editbank

                .endproc


;======================================
;
;======================================
mgett1          .proc
                jsr editbank
                jsr gettemp._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   LProceed()
;======================================
lproceed        .proc
                ldy #lbank
                sty curbank
                sty bank+lbank

                rts
                .endproc


;======================================
;
;======================================
options         .proc
                jsr lproceed
                jsr setopts

                jmp editbank

                .endproc


;======================================
;
;======================================
getkey          .proc
                sta bank+lbank

                jsr lgetkey

                jmp rstbank

                .endproc


;======================================
;
;======================================
splerr          .proc
                sta bank+lbank

                jmp lsplerr

                .endproc


;======================================
;
;======================================
emloop          .proc
                jsr editbank

                jmp monitor._ENTRY2

                .endproc


;======================================
;
;======================================
getargs         .proc
                pha                     ; save arg type load flag

                sty bank+lbank

                lda #1
                jsr getprop

                sta addr
                stx addr+1

                pla
                bne _XIT                ; don't load arg types

                sta abt                 ; A=0
                sta abt+1               ; flag as temp args
                sta abt+2               ; (default)

                ldy #2
                lda (props),Y
                sta numargs
                beq _XIT

                tax
                cpx #9
                bcs _XIT

_next1          iny
                lda (props),Y
                dex
                sta argtypes,X          ; args inverted
                bne _next1

_XIT            jmp rstbank

                .endproc


;======================================
; call only from LBANK!
;======================================
prth            .proc
                sty bank+ebank

                jsr printh

                sty bank+lbank

                jmp chkerr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
dret            .proc                   ; Dret()
                jsr lproceed

                jmp (dosvec)

                .endproc
