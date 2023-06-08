
; SPDX-FileName: main.bnk.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


en0             .text 5,"Error",$c0
                .addr jt_error
                .byte 3,138,138,138

en1             .text 3,"EOF",$9a
                .addr eof

en2             .text 5,"color",$8a
                .addr FILDAT

en3             .text 4,"LIST",$8a
                .addr list

en4             .text 6,"device",$8a
                .word device

en5             .text 5,"TRACE",$8a
                .addr trace


;======================================
;   CStrt()
;======================================
cstart          .proc
                ldy #ebank
                sty jt_curbank
                sty bank+ebank
                .endproc

                ;[fall-through]


;======================================
;   GetName(char)
;======================================
GetName         .proc
                sta bank+lbank
                jsr lGetName

                .endproc

                ;[fall-through]


;======================================
;   RestoreBank()
;======================================
RestoreBank     .proc
                php
                pha

                tya
                ldy jt_curbank
rbank1          sta bank,y
                tay

                pla
                plp
                rts
                .endproc


;======================================
;   Run(address)
;======================================
run             .proc
;   reset Error routine
                ldy #<splerr
                sty jt_error+1
                ldy #>splerr
                sty jt_error+2

                jsr lproceed
                jsr jsrind

                jmp EditBank

                .endproc


;======================================
;   Compile()
;======================================
compile         .proc
                ldy #cbank
                sty jt_curbank
                sty bank+cbank
                jsr ccompile

                .endproc


;======================================
;   EditBank()
;======================================
EditBank        .proc
                php
                pha
                tya
                ldy #ebank
                sty jt_curbank
                jmp RestoreBank.rbank1

                .endproc


;======================================
;   GetAlias()
;======================================
GetAlias        .proc
                lda #1
                jsr getprop

                cpx #0
                beq _gal1

                sta addr
                stx addr+1
                sta bank+lbank
                lda #0
                jsr getprop

                sta token
                jmp RestoreBank

_gal1           jmp mnum._varerr

                .endproc


;======================================
;   GNlocal()
;======================================
gnlocal         .proc
                sta bank+lbank
                jsr lGetName.lgnlocal

                jmp RestoreBank

                .endproc


;======================================
;   CStmtList()
;======================================
cstmtlst        .proc
                ldy #cbank
                sty jt_curbank
                sta bank+cbank
                jsr stmtlist

                jmp EditBank

                .endproc


;======================================
;
;======================================
mgett1          .proc
                jsr EditBank
                jsr GetTemp.gett1

                .endproc


;======================================
;   LProceed()
;======================================
lproceed        .proc
                ldy #lbank
                sty jt_curbank
                sty bank+lbank
                rts
                .endproc


;======================================
;
;======================================
options         .proc
                jsr lproceed
                jsr libOptSetOpts

                jmp EditBank
                .endproc


;======================================
;
;======================================
GetKey          .proc
                sta bank+lbank
                jsr libKeyGetKey

                jmp RestoreBank

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
                jsr EditBank

                jmp Monitor._mloop

                .endproc


;======================================
;
;======================================
GetArgs         .proc
                pha                     ; save arg type load flag
                sty bank+lbank
                lda #1
                jsr getprop

                sta addr
                stx addr+1

                pla
                bne _ga2                ; don't load arg types

                sta abt                 ; A=0
                sta abt+1               ; flag as temp args
                sta abt+2               ; (default)

                ldy #2
                lda (props),y
                sta numargs
                beq _ga2

                tax
                cpx #9
                bcs _ga2

_ga1            iny
                lda (props),y
                dex
                sta argtypes,x          ; args inverted
                bne _ga1

_ga2            jmp RestoreBank

                .endproc


;======================================
;
;======================================
prth            .proc                   ; call only from LBANK!
                sty bank+ebank
                jsr PrintH

                sty bank+lbank
                jmp libIOChkErr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
dret            .proc                   ; Dret()
                jsr lproceed

                jmp (DOSVEC)

                .endproc
