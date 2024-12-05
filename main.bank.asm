
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.bank.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


en0             .ptext "Error"
                .byte $C0
                .addr jt_error
                .byte 3,138,138,138

en1             .ptext "EOF"
                .byte $9A
                .addr eof

en2             .ptext "color"
                .byte $8A
                ;!!.addr FILDAT

en3             .ptext "LIST"
                .byte $8A
                .addr list

en4             .ptext "device"
                .byte $8A
                .word device

en5             .ptext "TRACE"
                .byte $8A
                .addr trace


;======================================
;   CStrt()
;======================================
CSTART          .proc
                ;!!ldy #ebank
                sty jt_curbank
                ;!!sty bank+ebank

                jmp START

                .endproc


;======================================
;   GetName(char)
;======================================
GetName         .proc
                ;!!sta bank+lbank
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
rbank1          ;!!sta bank,Y
                tay

                pla
                plp
init            rts
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
                ;!!ldy #cbank
                sty jt_curbank
                ;!!sty bank+cbank
                jsr ccompile

                .endproc

                ;[fall-through]


;======================================
;   EditBank()
;======================================
EditBank        .proc
                php
                pha

                tya
                ;!!ldy #ebank
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
                beq _XIT

                sta addr
                stx addr+1
                ;!!sta bank+lbank

                lda #0
                jsr getprop

                sta token

                jmp RestoreBank

_XIT            jmp mnum._varerr

                .endproc


;======================================
;   GNlocal()
;======================================
gnlocal         .proc
                ;!!sta bank+lbank

                jsr lGetName._ENTRY1

                jmp RestoreBank

                .endproc


;======================================
;   CStmtList()
;======================================
cstmtlst        .proc
                ;!!ldy #cbank
                sty jt_curbank
                ;!!sta bank+cbank

                jsr stmtlist

                jmp EditBank

                .endproc


;======================================
;
;======================================
mgett1          .proc
                jsr EditBank
                jsr GetTemp._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   LProceed()
;======================================
lproceed        .proc
                ;!!ldy #lbank
                sty jt_curbank
                ;!!sty bank+lbank

                rts
                .endproc


;======================================
;
;======================================
options         .proc
                jsr lproceed
                jsr libOptSet

                jmp EditBank

                .endproc


;======================================
;
;======================================
GetKey          .proc
                ;!!sta bank+lbank

                jsr libKeyGet

                jmp RestoreBank

                .endproc


;======================================
;
;======================================
splerr          .proc
                ;!!sta bank+lbank

                jmp lsplerr

                .endproc


;======================================
;
;======================================
emloop          .proc
                jsr EditBank

                jmp Monitor._ENTRY2

                .endproc


;======================================
;
;======================================
GetArgs         .proc
                pha                     ; save arg type load flag

                ;!!sty bank+lbank

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
                lda (zpAllocProps),Y
                sta numargs
                beq _XIT

                tax
                cpx #9
                bcs _XIT

_next1          iny
                lda (zpAllocProps),Y
                dex
                sta argtypes,X          ; args inverted
                bne _next1

_XIT            jmp RestoreBank

                .endproc


;======================================
; call only from LBANK!
;======================================
prth            .proc
                ;!!sty bank+ebank

                jsr PrintH

                ;!!sty bank+lbank

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
