
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
; bankCartStart()
;======================================
bankCartStart   .proc
                ;!!ldy #ebank
                sty jt_curbank
                ;!!sty bank+ebank

                jmp START

                .endproc


;======================================
; bankGetName(char)
;======================================
bankGetName     .proc
                ;!!sta bank+lbank
                jsr lGetName

                .endproc

                ;[fall-through]


;======================================
; bankRestoreBank()
;======================================
bankRestoreBank .proc
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
; bankRun(address)
;======================================
bankRun         .proc
;   reset Error routine
                ldy #<bankSplErr
                sty jt_error+1
                ldy #>bankSplErr
                sty jt_error+2

                jsr bankLProceed
                jsr mscJSRIndirect

                jmp bankEditBank

                .endproc


;======================================
; bankCompile()
;======================================
bankCompile     .proc
                ;!!ldy #cbank
                sty jt_curbank
                ;!!sty bank+cbank
                jsr ccompile

                .endproc

                ;[fall-through]


;======================================
; bankEditBank()
;======================================
bankEditBank    .proc
                php
                pha

                tya
                ;!!ldy #ebank
                sty jt_curbank

                jmp bankRestoreBank.rbank1

                .endproc


;======================================
; bankGetAlias()
;======================================
bankGetAlias    .proc
                lda #$01
                jsr mscGetProp

                cpx #$00
                beq _XIT

                sta addr
                stx addr+1
                ;!!sta bank+lbank

                lda #$00
                jsr mscGetProp

                sta token

                jmp bankRestoreBank

_XIT            jmp mscMNum._varerr

                .endproc


;======================================
; bankGnLocal()
;======================================
bankGnLocal     .proc
                ;!!sta bank+lbank

                jsr lGetName._ENTRY1

                jmp bankRestoreBank

                .endproc


;======================================
; bankCStmtList()
;======================================
bankCStmtList   .proc
                ;!!ldy #cbank
                sty jt_curbank
                ;!!sta bank+cbank

                jsr stmtlist

                jmp bankEditBank

                .endproc


;======================================
;
;======================================
bankMGetT1      .proc
                jsr bankEditBank
                jsr GetTemp._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; bankLProceed()
;======================================
bankLProceed    .proc
                ;!!ldy #lbank
                sty jt_curbank
                ;!!sty bank+lbank

                rts
                .endproc


;======================================
;
;======================================
bankOptions     .proc
                jsr bankLProceed
                jsr libOptSet

                jmp bankEditBank

                .endproc


;======================================
;
;======================================
bankGetKey      .proc
                ;!!sta bank+lbank

                jsr libKeyGet

                jmp bankRestoreBank

                .endproc


;======================================
;
;======================================
bankSplErr      .proc
                ;!!sta bank+lbank

                jmp lsplerr

                .endproc


;======================================
;
;======================================
bankEmLoop      .proc
                jsr bankEditBank

                jmp Monitor._ENTRY2

                .endproc


;======================================
;
;======================================
bankGetArgs     .proc
                pha                     ; save arg type load flag

                ;!!sty bank+lbank

                lda #$01
                jsr mscGetProp

                sta addr
                stx addr+1

                pla
                bne _XIT                ; don't load arg types

                sta abt                 ; A=0
                sta abt+1               ; flag as temp args
                sta abt+2               ; (default)

                ldy #$02
                lda (zpAllocProps),Y
                sta numargs
                beq _XIT

                tax
                cpx #$09
                bcs _XIT

_next1          iny
                lda (zpAllocProps),Y
                dex
                sta argtypes,X          ; args inverted
                bne _next1

_XIT            jmp bankRestoreBank

                .endproc


;======================================
; call only from LBANK!
;======================================
bankPrintH      .proc
                ;!!sty bank+ebank

                jsr PrintH

                ;!!sty bank+lbank

                jmp libIOChkErr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
bankDRet        .proc
                jsr bankLProceed

                jmp (DOSVEC)

                .endproc
