
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.bank.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


en0             .ptext "Error"
                .byte $C0
                .addr jt_error

; - - - - - - - - - - - - - - - - - - -
                .byte 3,138,138,138
; - - - - - - - - - - - - - - - - - - -

en1             .ptext "EOF"
                .byte $9A
                .addr eof

en2             .ptext "color"
                .byte $8A
                ;!!.addr FILDAT
                .word $0000     ; HACK:

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

                jmp editor.cartridge.START

                .endproc


;======================================
; bankGetName(char)
;======================================
bankGetName     .proc
                ;!!sta bank+lbank
                jsr ampl.symbol.GetName

                .endproc

                ;[fall-through]


;======================================
; bankRestore()
;======================================
bankRestore     .proc
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
                ldy #<bankSPLErr
                sty jt_error+1
                ldy #>bankSPLErr
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
                jsr compiler.Compile

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

                jmp bankRestore.rbank1

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

                jmp bankRestore

_XIT            jmp mscMNum._varerr

                .endproc


;======================================
; bankLocalName()
;======================================
bankLocalName   .proc
                ;!!sta bank+lbank

                jsr ampl.symbol.GetName._ENTRY1

                jmp bankRestore

                .endproc


;======================================
; bankCStmtList()
;======================================
bankCStmtList   .proc
                ;!!ldy #cbank
                sty jt_curbank
                ;!!sta bank+cbank

                jsr compiler.StmtList

                jmp bankEditBank

                .endproc


;======================================
;
;======================================
bankMGetT1      .proc
                jsr bankEditBank
                jsr editor.window.GetTemp._ENTRY1

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
                jsr liboptSet

                jmp bankEditBank

                .endproc


;======================================
;
;======================================
bankGetKey      .proc
                ;!!sta bank+lbank

                jsr libkeyGet

                jmp bankRestore

                .endproc


;======================================
; Scanner/Parser/Lexeme error
;======================================
bankSPLErr      .proc
                ;!!sta bank+lbank

                jmp coreSPLErr

                .endproc


;======================================
;
;======================================
bankEmLoop      .proc
                jsr bankEditBank

                jmp ampl.monitor.Monitor._ENTRY2

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

_XIT            jmp bankRestore

                .endproc


;======================================
; call only from LBANK!
;======================================
bankPrintH      .proc
                ;!!sty bank+ebank

                jsr ampl.monitor.PrintHex

                ;!!sty bank+lbank

                jmp libioChkErr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
bankDosRet      .proc
                jsr bankLProceed

                jmp (DOSVEC)

                .endproc
