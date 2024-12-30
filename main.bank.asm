
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: main.bank.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


mainbank        .namespace

en0             .ptext "Error"
                .byte $C0
                .addr jt_vecError

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
                .addr isListing

en4             .ptext "device"
                .byte $8A
                .word device

en5             .ptext "TRACE"
                .byte $8A
                .addr isTrace


;======================================
; CartStart()
;======================================
CartStart       .proc
                ldy #edtr_bank
                sty jt_curbank
                sty bank+edtr_bank

                jmp editor.cartridge.START

                .endproc


;======================================
; GetName(char)
;======================================
GetName         .proc
                sta bank+lib_bank
                jsr ampl.symbol.GetName

                .endproc

                ;[fall-through]


;======================================
; Restore()
;======================================
Restore         .proc
                php
                pha

                tya
                ldy jt_curbank
rbank1          sta bank,Y
                tay

                pla
                plp
init            rts
                .endproc


;======================================
; Run(address)
;======================================
Run             .proc
;   reset Error routine
                ldy #<SPLErr
                sty jt_vecError+1
                ldy #>SPLErr
                sty jt_vecError+2

                jsr LProceed
                jsr mscJSRIndirect
                jmp EditBank

                .endproc


;======================================
; Compile()
;======================================
Compile         .proc
                ldy #cmpl_bank
                sty jt_curbank
                sty bank+cmpl_bank

                jsr compiler.Compile

                .endproc

                ;[fall-through]


;======================================
; EditBank()
;======================================
EditBank        .proc
                php
                pha

                tya
                ldy #edtr_bank
                sty jt_curbank

                jmp Restore.rbank1

                .endproc


;======================================
; GetAlias()
;======================================
GetAlias        .proc
                lda #$01
                jsr mscGetProp

                cpx #$00
                beq _XIT

                sta addr
                stx addr+1
                sta bank+lib_bank

                lda #$00
                jsr mscGetProp

                sta token

                jmp Restore

_XIT            jmp mscMNum._varerr

                .endproc


;======================================
; LocalName()
;======================================
LocalName       .proc
                sta bank+lib_bank

                jsr ampl.symbol.GetName._ENTRY1
                jmp Restore

                .endproc


;======================================
; CStmtList()
;======================================
CStmtList       .proc
                ldy #cmpl_bank
                sty jt_curbank
                sta bank+cmpl_bank

                jsr compiler.StmtList
                jmp EditBank

                .endproc


;======================================
;
;======================================
MGetT1          .proc
                jsr EditBank
                jsr editor.window.GetTemp._ENTRY1

                .endproc

                ;[fall-through]


;======================================
; LProceed()
;======================================
LProceed        .proc
                ldy #lib_bank
                sty jt_curbank
                sty bank+lib_bank

                rts
                .endproc


;======================================
;
;======================================
Options         .proc
                jsr LProceed
                jsr lib.opt.Set
                jmp EditBank

                .endproc


;======================================
;
;======================================
GetKey          .proc
                sta bank+lib_bank

                jsr lib.key.Get
                jmp Restore

                .endproc


;======================================
; Scanner/Parser/Lexeme error
;======================================
SPLErr          .proc
                sta bank+lib_bank

                jmp coreSPLErr

                .endproc


;======================================
;
;======================================
EMLoop          .proc
                jsr EditBank
                jmp ampl.monitor.Monitor._ENTRY2

                .endproc


;======================================
;
;======================================
GetArgs         .proc
                pha                     ; save arg type load flag

                sty bank+lib_bank

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

_XIT            jmp Restore

                .endproc


;======================================
; call only from LBANK!
;======================================
PrintH          .proc
                sty bank+edtr_bank

                jsr ampl.monitor.PrintHex
                sty bank+lib_bank

                jmp lib.io.ChkErr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
DosRet          .proc
                jsr LProceed

                jmp (DOSVEC)

                .endproc

                .endnamespace
