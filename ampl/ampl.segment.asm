
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.segment.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


; low segment list> _:= low segment list> low segment> | low segment>
; low segment> _:= low segment type> low heading> (<dcl list>) (<stmt list>)
; low segment type> _:= PROC | low type> FUNC


;======================================
;
;======================================
Segment         .proc
                cmp #tokPROC
                beq _proc

                ldx nxttoken
                cpx #tokFUNC
                beq _func

                rts                     ; end of segment list

; - - - - - - - - - - - - - - - - - - -

_proc           lda #tokFUNC_t-tokVAR_t+tokCHAR-1
                sta type
                bra _1

; - - - - - - - - - - - - - - - - - - -

_func           clc
                adc #tokFUNC_t-tokVAR_t
                sta type

                jsr compiler.lexicon.GetNext

; - - - - - - - - - - - - - - - - - - -

_1              jsr compiler.MakeEntry
                jsr jt_vecSegEnd

                lda addr
                sta curproc
                lda addr+1
                sta curproc+1

                sta qglobal

                lda #$01
                jsr mscSTIncr           ; space for num args

                ldy #$03
                lda #$00                ; no args yet
                sta (zpAllocProps),Y
                sta argbytes

                tay
_next1          sta (symTblLocal),Y

                iny                     ; zap local st
                bne _next1

                lda symtab
                sta gbase
                lda symtab+1
                sta gbase+1

;   space for arg list (8 bytes) and room for name of next proc/func
;   up to 20 letters (24 bytes)
;   unused space will be reclaimed
;   see Params
                lda #$20
                jsr mscSTIncr           ; arg list space
                jsr ampl.cgu.TrashY

                lda nxttoken
                eor #tokEQU
                sta param               ; this is very tricky!!
                bne _2

                jsr compiler.IDEqual    ; param must = 0 here

                iny
                jsr compiler.StoreProps

                ldy #$00
                lda (zpAllocProps),Y
                ora #$08
                sta (zpAllocProps),Y    ; set Sys flag
                sta param

                jsr compiler.lexicon.GetNext

_2              jsr compiler.lexicon.GetNext

                cmp #tokLParen
                bne _argerr


;   low heading> _:= low id> (= low constant>) ( (<arg dcl list>) )
;   low arg dcl list> _:= low arg dcl list> , low dcl list> | low dcl list>

                jsr compiler.lexicon.GetNext

                cmp #tokRParen
                beq _3

_next2          jsr compiler.Declare

                ldx zpAllocPrevToken
                inc zpAllocPrevToken    ; in case 2 ,'s
                cpx #tokComma
                beq _next2

                cmp #tokRParen
                beq _3

_argerr         ldy #argERR

                jmp mainbank.SPLErr

_3              lda param
                pha

                lda #$00
                sta param

                jsr compiler.lexicon.GetNext
                jsr compiler.Declare    ; locals

;   handle procedure setup here
                pla
                bmi _6                  ; system proc

;   get beginning of arguments and save actual procedure address
                lda #$01
                jsr mscCProp

                sta arg0
                stx arg1

                jsr mscGetCodeOffset
                jsr compiler.StoreProps

;   get space for proc variable
                lda #$4C                ; JMP
                jsr ampl.cgu.Push1
                jsr mscGetCodeOffset    ; fill in address

                adc #$02
                bcc _4

                inx

_4              jsr ampl.cgu.Push2

;   QCODE to transfer arguments to local frame
_next3          lda argbytes
                beq _8                  ; no arguments

                cmp #$03
                bcs _7

                cmp #$02
                lda #$8D                ; STA addr16
                ldx arg0
                ldy arg1
                bcc _5

                lda #$8E                ; STX addr16

                inx
                bne _5

                iny

_5              jsr ampl.cgu.Push3

                dec argbytes

                jmp _next3

_6              jmp _9

_7              ldx #$0A
                jsr ampl.cgu.JSRTable

                lda arg0
                ldx arg1

                ldy argbytes
                dey

                jsr ampl.cgu.Push3

_8              lda isTrace             ; do trace?
                beq _9                  ;   no

                lda #$20                ; JSR CTrace
                ldx #<lib.msc.CTrace
                ldy #>lib.msc.CTrace

                jsr ampl.cgu.Push3

                ldy #$00
                lda (curproc),Y

                tay
                tax
_next4          lda (curproc),Y
                sta (QCODE),Y

                dey
                bpl _next4

                inx
                txa

                jsr mscCodeIncr

                lda arg0
                ldx arg1
                jsr ampl.cgu.Push2

                lda #$03
                jsr mscCProp

                tay
                tax

_next5          lda (zpAllocProps),Y
                sta (QCODE),Y

                dey
                bpl _next5

                inx
                txa

                jsr mscCodeIncr

_9              jsr compiler.StmtList
                jmp Segment

                .endproc
