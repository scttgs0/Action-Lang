
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

_proc           lda #tokFUNC_t-tokVAR_t+tokCHAR-1
                sta type
                bra _1

_func           clc
                adc #tokFUNC_t-tokVAR_t
                sta type

                jsr GetNext

_1              jsr makeentry
                jsr jt_segend

                lda addr
                sta curproc
                lda addr+1
                sta curproc+1

                sta qglobal

                lda #1
                jsr stincr              ; space for num args

                ldy #3
                lda #0                  ; no args yet
                sta (props),Y
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
                lda #32
                jsr stincr              ; arg list space
                jsr TrashY

                lda nxttoken
                eor #tokEQU
                sta param               ; this is very tricky!!
                bne _2

                jsr ideq                ; param must = 0 here

                iny
                jsr storprops

                ldy #0
                lda (props),Y
                ora #8
                sta (props),Y           ; set Sys flag
                sta param

                jsr GetNext

_2              jsr GetNext

                cmp #tokLParen
                bne _argerr


; low heading> _:= low id> (= low constant>) ( (<arg dcl list>) )
; low arg dcl list> _:= low arg dcl list> , low dcl list> | low dcl list>

                jsr GetNext

                cmp #tokRParen
                beq _3

_next2          jsr declare

                ldx lsttoken
                inc lsttoken            ; in case 2 ,'s
                cpx #tokComma
                beq _next2

                cmp #tokRParen
                beq _3

_argerr         ldy #argERR

                jmp splerr

_3          lda param
                pha

                lda #0
                sta param

                jsr GetNext
                jsr declare             ; locals

;   handle procedure setup here
                pla
                bmi _6                  ; system proc

;   get beginning of arguments and
;   save actual procedure address
                lda #1
                jsr cprop

                sta arg0
                stx arg1

                jsr getcdoff
                jsr storprops

;   get space for proc variable
                lda #$4C                ; JMP
                jsr Push1
                jsr getcdoff            ; fill in address

                adc #2
                bcc _4

                inx

_4              jsr Push2

;   QCODE to transfer arguments to
;   local frame
_next3          lda argbytes
                beq _8                  ; no arguments

                cmp #3
                bcs _7

                cmp #2
                lda #$8D                ; STA addr16
                ldx arg0
                ldy arg1
                bcc _5

                lda #$8E                ; STX addr16

                inx
                bne _5

                iny

_5              jsr Push3

                dec argbytes

                jmp _next3

_6              jmp _9

_7              ldx #10
                jsr JSRTable

                lda arg0
                ldx arg1

                ldy argbytes
                dey

                jsr Push3

_8              lda trace               ; check for trace
                beq _9                  ; no trace

                lda #$20                ; JSR CTrace
                ldx #<libMscCTrace
                ldy #>libMscCTrace

                jsr Push3

                ldy #0
                lda (curproc),Y

                tay
                tax
_next4          lda (curproc),Y
                sta (QCODE),Y

                dey
                bpl _next4

                inx
                txa

                jsr codeincr

                lda arg0
                ldx arg1
                jsr Push2

                lda #3
                jsr cprop

                tay
                tax

_next5          lda (props),Y
                sta (QCODE),Y

                dey
                bpl _next5

                inx
                txa

                jsr codeincr

_9              jsr stmtlist

                jmp Segment

                .endproc
