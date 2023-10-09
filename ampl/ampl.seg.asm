
; SPDX-FileName: ampl.seg.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


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
                bra _func1

_func           clc
                adc #tokFUNC_t-tokVAR_t
                sta type
                jsr GetNext

_func1          jsr makeentry
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
                sta (props),y
                sta argbytes

                tay
_funcst         sta (symTblLocal),y
                iny                     ; zap local st
                bne _funcst

                lda symtab
                sta gbase
                lda symtab+1
                sta gbase+1

; space for arg list (8 bytes) and
; room for name of next proc/func
; up to 20 letters (24 bytes)
; unused space will be reclaimed
; see Params
                lda #32
                jsr stincr              ; arg list space
                jsr TrashY

                lda nxttoken
                eor #tokEQU
                sta param               ; this is very tricky!!
                bne _funchd
                jsr ideq                ; param must = 0 here

                iny
                jsr storprops

                ldy #0
                lda (props),y
                ora #8
                sta (props),y           ; set Sys flag
                sta param
                jsr GetNext
_funchd         jsr GetNext

                cmp #tokLParen
                bne argerror


; low heading> _:= low id> (= low constant>) ( (<arg dcl list>) )
; low arg dcl list> _:= low arg dcl list> , low dcl list> | low dcl list>


                jsr GetNext

                cmp #tokRParen
                beq argerror._func2

_heading        jsr declare

                ldx lsttoken
                inc lsttoken            ; in case 2 ,'s
                cpx #tokComma
                beq _heading

                cmp #tokRParen
                beq argerror._func2

argerror        ldy #argERR
                jmp splerr

_func2          lda param
                pha
                lda #0
                sta param

                jsr GetNext
                jsr declare             ; locals

;   handle procedure setup here
                pla
                bmi _f4                 ; system proc

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
                bcc _fh2

                inx
_fh2            jsr Push2

;   qcode to transfer arguments to
;   local frame
_fh3            lda argbytes
                beq _func3              ; no arguments

                cmp #3
                bcs _fh5

                cmp #2
                lda #$8D                ; STA addr16
                ldx arg0
                ldy arg1
                bcc _fh4

                lda #$8E                ; STX addr16
                inx
                bne _fh4

                iny
_fh4            jsr Push3
                dec argbytes
                jmp _fh3

_f4             jmp _func4

_fh5            ldx #10
                jsr JSRTable

                lda arg0
                ldx arg1
                ldy argbytes
                dey
                jsr Push3

_func3          lda trace               ; check for trace
                beq _func4              ; no trace

                lda #$20                ; JSR CTrace
                ldx #<libMscCTrace
                ldy #>libMscCTrace
                jsr Push3

                ldy #0
                lda (curproc),y
                tay
                tax
_f3a            lda (curproc),y
                sta (qcode),y
                dey
                bpl _f3a

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
_f3b            lda (props),y
                sta (qcode),y
                dey
                bpl _f3b

                inx
                txa
                jsr codeincr

_func4          jsr stmtlist

                jmp Segment

                .endproc
