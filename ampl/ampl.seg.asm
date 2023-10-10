
;======================================
;   FILE: ampl.seg.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

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
;


; low segment list> _:= low segment list> low segment> | low segment>
; low segment> _:= low segment type> low heading> (<dcl list>) (<stmt list>)
; low segment type> _:= PROC | low type> FUNC


;======================================
;
;======================================
segment         .proc
                cmp #proc
                beq _proc

                ldx nxttoken
                cpx #func
                beq _func

                rts                     ; end of segment list

_proc           lda #funct-vart+char-1
                sta type
                bne _1                  ; [unc]

_func           clc
                adc #funct-vart
                sta type

                jsr getnext

_1              jsr makeentry
                jsr segend

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
_next1          sta (stlocal),y

                iny                     ; zap local st
                bne _next1

                lda symtab
                sta gbase
                lda symtab+1
                sta gbase+1

;   space for arg list (8 bytes) and
;   room for name of next proc/func
;   up to 20 letters (24 bytes)
;   unused space will be reclaimed
;   see Params
                lda #32
                jsr stincr              ; arg list space
                jsr trashy

                lda nxttoken
                eor #equalid
                sta param               ; this is very tricky!!
                bne _2

                jsr ideq                ; param must = 0 here

                iny
                jsr storprops

                ldy #0
                lda (props),y
                ora #8
                sta (props),y           ; set Sys flag
                sta param

                jsr getnext

_2              jsr getnext

                cmp #lparen
                bne _argerr


;   low heading> _:= low id> (= low constant>) ( (<arg dcl list>) )
;   low arg dcl list> _:= low arg dcl list> , low dcl list> | low dcl list>

                jsr getnext

                cmp #rparen
                beq _3

_next2          jsr declare

                ldx lsttoken
                inc lsttoken            ; in case 2 ,'s
                cpx #comma
                beq _next2

                cmp #rparen
                beq _3

_argerr         ldy #arger

                jmp splerr

_3              lda param
                pha

                lda #0
                sta param

                jsr getnext
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
                jsr push1
                jsr getcdoff            ; fill in address

                adc #2
                bcc _4

                inx

_4              jsr push2

;   qcode to transfer arguments to
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

_5              jsr push3

                dec argbytes

                jmp _next3

_6              jmp _9

_7              ldx #10
                jsr jsrtable

                lda arg0
                ldx arg1

                ldy argbytes
                dey

                jsr push3

_8              lda trace               ; check for trace
                beq _9                  ; no trace

                lda #$20                ; JSR CTrace
                ldx #<ctrace
                ldy #>ctrace

                jsr push3

                ldy #0
                lda (curproc),y

                tay
                tax
_next4          lda (curproc),y
                sta (qcode),y

                dey
                bpl _next4

                inx
                txa

                jsr codeincr

                lda arg0
                ldx arg1
                jsr push2

                lda #3
                jsr cprop

                tay
                tax

_next5          lda (props),y
                sta (qcode),y

                dey
                bpl _next5

                inx
                txa

                jsr codeincr

_9              jsr stmtlist

                jmp segment

                .endproc
