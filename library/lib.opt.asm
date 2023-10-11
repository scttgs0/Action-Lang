
;======================================
;   FILE: lib.opt.asm
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


;======================================
;
;======================================
setopts         .proc
;   Display On?
                ldx #domsg-optmsg
                ldy tvdisp
                jsr _14
                beq _1

                lda #0
                beq _2

_1              lda #$22
_2              sta tvdisp

;   Alarm?
                ldx #amsg-optmsg
                ldy alarm
                cpy #$60
                jsr _14
                beq _3

                lda #$60                ; RTS
                bne _4

_3              lda #$4C                ; JMP
_4              sta alarm

;   Case sensitive?
                ldx #cmsg-optmsg
                ldy stmask
                cpy #$DF
                jsr _14
                beq _5

                lda #$DF
                bne _6

_5              lda #$FF
_6              sta stmask

;   Trace On?
                ldx #tmsg-optmsg
                ldy trace
                jsr _14
                beq _7

                lda #0
                beq _8

_7              lda #$FF
_8              sta trace

;   List On?
                ldx #lstmsg-optmsg
                ldy list
                jsr _14
                beq _9

                lda #0
                beq _10

_9              lda #$FF
_10             sta list

;   window size
                lda wsize
                jsr _18

                ldx #wmsg-optmsg
                jsr _19

                cmp #5
                bcs _11                 ; make sure at least 5

                lda #5
_11             cmp #19
                bcc _12                 ; make sure less than 19

                lda #18
_12             sta wsize

                ldx numwd
                beq _13

                sta w1+wnlns

                tay
                iny
                sty w2+wytop

                sec
                lda #23
                sbc wsize
                sta w2+wnlns

;   line size
_13             lda linemax
                jsr _18

                ldx #lmsg-optmsg
                jsr _19

                sta linemax

;   left margin
                lda lmargin
                jsr _18

                ldx #lmmsg-optmsg
                jsr _19

                sta lmargin

;   EOL char
                lda eolch
                tay
                rol a
                rol a
                rol a
                rol a
                and #3

                tax
                tya
                and #$9F
                ora stoa_,X

                tay
                ldx #emsg-optmsg
                jsr _16

                lda tempbuf+1
                tay
                and #$60

                tax
                tya
                and #$9F
                ora chcvt,X
                sta eolch

                rts

_14             beq _15

                ldy #'Y'
                bne _16

_15             ldy #'N'
_16             sty tempbuf+1

                ldy #1
                jsr gettmpbuf

                lda tempbuf+1
                ldy tempbuf
                bne _17

                cmp #$1B
                bne _XIT1

_next1          pla
                pla

_XIT1           rts

_17             ora #$20
                cmp #'y'

                rts

; get string
_18             ldx #0
                ldy #>tempbuf
                sty arg3

                ldy #<tempbuf

                jmp strc

;   get number
_19             ldy tempbuf
                jsr gettmpbuf

                ldy tempbuf
                bne _20

                lda tempbuf+1
                cmp #$1B
                beq _next1

_20             lda #<tempbuf
                ldx #>tempbuf
                jsr valb

                lda args

                rts
                .endproc


;--------------------------------------
;--------------------------------------

domsg           .text 9,"Display?"

optmsg          = domsg-20              ; see GetTemp

amsg            .text 6,"Bell?"
cmsg            .text 16,"Case sensitive?"
tmsg            .text 7,"Trace?"
lstmsg          .text 6,"List?"
wmsg            .text 15,"Window 1 size:"
lmsg            .text 11,"Line size:"
lmmsg           .text 13,"Left margin:"
emsg            .text 10,"EOL char:"

stoa_           .byte $20,$40,$00,$60


;======================================
;   GetTmpBuf()
;======================================
gettmpbuf       .proc
                sty arg2

;   copy string to tempBuf+10
                ldy #20
_next1          lda optmsg+20,X
                sta tempbuf+10,Y

                dex
                dey
                bpl _next1

;   put space at end
                tay
                lda #' '
                sta tempbuf+10,Y

                lda #<(tempbuf+10)
                ldx #>(tempbuf+10)
                ldy arg2

                jmp mgett1

                .endproc
