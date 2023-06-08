
; SPDX-FileName: lib.opt.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   SetOpts()
;======================================
libOptSetOpts   .proc
;   Display On?
                ldx #domsg-optmsg
                ldy jt_tvdisp
                jsr _yn
                beq _do1

                lda #0
                beq _do2

_do1            lda #$22
_do2            sta jt_tvdisp

;   Alarm?
                ldx #amsg-optmsg
                ldy jt_alarm
                cpy #$60
                jsr _yn
                beq _a1

                lda #$60                ; RTS
                bne _a2

_a1             lda #$4c                ; JMP
_a2             sta jt_alarm

;   Case sensitive?
                ldx #cmsg-optmsg
                ldy jt_stmask
                cpy #$df
                jsr _yn
                beq _c1

                lda #$df
                bne _c2

_c1             lda #$ff
_c2             sta jt_stmask

;   Trace On?
                ldx #tmsg-optmsg
                ldy trace
                jsr _yn
                beq _t1

                lda #0
                beq _t2

_t1             lda #$ff
_t2             sta trace

;   List On?
                ldx #lstmsg-optmsg
                ldy list
                jsr _yn
                beq _lst1

                lda #0
                beq _lst2

_lst1           lda #$ff
_lst2           sta list

;   window size
                lda jt_wsize
                jsr _getstr

                ldx #wmsg-optmsg
                jsr _getnum

                cmp #5
                bcs _w0                 ; make sure at least 5

                lda #5
_w0             cmp #19
                bcc _w1                 ; make sure less than 19

                lda #18
_w1             sta jt_wsize
                ldx numwd
                beq _l1

                sta w1+wnlns
                tay
                iny
                sty w2+wytop
                sec
                lda #23
                sbc jt_wsize
                sta w2+wnlns

;   line size
_l1             lda jt_linemax
                jsr _getstr

                ldx #lmsg-optmsg
                jsr _getnum

                sta jt_linemax

;   left margin
                lda LMARGN
                jsr _getstr

                ldx #lmmsg-optmsg
                jsr _getnum

                sta LMARGN

;   EOL char
                lda jt_eolch
                tay
                rol a
                rol a
                rol a
                rol a
                and #3
                tax
                tya
                and #$9f
                ora stoa_,x
                tay
                ldx #emsg-optmsg
                jsr _yn2

                lda tempbuf+1
                tay
                and #$60
                tax
                tya
                and #$9f
                ora chrConvert,x
                sta jt_eolch
                rts

_yn             beq _yn1

                ldy #'Y'
                bne _yn2

_yn1            ldy #'N'
_yn2            sty tempbuf+1
                ldy #1
                jsr libOptGetTmpBuf

                lda tempbuf+1
                ldy tempbuf
                bne _yn5

                cmp #$1b
                bne _yn4

_yn3            pla
                pla
_yn4            rts

_yn5            ora #$20
                cmp #'y'
                rts

_getstr         ldx #0
                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf
                jmp libIOStrC

_getnum         ldy tempbuf
                jsr libOptGetTmpBuf

                ldy tempbuf
                bne _gn0

                lda tempbuf+1
                cmp #$1b
                beq _yn3

_gn0            lda #<tempbuf
                ldx #>tempbuf
                jsr libIOValB

                lda args
_gn1            rts
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
libOptGetTmpBuf .proc
                sty arg2

;   copy string to tempBuf+10
                ldy #20
_gt1            lda optmsg+20,x
                sta tempbuf+10,y
                dex
                dey
                bpl _gt1

;   put space at end
                tay
                lda #' '
                sta tempbuf+10,y

                lda #<(tempbuf+10)
                ldx #>(tempbuf+10)
                ldy arg2
                jmp mgett1

                .endproc
