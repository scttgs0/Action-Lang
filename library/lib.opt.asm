
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.opt.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


opt             .namespace

;======================================
; SetOpts()
;======================================
Set             .proc
;   Display On?
                ldx #domsg-optmsg
                ldy jt_tvdisp
                jsr _14
                beq _1

                lda #$00
                beq _2

_1              lda #$22
_2              sta jt_tvdisp

;   Alarm?
                ldx #amsg-optmsg
                ldy jt_vecAlarm
                cpy #$60
                jsr _14
                beq _3

                lda #$60                ; RTS
                bne _4

_3              lda #$4C                ; JMP
_4              sta jt_vecAlarm

;   Case sensitive?
                ldx #cmsg-optmsg
                ldy jt_stmask
                cpy #$DF
                jsr _14
                beq _5

                lda #$DF
                bne _6

_5              lda #$FF
_6              sta jt_stmask

;   Trace On?
                ldx #tmsg-optmsg
                ldy isTrace
                jsr _14
                beq _7

                lda #$00
                beq _8

_7              lda #$FF                ; isTrace=ON
_8              sta isTrace

;   List On?
                ldx #lstmsg-optmsg
                ldy isListing
                jsr _14
                beq _9

                lda #$00
                beq _10

_9              lda #$FF                ; isListing=ON
_10             sta isListing

;   window size
                lda jt_wsize
                jsr _18

                ldx #wmsg-optmsg
                jsr _19

                cmp #$05
                bcs _11                 ; make sure at least 5

                lda #$05
_11             cmp #$13
                bcc _12                 ; make sure less than 19

                lda #$12
_12             sta jt_wsize

                ldx is2Windows          ; single window?
                beq _13                 ;   yes

                sta win1Base+WNLINES

                tay
                iny
                sty win2Base+WYTOP

                sec
                lda #$17
                sbc jt_wsize
                sta win2Base+WNLINES

;   line size
_13             lda jt_linemax
                jsr _18

                ldx #lmsg-optmsg
                jsr _19

                sta jt_linemax

;   left margin
                lda LMARGN
                jsr _18

                ldx #lmmsg-optmsg
                jsr _19

                sta LMARGN

;   EOL char
                lda jt_eolch
                tay
                rol
                rol
                rol
                rol
                and #$03

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
                ora chrConvert,X
                sta jt_eolch

                rts

_14             beq _15

                ldy #'Y'
                bne _16

_15             ldy #'N'
_16             sty tempbuf+1

                ldy #$01
                jsr GetTmpBuf

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
_18             ldx #$00
                ldy #>tempbuf
                sty arg3

                ldy #<tempbuf

                jmp lib.io.StrC

; get number
_19             ldy tempbuf
                jsr GetTmpBuf

                ldy tempbuf
                bne _20

                lda tempbuf+1
                cmp #$1B
                beq _next1

_20             lda #<tempbuf
                ldx #>tempbuf
                jsr lib.io.ValB

                lda args

                rts
                .endproc


;--------------------------------------
;--------------------------------------

domsg           .text $09,"Display?"

optmsg          = domsg-20              ; see GetTemp

;   note, these are not 'ptext', because the length is off by one
amsg            .text $06,"Bell?"
cmsg            .text $10,"Case sensitive?"
tmsg            .text $07,"Trace?"
lstmsg          .text $06,"List?"
wmsg            .text $0F,"Window 1 size:"
lmsg            .text $0B,"Line size:"
lmmsg           .text $0D,"Left margin:"
emsg            .text $0A,"EOL char:"

stoa_           .byte $20,$40,$00,$60


;======================================
; GetTmpBuf()
;======================================
GetTmpBuf       .proc
                sty arg2

;   copy string to tempBuf+10
                ldy #$14
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

                jmp mainbank.MGetT1

                .endproc

                .endnamespace
