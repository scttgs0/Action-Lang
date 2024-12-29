
;======================================
;
;======================================
CLS             .proc
                jsr ClearScreen

                rts
                .endproc


;======================================
;
;======================================
CIOV            .proc
                lda IOCB0+ICCOM,X
                cmp #$03
                bne _1


; - - - - - - - - - - - - - - - - - - -
;   OPEN

                rts

_1              cmp #$0C
                bne _2


; - - - - - - - - - - - - - - - - - - -
;   CLOSE

                rts

_2              cmp #$05
                bne _3


; - - - - - - - - - - - - - - - - - - -
;   GETTEXT

                rts

_3              cmp #$07
                bne _4


; - - - - - - - - - - - - - - - - - - -
;   GETCHR

                rts

_4              cmp #$09
                bne _5


; - - - - - - - - - - - - - - - - - - -
;   PUTTEXT

                rts

_5              cmp #$0B
                bne _6


; - - - - - - - - - - - - - - - - - - -
;   PUTCHR

                lda IOCB0+ICBAL,X
                sta zpSource
                lda IOCB0+ICBAH,X
                sta zpSource+1

                ldy CURSOR_Y    ;!!ROWCRS
                asl

                lda #<CS_TEXT_MEM_PTR
                clc
                adc tblRowOffset,Y
                adc CURSOR_X    ;!!COLCRS
                sta zpDest
                lda #>CS_TEXT_MEM_PTR
                adc tblRowOffset+1,Y
                sta zpDest+1

; - - - - - - - - - - - - - - - - - - -
;   preserve IOPAGE control
                lda IOPAGE_CTRL
                pha

;   switch to TEXT page
                lda #iopPage2
                sta IOPAGE_CTRL
; - - - - - - - - - - - - - - - - - - -

                ldy #$00
                lda (zpSource),Y
_nextByteT      sta (zpDest),Y
                iny

                dec IOCB0+ICBLL,X
                bpl _nextByteT

; - - - - - - - - - - - - - - - - - - -
;   restore IOPAGE control
                pla
                sta IOPAGE_CTRL
; - - - - - - - - - - - - - - - - - - -

                rts

_6              cmp #$0D
                bne _XIT


; - - - - - - - - - - - - - - - - - - -
;   STATUS


_XIT            rts
                .endproc


;--------------------------------------
;--------------------------------------

tblRowOffset    .word $0000,$0028,$0050,$0078
                .word $00A0,$00C8,$00F0,$0118
                .word $0140,$0168,$0190,$01B8
                .word $01E0,$0208,$0230,$0258
                .word $0280,$02A8,$02D0,$02F8
                .word $0320,$0348,$0370,$0398
                .word $03C0,$03E8,$0410,$0438
                .word $0460,$0488,$04B0,$04D8
                .word $0500,$0528,$0550,$0578
                .word $05A0,$05C8,$05F0,$0618
                .word $0640,$0668,$0690,$06B8
                .word $06E0,$0708,$0730,$0758
                .word $0780,$07A8,$07D0,$07F8
                .word $0820,$0848,$0870,$0898
                .word $08C0,$08E8,$0910,$0938
