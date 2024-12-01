
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.substitute.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   Substitute()
;======================================
Substitute      .proc
                jsr SetSpacing
                jsr SaveWindow

                lda lastch
                cmp #$7D
                beq _2

                pha

                lda #<submsg
                ldx #>submsg

                ldy #>subbuf
                sty arg3
                ldy #<subbuf

                jsr CommandString

                pla

;   check for ESC key
                ldx subbuf
                bne _1

                ldx subbuf+1
                cpx #$1B
                beq _XIT1

_1              cmp #$F8
                beq _3                  ; string already found

                lda #<formsg
                ldx #>formsg
                jsr Find._ENTRY1
                bne _3

_XIT1           rts

_2              jsr Find._ENTRY2
                beq _XIT1

_3              lda #$7D
                sta curch
                sta isDirty             ; flag line as dirty

                sec
                lda subbuf
                sbc findbuf             ; get delta size
                sta arg3

                php                     ; save status for test below
                bcs _4

                lda #1
                sbc arg3                ; negate delta size

_4              clc
                adc buf
                sta arg0

                lda #0
                tay
                adc buf+1
                sta arg1

                lda (buf),Y
                plp
                bcc _5                  ; need to remove space
                beq _6                  ; same size

;   need to add space
                tay
_next1          lda (buf),Y
                sta (arg0),Y

                dey
                cpy sp
                bcs _next1
                bra _6

_5              sta arg2

                ldy sp
                dey

_next2          iny
                lda (arg0),Y
                sta (buf),Y

                cpy arg2
                bcc _next2

_6              ldy sp
                ldx #0
                beq _7

_next3          inx
                lda subbuf,X
                sta (buf),Y

                iny
_7              cpx subbuf
                bne _next3

                clc
                ldy #0
                lda (buf),Y
                adc arg3
                sta (buf),Y

                jmp RefreshBuf

                .endproc

;--------------------------------------

submsg          .ptext "Substitute? "
formsg          .ptext "for? "
