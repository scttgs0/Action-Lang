
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.sub.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   Subst()
;======================================
subst           .proc
                jsr setsp
                jsr savewd

                lda lastch
                cmp #$7d
                beq _s2

                pha
                lda #<submsg
                ldx #>submsg
                ldy #>subbuf
                sty arg3
                ldy #<subbuf
                jsr cmdstr

                pla

    ; check for ESC key
                ldx subbuf
                bne _s0

                ldx subbuf+1
                cpx #$1b
                beq _s1

_s0             cmp #$f8
                beq _s3                 ; string already found

                lda #<formsg
                ldx #>formsg
                jsr find.find1
                bne _s3

_s1             rts

_s2             jsr find.find2
                beq _s1

_s3             lda #$7d
                sta curch
                sta dirtyf              ; flag line as dirty
                sec
                lda subbuf
                sbc findbuf             ; get delta size
                sta arg3
                php                     ; save status for test below
                bcs _s4

                lda #1
                sbc arg3                ; negate delta size
_s4             clc
                adc buf
                sta arg0
                lda #0
                tay
                adc buf+1
                sta arg1
                lda (buf),Y
                plp
                bcc _s6                 ; need to remove space
                beq _s8                 ; same size

    ; need to add space
                tay
_s5             lda (buf),Y
                sta (arg0),Y
                dey
                cpy sp
                bcs _s5
                bcc _s8

_s6             sta arg2
                ldy sp
                dey
_s7             iny
                lda (arg0),Y
                sta (buf),Y
                cpy arg2
                bcc _s7

_s8             ldy sp
                ldx #0
                beq _s10

_s9             inx
                lda subbuf,X
                sta (buf),Y
                iny
_s10            cpx subbuf
                bne _s9

                clc
                ldy #0
                lda (buf),Y
                adc arg3
                sta (buf),Y
                jmp rfrshbuf

                .endproc

;--------------------------------------
;--------------------------------------

submsg          .text 12,"Substitute? "

formsg          .text 5,"for? "
