
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: storage.mac.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


slop            = 7                     ; can't be less than 4


;======================================
;   Allocate(size)
;======================================
allocate        .proc
                sta afsize              ; save size
                stx afsize+1
                lda #$ff                ; set best size
                sta afbsze
                sta afbsze+1
                lda #<afbase            ; last = base
                sta aflast
                lda #>afbase
                sta aflast+1
_afl1           ldy #0
                lda (aflast),Y          ; cur = last(0)
                sta afcur
                bne _afl2               ; while cur # 0

                iny
                lda (aflast),Y
                sta afcur+1
                bne _afl3
                beq _afl6               ; done

_afl2           iny
                lda (aflast),Y
                sta afcur+1
_afl3           ldy #3
                lda (afcur),Y           ; get size
                cmp afsize+1            ; high bytes
                bcc _afl5               ; size too small
                bne _afl4               ; size too big

                dey
                lda (afcur),Y
                cmp afsize              ; low bytes
                bcc _afl5               ; size too small
                beq _afl9               ; sizes equal

    ; Check for best fit

_afl4           ldy #2
                lda (afcur),Y
                cmp afbsze
                iny
                lda (afcur),Y
                sbc afbsze+1
                bcs _afl5

    ; save best guess so far

                lda (afcur),Y
                sta afbsze+1
                dey
                lda (afcur),Y
                sta afbsze
                lda aflast
                sta afbest
                lda aflast+1
                sta afbest+1

    ; get next entry in list and goto
    ; beginning of loop

_afl5           lda afcur
                sta aflast
                lda afcur+1
                sta aflast+1
                clc
                bcc _afl1

    ; no entry found, use best guess

_afl6           lda afbsze+1
                cmp #$ff
                beq _afl10              ; no free block

    ; see if we need to split block
                sec
                lda afbsze
                sbc #slop
                sta afbsze
                bcs _afl7

                dec afbsze+1
_afl7           lda afbsze+1
                cmp afsize+1
                bcc _afl8               ; use as is
                bne _afl11              ; split it

                lda afbsze
                cmp afsize
                bcs _afl11              ; split it

    ; don't split
_afl8           ldy #0
                lda (afbest),Y          ; cur =  best(0)
                sta afcur
                iny
                lda (afbest),Y
                sta afcur+1

                lda (afcur),Y           ; best(0) = cur(0)
                sta (afbest),Y
                dey
                lda (afcur),Y
                sta (afbest),Y
                rts

    ; found entry of right size
_afl9           ldy #0
                lda (afcur),Y
                sta (aflast),Y
                iny
                lda (afcur),Y
                sta (aflast),Y
_afl10          rts

    ; split best block

_afl11          ldy #0
                lda (afbest),Y          ; cur = best(0)
                sta afcur
                clc
                adc afsize
                sta (afbest),Y          ; best(0)=cur+size
                sta aflast              ; last = cur + size
                iny
                lda (afbest),Y
                sta afcur+1
                adc afsize+1
                sta (afbest),Y
                sta aflast+1

                iny
                sec
                lda (afcur),Y
                sbc afsize              ; last(1) = bsze-size
                sta (aflast),Y
                lda afsize
                sta (afcur),Y           ; cur(1) = size
                iny
                lda (afcur),Y
                sbc afsize+1
                sta (aflast),Y
                lda afsize+1
                sta (afcur),Y
                clc
                bcc _afl9

                .endproc


;======================================
;   Free(block)
;======================================
free            .proc
                sta afbest
                stx afbest+1

free1           lda #<afbase            ; cur = base
                sta afcur
                lda #>afbase
                sta afcur+1

_afl12          lda afcur               ; last = cur
                sta aflast
                lda afcur+1
                sta aflast+1

                ldy #0
                lda (aflast),Y          ; cur = last(0)
                sta afcur
                cmp afbest
                iny
                lda (aflast),Y
                sta afcur+1
                sbc afbest+1
                bcs _afl13              ; while cur ULS block

                lda afcur+1             ; and cur # 0
                bne _afl12

                lda afcur
                bne _afl12

_afl13          iny
                clc
                lda (afbest),Y
                adc afbest
                tax
                iny
                lda (afbest),Y
                and #$7f                ; clear tag flag
                sta (afbest),Y
                adc afbest+1
                cmp afcur+1
                bne _afl15

                cpx afcur               ; if cur =
                bne _afl15              ;  (block + block(1))

                dey
                clc                     ; block(1) =
                lda (afbest),Y          ;  block(1) +  cur(1)
                adc (afcur),Y
                sta (afbest),Y
                iny
                lda (afbest),Y
                adc (afcur),Y
                sta (afbest),Y
                ldy #0                  ; block(0) = cur(0)
                lda (afcur),Y
                sta (afbest),Y
                iny
                lda (afcur),Y
                sta (afbest),Y
                clc
                bcc _afl16

;:AFL14 PLP
_afl15          ldy #0                  ; block(0) = cur
                lda afcur
                sta (afbest),Y
                iny
                lda afcur+1
                sta (afbest),Y

_afl16          iny                     ; if block =
                clc                     ; (last + last(1))
                lda aflast
                adc (aflast),Y
                tax
                iny
                lda aflast+1
                adc (aflast),Y
                cmp afbest+1
                bne _afl18

                cpx afbest
                bne _afl18

                clc                     ; last(1) =
                dey                     ;   last(1)+block(1)
                lda (aflast),Y
                adc (afbest),Y
                sta (aflast),Y
                iny
                lda (aflast),Y
                adc (afbest),Y
                sta (aflast),Y

                ldy #0                  ; last(0) = block(0)
                lda (afbest),Y
                sta (aflast),Y
                iny
                lda (afbest),Y
                sta (aflast),Y
                rts

;:AFL17 PLP
_afl18          ldy #0                  ; last(0) = block
                lda afbest
                sta (aflast),Y
                iny
                lda afbest+1
                sta (aflast),Y
                rts
                .endproc
