
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: storage.mac.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


slop            = 7                     ; can't be less than 4


;======================================
;   Allocate(size)
;======================================
Allocate        .proc
                sta afsize              ; save size
                stx afsize+1

_ENTRY          lda #$FF                ; set best size
                sta afbsze
                sta afbsze+1

                lda #<afbase            ; last = base
                sta aflast
                lda #>afbase
                sta aflast+1

_next1          ldy #0
                lda (aflast),Y          ; cur = last(0)
                sta afcur
                bne _1                  ; while cur # 0

                iny
                lda (aflast),Y
                sta afcur+1
                bne _2
                bra _5

_1              iny
                lda (aflast),Y
                sta afcur+1

_2              ldy #3
                lda (afcur),Y           ; get size
                cmp afsize+1            ; high bytes
                bcc _4                  ; size too small
                bne _3                  ; size too big

                dey
                lda (afcur),Y
                cmp afsize              ; low bytes
                bcc _4                  ; size too small
                beq _next2              ; sizes equal

;   Check for best fit

_3              ldy #2
                lda (afcur),Y
                cmp afbsze

                iny
                lda (afcur),Y
                sbc afbsze+1
                bcs _4

;   save best guess so far

                lda (afcur),Y
                sta afbsze+1

                dey
                lda (afcur),Y
                sta afbsze

                lda aflast
                sta afbest
                lda aflast+1
                sta afbest+1

;   get next entry in list and goto beginning of loop
_4              lda afcur
                sta aflast
                lda afcur+1
                sta aflast+1

                clc
                bcc _next1

;   no entry found, use best guess

_5              lda afbsze+1
                cmp #$FF
                beq _XIT1               ; no free block

;   see if we need to split block
                sec
                lda afbsze
                sbc #slop
                sta afbsze
                bcs _6

                dec afbsze+1
_6              lda afbsze+1
                cmp afsize+1
                bcc _7                  ; use as is
                bne _8                  ; split it

                lda afbsze
                cmp afsize
                bcs _8                  ; split it

;   don't split
_7              ldy #0
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

;   found entry of right size
_next2          ldy #0
                lda (afcur),Y
                sta (aflast),Y
                iny
                lda (afcur),Y
                sta (aflast),Y

_XIT1           rts

;   split best block

_8              ldy #0
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
                bcc _next2

                .endproc


;======================================
;   Free(block)
;======================================
Free            .proc
                sta afbest
                stx afbest+1

_ENTRY1         lda #<afbase            ; cur = base
                sta afcur
                lda #>afbase
                sta afcur+1

_next1          lda afcur               ; last = cur
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
                bcs _1                  ; while cur ULS block

                lda afcur+1             ; and cur # 0
                bne _next1

                lda afcur
                bne _next1

_1              iny
                clc
                lda (afbest),Y
                adc afbest

                tax
                iny
                lda (afbest),Y
                and #$7F                ; clear tag flag
                sta (afbest),Y

                adc afbest+1
                cmp afcur+1
                bne _2

                cpx afcur               ; if cur =
                bne _2                  ;  (block + block(1))

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
                bcc _3

;:AFL14         plp

_2              ldy #0                  ; block(0) = cur
                lda afcur
                sta (afbest),Y
                iny
                lda afcur+1
                sta (afbest),Y

_3              iny                     ; if block =
                clc                     ; (last + last(1))
                lda aflast
                adc (aflast),Y

                tax
                iny
                lda aflast+1
                adc (aflast),Y
                cmp afbest+1
                bne _4

                cpx afbest
                bne _4

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

;:AFL17         plp

_4              ldy #0                  ; last(0) = block
                lda afbest
                sta (aflast),Y

                iny
                lda afbest+1
                sta (aflast),Y

                rts
                .endproc
