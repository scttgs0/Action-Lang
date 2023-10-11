
;======================================
;   FILE: storage.mac.asm
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


slop            = 7                     ; can't be less than 4


;======================================
;   Allocate(size)
;======================================
allocate        .proc
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
                lda (aflast),y          ; cur = last(0)
                sta afcur
                bne _1                  ; while cur # 0

                iny
                lda (aflast),y
                sta afcur+1
                bne _2
                beq _5                  ; [unc]

_1              iny
                lda (aflast),y
                sta afcur+1

_2              ldy #3
                lda (afcur),y           ; get size
                cmp afsize+1            ; high bytes
                bcc _4                  ; size too small
                bne _3                  ; size too big

                dey
                lda (afcur),y
                cmp afsize              ; low bytes
                bcc _4                  ; size too small
                beq _next2              ; sizes equal

;   Check for best fit

_3              ldy #2
                lda (afcur),y
                cmp afbsze

                iny
                lda (afcur),y
                sbc afbsze+1
                bcs _4

;   save best guess so far

                lda (afcur),y
                sta afbsze+1

                dey
                lda (afcur),y
                sta afbsze

                lda aflast
                sta afbest
                lda aflast+1
                sta afbest+1

;   get next entry in list and goto
;   beginning of loop

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
                lda (afbest),y          ; cur =  best(0)
                sta afcur
                iny
                lda (afbest),y
                sta afcur+1

                lda (afcur),y           ; best(0) = cur(0)
                sta (afbest),y
                dey
                lda (afcur),y
                sta (afbest),y

                rts

;   found entry of right size
_next2          ldy #0
                lda (afcur),y
                sta (aflast),y
                iny
                lda (afcur),y
                sta (aflast),y

_XIT1           rts

;   split best block

_8              ldy #0
                lda (afbest),y          ; cur = best(0)
                sta afcur

                clc
                adc afsize
                sta (afbest),y          ; best(0)=cur+size
                sta aflast              ; last = cur + size

                iny
                lda (afbest),y
                sta afcur+1
                adc afsize+1
                sta (afbest),y
                sta aflast+1

                iny
                sec
                lda (afcur),y
                sbc afsize              ; last(1) = bsze-size
                sta (aflast),y

                lda afsize
                sta (afcur),y           ; cur(1) = size

                iny
                lda (afcur),y
                sbc afsize+1
                sta (aflast),y

                lda afsize+1
                sta (afcur),y

                clc
                bcc _next2

                .endproc


;======================================
;   Free(block)
;======================================
free            .proc
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
                lda (aflast),y          ; cur = last(0)
                sta afcur

                cmp afbest
                iny
                lda (aflast),y
                sta afcur+1

                sbc afbest+1
                bcs _1                  ; while cur ULS block

                lda afcur+1             ; and cur # 0
                bne _next1

                lda afcur
                bne _next1

_1              iny
                clc
                lda (afbest),y
                adc afbest

                tax
                iny
                lda (afbest),y
                and #$7F                ; clear tag flag
                sta (afbest),y

                adc afbest+1
                cmp afcur+1
                bne _2

                cpx afcur               ; if cur =
                bne _2                  ;  (block + block(1))

                dey
                clc                     ; block(1) =
                lda (afbest),y          ;  block(1) +  cur(1)
                adc (afcur),y
                sta (afbest),y

                iny
                lda (afbest),y
                adc (afcur),y
                sta (afbest),y

                ldy #0                  ; block(0) = cur(0)
                lda (afcur),y
                sta (afbest),y

                iny
                lda (afcur),y
                sta (afbest),y

                clc
                bcc _3

;:AFL14         plp

_2              ldy #0                  ; block(0) = cur
                lda afcur
                sta (afbest),y
                iny
                lda afcur+1
                sta (afbest),y

_3              iny                     ; if block =
                clc                     ; (last + last(1))
                lda aflast
                adc (aflast),y

                tax
                iny
                lda aflast+1
                adc (aflast),y
                cmp afbest+1
                bne _4

                cpx afbest
                bne _4

                clc                     ; last(1) =
                dey                     ;   last(1)+block(1)
                lda (aflast),y
                adc (afbest),y
                sta (aflast),y

                iny
                lda (aflast),y
                adc (afbest),y
                sta (aflast),y

                ldy #0                  ; last(0) = block(0)
                lda (afbest),y
                sta (aflast),y

                iny
                lda (afbest),y
                sta (aflast),y

                rts

;:AFL17         plp

_4              ldy #0                  ; last(0) = block
                lda afbest
                sta (aflast),y

                iny
                lda afbest+1
                sta (aflast),y

                rts
                .endproc
