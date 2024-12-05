
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
                sta zpAllocSize         ; save size
                stx zpAllocSize+1

_ENTRY          lda #$FF                ; set best size
                sta zpAllocBSize
                sta zpAllocBSize+1

                lda #<zpAllocBase       ; last = base
                sta zpAllocLast
                lda #>zpAllocBase
                sta zpAllocLast+1

_next1          ldy #0
                lda (zpAllocLast),Y     ; cur = last(0)
                sta zpAllocCurrent
                bne _1                  ; while cur # 0

                iny
                lda (zpAllocLast),Y
                sta zpAllocCurrent+1
                bne _2
                bra _5

_1              iny
                lda (zpAllocLast),Y
                sta zpAllocCurrent+1

_2              ldy #3
                lda (zpAllocCurrent),Y  ; get size
                cmp zpAllocSize+1       ; high bytes
                bcc _4                  ; size too small
                bne _3                  ; size too big

                dey
                lda (zpAllocCurrent),Y
                cmp zpAllocSize         ; low bytes
                bcc _4                  ; size too small
                beq _next2              ; sizes equal

;   Check for best fit

_3              ldy #2
                lda (zpAllocCurrent),Y
                cmp zpAllocBSize

                iny
                lda (zpAllocCurrent),Y
                sbc zpAllocBSize+1
                bcs _4

;   save best guess so far

                lda (zpAllocCurrent),Y
                sta zpAllocBSize+1

                dey
                lda (zpAllocCurrent),Y
                sta zpAllocBSize

                lda zpAllocLast
                sta zpAllocBest
                lda zpAllocLast+1
                sta zpAllocBest+1

;   get next entry in list and goto beginning of loop
_4              lda zpAllocCurrent
                sta zpAllocLast
                lda zpAllocCurrent+1
                sta zpAllocLast+1

                clc
                bcc _next1

;   no entry found, use best guess

_5              lda zpAllocBSize+1
                cmp #$FF
                beq _XIT1               ; no free block

;   see if we need to split block
                sec
                lda zpAllocBSize
                sbc #slop
                sta zpAllocBSize
                bcs _6

                dec zpAllocBSize+1
_6              lda zpAllocBSize+1
                cmp zpAllocSize+1
                bcc _7                  ; use as is
                bne _8                  ; split it

                lda zpAllocBSize
                cmp zpAllocSize
                bcs _8                  ; split it

;   don't split
_7              ldy #0
                lda (zpAllocBest),Y     ; cur =  best(0)
                sta zpAllocCurrent
                iny
                lda (zpAllocBest),Y
                sta zpAllocCurrent+1

                lda (zpAllocCurrent),Y  ; best(0) = cur(0)
                sta (zpAllocBest),Y
                dey
                lda (zpAllocCurrent),Y
                sta (zpAllocBest),Y

                rts

;   found entry of right size
_next2          ldy #0
                lda (zpAllocCurrent),Y
                sta (zpAllocLast),Y
                iny
                lda (zpAllocCurrent),Y
                sta (zpAllocLast),Y

_XIT1           rts

;   split best block

_8              ldy #0
                lda (zpAllocBest),Y     ; cur = best(0)
                sta zpAllocCurrent

                clc
                adc zpAllocSize
                sta (zpAllocBest),Y     ; best(0)=cur+size
                sta zpAllocLast         ; last = cur + size

                iny
                lda (zpAllocBest),Y
                sta zpAllocCurrent+1
                adc zpAllocSize+1
                sta (zpAllocBest),Y
                sta zpAllocLast+1

                iny
                sec
                lda (zpAllocCurrent),Y
                sbc zpAllocSize         ; last(1) = bsze-size
                sta (zpAllocLast),Y

                lda zpAllocSize
                sta (zpAllocCurrent),Y  ; cur(1) = size

                iny
                lda (zpAllocCurrent),Y
                sbc zpAllocSize+1
                sta (zpAllocLast),Y

                lda zpAllocSize+1
                sta (zpAllocCurrent),Y

                clc
                bcc _next2

                .endproc


;======================================
;   Free(block)
;======================================
Free            .proc
                sta zpAllocBest
                stx zpAllocBest+1

_ENTRY1         lda #<zpAllocBase       ; cur = base
                sta zpAllocCurrent
                lda #>zpAllocBase
                sta zpAllocCurrent+1

_next1          lda zpAllocCurrent      ; last = cur
                sta zpAllocLast
                lda zpAllocCurrent+1
                sta zpAllocLast+1

                ldy #0
                lda (zpAllocLast),Y     ; cur = last(0)
                sta zpAllocCurrent

                cmp zpAllocBest
                iny
                lda (zpAllocLast),Y
                sta zpAllocCurrent+1

                sbc zpAllocBest+1
                bcs _1                  ; while cur ULS block

                lda zpAllocCurrent+1    ; and cur # 0
                bne _next1

                lda zpAllocCurrent
                bne _next1

_1              iny
                clc
                lda (zpAllocBest),Y
                adc zpAllocBest

                tax
                iny
                lda (zpAllocBest),Y
                and #$7F                ; clear tag flag
                sta (zpAllocBest),Y

                adc zpAllocBest+1
                cmp zpAllocCurrent+1
                bne _2

                cpx zpAllocCurrent      ; if cur =
                bne _2                  ;  (block + block(1))

                dey
                clc                     ; block(1) =
                lda (zpAllocBest),Y     ;  block(1) +  cur(1)
                adc (zpAllocCurrent),Y
                sta (zpAllocBest),Y

                iny
                lda (zpAllocBest),Y
                adc (zpAllocCurrent),Y
                sta (zpAllocBest),Y

                ldy #0                  ; block(0) = cur(0)
                lda (zpAllocCurrent),Y
                sta (zpAllocBest),Y

                iny
                lda (zpAllocCurrent),Y
                sta (zpAllocBest),Y

                clc
                bcc _3

;:AFL14         plp

_2              ldy #0                  ; block(0) = cur
                lda zpAllocCurrent
                sta (zpAllocBest),Y
                iny
                lda zpAllocCurrent+1
                sta (zpAllocBest),Y

_3              iny                     ; if block =
                clc                     ; (last + last(1))
                lda zpAllocLast
                adc (zpAllocLast),Y

                tax
                iny
                lda zpAllocLast+1
                adc (zpAllocLast),Y
                cmp zpAllocBest+1
                bne _4

                cpx zpAllocBest
                bne _4

                clc                     ; last(1) =
                dey                     ;   last(1)+block(1)
                lda (zpAllocLast),Y
                adc (zpAllocBest),Y
                sta (zpAllocLast),Y

                iny
                lda (zpAllocLast),Y
                adc (zpAllocBest),Y
                sta (zpAllocLast),Y

                ldy #0                  ; last(0) = block(0)
                lda (zpAllocBest),Y
                sta (zpAllocLast),Y

                iny
                lda (zpAllocBest),Y
                sta (zpAllocLast),Y

                rts

;:AFL17         plp

_4              ldy #0                  ; last(0) = block
                lda zpAllocBest
                sta (zpAllocLast),Y

                iny
                lda zpAllocBest+1
                sta (zpAllocLast),Y

                rts
                .endproc
