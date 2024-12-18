
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.command.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   Front()
;======================================
Front           .proc
                sec
                lda #0
                sbc indent
                sta choff

                jsr DisplayBuffer

                lda LMARGN
                jmp ResetColumn+6

                .endproc


;======================================
;   Back()
;======================================
Back            .proc
                ldy #0
                lda (buf),Y
_ENTRY1         pha

                clc
                adc LMARGN

                sec
                sbc RMARGN
                bcs _1

                lda #1
_1              sbc indent
                sta choff

                jsr DisplayBuffer

                sec
                pla
                sbc indent

                sec
                sbc choff

                clc
                adc LMARGN

                jmp ResetColumn+6

                .endproc


;======================================
;   PageUp()
;======================================
PageUp          .proc
                sec
                lda lnum
                sbc #2

                ldy #1
                bra PageContent

                .endproc


;======================================
;   PageDown()
;======================================
PageDown        .proc
                ldy #5

                sec
                lda #2
                sbc lnum

                .endproc

                ;[fall-through]

;--------------------------------------
;
;--------------------------------------
PageContent     .proc
                clc
                adc nlines
                sta arg14

                dec arg14
                beq _XIT

                sty arg13
                jsr CleanLine

_next1          ldy arg13
                jsr next

                dec arg14
                bne _next1

_XIT            jmp CenterLine

                .endproc


;======================================
;   Paste()
;======================================
Paste           .proc
                jsr DeleteTop
                beq _XIT

                stx dirty

                jsr CleanLine
                jsr nextup

                sta cur+1               ; tricky, fake out top

                jsr SaveWindow._ENTRY1
                jsr DeleteTop

_next1          jsr strptr
                jsr LoadBuffer._ENTRY1
                jsr InsertByte

                lda allocerr
                bne _1                  ; check for out of memory

                jsr DeleteNext
                bne _next1

_1              jsr ResetCursor

                ldy currentWindow
                lda w1+WCUR+1,Y
                beq _2

                jsr nextdwn

_2              lda #0
                jmp NewPage._ENTRY1

_XIT            rts
                .endproc


;======================================
;   old IndentL()
;======================================
IndentLeft      .proc
                lda indent
                beq ScrollInit._XIT

                dec indent

                jmp CenterLine

                .endproc


;======================================
;   old IndentR()
;======================================
IndentRight     .proc
                lda indent
                bmi ScrollInit._XIT

                inc indent

                jmp CenterLine

                .endproc


;======================================
;   InsrtT() insert/replace toggle
;======================================
InsertToggle    .proc
                lda #<_rmsg
                ldx #>_rmsg
                inc insert
                beq _XIT

                lda #$FF
                sta insert

                lda #<_imsg
                ldx #>_imsg

_XIT            jmp CommandMsg

;--------------------------------------

_imsg           .ptext "INSERT"
_rmsg           .ptext "REPLACE"

                .endproc


;======================================
; Intialize Scrolling
;======================================
ScrollInit      .proc
                sty arg13

                jsr CleanLine
                beq _XIT

                ldy arg13
                jsr next
                beq _1                ; EOF

                lda COLCRS
                sta x__

                ; lda choff
                ; beq _SI1

                lda #0
                sta choff

                jsr DisplayBuffer

                jmp LoadBuffer

_1              pla
                pla

_XIT            rts
                .endproc


;======================================
;   ScrollUp()
;======================================
ScrollUp        .proc
                ldy #1
                jsr ScrollInit

                dec lnum
                bmi _1

                jmp scrup

_1              inc lnum

                lda ytop
                sta y__

                jsr BottomLine

                lda nlines
                jsr MoveDown
                jsr ResetColumn

                jmp RefreshBuf

                .endproc


;======================================
;   ScrollDown()
;======================================
ScrollDown      .proc
                ldy #5
                jsr ScrollInit

                ldx lnum
                inx
                cpx nlines
                beq _1

                stx lnum

                jmp scrdwn

_1              jsr BottomLine

                stx y__

                lda nlines
                ldx ytop
                jsr MoveUp

                jsr ResetColumn
                jsr DisplayBuffer

                jmp ResetColumn

                .endproc


;======================================
;   BottomLine()
;======================================
BottomLine      .proc
                clc
                lda ytop
                adc nlines

                tax
                dex

_XIT            rts
                .endproc


;======================================
;   CheckColumn()
;======================================
CheckColumn     .proc
                jsr SetSpacing

                ldy #0
                lda (buf),Y
                cmp sp
                bcs _XIT

                jsr Back
                jsr SetSpacing

                clc
_XIT            rts
                .endproc


;======================================
;   ScrollLeft()
;======================================
ScrollLeft      .proc
                jsr CheckColumn

                lda LMARGN
                cmp COLCRS
                bcc _XIT

                clc
                lda choff
                adc indent
                beq CheckColumn._XIT

                dec choff

                jsr DisplayBuffer
                jsr scrrt

_XIT            jmp scrlft

                .endproc


;======================================
;   ScrlRt()
;======================================
ScrollRight     .proc
                jsr CheckColumn
                bcc CheckColumn._XIT

                lda COLCRS
                cmp RMARGN
                bcc _XIT

                inc choff

                jsr DisplayBuffer
                jsr scrlft

_XIT            jmp scrrt

                .endproc


;======================================
;   SetSpacing()
;======================================
SetSpacing      .proc
                sec
                lda indent
                adc choff

                clc
                adc COLCRS

                sec
                sbc LMARGN
                sta sp

                rts
                .endproc


;======================================
;   MoveDown(cnt, row)
;======================================
MoveDown        .proc
                ldy #+0-40              ; rowSize
                sty arg5

                ldy #$FF
                bra MoveContent

                .endproc


;======================================
;   MoveUp(cnt, row)
;======================================
MoveUp          .proc
                ldy #40                 ; rowSize
                sty arg5

                ldy #0

                .endproc

                ;[fall-through]


;======================================
;
;======================================
MoveContent     .proc
                sty arg6                ; save registers
                sta arg4

                stx ROWCRS
                jsr RestoreCursorChar
                jsr GetDisplayAddr     ; get display address

                ldx arg4
                dex

_next1          lda arg0
                sta arg2

                clc
                adc arg5
                sta arg0

                lda arg1
                sta arg3

                adc arg6
                sta arg1

                ldy #39
_next2          lda (arg0),Y
                sta (arg2),Y

                dey
                bpl _next2

                dex
                bne _next1

                rts
                .endproc
