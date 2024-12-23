
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.command.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


command         .namespace

;======================================
; Front()
;======================================
Front           .proc
                sec
                lda #$00
                sbc indent
                sta choff

                jsr ioDisplayBuffer

                lda LMARGN
                jmp ioResetColumn+6

                .endproc


;======================================
; Back()
;======================================
Back            .proc
                ldy #$00
                lda (buf),Y
_ENTRY1         pha

                clc
                adc LMARGN

                sec
                sbc RMARGN
                bcs _1

                lda #$01
_1              sbc indent
                sta choff

                jsr ioDisplayBuffer

                sec
                pla
                sbc indent

                sec
                sbc choff

                clc
                adc LMARGN

                jmp ioResetColumn+6

                .endproc


;======================================
; PageUp()
;======================================
PageUp          .proc
                sec
                lda lnum
                sbc #$02

                ldy #$01
                bra PageContent

                .endproc


;======================================
; PageDown()
;======================================
PageDown        .proc
                ldy #$05

                sec
                lda #$02
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
                jsr editor.display.CleanLine

_next1          ldy arg13
                jsr mscNext

                dec arg14
                bne _next1

_XIT            jmp editor.display.CenterLine

                .endproc


;======================================
; Paste()
;======================================
Paste           .proc
                jsr editor.chr.DeleteTop
                beq _XIT

                stx dirty

                jsr editor.display.CleanLine
                jsr mscNextUp

                sta cur+1               ; tricky, fake out top

                jsr editor.display.SaveWindow._ENTRY1
                jsr editor.chr.DeleteTop

_next1          jsr mscStrPtr
                jsr ioLoadBuffer._ENTRY1
                jsr editor.memory.InsertByte

                lda allocerr
                bne _1                  ; check for out of memory

                jsr editor.chr.DeleteNext
                bne _next1

_1              jsr ioResetCursor

                ldy currentWindow
                lda w1+WCUR+1,Y
                beq _2

                jsr mscNextDown

_2              lda #$00
                jmp editor.display.NewPage._ENTRY1

_XIT            rts
                .endproc


;======================================
; old IndentL()
;======================================
IndentLeft      .proc
                lda indent
                beq ScrollInit._XIT

                dec indent

                jmp editor.display.CenterLine

                .endproc


;======================================
; old IndentR()
;======================================
IndentRight     .proc
                lda indent
                bmi ScrollInit._XIT

                inc indent

                jmp editor.display.CenterLine

                .endproc


;======================================
; InsertToggle()
;--------------------------------------
; insert/replace toggle
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

_XIT            jmp editor.display.CommandMsg

;--------------------------------------

_imsg           .ptext "INSERT"
_rmsg           .ptext "REPLACE"

                .endproc


;======================================
; Initialize Scrolling
;======================================
ScrollInit      .proc
                sty arg13

                jsr editor.display.CleanLine
                beq _1

                ldy arg13
                jsr mscNext
                beq _1                  ; EOF

                lda COLCRS
                sta x__

                ; lda choff
                ; beq _SI1

                lda #$00
                sta choff

                jsr ioDisplayBuffer
                jmp ioLoadBuffer

_1              pla
                pla

_XIT            rts
                .endproc


;======================================
; ScrollUp()
;======================================
ScrollUp        .proc
                ldy #$01
                jsr ScrollInit

                dec lnum
                bmi _1
                jmp screenCursorUp

_1              inc lnum

                lda ytop
                sta y__

                jsr BottomLine

                lda nlines
                jsr MoveDown
                jsr ioResetColumn
                jmp editor.chr.RefreshBuf

                .endproc


;======================================
; ScrollDown()
;======================================
ScrollDown      .proc
                ldy #$05
                jsr ScrollInit

                ldx lnum
                inx
                cpx nlines
                beq _1

                stx lnum

                jmp screenCursorDown

_1              jsr BottomLine
                stx y__

                lda nlines
                ldx ytop
                jsr MoveUp

                jsr ioResetColumn
                jsr ioDisplayBuffer
                jmp ioResetColumn

                .endproc


;======================================
; BottomLine()
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
; CheckColumn()
;======================================
CheckColumn     .proc
                jsr SetSpacing

                ldy #$00
                lda (buf),Y
                cmp sp
                bcs _XIT

                jsr Back
                jsr SetSpacing

                clc
_XIT            rts
                .endproc


;======================================
; ScrollLeft()
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

                jsr ioDisplayBuffer
                jsr screenCursorRight

_XIT            jmp screenCursorLeft

                .endproc


;======================================
; ScrollRight()
;======================================
ScrollRight     .proc
                jsr CheckColumn
                bcc CheckColumn._XIT

                lda COLCRS
                cmp RMARGN
                bcc _XIT

                inc choff

                jsr ioDisplayBuffer
                jsr screenCursorLeft

_XIT            jmp screenCursorRight

                .endproc


;======================================
; SetSpacing()
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
; MoveDown(cnt, row)
;======================================
MoveDown        .proc
                ldy #+0-40              ; rowSize
                sty arg5

                ldy #$FF
                bra MoveContent

                .endproc


;======================================
; MoveUp(cnt, row)
;======================================
MoveUp          .proc
                ldy #40                 ; rowSize
                sty arg5

                ldy #$00

                .endproc

                ;[fall-through]


;======================================
;
;======================================
MoveContent     .proc
                sty arg6                ; save registers
                sta arg4
                stx ROWCRS

                jsr ioRestoreCursorChar
                jsr ioGetDisplayAddr    ; get display address

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

                .endnamespace
