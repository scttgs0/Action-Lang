
; SPDX-FileName: edit.dsp.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   CommandMsg(message)
;======================================
CommandMsg      .proc
                sta arg0

                jsr cmdcol

                lda #0
                sta arg3

                lda arg0
                ldy #$80
                jsr putstr

                jmp rstcol

                .endproc


;======================================
;   CleanLine()
;======================================
CleanLine       .proc
                jsr chkcur

                lda isDirty
                beq _XIT

                sta dirty

                lda #0
                sta isDirty

                jsr DeleteCurrentLine
                jsr InsertByte

_XIT            jmp chkcur

                .endproc


;======================================
;   SaveWindow()
;======================================
SaveWindow      .proc
                jsr CleanLine

_ENTRY1         clc
                lda #14
                tax
                adc currentWindow

                tay
_next1          lda sp,x
                sta w1,y

                dey
                dex
                bpl _next1

                rts
                .endproc


;======================================
;   RestoreWindow() restore window
;======================================
RestoreWindow   .proc
                clc
                lda #14
                tax
                adc currentWindow

                tay
_next1          lda w1,y
                sta sp,x

                dey
                dex
                bpl _next1

_XIT            rts
                .endproc


;======================================
;   EndLine()
;======================================
EndLine         .proc
                jsr CleanLine

                lda bot
                sta cur
                lda bot+1
                sta cur+1

                .endproc

                ;[fall-through]


;======================================
;   CenterLine() center line
;======================================
CenterLine      .proc
                lda #0
                sta temps

                jsr CleanLine
                beq _1

                jsr nextup
                beq _1

                inc temps

                jsr nextup
                beq _1

                inc temps

_1              jsr NewPage

_next1          lda temps
                beq RestoreWindow._XIT

                jsr ScrollDown

                dec temps

                jmp _next1

                .endproc


;======================================
;   TopLine()
;======================================
TopLine         .proc
                jsr CleanLine
                jsr chkcur._ldtop

                .endproc

                ;[fall-through]


;======================================
;   NewPage()
;======================================
NewPage         .proc
                lda #0
                sta lnum

_ENTRY1         sta choff

                jsr rstcsr              ; for command line

                lda LMARGN
                sta COLCRS

                .endproc

                ;[fall-through]


;======================================
;   Refresh()
;======================================
Refresh         .proc
                clc
                lda ytop
                adc lnum
                sta ROWCRS

                jsr savecol
                jsr SaveWindow

                inc ROWCRS

                jsr nextdwn

                sta arg9

                clc
                lda nlines
                sbc lnum
                sta arg10
                beq _2

_next1          ldy #0
                lda indent
                sta arg3

                ldx arg9
                beq _3

                jsr curstr

_next2          jsr putstr

                lda arg9
                bne _1

                tay
                sta (arg0),y

_1              inc ROWCRS

                jsr nextdwn

                sta arg9

                dec arg10
                bne _next1

_2              jsr rstcur
                jsr rstcol

                jmp RefreshBuf

_3              lda #<zero
                ldx #>zero

                bra _next2

                .endproc
