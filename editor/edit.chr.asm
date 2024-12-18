
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.chr.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   InsertChar() char in curCh
;======================================
InsertChar      .proc
                jsr SetSpacing

                ldy #0
                lda (buf),Y
                cmp jt_linemax
                bcc _1                 ; test line too long

                jsr scrbell

                ldy #0
                lda (buf),Y
_1              cmp sp
                bcs _2

                sta arg0

                lda sp
                sta (buf),Y

                ldy arg0
                lda #$20                ; pad with spaces
_next1          iny
                sta (buf),Y

                cpy sp
                bcc _next1

_next2          ldy sp
                lda curch
                sta (buf),Y

                lda #$FF
                sta isDirty

                jsr DisplayBuffer

                jmp ScrollRight

_2              ldx insert
                beq _next2

;   move buf right one char
                adc #0                  ; really 1, carry set
                sta (buf),Y

                tay
_next3          dey

                lda (buf),Y
                iny
                sta (buf),Y

                dey
                cpy sp
                bne _next3
                bra _next2

                .endproc


;======================================
;   InsrtSp()
;======================================
InsertSpace     .proc
                lda insert
                pha

                lda #$20
                sta insert
                sta curch

                jsr InsertChar

                pla
                sta insert

                jmp scrlft

                .endproc


;======================================
;   Insert_()
;======================================
Insert_         .proc
                jsr CleanLine
                jsr nextup

                sta cur+1               ; tricky

                jsr _ENTRY1

                lda #0
                jmp NewPage._ENTRY1

_ENTRY1         lda #0
                tay
_ENTRY2         sta (buf),Y

                iny
                sty dirty

                jmp InsertByte

                .endproc


;======================================
; handle pad if any
;======================================
csret           .proc
                jsr InsertSpace
                jsr DeleteChar

                ldy #0
                lda (buf),Y
                pha

                jsr SetSpacing

                sta isDirty             ; always non-zero

                sec
                sbc #1
                sta (buf),Y

                jsr CleanLine

                pla
                sta arg1

                inc arg1
                lda #0
                sta arg0
                beq _1

_next1          lda (buf),Y
                inc arg0
                ldy arg0
                sta (buf),Y

                inc sp
_1              ldy sp
                cpy arg1
                bcc _next1

                ldy #0
                lda arg0
                jsr Insert_._ENTRY2
                jsr nextup
                jsr Refresh

                jmp Return_._ENTRY1

                .endproc


;======================================
;   Return_()
;======================================
Return_         .proc
                ldx insert
                bne csret

                jsr CheckDown
                bne _ENTRY1

                jsr Insert_._ENTRY1
                jsr nextup
                jsr LoadBuffer

_ENTRY1         jsr ScrollDown

_XIT            jmp Front

                .endproc


;======================================
;   Delete_()
;======================================
Delete_         .proc
                jsr CleanLine

                lda delbuf
                ldx delbuf+1
                stx dirty

                ldy lastch
                cpy #$9C
                beq _1

                jsr DeleteFree

_1              sta arg3
                stx arg4

                jsr InsertBuffer
                jsr CheckDown           ; last line ?
                bne _2                  ;   no, delete it

                tay
                sta (buf),Y

                iny
                sty isDirty
                bra Return_._XIT

_2              jsr DeleteCurrentLine
                beq _3

                jsr nextdwn
_3              jsr ChkCursor

                lda #0

                jmp NewPage._ENTRY1

                .endproc


;======================================
;   DeleteTop()
;======================================
DeleteTop       .proc
                lda delbuf+4
                ldx delbuf+5
                sta delnxt
                stx delnxt+1

                .endproc

                ;[fall-through]


;======================================
;   DeleteEnd(ptr)
;======================================
DeleteEnd       .proc
                cmp #<delbuf
                bne _XIT

                cpx #>delbuf

_XIT            rts
                .endproc


;======================================
;   DeleteFree(bot)
;======================================
DeleteFree      .proc
                jsr DeleteEnd
                beq DeleteEnd._XIT

                jsr DeleteLine
                bne DeleteFree

                .endproc


;======================================
;   DeleteNext()
;======================================
DeleteNext      .proc
                ldy #5
                lda (delnxt),Y
                tax

                dey
                lda (delnxt),Y
                sta delnxt
                stx delnxt+1

                jmp DeleteEnd

                .endproc


;======================================
;   Undo()
;======================================
Undo            .proc
                jsr LoadBuffer

                jmp Front

                .endproc


;======================================
;   DeleteChar()
;======================================
DeleteChar      .proc
                jsr CheckColumn
                bcc CheckDown._XIT

                ldy #0
                lda (buf),Y
                sta isDirty

                sec
                sbc #1
                sta (buf),Y

                ldy sp
_next1          iny
                lda (buf),Y

                dey
                sta (buf),Y

                iny
                cpy isDirty
                bcc _next1              ; really checking for =

                .endproc


;======================================
;   RefreshBuf()
;======================================
RefreshBuf      .proc
                jsr DisplayBuffer

                jmp ResetColumn._ENTRY1

                .endproc


;======================================
;   CheckDown()
;======================================
CheckDown       .proc
                jsr CleanLine
                beq _XIT

                ldy #5
                lda (cur),Y

_XIT            rts
                .endproc


;======================================
;   BackSpc()
;======================================
BackSpc         .proc
                jsr SetSpacing

                cmp #2
                bcc CheckDown._XIT

_ENTRY1         jsr ScrollLeft
                jsr SetSpacing

                tay
                lda #$20
                sta (buf),Y
                sta isDirty

                lda insert
                bne DeleteChar

                jmp RefreshBuf

                .endproc


;======================================
;
;======================================
csbs            .proc
                jsr SetSpacing

                cmp #2
                bcs BackSpc._ENTRY1

                jsr ChkCursor
                beq CheckDown._XIT      ; no lines at all!

                ldy #1
                lda (cur),Y
                beq CheckDown._XIT      ; no line to merge with

    ; merge
                jsr ScrollUp
                jsr Back
                jsr nextdwn

                sta isDirty

                jsr curstr

                clc
                ldy #0
                lda (buf),Y
                sta arg2

                adc (arg0),Y
                sta (buf),Y

                lda (arg0),Y
                beq _next2

                sta arg3

_next1          iny
                sty arg4

                lda (arg0),Y
                inc arg2
                ldy arg2
                sta (buf),Y

                ldy arg4
                cpy arg3
                bne _next1

_next2          jsr DeleteCurrentLine

                jmp Refresh

                .endproc
