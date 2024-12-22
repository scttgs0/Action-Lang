
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.init.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


init            .namespace

;======================================
; Initialize memory
;======================================
Memory          .proc
                lda MEMLO
                sta zpAllocBase
                lda MEMLO+1
                sta zpAllocBase+1

                lda #$00
                tay
                sta (zpAllocBase),Y

                iny
                sta (zpAllocBase),Y

                sec
                lda MEMTOP
                sbc zpAllocBase

                iny
                sta (zpAllocBase),Y

                lda MEMTOP+1
                sbc zpAllocBase+1

                iny
                sta (zpAllocBase),Y

                lda #$00                ; allocate 2 pages of spare memory
                ldx #$02
                jsr Allocate

                lda zpAllocCurrent
                sta sparem
                ldx zpAllocCurrent+1
                stx sparem+1

                rts
                .endproc


;======================================
; Initialize window
;======================================
ZeroWindow      .proc
                lda #$00
                ldx #$0F

_next1          dex                     ; zero page0 window table
                sta sp,X
                bne _next1

                sta isDirty
                sta inbuf

                tay
                sta (buf),Y

                rts
                .endproc


;======================================
; Initialize secondary window
;======================================
Window2         .proc
                jsr editor.display.CenterLine

                lda jt_wsize
                sta nlines
                sta cmdln

                jsr editor.window.SaveWorld

                lda #w2-w1
                sta numwd
                sta currentWindow

                jsr ZeroWindow

                ldy jt_wsize
                iny
                sty ytop

                sec
                lda #$17
                sbc jt_wsize
                sta nlines

                bne EditorInit._ENTRY2  ; [unc]

                .endproc


;======================================
; Initialize the Editor
;======================================
EditorInit      .proc
                jsr Memory

                lda #$00
                ldx #$01
                jsr Allocate            ; get edit buffer

                lda zpAllocCurrent
                sta buf
                ldx zpAllocCurrent+1
                stx buf+1

                lda #$40
                sta chrConvert

                lda #<delbuf
                sta delbuf
                sta delbuf+4
                lda #>delbuf
                sta delbuf+1
                sta delbuf+5

;   initialize window
                jsr ZeroWindow

_ENTRY1         lda #$17                ; rowcount
                sta nlines
                sta cmdln

                lda #$00
                sta currentWindow
                sta ytop

_ENTRY2         jsr editor.display.CenterLine

_ENTRY3         lda #<editCmdMsg
                ldx #>editCmdMsg

                jmp editor.display.CommandMsg

;--------------------------------------

editCmdMsg      .ptext "ACTION! (c)1983 ACS"
                .endproc

                .endnamespace
