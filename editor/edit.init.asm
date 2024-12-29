
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

;   calculate memory available for allocation
                sec
                lda MEMTOP
                sbc zpAllocBase
                iny
                sta (zpAllocBase),Y

                lda MEMTOP+1
                sbc zpAllocBase+1
                iny
                sta (zpAllocBase),Y

                lda #<$0200             ; allocate 2 pages of spare memory
                ldx #>$0200
                jsr Allocate

                lda zpAllocCurrent
                sta spareMem
                ldx zpAllocCurrent+1
                stx spareMem+1

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

;   activate window2
                lda #win2Base-win1Base
                sta is2Windows          ; non-zero means window2 is active
                sta currentWindow

                jsr ZeroWindow

                ldy jt_wsize
                iny
                sty ytop

                sec
                lda #$1D        ;;#59
                sbc jt_wsize
                sta nlines

                bra EditorInit._ENTRY2

                .endproc


;======================================
; Initialize the Editor
;======================================
EditorInit      .proc
    ; [debug] $B621
                jsr Memory

                lda #<$0100             ; allocate 1 page for edit buffer
                ldx #>$0100
                jsr Allocate

                lda zpAllocCurrent
                sta buf
                ldx zpAllocCurrent+1
                stx buf+1

                lda #$40             ; translate screen code into ascii code
                ;;lda #$00    ; HACK:
                sta chrConvert

;   set HEAD and TAIL within the delete buffer
                lda #<delbuf
                sta delbuf
                sta delbuf+4
                lda #>delbuf
                sta delbuf+1
                sta delbuf+5

;   initialize window
                jsr ZeroWindow

_ENTRY1         lda #$1D        ;;#59                 ; rowcount
                sta nlines
                sta cmdln

;   activate window1
                lda #$00
                sta currentWindow
                sta ytop

_ENTRY2         jsr editor.display.CenterLine

_ENTRY3         lda #<msgEditCmd
                ldx #>msgEditCmd
                jmp editor.display.CommandMsg

;--------------------------------------

msgEditCmd      .ptext "ACTION! (c)2024 GPL3"
                .endproc

                .endnamespace
