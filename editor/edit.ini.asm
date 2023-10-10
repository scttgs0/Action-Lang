
; SPDX-FileName: edit.ini.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
; Initialize memory
;======================================
MemoryInit      .proc
                ;!!lda MEMLO
                sta afbase
                ;!!lda MEMLO+1
                sta afbase+1

                lda #0
                tay
                sta (afbase),y

                iny
                sta (afbase),y

                sec
                ;!!lda MEMTOP
                sbc afbase

                iny
                sta (afbase),y

                ;!!lda MEMTOP+1
                sbc afbase+1

                iny
                sta (afbase),y

                lda #0                  ; allocate 2 pages of spare memory
                ldx #2
                jsr Allocate

                lda afcur
                sta sparem
                ldx afcur+1
                stx sparem+1

                rts
                .endproc


;======================================
; Initialize window
;======================================
ZeroWindow      .proc
                lda #0
                ldx #15

_next1          dex                     ; zero page0 window table
                sta sp,x
                bne _next1

                sta isDirty
                sta inbuf

                tay
                sta (buf),y

                rts
                .endproc


;======================================
; Initialize secondary window
;======================================
Window2Init     .proc
                jsr CenterLine

                lda jt_wsize
                sta nlines
                sta cmdln

                jsr SaveWorld

                lda #w2-w1
                sta numwd
                sta currentWindow

                jsr ZeroWindow

                ldy jt_wsize
                iny
                sty ytop

                sec
                lda #23
                sbc jt_wsize
                sta nlines

                bra EditorInit._ENTRY2

                .endproc


;======================================
; Initialize the Editor
;======================================
EditorInit      .proc
                lda #<$7FFF
                ;!!sta MEMTOP
                lda #>$7FFF
                ;!!sta MEMTOP+1

                lda #<$0600
                ;!!sta MEMLO
                lda #>$0600
                ;!!sta MEMLO+1

                jsr MemoryInit

                lda #0
                ldx #1
                jsr Allocate            ; get edit buffer

                lda afcur
                sta buf
                ldx afcur+1
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

_ENTRY1         lda #23                 ; rowcount
                sta nlines
                sta cmdln

                lda #0
                sta currentWindow
                sta ytop

_ENTRY2         jsr CenterLine

_ENTRY3         lda #<editCmdMsg
                ldx #>editCmdMsg

                jmp CommandMsg

;--------------------------------------

editCmdMsg      .ptext "ACTION! (c) 2023 GPL3"
                .endproc
