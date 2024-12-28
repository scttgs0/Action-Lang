
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.tag.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


tag            .namespace

;======================================
; Set()
;======================================
Set             .proc
                jsr ID

                lda tempbuf
                beq NoTag

                jsr editor.display.CleanLine

                lda tempbuf+1
                jsr Get
                bne _1                  ; tag already exists

;   get a new tag
                lda #$08                ; allocate 8 bytes???
                jsr Allocate

                ldy #$01
                lda taglist+1
                sta (zpAllocCurrent),Y

                dey
                lda taglist
                sta (zpAllocCurrent),Y

                lda zpAllocCurrent
                sta taglist

                ldx zpAllocCurrent+1
                stx taglist+1

_1              ldy #$04
                lda tempbuf+1
                sta (zpAllocCurrent),Y

                iny
                lda cur
                sta (zpAllocCurrent),Y

                iny
                lda cur+1
                sta (zpAllocCurrent),Y

                iny
                jsr editor.command.SetSpacing

                sta (zpAllocCurrent),Y

;   flag line as tagged
                ldy #$03
                lda (cur),Y
                ora #$80
                sta (cur),Y

                rts
                .endproc


;======================================
; NoTag()
;======================================
NoTag           .proc
                lda #<_msgNoTag
                ldx #>_msgNoTag

                jmp editor.display.CommandMsg

;--------------------------------------

_msgNoTag       .ptext "tag not set"

                .endproc


;======================================
; ID()
;======================================
ID              .proc
                lda #<_msgTagID
                ldx #>_msgTagID

                jmp editor.window.GetTemp

;--------------------------------------

_msgTagID       .ptext "tag id: "

                .endproc


;======================================
; Locate()
;======================================
Locate          .proc
                jsr ID

                lda tempbuf
                beq Get._XIT

                jsr editor.display.CleanLine

                lda tempbuf+1
                jsr Get
                beq NoTag

                ldy #$06
                lda (zpAllocCurrent),Y

                tax
                dey
                lda (zpAllocCurrent),Y
                jsr FindLine
                beq NoTag

                ldy #$03
                lda (arg2),Y
                bpl NoTag

                ldy #$07
                lda (zpAllocCurrent),Y
                sta sp

                lda arg2
                sta cur
                ldx arg3
                stx cur+1

                jmp editor.find.Found

                .endproc


;======================================
; Get(tag)
;======================================
Get             .proc
                sta arg0

                lda taglist
                ldx taglist+1
                bne _1

_XIT            rts

_next1          ldy #$04
                lda (zpAllocCurrent),Y
                cmp arg0
                beq _2

                ldy #$01
                lda (zpAllocCurrent),Y

                tax
                dey
                lda (zpAllocCurrent),Y
_1              sta zpAllocCurrent
                stx zpAllocCurrent+1

                txa
                bne _next1

_2              ldx zpAllocCurrent+1

                rts
                .endproc


;======================================
; FreeTags()
;======================================
FreeTags        .proc
                lda taglist
                ldx taglist+1
                beq _XIT

_next1          sta zpAllocBest
                stx zpAllocBest+1

                ldy #$00
                lda (zpAllocBest),Y
                sta arg0

                iny
                lda (zpAllocBest),Y
                sta arg1

                jsr Free._ENTRY1

                lda arg0
                ldx arg1
                bne _next1

                stx taglist+1

_XIT            rts
                .endproc


;======================================
; FindLine(line)
;======================================
FindLine        .proc
                sta arg0
                stx arg1

                lda top
                ldx top+1
                bne _1

                rts

_next1          ldy #$05
                lda (arg2),Y

                tax
                dey
                lda (arg2),Y

_1              sta arg2
                stx arg3

                cmp arg0
                bne _2

                cpx arg1
                beq _3

_2              txa
                bne _next1

_3              ldx arg3

                rts
                .endproc

                .endnamespace
