
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.tag.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   SetTag()
;======================================
SetTag          .proc
                jsr tagid

                lda tempbuf
                beq notag

                jsr CleanLine

                lda tempbuf+1
                jsr GetTag
                bne _1                  ; tag already exists

;   get a new tag
                lda #8
                jsr Allocate

                ldy #1
                lda taglist+1
                sta (zpAllocCurrent),Y

                dey
                lda taglist
                sta (zpAllocCurrent),Y

                lda zpAllocCurrent
                sta taglist

                ldx zpAllocCurrent+1
                stx taglist+1

_1              ldy #4
                lda tempbuf+1
                sta (zpAllocCurrent),Y

                iny
                lda cur
                sta (zpAllocCurrent),Y

                iny
                lda cur+1
                sta (zpAllocCurrent),Y

                iny
                jsr SetSpacing

                sta (zpAllocCurrent),Y

;   flag line as tagged
                ldy #3
                lda (cur),Y
                ora #$80
                sta (cur),Y

                rts
                .endproc


;======================================
;   NoTag()
;======================================
notag           .proc
                lda #<_ntmsg
                ldx #>_ntmsg

                jmp CommandMsg

;--------------------------------------

_ntmsg          .ptext "tag not set"

                .endproc


;======================================
;   TagId()
;======================================
tagid           .proc
                lda #<_stmsg
                ldx #>_stmsg

                jmp GetTemp

;--------------------------------------

_stmsg          .ptext "tag id: "

                .endproc


;======================================
;   LocateTag()
;======================================
LocateTag       .proc
                jsr tagid

                lda tempbuf
                beq GetTag._XIT

                jsr CleanLine

                lda tempbuf+1
                jsr GetTag
                beq notag

                ldy #6
                lda (zpAllocCurrent),Y

                tax
                dey
                lda (zpAllocCurrent),Y
                jsr FindLine
                beq notag

                ldy #3
                lda (arg2),Y
                bpl notag

                ldy #7
                lda (zpAllocCurrent),Y
                sta sp

                lda arg2
                sta cur
                ldx arg3
                stx cur+1

                jmp Found

                .endproc


;======================================
;   GetTag(tag)
;======================================
GetTag          .proc
                sta arg0

                lda taglist
                ldx taglist+1
                bne _1

_XIT            rts

_next1          ldy #4
                lda (zpAllocCurrent),Y
                cmp arg0
                beq _2

                ldy #1
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
;   FreeTags()
;======================================
FreeTags        .proc
                lda taglist
                ldx taglist+1
                beq _XIT

_next1          sta zpAllocBest
                stx zpAllocBest+1

                ldy #0
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
;   FindLine(line)
;======================================
FindLine        .proc
                sta arg0
                stx arg1

                lda top
                ldx top+1
                bne _1

                rts

_next1          ldy #5
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
