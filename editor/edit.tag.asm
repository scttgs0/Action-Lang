
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.tag.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   SetTag()
;======================================
settag          .proc
                jsr tagid

                lda tempbuf
                beq notag

                jsr clnln

                lda tempbuf+1
                jsr gettag
                bne _st1                ; tag already exists

    ; get a new tag
                lda #8
                jsr allocate

                ldy #1
                lda taglist+1
                sta (afcur),Y
                dey
                lda taglist
                sta (afcur),Y
                lda afcur
                sta taglist
                ldx afcur+1
                stx taglist+1

_st1            ldy #4
                lda tempbuf+1
                sta (afcur),Y
                iny
                lda cur
                sta (afcur),Y
                iny
                lda cur+1
                sta (afcur),Y
                iny
                jsr setsp

                sta (afcur),Y

    ; flag line as taged
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
                jmp cmdmsg

;--------------------------------------

_ntmsg          .text 11,"tag not set"
                .endproc


;======================================
;   TagId()
;======================================
tagid           .proc
                lda #<_stmsg
                ldx #>_stmsg
                jmp gettemp

;--------------------------------------

_stmsg          .text 8,"tag id: "
                .endproc


;======================================
;   LocTag()
;======================================
loctag          .proc
                jsr tagid

                lda tempbuf
                beq gettag._ltret

                jsr clnln

                lda tempbuf+1
                jsr gettag
                beq notag

                ldy #6
                lda (afcur),Y
                tax
                dey
                lda (afcur),Y
                jsr findln
                beq notag

                ldy #3
                lda (arg2),Y
                bpl notag

                ldy #7
                lda (afcur),Y
                sta sp
                lda arg2
                sta cur
                ldx arg3
                stx cur+1
                jmp found

                .endproc


;======================================
;   GetTag PROC ; GetTag(tag)
;======================================
gettag          .proc
                sta arg0
                lda taglist
                ldx taglist+1
                bne _gt2

_ltret          rts

_gt1            ldy #4
                lda (afcur),Y
                cmp arg0
                beq _gt3

                ldy #1
                lda (afcur),Y
                tax
                dey
                lda (afcur),Y
_gt2            sta afcur
                stx afcur+1
                txa
                bne _gt1

_gt3            ldx afcur+1
                rts
                .endproc


;======================================
;   FreeTags()
;======================================
freetags        .proc
                lda taglist
                ldx taglist+1
                beq _ft2

_ft1            sta afbest
                stx afbest+1
                ldy #0
                lda (afbest),Y
                sta arg0
                iny
                lda (afbest),Y
                sta arg1
                jsr free.free1

                lda arg0
                ldx arg1
                bne _ft1
                stx taglist+1
_ft2            rts
                .endproc


;======================================
;   FindLn(line)
;======================================
findln          .proc
                sta arg0
                stx arg1
                lda top
                ldx top+1
                bne _fl2

                rts

_fl1            ldy #5
                lda (arg2),Y
                tax
                dey
                lda (arg2),Y
_fl2            sta arg2
                stx arg3
                cmp arg0
                bne _fl3

                cpx arg1
                beq _fl4

_fl3            txa
                bne _fl1

_fl4            ldx arg3
                rts
                .endproc
