;======================================
;   FILE: edit.tag.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


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
                sta (afcur),y
                dey
                lda taglist
                sta (afcur),y
                lda afcur
                sta taglist
                ldx afcur+1
                stx taglist+1

_st1            ldy #4
                lda tempbuf+1
                sta (afcur),y
                iny
                lda cur
                sta (afcur),y
                iny
                lda cur+1
                sta (afcur),y
                iny
                jsr setsp

                sta (afcur),y

    ; flag line as taged
                ldy #3
                lda (cur),y
                ora #$80
                sta (cur),y
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
                lda (afcur),y
                tax
                dey
                lda (afcur),y
                jsr findln
                beq notag

                ldy #3
                lda (arg2),y
                bpl notag

                ldy #7
                lda (afcur),y
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
                lda (afcur),y
                cmp arg0
                beq _gt3

                ldy #1
                lda (afcur),y
                tax
                dey
                lda (afcur),y
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
                lda (afbest),y
                sta arg0
                iny
                lda (afbest),y
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
                lda (arg2),y
                tax
                dey
                lda (arg2),y
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
