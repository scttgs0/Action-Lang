
;         EDIT.FND

; Copyright 1983 by Clinton W Parker
; All Rights Reserved
; last modified May 4, 1983
;
; This file is part of Action!.
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

;    Find()
;    ------
find            .proc
                jsr setsp
                jsr savewd
                lda lastch
                cmp #$f8
                beq find2
                lda #<findmsg
                ldx #>findmsg
find1           ldy #>findbuf
                sty arg3
                ldy #<findbuf
                jsr cmdstr
                lda #$f8
                sta curch

find2           lda findbuf
                beq _f7

_f2             ldy #0
                lda (buf),y
                tay
                iny
                sty arg0

_f3             ldy sp
                iny
                cpy arg0
                bcs _f5
                sty sp
                ldx #0

_f4             lda (buf),y
                inx
                cmp findbuf,x
                bne _f3
                iny
                cpx findbuf
                beq found
                cpy arg0
                bcc _f4

_f5             jsr nextdwn
                beq _f6
                jsr ldbuf
                lda #0
                sta sp
                beq _f2

_f6             sta curch
                jsr rstcur
                jsr ldbuf
                lda #<notfnd
                ldx #>notfnd
                jsr cmdmsg
                lda #0
_f7             sta curch
                rts
                .endproc

found           .proc
                jsr ctrln
                ldy sp
                dey
                tya
                jsr back.back0
                lda #$fe
                rts
                .endproc

notfnd          .text 9,"not found"

findmsg         .text 6,"Find? "


;         EDIT.SUB

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified September 8, 1983

;    Subst()
;    -------
subst           .proc
                jsr setsp
                jsr savewd
                lda lastch
                cmp #$7d
                beq _s2
                pha
                lda #<submsg
                ldx #>submsg
                ldy #>subbuf
                sty arg3
                ldy #<subbuf
                jsr cmdstr
                pla
    ; check for ESC key
                ldx subbuf
                bne _s0
                ldx subbuf+1
                cpx #$1b
                beq _s1
_s0             cmp #$f8
                beq _s3                 ; string already found
                lda #<formsg
                ldx #>formsg
                jsr find.find1
                bne _s3

_s1             rts

_s2             jsr find.find2
                beq _s1
_s3             lda #$7d
                sta curch
                sta dirtyf              ; flag line as dirty
                sec
                lda subbuf
                sbc findbuf             ; get delta size
                sta arg3
                php                     ; save status for test below
                bcs _s4
                lda #1
                sbc arg3                ; negate delta size
_s4             clc
                adc buf
                sta arg0
                lda #0
                tay
                adc buf+1
                sta arg1
                lda (buf),y
                plp
                bcc _s6                 ; need to remove space
                beq _s8                 ; same size
    ; need to add space
                tay
_s5             lda (buf),y
                sta (arg0),y
                dey
                cpy sp
                bcs _s5
                bcc _s8

_s6             sta arg2
                ldy sp
                dey
_s7             iny
                lda (arg0),y
                sta (buf),y
                cpy arg2
                bcc _s7

_s8             ldy sp
                ldx #0
                beq _s10
_s9             inx
                lda subbuf,x
                sta (buf),y
                iny
_s10            cpx subbuf
                bne _s9
                clc
                ldy #0
                lda (buf),y
                adc arg3
                sta (buf),y
                jmp rfrshbuf
                .endproc

submsg          .text 12,"Substitute? "

formsg          .text 5,"for? "


;         EDIT.TAB

; Copyright 1983 by Action Computer Services
; All rights reserved.

; last modified March 9, 1983


;    Tab()
;    -----
tab             .proc
                jsr setsp
                jsr tabloc._tabpos

_t1             lda tabmap,x
                beq _t3
                and _onbit,y
                beq _t2
    ; found tab setting
                sty arg0
                txa
                asl a
                asl a
                asl a
                ora arg0
                jmp back.back0          ; do the tab

_t2             iny
                cpy #8
                bmi _t1
_t3             ldy #0
                inx
                cpx #15
                bmi _t1
                rts

_onbit          .byte $80,$40,$20,$10,8,4,2,1,0
_offbit         .byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe,$ff
                .endproc


settab          .proc
                jsr tabloc
                lda tabmap,x
                ora tab._onbit,y
                sta tabmap,x
                rts
                .endproc


clrtab          .proc
                jsr tabloc
                lda tabmap,x
                and tab._offbit,y
                sta tabmap,x
                rts
                .endproc


tabloc         .proc
                jsr setsp
                sec
                sbc #1
_tabpos         tay
                lsr a
                lsr a
                lsr a
                tax
                tya
                and #7
                tay
                cpx #15
                bmi _tp1
                ldy #8
_tp1            rts
                .endproc
