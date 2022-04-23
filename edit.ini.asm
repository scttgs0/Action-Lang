;======================================
;   FILE: edit.ini.asm
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


minit           .proc                   ; initialize memory
                lda memlo
                sta afbase
                lda memlo+1
                sta afbase+1
                lda #0
                tay
                sta (afbase),y
                iny
                sta (afbase),y
                sec
                lda memtop
                sbc afbase
                iny
                sta (afbase),y
                lda memtop+1
                sbc afbase+1
                iny
                sta (afbase),y

                lda #0                  ; allocate 2 pages of
                ldx #2                  ; spare memory
                jsr allocate
                lda afcur
                sta sparem
                ldx afcur+1
                stx sparem+1
miret           rts
                .endproc


zerow           .proc                   ; initialize window
                lda #0
                ldx #15
_zw1            dex                     ; zero page0 window table
                sta sp,x
                bne _zw1
                sta dirtyf
                sta inbuf
                tay
                sta (buf),y
                rts
                .endproc


w2init          .proc                   ; W2Init()
                jsr ctrln
                lda wsize
                sta nlines
                sta cmdln
                jsr savworld
                lda #w2-w1
                sta numwd
                sta curwdw
                jsr zerow
                ldy wsize
                iny
                sty ytop
                sec
                lda #23
                sbc wsize
                sta nlines
                bne einit.fcmsg         ; uncond.
                .endproc


einit           .proc
                jsr minit

                lda #0
                ldx #1
                jsr allocate            ; get edit buffer
                lda afcur
                sta buf
                ldx afcur+1
                stx buf+1

                lda #$40
                sta chcvt

                lda #<delbuf
                sta delbuf
                sta delbuf+4
                lda #>delbuf
                sta delbuf+1
                sta delbuf+5

winit           jsr zerow

winit1          lda #23
                sta nlines
                sta cmdln
                lda #0
                sta curwdw
                sta ytop

fcmsg           jsr ctrln
fcmsg1          lda #<editc
                ldx #>editc
                jmp cmdmsg

editc           .text 19,"ACTION! (c)1983 ACS"
                .endproc
