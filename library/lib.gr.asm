;======================================
;   FILE: lib.gr.asm
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
; PROC Graphics(BYTE mode)
; same as BASIC
;======================================
graphics        .proc                   ; Graphics(mode)
                pha
                lda #0
                jsr clos

                lda #$0c
                sta arg3
                lda #0
                ldx #<_e
                ldy #>_e
                jsr open
                jsr chkerr

                lda #6
                jsr clos

                pla
                sta arg4
                and #$30
                eor #$1c
                sta arg3
                lda #6
                ldx #<_devs
                ldy #>_devs
                jsr open

                jmp chkerr

;--------------------------------------

_e              .text 2,"E:",eol
_devs           .text 2,"S:",eol
_color          = $03_02FD
_atachr         = $03_02FB
                .endproc


;======================================
; PROC DrawTo(CARD col, BYTE row)
; same as BASIC
;======================================
drawto          .proc
                jsr _grio               ; DrawTo(col, row)

                ldy #$11
                jmp xio

_grio           jsr position.pos1

                lda graphics._color
                sta graphics._atachr
                lda #<graphics._devs
                sta arg5
                lda #>graphics._devs
                sta arg6
                lda #0
                sta arg3
                sta arg4
                lda #6
                rts
                .endproc


;======================================
; PROC Position(CARD col, BYTE row)
; same as BASIC
;======================================
position        .proc
                sta OLDCOL              ; Position(col, row)
                stx OLDCOL+1
                sty OLDROW
pos1            sta COLCRS
                stx COLCRS+1
                sty ROWCRS
                rts
                .endproc


;======================================
; BYTE FUNC Locate(CARD col, BYTE row)
; same as BASIC
;======================================
locate          .proc
                jsr position            ; Locate(col, row)

                lda #6
                jmp getd

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
; same as BASIC
;======================================
plot            .proc
                jsr position.pos1       ; Plot(col, row)

                lda #6
                ldx graphics._color
                jmp putd

                .endproc


;======================================
; PROC SetColor(BYTE reg, hue, lum)
; same as BASIC
;======================================
setcolor        .proc
                cmp #5                  ; SetColor(reg, hue, lum)
                bpl _sc1

                sta arg0
                tya
                and #$0f
                sta arg2
                txa
                asl a
                asl a
                asl a
                asl a
                ora arg2
                ldx arg0
                sta $03_02C4,x ;!! COLOR0,x
                sta $03_D016,x ;!! COLPF0,x
_sc1            rts
                .endproc


;======================================
; PROC Fill(CARD col, BYTE row)
; same as:
;   POSITION col, row
;   POKE 765, color
;   XIO 18,#6,0,0,"S:"
; in BASIC
;======================================
fill            .proc
                jsr drawto._grio

                ldy #$12
                jmp xio

                .endproc
