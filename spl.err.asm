;======================================
;   FILE: spl.err.asm
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
;   SPLErr(,,error)
;======================================
lsplerr         .proc
                lda top+1
                beq spler1

    ; set pointer to error
                ldx curwdw
                lda spln
                sta w1+wsp,x
                lda curln
                sta w1+wcur,x
                lda curln+1
                sta w1+wcur+1,x
spler1          jsr syserr
                jsr puteol
                jsr printbuf

                lda #0
                ldx #<sermsg
                ldy #>sermsg
                jsr output

                lda #0
                sta $02E3 ;!! INITAD+1
                ldx #<numbuf
                ldy #>numbuf
                jsr print

                jmp emloop

                .endproc
