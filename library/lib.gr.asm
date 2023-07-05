
; SPDX-FileName: lib.gr.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
; PROC Graphics(BYTE mode)
; same as BASIC
;======================================
libGrGraphics   .proc                   ; Graphics(mode)
                pha
                lda #0
                jsr libIOClose

                lda #$0c
                sta arg3
                lda #0
                ldx #<_e
                ldy #>_e
                jsr Open
                jsr libIOChkErr

                lda #6
                jsr libIOClose

                pla
                sta arg4
                and #$30
                eor #$1c
                sta arg3
                lda #6
                ldx #<_devs
                ldy #>_devs
                jsr Open

                jmp libIOChkErr

;--------------------------------------

_e              .text 2,"E:",eol
_devs           .text 2,"S:",eol
_color          = $02FD
_atachr         = $02FB
                .endproc


;======================================
; PROC DrawTo(CARD col, BYTE row)
; same as BASIC
;======================================
libGrDrawTo     .proc
                jsr _grio               ; DrawTo(col, row)

                ldy #$11
                jmp libIOXIO

_grio           jsr libGrPosition.pos1

                lda libGrGraphics._color
                sta libGrGraphics._atachr
                lda #<libGrGraphics._devs
                sta arg5
                lda #>libGrGraphics._devs
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
libGrPosition   .proc
pos1            sta COLCRS
                stx COLCRS+1
                sty ROWCRS
                rts
                .endproc


;======================================
; BYTE FUNC Locate(CARD col, BYTE row)
; same as BASIC
;======================================
libGrLocate     .proc
                jsr libGrPosition       ; Locate(col, row)

                lda #6
                jmp libIOGetD

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
; same as BASIC
;======================================
libGrPlot       .proc
                jsr libGrPosition.pos1  ; Plot(col, row)

                lda #6
                ldx libGrGraphics._color
                jmp libIOPutD

                .endproc


;======================================
; PROC SetColor(BYTE reg, hue, lum)
; same as BASIC
;======================================
libGrSetColor   .proc
                cmp #5                  ; SetColor(reg, hue, lum)
                bpl _sc1

                sta arg0
                tya
                and #$0f
                sta arg2
                txa
                asl
                asl
                asl
                asl
                ora arg2
                ldx arg0
                ;!!sta COLOR0,x
                ;!!sta COLPF0,x
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
libGrFill       .proc
                jsr libGrDrawTo._grio

                ldy #$12
                jmp libIOXIO

                .endproc
