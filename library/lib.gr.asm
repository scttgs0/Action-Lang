
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.gr.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
; PROC Graphics(BYTE mode)
;--------------------------------------
; same as BASIC
;======================================
libGrGraphics   .proc
                pha

                lda #0
                jsr libIOClose

                lda #$0C
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
                eor #$1C
                sta arg3

                lda #6
                ldx #<_devs
                ldy #>_devs

                jsr Open

                jmp libIOChkErr

;--------------------------------------

_e              .ptext "E:"
                .byte EOL
_devs           .ptext "S:"
                .byte EOL

_color          = $02FD
_atachr         = $02FB

                .endproc


;======================================
; PROC DrawTo(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
libGrDrawTo     .proc
                jsr graphicIO           ; DrawTo(col, row)

                ldy #$11
                jmp libIOXIO

                .endproc


;==================
;
;==================
graphicIO       .proc
                jsr libGrPosition.pos1

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
;--------------------------------------
; same as BASIC
;======================================
libGrPosition   .proc
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
;--------------------------------------
; same as BASIC
;======================================
libGrLocate     .proc
                jsr libGrPosition       ; Locate(col, row)

                lda #6
                jmp libIOGetD

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
;--------------------------------------
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
;--------------------------------------
; same as BASIC
;======================================
libGrSetColor   .proc
                cmp #5                  ; SetColor(reg, hue, lum)
                bpl _XIT

                sta arg0

                tya
                and #$0F
                sta arg2

                txa
                asl
                asl
                asl
                asl
                ora arg2

                ldx arg0
                ;!!sta COLOR0,X
                ;!!sta COLPF0,X

_XIT            rts
                .endproc


;======================================
; PROC Fill(CARD col, BYTE row)
;--------------------------------------
; same as:
;   POSITION col, row
;   POKE 765, color
;   XIO 18,#6,0,0,"S:"
; in BASIC
;======================================
libGrFill       .proc
                jsr graphicIO

                ldy #$12
                jmp libIOXIO

                .endproc
