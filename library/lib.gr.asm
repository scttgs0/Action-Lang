
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
libgrGraphics   .proc
                pha

                lda #$00
                jsr libioClose

                lda #$0C
                sta arg3

                lda #$00
                ldx #<_e
                ldy #>_e

                jsr ioOpen
                jsr libioChkErr

                lda #$06
                jsr libioClose

                pla
                sta arg4

                and #$30
                eor #$1C
                sta arg3

                lda #$06
                ldx #<_devs
                ldy #>_devs

                jsr ioOpen

                jmp libioChkErr

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
libgrDrawTo     .proc
                jsr graphicIO           ; DrawTo(col, row)

                ldy #$11
                jmp libioXIO

                .endproc


; = = = = = = = = = = = = = = = = = = =
;
; = = = = = = = = = = = = = = = = = = =
graphicIO       .proc
                jsr libgrPosition.pos1

                lda libgrGraphics._color
                sta libgrGraphics._atachr

                lda #<libgrGraphics._devs
                sta arg5
                lda #>libgrGraphics._devs
                sta arg6

                lda #$00
                sta arg3
                sta arg4

                lda #$06

                rts
                .endproc


;======================================
; PROC Position(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
libgrPosition   .proc
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
libgrLocate     .proc
                jsr libgrPosition       ; Locate(col, row)

                lda #$06
                jmp libioGetD

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
libgrPlot       .proc
                jsr libgrPosition.pos1  ; Plot(col, row)

                lda #$06
                ldx libgrGraphics._color

                jmp libioPutD

                .endproc


;======================================
; PROC SetColor(BYTE reg, hue, lum)
;--------------------------------------
; same as BASIC
;======================================
libgrSetColor   .proc
                cmp #$05                ; SetColor(reg, hue, lum)
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
                sta COLOR0,X
                sta COLPF0,X

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
libgrFill       .proc
                jsr graphicIO

                ldy #$12
                jmp libioXIO

                .endproc
