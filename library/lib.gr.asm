
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.gr.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


gr              .namespace

;======================================
; PROC Graphics(BYTE mode)
;--------------------------------------
; same as BASIC
;======================================
Graphics        .proc
                pha

                lda #$00
                jsr lib.io.Close

                lda #$0C
                sta arg3

                lda #$00
                ldx #<_e
                ldy #>_e

                jsr ioOpen
                jsr lib.io.ChkErr

                lda #$06
                jsr lib.io.Close

                pla
                sta arg4

                and #$30
                eor #$1C
                sta arg3

                lda #$06
                ldx #<_devs
                ldy #>_devs

                jsr ioOpen

                jmp lib.io.ChkErr

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
DrawTo          .proc
                jsr GfxIO

                ldy #$11
                jmp lib.io.XIO

                .endproc


; = = = = = = = = = = = = = = = = = = =
;
; = = = = = = = = = = = = = = = = = = =
GfxIO           .proc
                jsr Position.pos1

                lda Graphics._color
                sta Graphics._atachr

                lda #<Graphics._devs
                sta arg5
                lda #>Graphics._devs
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
Position        .proc
                sta OLDCOL
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
Locate          .proc
                jsr Position

                lda #$06                ; channel #6
                jmp lib.io.GetD

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
Plot            .proc
                jsr Position.pos1

                lda #$06
                ldx Graphics._color

                jmp lib.io.PutD

                .endproc


;======================================
; PROC SetColor(BYTE reg, hue, lum)
;--------------------------------------
; same as BASIC
;======================================
SetColor        .proc
                cmp #$05
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
Fill            .proc
                jsr GfxIO

                ldy #$12
                jmp lib.io.XIO

                .endproc

                .endnamespace
