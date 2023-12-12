
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.gr.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
; PROC Graphics(BYTE mode)
;--------------------------------------
; same as BASIC
;======================================
graphics        .proc
                pha

                lda #0
                jsr clos

                lda #$0C
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
                eor #$1C
                sta arg3

                lda #6
                ldx #<_devs
                ldy #>_devs

                jsr open

                jmp chkerr

;--------------------------------------

_e              .text 2,"E:",eol
_devs           .text 2,"S:",eol
_color          = $02FD
_atachr         = $02FB

                .endproc


;======================================
; PROC DrawTo(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
drawto          .proc
                jsr graphicIO           ; DrawTo(col, row)

                ldy #$11
                jmp xio

                .endproc


;======================================
;
;======================================
graphicIO       .proc
                jsr position.pos1

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
;--------------------------------------
; same as BASIC
;======================================
position        .proc
                sta oldcol              ; Position(col, row)
                stx oldcol+1
                sty oldrow

pos1            sta colcrs
                stx colcrs+1
                sty rowcrs

                rts
                .endproc


;======================================
; BYTE FUNC Locate(CARD col, BYTE row)
;--------------------------------------
; same as BASIC
;======================================
locate          .proc
                jsr position            ; Locate(col, row)

                lda #6

                jmp getd

                .endproc


;======================================
; PROC Plot(CARD col, BYTE row)
;--------------------------------------
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
;--------------------------------------
; same as BASIC
;======================================
setcolor        .proc
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
fill            .proc
                jsr graphicIO

                ldy #$12
                jmp xio

                .endproc
