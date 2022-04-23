;======================================
;   FILE: lib.gr.asm
;======================================

; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified June 21, 1983


;
; PROC Graphics(BYTE mode)
; same as BASIC
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
;
_e              .text 2,"E:",eol
_devs           .text 2,"S:",eol
_color          = $02fd
_atachr         = $02fb
                .endproc


;
; PROC DrawTo(CARD col, BYTE row)
; same as BASIC
drawto          .proc
                jsr _grio               ; DrawTo(col, row)
                ldy #$11
                jmp xio
;
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


;
; PROC Position(CARD col, BYTE row)
; same as BASIC
position        .proc
                sta oldcol              ; Position(col, row)
                stx oldcol+1
                sty oldrow
pos1            sta colcrs
                stx colcrs+1
                sty rowcrs
                rts
                .endproc


;
; BYTE FUNC Locate(CARD col, BYTE row)
; same as BASIC
locate          .proc
                jsr position            ; Locate(col, row)
                lda #6
                jmp getd
                .endproc


;
; PROC Plot(CARD col, BYTE row)
; same as BASIC
plot            .proc
                jsr position.pos1       ; Plot(col, row)
                lda #6
                ldx graphics._color
                jmp putd
                .endproc


;
; PROC SetColor(BYTE reg, hue, lum)
; same as BASIC
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
                sta $02c4,x
                sta $d016,x
_sc1            rts
                .endproc


;
; PROC Fill(CARD col, BYTE row)
; same as:
;   POSITION col, row
;   POKE 765, color
;   XIO 18,#6,0,0,"S:"
; in BASIC
fill            .proc
                jsr drawto._grio
                ldy #$12
                jmp xio
                .endproc
