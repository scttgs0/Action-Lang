
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.tab.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   Tab()
;======================================
Tab_            .proc
                jsr SetSpacing
                jsr CalcTableByteBit._ENTRY1    ; X=byte, Y=bit

_next1          lda TABMAP,X            ; ignore if no tabstops within this byte
                beq _2

                and _onbit,Y            ; is there a tabstop here?
                beq _1                  ;   no

;   found, calculate line offset
                sty arg0
                txa
                asl                     ; *8
                asl
                asl
                ora arg0                ; add bit offset

                jmp Back._ENTRY1        ; do the tab

_1              iny                     ; advance to next position within this byte
                cpy #8
                bmi _next1

_2              ldy #0                  ; advance to next byte, high-bit
                inx
                cpx #15                 ; reached EOL?
                bmi _next1              ;   no

                rts

;--------------------------------------

_onbit          .byte $80,$40,$20,$10
                .byte $08,$04,$02,$01
                .byte $00               ; end of line, no change (e.g. ignore)

_offbit         .byte $7F,$BF,$DF,$EF
                .byte $F7,$FB,$FD,$FE
                .byte $FF               ; end of line, no change (e.g. ignore)

                .endproc


;======================================
; set tabstop at the cursor position
;======================================
SetTab          .proc
                jsr CalcTableByteBit    ; X=byte, Y=bit

                lda TABMAP,X
                ora Tab_._onbit,Y       ; set tabstop
                sta TABMAP,X

                rts
                .endproc


;======================================
; clear tabstop at the cursor position
;======================================
ClearTab        .proc
                jsr CalcTableByteBit    ; X=byte, Y=bit

                lda TABMAP,X
                and Tab_._offbit,Y      ; clear tabstop
                sta TABMAP,X

                rts
                .endproc


;======================================
; calculate the byte and bit offset
; within the TABMAP table
;--------------------------------------
; on exit:
;   X           byte offset [0:14]
;               =15 when at EOL
;   Y           bit offset  [0:7]
;               =8 when at EOL
;======================================
CalcTableByteBit .proc
                jsr SetSpacing          ; A=sp (sp=indent+choff+COLCRS-LMARGN)

                sec
                sbc #1                  ; 0-indexed adjustment

_ENTRY1         tay                     ; preserve

;   calculate the byte offset
                lsr                     ; /8
                lsr
                lsr
                tax                     ; X=byte offset

;   calculate the bit offset
                tya                     ; restore
                and #7
                tay                     ; Y=bit offset

;   check bounds
                cpx #15
                bmi _XIT                ; good

                ldy #8                  ; end of line, special value to prevent change (e.g. ignore)

_XIT            rts
                .endproc
