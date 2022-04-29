
;======================================
; Clear the visible screen
;======================================
ClearScreen     .proc
                ldx #$00
                ldy #$12

_clearNext      lda #$00
_setAddr1       sta CS_TEXT_MEM_PTR,x
                lda #$46
_setAddr2       sta CS_COLOR_MEM_PTR,x
                inx
                bne _clearNext

                inc _setAddr1+2
                inc _setAddr2+2
                dey
                bne _clearNext

                rts
                .endproc


;======================================
; Initialize the CHAR_LUT tables
;======================================
InitCharLUT     .proc
                ldx #$00
_next1          lda Custom_LUT,x
                sta FG_CHAR_LUT_PTR,X
                sta BG_CHAR_LUT_PTR,X
                inx
                cpx #64
                bne _next1

                rts

;--------------------------------------

Custom_LUT      .dword $00143382        ; 0: Saint Patrick Blue
                .dword $006B89D7        ; 1: Blue Gray
                .dword $0076ADEB        ; 2: Maya Blue
                .dword $003474CB        ; 3: Han Blue
                .dword $00610672        ; 4: Indigo
                .dword $00B146C2        ; 5: Deep Fuchsia
                .dword $00282828        ; 6: Dark Jungle Green
                .dword $00383838        ; 7: Dark Lava
                .dword $00909090        ; 8: Taupe Gray
                .dword $0069C372        ; 9: Mantis Green
                .dword $00BC3936        ; A: Medium Carmine
                .dword $0073AA24        ; B: Green
                .dword $00FFFF00        ; C: Bright Yellow
                .dword $000000FF        ; D: Bright Blue
                .dword $00FC7F00        ; E: Bright Orange
                .dword $00AAAAAA        ; F: Bright White
                .endproc


;======================================
; Restore the System CHAR_LUT tables
;======================================
RestoreCharLUT  .proc
                ldx #$00
_next1          lda Default_LUT,x
                sta FG_CHAR_LUT_PTR,X
                sta BG_CHAR_LUT_PTR,X
                inx
                cpx #64
                bne _next1

                rts

;--------------------------------------

Default_LUT     .dword $00000000        ; 0: Black
                .dword $00800000        ; 1: Red
                .dword $00008000        ; 2: Green
                .dword $00808000        ; 3: Yellow
                .dword $00000080        ; 4: Blue
                .dword $00800080        ; 5: Magenta
                .dword $00008080        ; 6: Cyan
                .dword $00C0C0C0        ; 7: White
                .dword $00808080        ; 8: Bright Black (grey)
                .dword $00FF0000        ; 9: Bright Red
                .dword $0000FF00        ; A: Bright Green
                .dword $00FFFF00        ; B: Bright Yellow
                .dword $000000FF        ; C: Bright Blue
                .dword $00FC7F00        ; D: Bright Orange
                .dword $0000FFFF        ; E: Bright Cyan
                .dword $00FFFFFF        ; F: Bright White
                .endproc

;--------------------------------------