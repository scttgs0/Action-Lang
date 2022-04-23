;======================================
;   FILE: lib.key.asm
;======================================

; Copyright 1983 by Action Computer Services
; All rights reserved
;
; last modified September 28, 1983
;
srtimr          = $022b
invflg          = $02b6
shflok          = $02be

;
lgetkey         .proc
    ; Get next key in buffer
                clc                     ; blink cursor
                lda rtclok+2
                adc #14
                tax
_bc1            lda ch                  ; key down?
                eor #$ff
                bne _gk0
                cpx rtclok+2
                bpl _bc1
                ldy #0
                lda (oldadr),y
                eor #$80
                sta (oldadr),y
                jmp lgetkey
;
_gk0            ldy #0
                lda oldchr
                eor #$80
                sta (oldadr),y          ; restore cursor
                ldx srtimr              ; faster repeat
                cpx #$0c
                bcs _gk5
                cpx #4
                bcc _gk2
                ldx #3
_gk1            stx srtimr
_gk2            lda ch
                cmp #$c0
                bcc _gk3                ; not Ctrl-Shft
_cskey          jsr click
                bmi _gk4                ; uncond.
_gk3            and #$3f
                cmp #$3c                ; caps key
                beq _caps
                cmp #$27                ; Atari key
                beq _atari
_gkey           ldx #$70
                lda #7                  ; GETCHR
                sta brkkey              ; ignore BREAK key
                jsr putch.putch2
_gk4            ldx srtimr
                cpx #10
                bcs _gkret
                ldx #3
                stx srtimr
_gkret          sta curch
                rts
_gk5            ldx #20
                bne _gk1
_caps           lda ch
                and #$c0
                sta shflok
_caps1          jsr click
                bmi lgetkey
_atari          lda invflg
                eor #$80
                sta invflg
                jmp _caps1
                .endproc

;
click           .proc
; Click() click the keyboard
                ldx #$7f
_click1         stx consol
                stx wsync
                dex
                bpl _click1
                stx ch
                rts
                .endproc

