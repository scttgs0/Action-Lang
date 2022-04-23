;======================================
;   FILE: spl.err.asm
;======================================

; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified July 30, 1983
;

;    SPLErr(,,error)
;    ---------------
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
                sta $02e3
                ldx #<numbuf
                ldy #>numbuf
                jsr print
                jmp emloop
                .endproc
