;======================================
;       FILE: ampl.arr.asm
;======================================

; Copyright 1983 by Action Computer Services
; All Rights reserved

; last modified June 7, 1983


;    ArrRef()
;    --------
arrref          .proc
                ldx nxttoken
                cpx #lparen
                beq arrconst._arr0
                cpx #uparrow
                beq arrconst._arr0
arrvar          ldy #vart+cardt         ; no index!
                sty token
                cmp #arrayt+8
                bcc arrconst._arr1
arrconst        jsr procref.stconst
_arr1           jmp pushst

_arptr          ldy #0
                lda (stack),y
                cmp #arrayt+8
                bcs _arpt1              ; small array
                iny
                jsr stkp
                cpx #0
                bne _arpt1
    ; page zero pointer
                ldy #1
                sta (stack),y
                dey
                lda (stack),y
                ora #$b0                ; temp array mode
                sta (stack),y
                rts

_arpt1          jsr zerost
                bne _ar0                ; uncond.

_arr0           jsr pushnext
                cmp #uparrow
                beq _arptr
                jsr getexp
                cmp #rparen
                bne arrerr
                ldx op
                bne arrerr
_ar0            ldy #7
                lda (stack),y
arra0           pha
                lda #vart+cardt
                sta (stack),y
                lda #plusid
                jsr genops
                pla
                cmp #arrayt+8
                bcs arrerr._arsmall
                and #7
                tax
                ora #$b0                ; temp array mode
                sta arg7
                ldy arg1
                cpy #constt+strt
                ldy #1                  ; clear Z flag if we branch
                bcs _ar1
                lda (stack),y
                iny
                ora (stack),y
_ar1            sta fr1
                beq _arint              ; pointer
                ldy vartype-1,x
                beq arrerr._arbyte

; CPY #3
; BEQ _ARReal
; integer or cardinal

_arint          jsr gettemps
                lda #$a1                ; LDA
                ldx fr1
                beq _ari1
                jsr load2l
                lda #$0a                ; ASL A
                ldx #$08                ; PHP
                ldy #$18                ; CLC
                jsr push3
                lda #$61                ; ADC
        .if ramzap
                sta (arg8),y
        .else
                nop
                nop
        .endif
_ari1           jsr loadx.op1l
                jsr stempl
                lda #$a1                ; LDA
                ldx fr1
                beq _ari2
                jsr load2h
                lda #$2a                ; ROL A
                ldx #$28                ; PLP, restore carry
                jsr push2
                lda #$61                ; ADC
_ari2           jsr op1h
                jmp cgadd.cgadd2

arrerr          ldy #arrer              ; bad array ref
                jmp splerr

_arbyte         jmp codegen.cg1

_arsmall        ldy #7
                sta (stack),y           ; restore correct type
                lda arg1
                bpl arrerr              ; can't index with bool.
                bit arrmode
                bne arrerr              ; can't index with array
                ldy #10
                sta (stack),y
                ldy #2
                jsr loadi
                ldy #11
                jsr savecd.savstk
                jmp popst
                .endproc
