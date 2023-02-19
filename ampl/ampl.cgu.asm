
;======================================
;   FILE: ampl.cgu.asm
;======================================

; Action! Programming Language
; Copyright 1983 by Clinton W Parker

;
; Action! is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Action! is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Action!.  If not, see <http://www.gnu.org/licenses/>.
;


;======================================
;   LoadY() value in arg12
;======================================
LoadY           .proc
                lda cury
                cmp arg12
                beq _lyret
                jsr Push0

                cmp #1
                bne _ly2

                lda #$88                ; DEY
_ly1            jsr Insrt1
                jmp _ly4

_ly2            cmp #0
                bne _ly3

                lda #$c8                ; INY
                bne _ly1

_ly3            lda #$a0
                ldx arg12
                jsr Insrt2              ; LDY #0 or #1

_ly4            lda arg12
                sta cury
                ldy arg13
_lyret          rts
                .endproc


;======================================
;   TrashY()
;======================================
TrashY          .proc
                lda #$ff
                sta cury
                rts
                .endproc


;======================================
;   LoadX(,,offset)
;======================================
LoadX           .proc
    ; NOTE:  this proc can only be called
    ; from Op below, see _LXC
                lda (stack),y
                iny
                bit tempmode
                bne _lxt

                bit cnstmode
                beq _lxc

    ; var to load
                jsr StkProp

                beq _lxz

                lda #$ae                ; LDX addr16
                jmp Push3

_lxt            lda (stack),y
                tax
                dec temps-args,x
_lxz            lda #$a6                ; LDX addr
_lx1            jmp Push2

_lxc            lda (stack),y

                ; tax
                ; lda #$A2
                ; ldx data
                ; bne _LX1

                sta arg12
                pla
                pla
                pla
                tay
                bra ophigh._opv

_optype         and #$20
                beq ophigh._operr       ; con. exp.

                jsr StkAddr

                lda arg12
                beq ophigh._opv1

                inx
                bne ophigh._opv1

                iny
                jmp ophigh._opv1


;======================================
;   Load1L
;======================================
Load1L          lda #$a1                ; LDA op


;======================================
;   Op1L(op)
;======================================
Op1L            pha
                lda arg2
                ldy #8
oplow           ldx #0
ophigh          stx arg12

; NOTE:  the order of following
; comparisons is important!

                tax
                bpl LoadX._optype

                bit procmode
                bne _opp

                bit arrmode
                bne _opa                ; array

                bit tempmode
                bne _opt                ; temp

                bit cnstmode
                beq _opc                ; constant

    ; var if we get here
_opv            jsr StkProp

                beq _opz                ; page zero var

    ; 16 bit address
_opv1           pla
                ora #$0c                ; addr16
                jmp Push3

_opp            inc arg12               ; skip JMP byte
                and #8
                beq _opv

_operr          jmp chkcond.conderr     ; cond. exp.

_opa            bit cnstmode
                bne _opa2

                jsr LoadY

                ; lda arg7
                ; and #$F7
                ; sta arg7 ; flag Y reg used

                lda #0
                sta arg12
                lda #$10                ; (addr),y
_opa1           sta arg10
                lda (stack),y
                clc
                adc arg12
                cmp #args
                bcc _opc2

                cmp #args+16
                bcs _opc2

                tax
                dec temps-args,x        ; free temp
                bra _opc3

_opa2           tya                     ; small array
                pha
                iny
                iny
                jsr LoadX

                pla
                tay
                jsr StkProp
                beq _opa2a              ; page zero

                pla
                ora #$1c                ; addr16,x
                jmp Push3

_opa2a          pla
                ora #$14                ; addr,x
                jmp Push2

_opc            lda #$08                ; data
                sta arg10
                lda arg12
                beq _opc1

                iny
_opc1           lda (stack),y
_opc2           tax
_opc3           pla
                ora arg10               ; op mode
                jmp Push2

_opt            lda #$04                ; addr
                bra _opa1

_opz            pla
                ora #$04                ; addr
                jmp Push2

                .endproc


;======================================
;   Load2L()
;======================================
Load2L          .proc
                lda #$a1                ; LDA op
                .endproc


;======================================
;   Op2L(op)
;======================================
Op2L            .proc
                pha
                lda arg1
                ldy #1
                jmp LoadX.oplow

                .endproc


;======================================
;   Load1H()
;======================================
Load1H          .proc
                lda #$a1                ; LDA op
                .endproc


;======================================
;   Op1H(op)
;======================================
Op1H            .proc
                ldx arg4
                beq Op2H.ophz

                pha
                lda arg2
                ldy #8
op1h1           ldx #1
                jmp LoadX.ophigh

                .endproc


;======================================
;   Load2H()
;======================================
Load2H          .proc
                lda #$a1                ; LDA op
                .endproc


;======================================
;   Op2H(op)
;======================================
Op2H            .proc
                ldx arg3
                beq ophz

                pha
                lda arg1
                ldy #1
                bne Op1H.op1h1

ophz            ora #$08
                jmp Push2

                .endproc

;--------------------------------------
;--------------------------------------

arrmode         .byte $10
cnstmode        .byte $08
tempmode        .byte $20
procmode        .byte $40

; see CG
outtype         .byte $82,3,$84,tokREAL_t
                .byte 3,3,$84,tokREAL_t
                .byte $84,$84,$84,tokREAL_t
                .byte tokREAL_t,tokREAL_t,tokREAL_t,tokREAL_t


;======================================
;   GetTemps()
;======================================
GetTemps        .proc
                ldx #args+16
                ldy #7
_gtl1           dex
                dex
                dey
                bmi _gtlerr             ; exp. too complex

                lda temps-args,x
                bne _gtl1

                inc temps-args,x
                lda arg5                ; see if byte temp
                beq _gt2                ; yes

                inc temps-args+1,x
        .if ramzap
                inc SArgs,x
        .else
                nop
                nop
                nop
        .endif
_gt2            stx arg9
                rts

_gtlerr         jmp experr

                .endproc


;======================================
;   LoadI(,,offset)
;======================================
LoadI           .proc
                lda (stack),y
                sta arg15
                tax
                dey
                lda (stack),y
                sta arg14
                rts
                .endproc


;======================================
;   LdCdZ(,,stkoff)
;======================================
LdCdZ           .proc
                lda #0
                .endproc


;======================================
;   LoadCd(cdoff,,stkoff)
;======================================
LoadCd          .proc
                clc
                adc (stack),y
                sta qcode
                iny
                lda #0
                adc (stack),y
                sta qcode+1
                rts
                .endproc


;======================================
;   SaveCd(,,offset)
;======================================
SaveCd          .proc
                lda qcode
                ldx qcode+1
savstk          sta (stack),y
                txa
                iny
                sta (stack),y
                rts
                .endproc


;======================================
;   RelOp()
;======================================
RelOp           .proc
                lda arg6
                bpl _ro1

                inc arg8
_ro1            rts
                .endproc


;======================================
;   ChkZero()
;======================================
ChkZero         .proc
                lda arg3
                bne _cz1

                lda arg1
                bpl _cz1                ; not const

                cmp #tokVAR_t
                bcs _cz1

                ldy #1
                lda (stack),y
_cz1            rts
                .endproc


;======================================
;   OpCd1()
;======================================
OpCd1           .proc
                ldx arg8
                lda cgopscd+1,x
                rts
                .endproc


;======================================
;   StkAddr(,,offset)
;======================================
StkAddr         .proc
                lda (stack),y
                tax
                iny
                lda (stack),y
                tay
                rts
                .endproc


;======================================
;   StkP(,,offset)
;======================================
StkP            .proc
                jsr StkAddr

                lda #1
                jmp gprop

                .endproc


;======================================
;   StkPZ(,,offset)
;======================================
StkPZ           .proc
                lda #0
                .endproc


;======================================
;    StkPS(,,offset)
;======================================
StkPS           .proc
                sta arg12
                .endproc


;======================================
;    StkProp(,,offset)
;======================================
StkProp         .proc
                jsr StkP

                clc
                adc arg12
                tax
                iny
                lda (props),y
                adc #0
                tay
                rts
                .endproc


;======================================
;   JSRTable(,index)
;======================================
JSRTable        .proc
;.IF RAMzap
;    ldy LTab+1,x
;    lda LTab,x
;.ELSE

                ldy jt_lsh+1,x
                lda jt_lsh,x
;.ENDIF
                tax
                lda #$20                ; JSR
                jmp Push3

                .endproc


;======================================
;   Push0()
;======================================
Push0           .proc
                sty arg13
                ldy qcode
                sty arg14
                ldy qcode+1
                sty arg15
                ldy #0
                rts
                .endproc


;======================================
;   PushTrue(op)
;======================================
PushTrue        .proc
                jsr Push1

                ldy #10
                jsr SaveCd

                lda #0                  ; no other true branches
                sta arg9
                .endproc

                ;[fall-through]


;======================================
;   Push1(op)
;======================================
Push1           .proc
                jsr Push0

                sta (arg14),y
                beq Insrt1.i11

                .endproc


;======================================
;   Insrt1(op)
;======================================
Insrt1          .proc
                ldy #1
                jsr AddCdSp

i11             iny
                tya
                jmp codeincr

                .endproc


;======================================
;   STempH()
;======================================
STempH          .proc
                inc arg9
                ldy #12
                bra STempL.stemp

                .endproc


;======================================
;   STempL()
;======================================
STempL          .proc
                ldy #10
stemp           jsr SaveCd

                lda arg9
                tax
                and #$fe                ; set to low address
                sta arg9
                lda #$85                ; STA addr
                .endproc

                ;[fall-through]


;======================================
;   Push2(op,,data)
;======================================
Push2           .proc
                jsr Push0

                sta (arg14),y
                beq Insrt2.i21

                .endproc


;======================================
;   Insrt2(op,data)
;======================================
Insrt2          .proc            
                ldy #2
                jsr AddCdSp

i21             txa
                iny
                sta (arg14),y
                bne Insrt1.i11

                .endproc


;======================================
;   Push3(op,data16)
;======================================
Push3           .proc            
                jsr Push0

                sta (arg14),y
                beq Insrt3.i31

                .endproc


;======================================
;   Insrt3(op,data16)
;======================================
Insrt3          .proc            
                sty arg13
                ldy #3
i30             jsr AddCdSp

i31             txa
                ldx arg13
                iny
                sta (arg14),y
                bne Insrt2.i21

                .endproc


;======================================
;   AddCdSp()
;--------------------------------------
; AddCdSp(,,size) add qcode space
; does NOT change qcode or codeOff
;======================================
AddCdSp         .proc
                pha
                clc
                tya
                adc arg14
                sta arg10
                lda #0
                adc arg15
                sta arg11
                sec
                lda qcode
                sbc arg14

                tay
                beq _acsret

_acs1           lda (arg14),y
                sta (arg10),y
                dey
                bne _acs1

                lda (arg14),y
                sta (arg10),y
_acsret         pla
                sta (arg14),y
                rts
                .endproc
