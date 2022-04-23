
;         AMPL.SEG

; Copyright 1983 by Clinton W Parker
; All Rights Reserved
; last modified October 15, 1983
;
; This file is part of Action!.
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

; low segment list> _:= low segment list> low segment> | low segment>
; low segment> _:= low segment type> low heading> (<dcl list>) (<stmt list>)
; low segment type> _:= PROC | low type> FUNC



segment         .proc
                cmp #proc
                beq _proc
                ldx nxttoken
                cpx #func
                beq _func
                rts                     ; end of segment list

_proc           lda #funct-vart+char-1
                sta type
                bne _func1              ; uncond.

_func           clc
                adc #funct-vart
                sta type
                jsr getnext

_func1          jsr makeentry
                jsr segend
                lda addr
                sta curproc
                lda addr+1
                sta curproc+1
                sta qglobal

                lda #1
                jsr stincr              ; space for num args

                ldy #3
                lda #0                  ; no args yet
                sta (props),y
                sta argbytes

                tay
_funcst         sta (stlocal),y
                iny                     ; zap local st
                bne _funcst

                lda symtab
                sta gbase
                lda symtab+1
                sta gbase+1

; space for arg list (8 bytes) and
; room for name of next proc/func
; up to 20 letters (24 bytes)
; unused space will be reclaimed
; see Params
                lda #32
                jsr stincr              ; arg list space

                jsr trashy

                lda nxttoken
                eor #equalid
                sta param               ; this is very tricky!!
                bne _funchd
                jsr ideq                ; param must = 0 here
                iny
                jsr storprops
                ldy #0
                lda (props),y
                ora #8
                sta (props),y           ; set Sys flag
                sta param
                jsr getnext

_funchd         jsr getnext
                cmp #lparen
                bne argerr


; low heading> _:= low id> (= low constant>) ( (<arg dcl list>) )
; low arg dcl list> _:= low arg dcl list> , low dcl list> | low dcl list>


                jsr getnext
                cmp #rparen
                beq argerr._func2

_heading        jsr declare
                ldx lsttoken
                inc lsttoken            ; in case 2 ,'s
                cpx #comma
                beq _heading

                cmp #rparen
                beq argerr._func2

argerr          ldy #arger
                jmp splerr

_func2          lda param
                pha
                lda #0
                sta param

                jsr getnext
                jsr declare             ; locals

    ; handle procedure setup here
                pla
                bmi _f4                 ; system proc

    ; get beginning of arguments and
    ; save actual procedure address
                lda #1
                jsr cprop
                sta arg0
                stx arg1
                jsr getcdoff
                jsr storprops

    ; get space for proc variable
                lda #$4c                ; JMP
                jsr push1
                jsr getcdoff            ; fill in address
                adc #2
                bcc _fh2
                inx
_fh2            jsr push2

    ; qcode to transfer arguments to
    ; local frame
_fh3            lda argbytes
                beq _func3              ; no arguments
                cmp #3
                bcs _fh5
                cmp #2
                lda #$8d                ; STA addr16
                ldx arg0
                ldy arg1
                bcc _fh4
                lda #$8e                ; STX addr16
                inx
                bne _fh4
                iny
_fh4            jsr push3
                dec argbytes
                jmp _fh3

_f4             jmp _func4

_fh5            ldx #10
                jsr jsrtable
                lda arg0
                ldx arg1
                ldy argbytes
                dey
                jsr push3

_func3          lda trace               ; check for trace
                beq _func4              ; no trace
                lda #$20                ; JSR CTrace
                ldx #<ctrace
                ldy #>ctrace
                jsr push3

                ldy #0
                lda (curproc),y
                tay
                tax
_f3a            lda (curproc),y
                sta (qcode),y
                dey
                bpl _f3a
                inx
                txa
                jsr codeincr

                lda arg0
                ldx arg1
                jsr push2

                lda #3
                jsr cprop
                tay
                tax
_f3b            lda (props),y
                sta (qcode),y
                dey
                bpl _f3b
                inx
                txa
                jsr codeincr

_func4          jsr stmtlist
                jmp segment
	            .endproc


;         AMPL.PF

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified November 3, 1983


                 ; PF()

ld1             ldy #0
                lda (stack),y
                cmp #arrayt
                bcs pf._c5a
                inc abt-args,x
                ldy #7
                lda (stack),y
                cmp #tempt+bytet
                beq pf._c5b
                dec abt+1-args,x
                cpx #args+2
                bcc pf._c5b
                jsr genops.gops
                jsr load2h
                lda #$81                ; STA
                jsr op1h
                jmp pf._c5b


pf              lda #0                  ; load arg types flag
                jsr getargs
                jsr pushst
                jsr getnext
                ldx #args
                stx argbytes
                ldx nxttoken
                cpx #rparen
                bne _c4

                jsr getnext
                bne _c7                 ; uncond.

_c4             ldx numargs
                ldy #tempt+bytet
                lda argtypes-1,x
                ldx argbytes
                stx abt+3
                cmp #$7f
                bcs _c5                 ; one byte arg
                sta temps-args+1,x
                inc argbytes
                iny
_c5             sta temps-args,x
                inc argbytes
                txa
                jsr storst
                jsr getexp
                dec numargs
                bmi callerr
                ldx abt+3
                cpx #args+3
                bcc ld1
_c5a            jsr cgassign
_c5b            lda token
                cmp #comma
                beq _c4

                cmp #rparen
                bne callerr
_c6             lda argbytes
                cmp #args+3
                bcs _c8
                cmp #args+2
                bcs _c9
                cmp #args+1
                bcs _c10
_c7             jsr trashy
                ldy #1
                jsr stkaddr
                lda #$20                ; JSR
                jmp push3

_c8             ldx #args+2
                jsr callerr._push
_c9             ldx #args+1
                jsr callerr._push
_c10            ldx #args
                jsr callerr._push
                jmp _c7

callerr         jmp segment.argerr


_push           lda abt-args,x
                bne _p1
                lda _ops-args,x
                ora #$04
                jmp push2

_p1             stx arg0
                jsr genops.gops
                ldx arg0
                lda _ops-args,x

; all of this for LDX # and LDY #
; can't use OpXX for these instr.

                cpx #args
                beq _p4                 ; LDA instr.
                ldy arg1
                bpl _p3a                ; record element
                cpy #vart
                ldy abt-args,x
                bcs _p3                 ; not const.
                pha
                sty arg0
                ldy #2
                jsr loadi
                ldy arg0
                bmi _p2
                tax
                pla
                jsr push2               ; low byte of const
                jmp cgassign.cga1

_p2             pla
_p2a            jmp push2               ; high byte

_p3a            ldy abt-args,x
_p3             bpl _p4
                ldx arg3
                beq _p2a
                jmp op2h

_p4             jsr op2l
                jmp cgassign.cga1

_ops            .byte $a1,$a2,$a0       ; LDA, LDX, LDY


;         AMPL.ARR

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


;         AMPL.CGU

; Copyright 1982 by Action Computer Services
; All Rights reserved

; last modified October 15, 1983


loady           .proc                   ; LoadY() value in arg12
                lda cury
                cmp arg12
                beq _lyret
                jsr push0
                cmp #1
                bne _ly2
                lda #$88                ; DEY
_ly1            jsr insrt1
                jmp _ly4

_ly2            cmp #0
                bne _ly3
                lda #$c8                ; INY
                bne _ly1

_ly3            lda #$a0
                ldx arg12
                jsr insrt2              ; LDY #0 or #1

_ly4            lda arg12
                sta cury
                ldy arg13

_lyret          rts
                .endproc


;    TrashY()
;    --------
trashy          .proc
                lda #$ff
                sta cury
                rts
                .endproc


;    LoadX(,,offset)
;    ---------------
loadx           .proc
    ; NOTE:  this proc can only be called
    ; from Op below, see _LXC
                lda (stack),y
                iny
                bit tempmode
                bne _lxt
                bit cnstmode
                beq _lxc
    ; var to load
                jsr stkprop
                beq _lxz
                lda #$ae                ; LDX addr16
                jmp push3

_lxt            lda (stack),y
                tax
                dec temps-args,x
_lxz            lda #$a6                ; LDX addr
_lx1            jmp push2

_lxc            lda (stack),y

; TAX
; LDA #$A2 ; LDX data
; BNE _LX1

                sta arg12
                pla
                pla
                pla
                tay
                bne ophigh._opv         ; uncond.


_optype         and #$20
                beq ophigh._operr       ; con. exp.
                jsr stkaddr
                lda arg12
                beq ophigh._opv1
                inx
                bne ophigh._opv1
                iny
                jmp ophigh._opv1


;Load1L PROC ; Load1L()
load1l          lda #$a1                ; LDA op


;Op1L PROC ; Op1L(op)
op1l            pha
                lda arg2
                ldy #8
oplow           ldx #0
ophigh          stx arg12

; NOTE:  the order of following
; comparisons is important!

                tax
                bpl loadx._optype
                bit procmode
                bne _opp
                bit arrmode
                bne _opa                ; array
                bit tempmode
                bne _opt                ; temp
                bit cnstmode
                beq _opc                ; constant
    ; var if we get here
_opv            jsr stkprop
                beq _opz                ; page zero var
    ; 16 bit address
_opv1           pla
                ora #$0c                ; addr16
                jmp push3

_opp            inc arg12               ; skip JMP byte
                and #8
                beq _opv
_operr          jmp chkcond.conderr     ; cond. exp.

_opa            bit cnstmode
                bne _opa2
                jsr loady

; LDA arg7
; AND #$F7
; STA arg7 ; flag Y reg used

                lda #0
                sta arg12
                lda #$10                ; (addr),Y
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
                bcc _opc3               ; uncond.

_opa2           tya                     ; small array
                pha
                iny
                iny
                jsr loadx
                pla
                tay
                jsr stkprop
                beq _opa2a              ; page zero
                pla
                ora #$1c                ; addr16,X
                jmp push3

_opa2a          pla
                ora #$14                ; addr,X
                jmp push2

_opc            lda #$08                ; data
                sta arg10
                lda arg12
                beq _opc1
                iny
_opc1           lda (stack),y
_opc2           tax
_opc3           pla
                ora arg10               ; op mode
                jmp push2

_opt            lda #$04                ; addr
                bne _opa1               ; uncond.

_opz            pla
                ora #$04                ; addr
                jmp push2
                .endproc


;    Load2L()
;    --------
load2l          .proc
                lda #$a1                ; LDA op
                .endproc


;    Op2L(op)
;    --------
op2l            .proc
                pha
                lda arg1
                ldy #1
                jmp loadx.oplow
                .endproc


;    Load1H()
;    --------
load1h          .proc
                lda #$a1                ; LDA op
                .endproc


;    Op1H(op)
;    --------
op1h            .proc
                ldx arg4
                beq op2h.ophz
                pha
                lda arg2
                ldy #8
op1h1           ldx #1
                jmp loadx.ophigh
                .endproc


;    Load2H()
;    --------
load2h          .proc
                lda #$a1                ; LDA op
                .endproc


;    Op2H(op)
;    --------
op2h            .proc
                ldx arg3
                beq ophz
                pha
                lda arg1
                ldy #1
                bne op1h.op1h1

ophz            ora #$08
                jmp push2
                .endproc

arrmode         .byte $10
cnstmode        .byte $08
tempmode        .byte $20
procmode        .byte $40

; see CG
outtype         .byte $82,3,$84,realt
                .byte 3,3,$84,realt
                .byte $84,$84,$84,realt
                .byte realt,realt,realt,realt


;    GetTemps()
;    ----------
gettemps        .proc
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
                inc sargs,x
        .else
                nop
                nop
                nop
        .endif
_gt2            stx arg9
                rts

_gtlerr         jmp experr
                .endproc


;    LoadI(,,offset)
;    ---------------
loadi           .proc
                lda (stack),y
                sta arg15
                tax
                dey
                lda (stack),y
                sta arg14
                rts
                .endproc


;    LdCdZ(,,stkoff)
;    ---------------
ldcdz           .proc
                lda #0
                .endproc


;    LoadCd(cdoff,,stkoff)
;    ---------------------
loadcd          .proc
                clc
                adc (stack),y
                sta qcode
                iny
                lda #0
                adc (stack),y
                sta qcode+1
                rts
                .endproc


;    SaveCd(,,offset)
;    ----------------
savecd          .proc
                lda qcode
                ldx qcode+1
savstk          sta (stack),y
                txa
                iny
                sta (stack),y
                rts
                .endproc


;    RelOp()
;    -------
relop           .proc
                lda arg6
                bpl _ro1
                inc arg8
_ro1            rts
                .endproc


chkzero         .proc
                lda arg3
                bne _cz1
                lda arg1
                bpl _cz1                ; not const
                cmp #vart
                bcs _cz1
                ldy #1
                lda (stack),y
_cz1            rts
                .endproc


;    OpCd1()
;    -------
opcd1           .proc
                ldx arg8
                lda cgopscd+1,x
                rts
                .endproc


;    StkAddr(,,offset)
;    -----------------
stkaddr         .proc
                lda (stack),y
                tax
                iny
                lda (stack),y
                tay
                rts
                .endproc


;    StkP(,,offset)
;    --------------
stkp            .proc
                jsr stkaddr
                lda #1
                jmp gprop
                .endproc


;    StkPZ(,,offset)
;    ---------------
stkpz           .proc
                lda #0
                .endproc

;    StkPS(,,offset)
;    ---------------
stkps           .proc
                sta arg12
                .endproc


;    StkProp(,,offset)
;    -----------------
stkprop         .proc
                jsr stkp
                clc
                adc arg12
                tax
                iny
                lda (props),y
                adc #0
                tay
                rts
                .endproc


;    JSRTable(,index)
;    ----------------
jsrtable        .proc
;.IF RAMzap
;  LDY LTab+1,X
;  LDA LTab,X
;.ELSE

                ldy lsh+1,x
                lda lsh,x
;.ENDIF
                tax
                lda #$20                ; JSR
                jmp push3
                .endproc


;    Push0()
;    -------
push0           .proc
                sty arg13
                ldy qcode
                sty arg14
                ldy qcode+1
                sty arg15
                ldy #0
                rts
                .endproc


;    PushTrue(op)
;    ------------
pushtrue        .proc
                jsr push1
                ldy #10
                jsr savecd
                lda #0                  ; no other true branches
                sta arg9
    ; falls into Push1
                .endproc


;    Push1(op)
;    ---------
push1           .proc
                jsr push0
                sta (arg14),y
                beq insrt1.i11
                .endproc


;    Insrt1(op)
;    ----------
insrt1          .proc
                ldy #1
                jsr addcdsp
i11             iny
                tya
                jmp codeincr
                .endproc


;    STempH()
;    --------
stemph          .proc
                inc arg9
                ldy #12
                bne stempl.stemp        ; uncond.
                .endproc

;    STempL()
;    --------
stempl          .proc
                ldy #10
stemp           jsr savecd
                lda arg9
                tax
                and #$fe                ; set to low address
                sta arg9
                lda #$85                ; STA addr
; falls into Push2
                .endproc


;    Push2(op,,data)
;    ---------------
push2           .proc
                jsr push0
                sta (arg14),y
                beq insrt2.i21
                .endproc


;    Insrt2(op,data)
;    ---------------
insrt2          .proc            
                ldy #2
                jsr addcdsp
i21             txa
                iny
                sta (arg14),y
                bne insrt1.i11
                .endproc


;    Push3(op,data16)
;    ----------------
push3           .proc            
                jsr push0
                sta (arg14),y
                beq insrt3.i31
                .endproc


;    Insrt3(op,data16)
;    -----------------
insrt3          .proc            
                sty arg13
                ldy #3
i30             jsr addcdsp
i31             txa
                ldx arg13
                iny
                sta (arg14),y
                bne insrt2.i21
                .endproc


addcdsp         .proc
    ; AddCdSp(,,size) add qcode space
    ; does NOT change qcode or codeOff
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
