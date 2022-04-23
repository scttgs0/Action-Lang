;      AMPL.MTH
; Copyright 1983 by Clinton W Parker
; All Rights Reserved
; last modified September 9, 1983
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

math            .proc
_a             = aflast+1
_b             = aflast
_c             = afcur+1
_d             = afcur
_rl            = afsize
_rh            = afsize+1
_t1            = addr
_t2            = addr+1
_sign          = token
                .endproc


;    MultI(op1, op2)
;    ---------------
multi           .proc
; op2 is in c & d
;  r = ab * cd
;  r = (a*d + c*b)*2^8 + b*d
                jsr smops
                ldx math._b
                beq _mc5
                stx math._t1
                ldx math._d
                beq _mc5
                dex
                stx math._t2
                ldx #8
_mc3            asl a                   ; b*d, 16 bit result
                rol math._rh
                asl math._t1
                bcc _mc4
                adc math._t2
                bcc _mc4
                inc math._rh
_mc4            dex
                bne _mc3
_mc5            sta math._rl
                lda math._b
                ldx math._c
                jsr mulb                ; b*c, 8 bit result
                lda math._a
                ldx math._d
                jsr mulb                ; a*d, 8 bit result
;
_setsign        ldy math._sign
                bpl _ss2
        .if ramzap
                sta mulb,x
        .else
                nop
                nop
                nop
        .endif
_ss1            sta math._rl
                stx math._rh
                sec
                lda #0
                sbc math._rl
                tay
                lda #0
                sbc math._rh
                tax
                tya
_ss2            rts
                .endproc


;
mulb            .proc
                beq _mb3
                dex
                stx math._t2
                tax
                beq _mb3
                stx math._t1
                lda #0
                ldx #8
_mb1            asl a
                asl math._t1
                bcc _mb2
                adc math._t2
_mb2            dex
                bne _mb1
                clc
                adc math._rh
                sta math._rh
_mb3            lda math._rl
                ldx math._rh
                rts
                .endproc


;
smops           .proc
                stx math._sign
                cpx #0                  ; check signs
                bpl _smo1
                jsr multi._ss1
_smo1           sta math._b
                stx math._a
                lda math._c
                bpl _smo2
                tax
                eor math._sign
                sta math._sign
                lda math._d
                jsr multi._ss1
                sta math._d
                stx math._c
_smo2           lda #0
                sta math._rh
                rts
                .endproc


;    DivC(op1, op2)
;    --------------
divi            .proc
                jsr smops
    ; see MultC above
                lda math._c
                beq _dsmall
;
_dlarge         ldx #8
_dl1            rol math._b
                rol math._a
                rol math._rh
                sec
                lda math._a
                sbc math._d
                tay
                lda math._rh
                sbc math._c
                bcc _dl2                ; overflow, don't subtract
                sta math._rh
                sty math._a
_dl2            dex
                bne _dl1
                lda math._b
                rol a
                ldx #0
                ldy math._a
                sty math._rl                 ; save low byte of REM
                jmp multi._setsign
;
_dsmall         ldx #16
_ds1            rol math._b
                rol math._a
                rol a
                bcs _ds1a               ; keep track of shift output
                cmp math._d
                bcc _ds2                ; overflow, don't subtract
_ds1a           sbc math._d
                sec                     ; for carry out in ROL A above
_ds2            dex
                bne _ds1
                rol math._b
                rol math._a
                sta math._rl
                lda math._b
                ldx math._a
                jmp multi._setsign
                .endproc

;
remi            .proc
                jsr divi
                lda math._rl
                ldx math._rh
_rem1           rts
                .endproc

;    RShift(val, cnt)
;    ----------------
rshift          .proc
                ldy math._d
                beq _rshret
                stx math._c
_rsh1           lsr math._c
                ror a
                dey
                bne _rsh1
                ldx math._c
_rshret         rts
                .endproc

;
sargs           .proc                   ; saves args for call
                sta arg0
                stx arg1
                sty arg2
                clc
                pla
                sta afcur
                adc #3                  ; jump over data
                tay
                pla
                sta afcur+1
                adc #0
                pha
                tya
                pha
                ldy #1
                lda (afcur),y           ; local address
                sta aflast
                iny
                lda (afcur),y
                sta aflast+1
                iny
                lda (afcur),y           ; # of bytes
                tay
_sa1            lda args,y
                sta (aflast),y
                dey
                bpl _sa1
    ; check for break key
                lda brkkey
                bne _sa2
                inc brkkey
                jmp break
_sa2            rts
                .endproc

                ;.endproc

;
; IToReal(int) -> FR0
;IToReal STX _sign
; JSR _SetSign
; STA FR0
; STX FR0+1
; JSR IFP
;:FSign LDA _sign
; BPL _Rem1
; JSR FMOVE
; JSR ZFR0
; JMP FSUB
; RToInt() real in FR0
;RToInt LDA FR0
; STA _sign
; JSR _FSign
; JSR FPI
; LDA FR0
; LDA FR0+1
; JMP _SetSign
;
;      AMPL.SYM
;
; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified June 29, 1983
;

;    STM(table)
;    ----------
stm             .proc
                sta arg2
                stx arg3
                sta arg4
                inx
                stx arg5
                ldy arg15
                sty arg13
_stm1           lda (arg2),y
                sta nxtaddr+1
                beq _stm3
                lda (arg4),y
                sta nxtaddr
                ldy #0
_stm2           lda (nxtaddr),y
                eor (symtab),y
                and stmask
                bne _stm4
                cpy arg14
                iny
                bcc _stm2
                lda (nxtaddr),y         ; matched
_stm3           rts
_stm4           inc arg13               ; try next entry
                ldy arg13
                cpy arg15
                bne _stm1
                ldy #ster
                lda arg3
                cmp stglobal+1
                beq _stm5
                iny
_stm5           jmp splerr
stmres          jmp (stmradr)
                .endproc

; this normially goes to ISTMres below

;    STMres() lookup reserved names
;    --------
istmres         .proc
                ldy arg14
                cpy #8
_stmr0          lda #$ff                ; if name too long!
                bcs _stmr3              ; not reserved name
                iny
                sty arg0
                ldx rwstrt-2,y
_stmr1          stx arg1
                ldy #1
_stmr2          lda resw1,x
                bmi _stmr3
                eor (symtab),y
                and stmask
                bne _stmr4
                inx
                iny
                cpy arg0
                bcc _stmr2
    ; we have a match
                lda resw1,x             ; get token value
_stmr3          rts
;
_stmr4          clc
                lda arg1
                adc arg0
                tax
                bne _stmr1              ; try next entry
                .endproc


;    GetName(char)
;    -------------
lgetname        .proc
                ldy #0
                sta frstchar
                tax
                ora #$20
                sta arg15               ; initial hash
                txa
_gname1         iny
                sty arg14
                sta (symtab),y
                ora #$20
                asl arg15
                adc arg15
                sta arg15
                jsr nextchar
                ldy arg14
                cmp #'_'
                beq _gname1
                jsr alphanum
                bne _gname1
                tya
                ldy #0
                sta (symtab),y
                dec choff               ; put character back
                jsr stm.stmres          ; check for res. name
                bpl istmres._stmr3      ; return
                lda qglobal
                beq gnglobal
                lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._stmr3      ; return
;
gnglobal        lda stglobal
                ldx stglobal+1
                ldy frstchar
                cpy bigst
                bpl _gng1
                lda stg2
                ldx stg2+1
_gng1           jsr stm
                bne istmres._stmr3      ; return
                lda qglobal
                beq newentry
;
lgnlocal        lda stlocal
                ldx stlocal+1
                jsr stm
                bne istmres._stmr3
        .if ramzap
                inc stm,x
        .else
                nop
                nop
                nop
        .endif
                .endproc
;
newentry        .proc
    ; Make new entry in symbol table
                lda symtab+1
                sta (arg2),y
                lda symtab
                sta (arg4),y
                lda #<libst
                ldx #>libst
                jsr stm                 ; lookup shadow name
                lda #undec
                ldy arg14
                iny
                sta (symtab),y
                lda nxtaddr
                iny
                sta (symtab),y          ; save shadow entry
                lda nxtaddr+1
                iny
                sta (symtab),y
                lda symtab
                sta nxtaddr
                ldx symtab+1
                stx nxtaddr+1
                iny
                tya
                jsr stincr
                lda #undec
                rts
                .endproc


;
rwstrt          .byte 0
                .byte resw2-resw1
                .byte resw3-resw1
                .byte resw4-resw1
                .byte resw5-resw1
                .byte resw6-resw1
                .byte resw7-resw1
;
resw1           .byte $ff

;
resw2           .text "DO",do
                .text "FI",fi
    ; .BYTE "FO",esac
                .text "IF",ifid
                .text "OD",od
    ; .BYTE "OF",of
                .text "OR",orid
                .text "TO",to
                .byte $ff

;
resw3           .text "AND",andid
                .text "FOR",forid
    ; .BYTE "GET",get
                .text "INT",int
                .text "LSH",lshid
                .text "MOD",remid
    ; .BYTE "NOT",notId
                .text "RSH",rshid
                .text "SET",set
                .text "XOR",xorid
                .byte $ff
;
resw4           .text "BYTE",byte
                .text "CARD",card
    ; .BYTE "CASE",caseId
                .text "CHAR",char
                .text "ELSE",else
    ; .BYTE "ESAC",esac
                .text "EXIT",exitid
                .text "FUNC",func
                .text "PROC",proc
    ; .BYTE "REAL",real
                .text "STEP",step
                .text "THEN",then
                .text "TYPE",typeid
                .byte $ff

;
resw5           .text "ARRAY",array
                .text "UNTIL",untilid
                .text "WHILE",whileid
                .byte $ff

;
resw6           .text "DEFINE",define
    ; .BYTE "DOWNTO",downto
                .text "ELSEIF",elseif
                .text "MODULE",modid
                .text "RETURN",retid
                .byte $ff

;
resw7           .text "INCLUDE",get
                .text "POINTER",pointer
                .byte $ff

;
;
;      LIB.KEY
;
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


;
;      SPL.ERR

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


;
;         LIB.IO
;
; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified August 13, 1983
;

;
;PROC ChkErr=*(BYTE result, block, errCode)
; checks for error return from CIO
; Sets EOF(block) to true on error
; does not call Error if EOF error ($88)
; see Hardware manual for CIO details
chkerr          .proc
                bpl _ce2
                cpy #$88                ; EOF
                beq _ce1
                tya
                cpy #$80                ; break key
                beq break1
                jmp error
_ce1            txa
                lsr a
                lsr a
                lsr a
                lsr a
                tax
                tya
                sta eof,x
_ce2
        .if ramzap
                dec chkerr-$10,x
        .else
                nop
                nop
                nop
        .endif
                rts
                .endproc


;    Break1(error)
;    -------------
break1          .proc
                ldx #1
                stx brkkey
                pha
                jsr break
                pla
                tay
                .endproc

pfe             rts


;
;PROC PrintF(STRING f, CARD a1, a2, a3, a4, a5)
; outputs a1-a5 to default device
; using format f.  Any non '%' char
; in f is output directly.  '%' char
; is interpreted as follows:
;    %S - output next arg as string
;    %I - output next arg as signed number
;    %U - output next arg as unsigned number
;    %C - output next arg as CHAR
;    %H - output next arg as HEX
;    %% - output '%' char
;    %E - output EOL
; any other char after % is treated
; the same as %U.
;
prtf            .proc
                sta addr
                stx addr+1
                sty temps
                ldy #0
                lda (addr),y
                sta token
                inc token
                ldx #13
_pf1            lda args+2,x
                sta temps,x
                dex
                bne _pf1
                stx lsttoken
                stx op
_pf2            inc op
                ldy op
                cpy token
                bcs pfe
                lda (addr),y
                cmp #'%'
                bne _pf3
                inc op
                iny
                lda (addr),y
                cmp #'%'
                beq _pf3
                cmp #'E'
                bne _pf4
                lda #eol
_pf3            jsr put
                jmp _pf2
;
_pf4            ldy lsttoken
                inc lsttoken
                inc lsttoken
                sta args
                lda temps,y
                ldx temps+1,y
                ldy args
                cpy #'C'
                beq _pf3
                cpy #'S'
                bne _pf5
                jsr prt
                jmp _pf2
_pf5            cpy #'I'
                bne _pf6
                jsr prti
                jmp _pf2
_pf6            cpy #'H'
                bne _pf7
                jsr prth
                jmp _pf2
_pf7            jsr prtc
                jmp _pf2
                .endproc


;
;
;PROC Open(BYTE dev, STRING fileSpec, BYTE mode, aux2)
; opens fileSpec and assigns it to IOCB dev
opn             .proc
                pha
                stx arg1
                sty arg2
                tay
                lda #0
                sta eof,y
                tay
                lda (arg1),y
                sta (buf),y
                tay
                iny
                lda #eol
                bne _op2                ; uncond.
_op1            lda (arg1),y
_op2            sta (buf),y
                dey
                bne _op1
                pla
                ldx buf
                ldy buf+1
                jsr open
                jmp chkerr
                .endproc


;
;
;PROC PrintE(STRING str)
; outputs str to default IOCB with EOL
prte            .proc
                stx arg1
                tax
                ldy arg1
                lda device

    ; falls into PrintDE
                .endproc

;
;PROC PrintDE(BYTE dev, STRING str)
; outputs str to IOCB dev appended with an EOL
prtde           .proc
                jsr print
                jmp chkerr
                .endproc


;
;PROC Close(BYTE dev)
; closes IOCB dev
clos            .proc
                jsr close
                jmp chkerr
                .endproc


;
;PROC Print(STRING str)
; outputs str to default IOCB
prt             .proc
                stx arg1
                tax
                ldy arg1
                lda device
    ; falls into PrintD
                .endproc


;
;PROC PrintD(BYTE dev, STRING str)
; outputs str to IOCB dev
prtd            .proc
                jsr output
                jmp chkerr
                .endproc


;
;PROC InputS(STRING str)
; same as InputSD, but uses default IOCB
ins             .proc
                stx arg2
                tax
                ldy arg2
                lda device
    ; falls into InputSD
                .endproc


;
;PROC InputSD(BYTE dev, STRING str)
; see Input, size set to 255
insd            .proc
                pha
                lda #255
                sta arg3
                pla
    ; falls into InputMD
                .endproc


;
;PROC InputMD(BYTE dev, STRING str, BYTE max)
; see Input, size set to max
inmd            .proc
                pha
                stx arg1
                sty arg2
                ldy #0
                lda arg3
                sta (arg1),y
                pla
                ldy arg2
    ; falls into InputD
                .endproc


;
;PROC InputD(BYTE dev, STRING str)
; inputs str from IOCB dev
; first byte must be set to maximum size
; on return, first byte set to size of string input
ind             .proc
                jsr rdbuf.inputs
                jmp chkerr
                .endproc


;
;BYTE FUNC GetD(BYTE dev)
; inputs character from IOCB dev
getd            .proc
                ldx #$07
ccio            stx arg4
                asl a
                asl a
                asl a
                asl a
                tax
                lda arg4
                sta $0342,x
                lda #0
                sta $0348,x
                sta $0349,x
                tya
                jsr $e456
                sta args
                jmp chkerr
                .endproc


;
;PROC PutE()
; output EOL do default IOCB
pute            .proc
                lda #eol
    ; falls into Put
                .endproc


;
;PROC Put(CHAR ch)
; outputs ch to default IOCB
put             .proc
                tax
                lda device
    ; falls into PutD
                .endproc


;
;PROC PutD(BYTE dev, CHAR ch)
; outputs ch to IOCB dev
putd            .proc
                stx arg1
                ldy arg1
putd1           ldx #$0b
                jmp getd.ccio
                .endproc


;
;PROC PutDE(BYTE dev)
; outputs EOL to IOCD dev
putde           .proc
                ldy #eol
                bne putd.putd1          ; uncond.
                .endproc


;
;PROC XIOstr(BYTE dev, fill, cmd, aux1, aux2, STRING str)
; see Hardware manual for CIO details
; performs system CIO call where:
;   ICCOM = cmd
;   ICBL = str(0)
;   ICBA = str+1
;   ICAX1 = aux1
;   ICAX2 = aux2
; CIO is not called if str(0)=0
; ICAX1 and ICAX2 are not set if aux1=0
xio             .proc
                jsr xiostr
                jmp chkerr
                .endproc


;
;PROC PrintB(BYTE num)
; outputs byte num to default IOCB
prtb            .proc
                ldx #0
                .endproc


;PROC PrintC(CARD num)
; outputs cardinal num to default IOCB
prtc            .proc
                jsr printc
                jmp chkerr
                .endproc


;
;PROC PrintBE(BYTE num)
; same as PrintB except EOL appended
prtbe           .proc
                ldx #0
                .endproc


;PROC PrintCE(CARD num)
; same as PrintC except EOL appended
prtce           .proc
                jsr prtc
                jmp pute
                .endproc


;
;PROC PrintBD(BYTE dev, BYTE num)
; output byte num to IOCB dev
prtbd           .proc
                ldy #0
                .endproc


;PROC PrintCD(BYTE dev, CARD num)
; output cardinal num to IOCB dev
prtcd           .proc
                sta arg0
                txa
                sty arg2
                ldx arg2
                jsr ctostr
                lda arg0
                jsr printc.pnum+2
                jmp chkerr
                .endproc


;
;PROC PrintBDE(BYTE dev, BYTE num)
; output num to IOCB dev with EOL
prtbde          .proc
                ldy #0
                .endproc


;PROC PrintCDE(BYTE dev, CARD num)
; output num to IOCB dev with EOL
prtcde          .proc
                jsr prtcd
                lda arg0
                jmp putde
                .endproc


;
;PROC PrintI(INT num)
; outputs integer num to default IOCB
prti            .proc
                stx arg2
                tax
                ldy arg2
                lda device
    ; falls into PrintID
                .endproc


;
;PROC PrintID(BYTE dev, INT num)
; outputs integer num to IOCB dev
prtid           .proc
                cpy #0
                bpl prtcd
                pha
                stx arg1
                sty arg2
                ldy #'-'
                jsr putd.putd1
                sec
                lda #0
                sbc arg1
                tax
                lda #0
                sbc arg2
                tay
                pla
                jmp prtcd
                .endproc


;
;PROC PrintIE(INT num)
; same as PrintI with EOL
prtie           .proc
                jsr prti
                jmp pute
                .endproc


;
;PROC PrintIDE(BYTE dev, INT num)
; same as PrintID with EOL
prtide          .proc
                jsr prtid
                lda arg0
                jmp putde
                .endproc


;
;PROC StrB(BYTE n, STRING s)
; convert number to string
strb            .proc
                stx arg2
                sty arg3
                ldx #0
                ldy arg2
    ; falls into StrC
                .endproc


;
;PROC StrC(CARD n, STRING s)
; convert number to string
strc            .proc
                sty arg2
                jsr ctostr
                iny
_strc1          lda numbuf,y
                sta (arg2),y
                dey
                bpl _strc1
                rts
                .endproc


;
;PROC StrI(INT n, STRING s)
; convert number to string
stri            .proc
                cpx #0
                bpl strc
                sta arg0
                stx arg1
                sty arg2
                sec
                lda #0
                sbc arg0
                tay
                lda #0
                sbc arg1
                tax
                tya
                jsr ctostr
                inx
                txa
                tay
_s1             lda numbuf-1,y
                sta (arg2),y
                dey
                bne _s1
                txa
                sta (arg2),y
                iny
                lda #'-'
                sta (arg2),y
                rts
                .endproc


;
;BYTE FUNC InputB()
;CARD FUNC InputC()
;INT FUNC InputI()
; input number from default IOCB
; number must be terminated with EOL
inb
inc:
ini
                lda    device
    ; falls into InputND


;
;BYTE FUNC InputBD()
;CARD FUNC InputCD()
;INT FUNC InputID(BYTE dev)
; same as InputI, but from IOCB dev
inbd
incd
inid
                ldx #19
                stx numbuf
                ldx #<numbuf
                ldy #>numbuf
                jsr ind
                lda #<numbuf
                ldx #>numbuf
    ; falls into ValI


;
; BYTE FUNC ValB(STRING s)
; INT FUNC ValI(STRING s)
; CARD FUNC ValC(STRING s)
; returns numeric value of s
valb
vali
valc
                sta arg4
                stx arg5
                ldy #0
                sty arg0
                sty arg1
                sty arg2
                lda (arg4),y
                sta arg3
                inc arg3
                lda #32
                iny
_i1             cmp (arg4),y
                bne _i2
                iny
                cpy arg3
                bmi _i1
_i2             lda (arg4),y
                cmp #'-'
                bne _i3
                sta arg2
                iny
_i3             cpy arg3
                bpl _i6
_i4             lda (arg4),y
                cmp #'0'
                bmi _i6
                cmp #':'                ; '9'+1
                bpl _i6
                sec
                sbc #'0'
                tax
    ; arg01*10
                lda arg1
                pha
                lda arg0
                asl a
                rol arg1
                asl a
                rol arg1
                clc
                adc arg0
                sta arg0
                pla
                adc arg1
                sta arg1
                asl arg0
                rol arg1
                clc
                txa
                adc arg0                ; add in digit
                sta arg0
                bcc _i5
                inc arg1
_i5             iny
                cpy arg3
                bmi _i4
_i6             lda arg2
                beq _i7
                sec
                lda #0
                sbc arg0
                sta arg0
                lda #0
                sbc arg1
                sta arg1
_i7             rts


;
;PROC Note(BYTE dev, CARD POINTER sector, BYTE POINTER offset)
; returns disk sector and offset in that
; sector of next byte to be read or
; written to IOCB dev
; example:  Note(1, @sect, @pos)
; see Hardware manual
note            .proc
                stx arg1
                sty arg2
                asl a
                asl a
                asl a
                asl a
                tax
                lda #$26                ; NOTE
                sta $0342,x             ; ICCOM
                jsr $e456               ; CIOV
                jsr chkerr
                ldy #0
                lda $034e,x             ; offset
                sta (arg3),y
                lda $034c,x             ; low byte of sector
                sta (arg1),y
                lda $034d,x             ; high byte of sector
                iny
                sta (arg1),y
                rts
                .endproc


;
;PROC Point(BYTE dev, CARD sector, BYTE offset)
; Sets next byte to be read or written
; to be byte offset of sector.    File
; must be open for update (mode=12)
; see Hardware manual
point           .proc
                stx arg1
                asl a
                asl a
                asl a
                asl a
                tax
                tya                     ; sector+1
                sta $034d,x
                lda arg1                ; sector
                sta $034c,x
                lda arg3                ; offset
                sta $034e,x
                lda #$25                ; POINT
                sta $0342,x             ; ICCOM
                jsr $e456               ; CIOV
                jmp chkerr
                .endproc


;
;         LIB.GR
;
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


;
;         LIB.MSC
;
; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified November 3, 1983
;


;misc            .proc


;
;BYTE FUNC Rand(BYTE range)
; returns random number between 0 and
; range-1.  If range=0, then a random
; number between 0 and 255 is returned
rand            .proc
                ldx $d20a               ; RANDOM
                cmp #0
                beq _rand1
                stx afcur
                ldx #0
                stx afcur+1
                jsr multi
_rand1          stx args
                rts
                .endproc


;
;PROC Sound(BYTE v, p, d, vol)
; set voice to specified pitch, distortion,
; and volume.  Assumes volume low  16.
sound           .proc
                asl a
                sty arg2
                tay
                cmp #7
                bmi _snd1
                ldy #100
                jsr error
_snd1           txa
                sta $d200,y
                lda arg2
                asl a
                asl a
                asl a
                asl a
                ora arg3
                sta $d201,y
                rts
                .endproc


;
;PROC SndRst()
; reset sound channels
sndrst          .proc
                lda $0232
                and #$ef                ; turn off two tone bit
                sta $0232
                sta $d20f
                lda #0
                ldx #8
_sr1            sta $d200,x             ; zero sound regs
                dex
                bpl _sr1
                rts
                .endproc


;
;BYTE FUNC Paddle(BYTE port)
; returns paddle value of port
; Assumes port low  8.
; see LIB.ST
;Paddle TAX
; LDA $D200,X
; STA args
; RTS
;
;BYTE FUNC PTrig(BYTE port)
; returns zero if trigger of paddle
; port is depressed.  Assumes port<8
ptrig           .proc
                ldx #0
                cmp #4
                bmi _pt1
                inx
                and #3
_pt1            tay
                lda $d300,x
                and _pt2,y
                sta args
                rts
;
_pt2            .byte $04,$08,$40,$80
                .endproc


;
;BYTE FUNC Stick(BYTE port)
; returns current value of joystick
; controller port.  Assumes port<4
stick           .proc
                ldx #0
                cmp #2
                bmi _stk1
                inx
                and #1
_stk1           tay
                lda $d300,x
                dey
                bne _stk2
                lsr a
                lsr a
                lsr a
                lsr a
_stk2           and #$0f
                sta args
                rts
                .endproc


;
;BYTE FUNC STrig(BYTE port)
; returns zero if trigger of joystick
; port is depressed.  Assumes port<4
;
; see LIB.ST
;STrig TAX
; LDA $D010,X
; STA args
; RTS
;


;BYTE FUNC Peek(CARD address)
; returns value stored at address
peek            .proc
    ; falls into PeekC
                .endproc


;
;CARD FUNC PeekC(CARD address)
; returns value stored at address
peekc           .proc
                sta arg2
                stx arg3
                ldy #0
                lda (arg2),y
                sta args
                iny
                lda (arg2),y
                sta args+1
                rts
                .endproc


;
;PROC Poke(CARD address, BYTE value)
; store byte or char value at address
; (single byte store)
poke            .proc
                sta arg0
                stx arg1
                tya
                ldy #0
                sta (arg0),y
                rts
                .endproc


;
;PROC PokeC(CARD address, value)
; store cardinal or integer value at
; address (2 byte store)
pokec           .proc
                jsr poke
                iny
                lda arg3
                sta (arg0),y
                rts
                .endproc


;
;PROC Zero(BYTE POINTER address, CARD size)
; set memory bytes starting at address
; up to (but not including) address+size
; to zero.  Note this modifies size
; bytes of memory.
mzero           .proc
                pha
                lda #0
                sta arg4
                pla
    ; falls into SetBlock
                .endproc


;
;PROC SetBlock(BYTE POINTER address, CARD size, BYTE value)
; set memory bytes starting at address
; up to (but not including) address+size
; to value.  Note this modifies size
; bytes of memory.
setblock        .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda arg4
                ldx arg3
                beq _sb3
_sb1            sta (arg0),y
                iny
                bne _sb1
                inc arg1
                dec arg3
                bne _sb1
                beq _sb3
_sb2            sta (arg0),y
                iny
_sb3            cpy arg2
                bne _sb2
                rts
                .endproc


;
;PROC MoveBlock(BYTE POINTER dest, src, CARD size)
; moves size bytes from src through
; src+size-1 to dest through dest+size-1.
; If dest>src and dest<=src+size-1 then
; transfer will not work properly!
moveblock       .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda arg5
                beq _mb4
_mb2            lda (arg2),y
                sta (arg0),y
                iny
                bne _mb2
                inc arg1
                inc arg3
                dec arg5
                bne _mb2
                beq _mb4
_mb3            lda (arg2),y
                sta (arg0),y
                iny
_mb4            cpy arg4
                bne _mb3
                rts
                .endproc


;
;PROC Break()
; returns to monitor after saving
; stack pointer in procSP
break           .proc
                tsx
                stx procsp
                ldy #brker
                tya
                jmp error
                .endproc


;
ctrace          .proc                   ; Call Trace handler
    ; name passed following JSR
                clc
                pla
                adc #1
                sta arg10
                pla
                adc #0
                sta arg11
    ; address of name now in arg10-11
    ; ok, let's print the name
                lda arg10
                ldx arg11
                jsr prt
                lda #'('
                jsr put
    ; now get addr of args
                sec
                lda arg10
                ldy #0
                sty arg15
                adc (arg10),y
                sta arg10
                bcc _ct1
                inc arg11
_ct1            lda (arg10),y
                sta arg12
                iny
                lda (arg10),y
                sta arg13
    ; get number of args
                iny
                lda (arg10),y
                sta arg9
                sty arg14
                beq _ct7                ; no args
_ct2            inc arg14
                ldy arg14
                lda (arg10),y
                bmi _ct4                ; byte
                cmp #cardt
                inc arg15
                ldy arg15
                lda (arg12),y
                tax
                dey
                bcs _ct5                ; cardinal
    ; integer
                lda (arg12),y
                jsr prti
                jmp _ct6
_ct4            ldx #0
                ldy arg15
_ct5            lda (arg12),y
                jsr prtc
_ct6            inc arg15
                dec arg9
                beq _ct7                ; all done
                lda #','
                jsr put
                jmp _ct2
    ; setup return
_ct7            clc
                lda arg10
                adc arg14
                tax
                lda arg11
                adc #0
                pha
                txa
                pha
                lda #')'
                jsr put
                jmp pute
                .endproc

                ;.endproc

;
;         LIB.STR
;
; Copyright 1983 by Action Computer Services
; All Rights Reserved
;
; last modified November 3, 1983
;
;lstr            .proc


;
;INT FUNC SCompare(STRING a,b)
; result returned is:
;   =0 if a=b
;   low 0 if a<b
;   high 0 if a>b
scompare        .proc
                sta arg4
                stx arg5
                sty arg2
                ldy #0
                sty args
                sty args+1
                lda (arg4),y
                cmp (arg2),y
                beq _sc1
                jsr _sc4
_sc1            cmp #0
                bne _sc2
                rts
;
_sc2            sta arg6
_sc3            iny
                lda (arg4),y
                cmp (arg2),y
                bne _sc4
                cpy arg6
                bcc _sc3
                rts
;
_sc4            ldx #$ff
                stx args
                bcc _sc5
                lda (arg2),y
                inx
_sc5            stx args+1
                rts
                .endproc


;
;PROC SCopy(STRING dest, src)
; dest = src
scopy           .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
_scopy1         sta (arg0),y
                beq _scp2
_scopy2         tay
_scp1           lda (arg2),y
                sta (arg0),y
                dey
                bne _scp1
_scp2           rts
                .endproc


;
;PROC SCopyS(STRING dest, src, BYTE start, stop)
; if LEN(src)<stop then stop=LEN(src)
; dest = src(start, stop)
scopys          .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
                cmp arg5
                bcs _scs1
                sta arg5
_scs1           dec arg4
                clc
                lda arg2
                adc arg4
                sta arg2
                bcc _scs2
                inc arg3
_scs2           sec
                lda arg5
                sbc arg4
                bcs _scs3
                lda #0
_scs3           jmp scopy._scopy1
                .endproc


;
;PROC SAssign(STRING dest, src, BYTE start, stop)
; IF stop-start+1>LEN(src) THEN
;   stop = LEN(src)+start-1
; IF LEN(dest)<stop THEN
;   LEN(dest) = stop
; dest(start, stop) = src
sassign         .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
                beq _sa1
                sta arg6
                dec arg4
                sec
                lda arg5
                sbc arg4
                beq _sa1
                bcs _sa2
_sa1            rts
_sa2            tax
                cmp  arg6
                bcc  _sa3
                clc
                lda arg6
                tax
                adc arg4
                sta arg5
_sa3            lda arg5
                cmp (arg0),y
                bcc _sa4
                sta (arg0),y
                clc
_sa4            lda arg0
                adc arg4
                sta arg0
                bcc _sa5
                inc arg1
_sa5            txa
                jmp scopy._scopy2
                .endproc

                ;.endproc


;
            ; symbol table
;
;:EN6 .BYTE 6,"PrintF",200
; .WORD PrtF ; #117
; .BYTE 6,17,12,12,12,12,12

_en7            .text 4,"Open",200
                .word opn               ; #96
                .byte 4,138,17,138,138
_en8            .text 6,"PrintE",200
                .word prte              ; #116
                .byte 1,17
_en9            .text 7,"PrintDE",200
                .word prtde             ; #75
                .byte 2,138,17
_en10           .text 5,"Close",200
                .word clos              ; #253
                .byte 1,138
_en11           .text 5,"Print",200
                .word prt               ; #135
                .byte 1,17
_en12           .text 6,"PrintD",200
                .word prtd              ; #115
                .byte 2,138,17
_en13           .text 6,"InputS",200
                .word ins               ; #249
                .byte 1,17
_en14           .text 7,"InputSD",200
                .word insd              ; #87
                .byte 2,138,17
_en15           .text 7,"InputMD",200
                .word inmd              ; #75
                .byte 3,138,17,138
_en16           .text 4,"GetD",202
                .word getd              ; #138
                .byte 1,138
_en17           .text 4,"PutE",200
                .word pute              ; #162
                .byte 0
_en18           .text 3,"Put",200
                .word put               ; #158
                .byte 1,137
_en19           .text 4,"PutD",200
                .word putd              ; #161
                .byte 2,138,137
_en20           .text 5,"PutDE",200
                .word putde             ; #168
                .byte 1,138
_en21           .text 3,"XIO",200
                .word xio               ; #225
                .byte 6,138,138,138,138,138,17
_en22           .text 6,"PrintB",200
                .word prtb              ; #113
                .byte 1,138
_en23           .text 7,"PrintBE",200
                .word prtbe             ; #71
                .byte 1,138

;:EN24 .BYTE 7,"PrintBD",200
; .WORD PrtBD ; #70
; .BYTE 2,138,138

_en25           .text 8,"PrintBDE",200
                .word prtbde            ; #241
                .byte 2,138,138
_en26           .text 6,"PrintC",200
                .word prtc              ; #114
                .byte 1,12
_en27           .text 7,"PrintCE",200
                .word prtce             ; #73
                .byte 1,12
_en28           .text 7,"PrintCD",200
                .word prtcd             ; #72
                .byte 2,138,12
_en29           .text 8,"PrintCDE",200
                .word prtcde            ; #245
                .byte 2,138,12
_en30           .text 6,"PrintI",200
                .word prti              ; #120
                .byte 1,11
_en31           .text 7,"PrintID",200
                .word prtid             ; #84
                .byte 2,138,11
_en32           .text 7,"PrintIE",200
                .word prtie             ; #85
                .byte 1,11
_en33           .text 8,"PrintIDE",200
                .word prtide            ; #13
                .byte 2,138,11
_en34           .text 6,"InputB",202
                .word inb               ; #232
                .byte 0
_en35           .text 7,"InputBD",202
                .word inbd              ; #53
                .byte 1,138
_en36           .text 6,"InputI",203
                .word ini               ; #239
                .byte 0
_en37           .text 7,"InputID",203
                .word inid              ; #67
                .byte 1,138
_en38           .text 6,"InputC",204
                .word inc               ; #233
                .byte 0
_en39           .text 7,"InputCD",204
                .word incd              ; #55
                .byte 1,138
_en40           .text 4,"ValB",202
                .word valb              ; #207
                .byte 1,17
_en41           .text 4,"ValI",203
                .word vali              ; #214
                .byte 1,17
_en42           .text 4,"ValC",204
                .word valc              ; #208
                .byte 1,17
_en43           .text 4,"StrB",200
                .word strb              ; #223
                .byte 2,138,17
_en44           .text 4,"StrI",200
                .word stri              ; #230
                .byte 2,11,17
_en45           .text 4,"StrC",200
                .word strc              ; #224
                .byte 2,12,17
_en46           .text 4,"Note",200
                .word note              ; #89
                .byte 3,138,20,18
_en47           .text 5,"Point",200
                .word point             ; #110
                .byte 3,138,12,138
_en48           .text 8,"Graphics",200
                .word graphics          ; #108
                .byte 1,138
_en49           .text 6,"DrawTo",200
                .word drawto            ; #231
                .byte 2,12,138
_en50           .text 8,"Position",200
                .word position          ; #94
                .byte 2,12,138
_en51           .text 6,"Locate",202
                .word locate            ; #97
                .byte 2,12,138
_en52           .text 4,"Plot",200
                .word plot              ; #131
                .byte 2,12,138
_en53           .text 8,"SetColor",200
                .word setcolor          ; #6
                .byte 3,138,138,138
_en54           .text 4,"Fill",200
                .word fill              ; #122
                .byte 2,12,138
_en55           .text 4,"Rand",202
                .word rand              ; #117
                .byte 1,138
_en56           .text 5,"Sound",200
                .word sound             ; #31
                .byte 4,138,138,138,138
_en57           .text 6,"SndRst",200
                .word sndrst            ; #73
                .byte 0
_en58           .text 6,"Paddle",202
                .word paddle            ; #254
                .byte 1,138
_en59           .text 5,"PTrig",202
                .word ptrig             ; #164
                .byte 1,138
_en60           .text 5,"Stick",202
                .word stick             ; #8
                .byte 1,138
_en61           .text 5,"STrig",202
                .word strig             ; #52
                .byte 1,138

;:EN62 .BYTE 4,"Peek",202
; .WORD Peek ; #73
; .BYTE 1,12
;:EN63 .BYTE 5,"PeekC",204
; .WORD PeekC ; #245
; .BYTE 1,12

_en64           .text 4,"Poke",200
                .word poke              ; #120
                .byte 2,12,138
_en65           .text 5,"PokeC",200
                .word pokec             ; #83
                .byte 2,12,12
_en66           .text 4,"Zero",200
                .word mzero             ; #88
                .byte 2,18,12
_en67           .text 8,"SetBlock",200
                .word setblock          ; #203
                .byte 3,18,12,138

;:EN68 .BYTE 9,"MoveBlock",200
; .WORD MoveBlock ; #85
; .BYTE 3,18,18,12

_en69           .text 5,"Break",200
                .word break             ; #183
_en70           .text 8,"SCompare",203
                .word scompare          ; #92
                .byte 2,17,17
_en71           .text 5,"SCopy",200
                .word scopy             ; #192
                .byte 2,17,17
_en72           .text 6,"SCopyS",200
                .word scopys            ; #244
                .byte 4,17,17,138,138
_en73           .text 7,"SAssign",200
                .word sassign           ; #23
                .byte 4,17,17,138,138


;
; hash table
;
libst           .byte 0                 ; 1
                .byte >en1              ; EOF #1
                .byte >en5              ; TRACE #2
                .byte 0,0,0             ; 3
                .byte >_en53            ; SetColor #6
                .byte 0                 ; 1
                .byte >_en60            ; Stick #8
                .byte 0,0,0,0           ; 4
                .byte >_en33            ; PrintIDE #13
                .byte >en2              ; color #14
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en73            ; SAssign #23
                .byte 0,0,0,0,0,0,0     ; 7
                .byte >_en56            ; Sound #31
                .byte >en3              ; LIST #32
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0,0 ; 19
                .byte >_en61            ; STrig #52
                .byte >_en35            ; InputBD #53
                .byte 0                 ; 1
                .byte >_en39            ; InputCD #55
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0                 ; 11
                .byte >_en37            ; InputID #67
                .byte 0,0               ; 2
                .byte >_en24            ; PrintBD #70
                .byte >_en23            ; PrintBE #71
                .byte >_en28            ; PrintCD #72
                .byte >_en27            ; PrintCE #73
                .byte >_en57            ; SndRst #74
                .byte >_en9             ; PrintDE #75
                .byte >_en15            ; InputMD #76
                .byte >_en62            ; Peek #77
                .byte 0,0,0,0,0         ; 5
                .byte >_en65            ; PokeC #83
                .byte >_en31            ; PrintID #84
                .byte >_en32            ; PrintIE #85
                .byte >_en68            ; MoveBlock #86
                .byte >_en14            ; InputSD #87
                .byte >_en66            ; Zero #88
                .byte >_en46            ; Note #89
                .byte 0,0               ; 2
                .byte >en4              ; device #92
                .byte >_en70            ; SCompare #93
                .byte >_en50            ; Position #94
                .byte 0                 ; 1
                .byte >_en7             ; Open #96
                .byte >_en51            ; Locate #97
                .byte 0,0,0,0,0,0,0,0,0,0 ; 10
                .byte >_en48            ; Graphics #108
                .byte 0                 ; 1
                .byte >_en47            ; Point #110
                .byte 0,0               ; 2
                .byte >_en22            ; PrintB #113
                .byte >_en26            ; PrintC #114
                .byte >_en12            ; PrintD #115
                .byte >_en8             ; PrintE #116
                .byte >_en6             ; PrintF #117
                .byte >_en55            ; Rand #118
                .byte 0                 ; 1
                .byte >_en30            ; PrintI #120
                .byte >_en64            ; Poke #121
                .byte >_en54            ; Fill #122
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en52            ; Plot #131
                .byte 0,0,0             ; 3
                .byte >_en11            ; Print #135
                .byte 0,0               ; 2
                .byte >_en16            ; GetD #138
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0   ; 18
                .byte >en0              ; Error #157
                .byte >_en18            ; Put #158
                .byte 0,0               ; 2
                .byte >_en19            ; PutD #161
                .byte >_en17            ; PutE #162
                .byte 0                 ; 1
                .byte >_en59            ; PTrig #164
                .byte 0,0,0             ; 3
                .byte >_en20            ; PutDE #168
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0           ; 14
                .byte >_en69            ; Break #183
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en71            ; SCopy #192
                .byte 0,0,0,0,0,0,0,0,0,0 ; 10
                .byte >_en67            ; SetBlock #203
                .byte 0,0,0             ; 3
                .byte >_en40            ; ValB #207
                .byte >_en42            ; ValC #208
                .byte 0,0,0,0,0         ; 5
                .byte >_en41            ; ValI #214
                .byte 0,0,0,0,0,0,0,0 ; 8
                .byte >_en43            ; StrB #223
                .byte >_en45            ; StrC #224
                .byte >_en21            ; XIO #225
                .byte 0,0,0,0           ; 4
                .byte >_en44            ; StrI #230
                .byte >_en49            ; DrawTo #231
                .byte >_en34            ; InputB #232
                .byte >_en38            ; InputC #233
                .byte 0,0,0,0,0         ; 5
                .byte >_en36            ; InputI #239
                .byte 0                 ; 1
                .byte >_en25            ; PrintBDE #241
                .byte 0,0               ; 2
                .byte >_en72            ; SCopyS #244
                .byte >_en29            ; PrintCDE #245
                .byte >_en63            ; PeekC #246
                .byte 0,0               ; 2
                .byte >_en13            ; InputS #249
                .byte 0,0,0             ; 3
                .byte >_en10            ; Close #253
                .byte >_en58            ; Paddle #254
                .byte 0                 ; 1
;
                .byte 0                 ; 1
                .byte <en1
                .byte <en5
                .byte 0,0,0             ; 3
                .byte <_en53
                .byte 0                 ; 1
                .byte <_en60
                .byte 0,0,0,0           ; 4
                .byte <_en33
                .byte <en2
                .byte 0                 ; 1

;
; .BYTE 0,0,0,0,0,0,0,0 ; 7

strig           .proc
                tax
                lda $d010,x
                sta args
                rts
                .endproc

;
                .byte <_en73

;
; .BYTE 0,0,0,0,0,0,0 ; 7

paddle          .proc
                tax
                lda $0270,x
                sta args
                rts
                .endproc


;
                .byte <_en56
                .byte <en3
                .byte 0,0               ; 2
;
; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0,0,0,0,0,0               ; 17

_en6            .text 6,"PrintF",200
                .word prtf        ; #117
                .byte 6,17,12,12,12,12,12
;
                .byte <_en61
                .byte <_en35
                .byte 0                 ; 1
                .byte <_en39
;
; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0                               ; 11
_en63           .text 5,"PeekC",204
                .word peekc             ; #245
                .byte 1,12
;
                .byte <_en37
                .byte 0,0               ; 2
                .byte <_en24
                .byte <_en23
                .byte <_en28
                .byte <_en27
                .byte <_en57
                .byte <_en9
                .byte <_en15
                .byte <_en62
                .byte 0,0,0,0,0         ; 5
                .byte <_en65
                .byte <_en31
                .byte <_en32
                .byte <_en68
                .byte <_en14
                .byte <_en66
                .byte <_en46
                .byte 0,0               ; 2
                .byte <en4
                .byte <_en70
                .byte <_en50
                .byte 0                 ; 1
                .byte <_en7
                .byte <_en51

;
; .BYTE 0,0,0,0,0,0,0,0,0,0             ; 10
; (c)1983ACS in internal char. qcode

copyright       .byte 8,99,9,17,25,24,19,33,35,51

;
                .byte <_en48
                .byte 0                 ; 1
                .byte <_en47
                .byte 0,0               ; 2
                .byte <_en22
                .byte <_en26
                .byte <_en12
                .byte <_en8
                .byte <_en6
                .byte <_en55
                .byte 0                 ; 1
                .byte <_en30
                .byte <_en64
                .byte <_en54
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en52
                .byte 0,0,0             ; 3
                .byte <_en11
                .byte 0,0               ; 2
                .byte <_en16
                .byte 0                 ; 1

;
; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0,0,0,0,0 ; 17

_en68           .text 9,"MoveBlock",200
                .word moveblock         ; #85
                .byte 3,18,18,12
;
                .byte <en0
                .byte <_en18
                .byte 0,0               ; 2
                .byte <_en19
                .byte <_en17
                .byte 0                 ; 1
                .byte <_en59
                .byte 0,0,0             ; 3
                .byte <_en20

;
; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0 ; 14

_en24           .text 7,"PrintBD",200
                .word prtbd             ; #70
                .byte 2,138,138
;
                .byte <_en69
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en71

;
; .BYTE 0,0,0,0,0,0,0,0,0,0 ; 10

_en62           .text 4,"Peek",202
                .word peek              ; #73
                .byte 1,12
;
                .byte <_en67
                .byte 0,0,0             ; 3
                .byte <_en40
                .byte <_en42
                .byte 0,0,0,0,0         ; 5
                .byte <_en41
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en43
                .byte <_en45
                .byte <_en21
                .byte 0,0,0,0           ; 4
                .byte <_en44
                .byte <_en49
                .byte <_en34
                .byte <_en38
                .byte 0,0,0,0,0         ; 5
                .byte <_en36
                .byte 0                 ; 1
                .byte <_en25
                .byte 0,0               ; 2
                .byte <_en72
                .byte <_en29
                .byte <_en63
                .byte 0,0               ; 2
                .byte <_en13
                .byte 0,0,0             ; 3
                .byte <_en10
                .byte <_en58

; .BYTE 0 ; 1
