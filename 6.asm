; TITLE "STORAGE.MAC"

; Copyright 1983 by Clinton W Parker
; All Rights Reserved
; Last modified June 17, 1983
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


slop            = 7                     ; can't be less than 4


;    Allocate(size)
;    --------------
allocate        .proc
                sta afsize              ; save size
                stx afsize+1
                lda #$ff                ; set best size
                sta afbsze
                sta afbsze+1
                lda #<afbase            ; last = base
                sta aflast
                lda #>afbase
                sta aflast+1
_afl1           ldy #0
                lda (aflast),y          ; cur = last(0)
                sta afcur
                bne _afl2               ; while cur # 0
                iny
                lda (aflast),y
                sta afcur+1
                bne _afl3
                beq _afl6               ; done
_afl2           iny
                lda (aflast),y
                sta afcur+1
_afl3           ldy #3
                lda (afcur),y           ; get size
                cmp afsize+1            ; high bytes
                bcc _afl5               ; size too small
                bne _afl4               ; size too big
                dey
                lda (afcur),y
                cmp afsize              ; low bytes
                bcc _afl5               ; size too small
                beq _afl9               ; sizes equal

    ; Check for best fit

_afl4           ldy #2
                lda (afcur),y
                cmp afbsze
                iny
                lda (afcur),y
                sbc afbsze+1
                bcs _afl5

    ; save best guess so far

                lda (afcur),y
                sta afbsze+1
                dey
                lda (afcur),y
                sta afbsze
                lda aflast
                sta afbest
                lda aflast+1
                sta afbest+1

    ; get next entry in list and goto
    ; beginning of loop

_afl5           lda afcur
                sta aflast
                lda afcur+1
                sta aflast+1
                clc
                bcc _afl1

    ; no entry found, use best guess

_afl6           lda afbsze+1
                cmp #$ff
                beq _afl10              ; no free block

    ; see if we need to split block

                sec
                lda afbsze
                sbc #slop
                sta afbsze
                bcs _afl7
                dec afbsze+1
_afl7           lda afbsze+1
                cmp afsize+1
                bcc _afl8               ; use as is
                bne _afl11              ; split it
                lda afbsze
                cmp afsize
                bcs _afl11              ; split it

    ; don't split

_afl8           ldy #0
                lda (afbest),y          ; cur =  best(0)
                sta afcur
                iny
                lda (afbest),y
                sta afcur+1

                lda (afcur),y           ; best(0) = cur(0)
                sta (afbest),y
                dey
                lda (afcur),y
                sta (afbest),y
                rts

    ; found entry of right size

_afl9           ldy #0
                lda (afcur),y
                sta (aflast),y
                iny
                lda (afcur),y
                sta (aflast),y
_afl10          rts

    ; split best block

_afl11          ldy #0
                lda (afbest),y          ; cur = best(0)
                sta afcur
                clc
                adc afsize
                sta (afbest),y          ; best(0)=cur+size
                sta aflast              ; last = cur + size
                iny
                lda (afbest),y
                sta afcur+1
                adc afsize+1
                sta (afbest),y
                sta aflast+1

                iny
                sec
                lda (afcur),y
                sbc afsize              ; last(1) = bsze-size
                sta (aflast),y
                lda afsize
                sta (afcur),y           ; cur(1) = size
                iny
                lda (afcur),y
                sbc afsize+1
                sta (aflast),y
                lda afsize+1
                sta (afcur),y
                clc
                bcc _afl9
                .endproc


;    Free(block)
;    -----------
free            .proc
                sta afbest
                stx afbest+1

free1           lda #<afbase            ; cur = base
                sta afcur
                lda #>afbase
                sta afcur+1

_afl12          lda afcur               ; last = cur
                sta aflast
                lda afcur+1
                sta aflast+1

                ldy #0
                lda (aflast),y          ; cur = last(0)
                sta afcur
                cmp afbest
                iny
                lda (aflast),y
                sta afcur+1
                sbc afbest+1
                bcs _afl13              ; while cur ULS block
                lda afcur+1             ; and cur # 0
                bne _afl12
                lda afcur
                bne _afl12

_afl13          iny
                clc
                lda (afbest),y
                adc afbest
                tax
                iny
                lda (afbest),y
                and #$7f                ; clear tag flag
                sta (afbest),y
                adc afbest+1
                cmp afcur+1
                bne _afl15
                cpx afcur               ; if cur =
                bne _afl15              ;  (block + block(1))
                dey
                clc                     ; block(1) =
                lda (afbest),y          ;  block(1) +  cur(1)
                adc (afcur),y
                sta (afbest),y
                iny
                lda (afbest),y
                adc (afcur),y
                sta (afbest),y
                ldy #0                  ; block(0) = cur(0)
                lda (afcur),y
                sta (afbest),y
                iny
                lda (afcur),y
                sta (afbest),y
                clc
                bcc _afl16

;:AFL14 PLP
_afl15          ldy #0                  ; block(0) = cur
                lda afcur
                sta (afbest),y
                iny
                lda afcur+1
                sta (afbest),y

_afl16          iny                     ; if block =
                clc                     ; (last + last(1))
                lda aflast
                adc (aflast),y
                tax
                iny
                lda aflast+1
                adc (aflast),y
                cmp afbest+1
                bne _afl18
                cpx afbest
                bne _afl18

                clc                     ; last(1) =
                dey                     ;   last(1)+block(1)
                lda (aflast),y
                adc (afbest),y
                sta (aflast),y
                iny
                lda (aflast),y
                adc (afbest),y
                sta (aflast),y

                ldy #0                  ; last(0) = block(0)
                lda (afbest),y
                sta (aflast),y
                iny
                lda (afbest),y
                sta (aflast),y
                rts

;:AFL17 PLP
_afl18          ldy #0                  ; last(0) = block
                lda afbest
                sta (aflast),y
                iny
                lda afbest+1
                sta (aflast),y
                rts
                .endproc


;         EDIT.MEM

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified March 9, 1983


;    GetMem(size)
;    ------------
getmem          .proc
                clc
                adc #4
                sta afsize
                bcc _gm0
                inx
_gm0            stx afsize+1
_gm1            jsr allocate+4

                ldx afcur+1
                beq gmerr               ; no memory allocated !
                clc
                lda afcur
                adc #4
                bcc _gm2
                inx

_gm2            rts
                .endproc

gmerr           .proc
                ldy #0
                jsr syserr

                lda sparem
                ldx sparem+1
                ldy allocerr
                bne punt                ; really out of memory
                inc allocerr
                jsr free
                jmp getmem._gm1         ; retry

punt            jsr savewd              ; we're in big trouble
                jmp rstwnd
                .endproc


;    FreeMem(addr)
;    -------------
freemem         .proc
                sec
                sbc #4
                bcs freem1
                dex
freem1          jmp free
                .endproc

;    InstB()
;    -------
instb           .proc
                lda cur
                sta arg3
                lda cur+1
                sta arg4
                jsr instbuf
                sta cur
                stx cur+1
                rts
                .endproc


;    InstBuf(,,up)
;    -------------
instbuf         .proc
                ldy #0
                lda (buf),y
                ldx buf
                ldy buf+1
                .endproc


;    InstLn(sze,sloc,up)
;    -------------------
instln          .proc
                sta arg0                ; save sze
                stx arg1                ; save sloc
                sty arg2
                clc
                adc #3
                ldx #0
                jsr getmem

                clc
                adc #2
                sta arg5
                txa
                adc #0
                sta arg6
                ldy arg0
                beq _il1a
_il1            lda (arg1),y
                sta (arg5),y
                dey
                bne _il1
_il1a           lda arg0
                sta (arg5),y

                lda arg4
                bne _il4                ; up # 0

                lda top                 ; down _= top
                sta arg5
                ldy #4                  ; AFcur(2) _= down
                sta (afcur),y
                lda top+1
                sta arg6
                iny
                sta (afcur),y

                lda afcur               ; top _= AFcur
                sta top
                lda afcur+1
                sta top+1

                ldy #0                  ; AFcur(0) _= 0
                tya
                sta (afcur),y
                iny
                sta (afcur),y

_il2            lda arg6
                bne _il3                ; down # 0
                lda afcur               ; bot _= AFcur
                sta bot
                ldx afcur+1
                stx bot+1
                rts

_il3            ldy #1
                lda afcur+1             ; @down _= AFcur
                sta (arg5),y
                dey
                lda afcur
                sta (arg5),y
                ldx afcur+1
                rts

_il4            ldy #4
                lda (arg3),y
                sta arg5                ; down _= Next(up)
                sta (afcur),y           ; AFcur(2) _= down
                lda afcur
                sta (arg3),y            ; up(2) _= AFcur
                iny
                lda (arg3),y
                sta arg6
                sta (afcur),y
                lda afcur+1
                sta (arg3),y

                ldy #0
                lda arg3
                sta (afcur),y
                iny
                lda arg4
                sta (afcur),y

                jmp _il2
                .endproc


;    DelCur()
;    --------
delcur          .proc
                lda cur
                ldx cur+1
                jsr delln
                sta cur
                stx cur+1
dln1            rts
                .endproc


;    DelLn(lineptr)
;    --------------
delln           .proc
                cpx #0
                beq delcur.dln1
                sta arg0
                stx arg1
                ldy #4
                lda (arg0),y
                sta arg4                ; down _= Next(ptr)
                iny
                lda (arg0),y
                sta arg5

                ldy #0
                lda (arg0),y
                sta arg2                ; up _= Prev(ptr)
                iny
                lda (arg0),y
                sta arg3

                bne _dln2               ; up # 0
                lda arg4
                sta top                 ; top _= down
                lda arg5
                sta top+1
                jmp _dln3

_dln2           ldy #4
                lda arg4
                sta (arg2),y            ; up(2) _= down
                iny
                lda arg5
                sta (arg2),y

_dln3           lda arg5
                bne _dln4               ; down # 0
                lda arg2
                sta bot                 ; bot _= up
                lda arg3
                sta bot+1
                jmp _dln5

_dln4           ldy #0
                lda arg2
                sta (arg4),y            ; down(0) _= up
                iny
                lda arg3
                sta (arg4),y

_dln5           lda arg0
                ldx arg1
                jsr free

                lda arg2
                ldx arg3
                rts
                .endproc


;         EDIT.CAR

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 29, 1983


emjmps          rts                     ; Seg catch all

                .word 0
                .byte ebank             ; curBank
                .byte $df               ; stMask
                jmp splerr              ; Error

                .byte 18                ; wSize
                .byte 120               ; line input max
                .byte $20               ; ChCvt2
                rts                     ; Exp catch all

                .word 0
                rts                     ; Dcl catch all

                .word 0
                rts                     ; CodeGen catch all

                .word 0
                rts                     ; ArrRef Catch all
zero            .word 0
                rts                     ; SPLEnd

                .word 0
                jmp scrbell             ; Alarm

                .byte 0                 ; EOLch (default = space)
ltab            .word lsh1._lshift      ; LSH
                .word rshift
                .word multi
                .word divi
                .word remi
                .word sargs
                .byte $60               ; ChCvt3
                .byte $22               ; tvDisp

                jmp insrtch             ; normal char
                rts                     ; ctrl-shift char
serial          .word 0                 ; serial number of ROM
    ; to be filled in before blowing ROM

                jmp getnext.ismt        ; STM catch all
                rts                     ; illegal monitor cmd

                .byte $86
                .byte $9d
                .word istmres           ; STMrAdr in EDIT.DEF

;Init RTS

start           .proc
                jsr initkeys            ; get keyboard
                lda warmst
                beq cold
                lda chcvt3
                cmp #$60                ; make sure RAM initialized
                bne cold

_warm           lda mpc                 ; see where we were
                beq _w1
                jmp monitor._mon1       ; monitor
_w1             jmp gmerr.punt          ; editor

cold            lda #0
                tay
_c0             sta $0480,y             ; zero RAM
                dey
                bne _c0

                ldy #$3a
_cold1          lda emjmps-1,y          ; init RAM
                dey
                sta jmps,y
                bne _cold1
    ; STY ChCvt1 ; Y=0

                jsr einit               ; init editor

;SPLInit PROC ; init compiler RAM
        .if ramzap
                jsr zap4
        .else
                nop
                nop
                nop
        .endif
                ldx #8                  ; 2K id space
                stx stsp

                lda #0
                ldx #4
                ldy bigst               ; big s.t. ?
                beq _si1                ; no
                ldx #6
_si1            jsr getmem              ; get hash table
                sta stglobal            ; qglobal hash table
                stx stglobal+1
                ldy bigst               ; big s.t. ?
                beq _si2                ; no
                inx
                inx
                sta stg2                ; big s.t. hash table
                stx stg2+1
_si2            inx
                inx
                sta stlocal             ; local hash table
                stx stlocal+1
                .endproc

; RTS


;         EDIT.MAN

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 27, 1983



; Main program for EDIT/FLASH

floop           .proc
                lda allocerr
                beq _fm1
                lda #<outmem
                ldx #>outmem
                jsr cmdmsg

_fm1            lda curch
                sta lastch
                jsr getkey

                jsr einit.fcmsg1
                lda curch

                ldy kbcode
                cpy #$c0                ; Ctrl-Shft
                bcs _fmcs

                ldy lastch
                cpy #$1b                ; escape
                bne _fmch
                cmp #eol
                beq floop
                jsr insrtch
                jmp floop

_fmcs           ldx #<fmcscmd
                ldy #>fmcscmd
                bne _fmlu

_fmch           ldx #<fmcmd
                ldy #>fmcmd

_fmlu           jsr lookup
                jmp floop
                .endproc

fmcmd           .word disptb            ; default routine
                .byte 50                ; table size
                .word scrlup
                .byte $1c
                .word scrldwn
                .byte $1d
                .word scrlrt
                .byte $1f
                .word scrllft
zap2            .byte $1e
                .word delch
zap3            .byte $fe
                .word backsp
zap4            .byte $7e
                .word insrtch
                .byte $60
                .word insrtsp
                .byte $ff
                .word return
                .byte eol
                .word tab
                .byte $7f
                .word delete
                .byte $9c
                .word botln.escape
                .byte $1b
                .word clear
                .byte $7d
                .word insrt
                .byte $9d
                .word settab
                .byte $9f
                .word clrtab
                .byte $9e


fmcscmd         .word disptb+3          ; default
                .byte 71                ; table size
                .word front
                .byte $f6
                .word back
                .byte $f7
                .word pgup
                .byte $ce
                .word pgdwn
                .byte $cf
                .word indntl
                .byte $e0
                .word indntr
                .byte $e2
                .word fread
                .byte $e8
                .word fwrite
                .byte $ee
                .word paste
                .byte $ca
                .word insrtt
                .byte $cd
                .word monitor
                .byte $e5
                .word find
                .byte $f8
                .word subst
                .byte $fe
                .word wind1
                .byte $df
                .word wind2
                .byte $de
                .word delwd
                .byte $fa
                .word csbs
                .byte $f4
                .word csret
                .byte $cc
                .word undo
                .byte $cb
                .word topln
                .byte $f9
                .word endln
                .byte $ea
                .word settag
                .byte $ed
                .word loctag
                .byte $fd


outmem          .text 14," "
                .text +$80,"Out"
                .text " "
                .text +$80,"of"
                .text " "
                .text +$80,"Memory"


;         EDIT.CHR

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified September 8, 1983


;    InsrtCh() char in curCh
;    ---------
insrtch         .proc
                jsr setsp
_padbuf         ldy #0
                lda (buf),y
                cmp linemax
                bcc _pbuf0              ; test line too long
                jsr scrbell
                ldy #0
                lda (buf),y
_pbuf0          cmp sp
                bcs _pbuf2
                sta arg0
                lda sp
                sta (buf),y
                ldy arg0
                lda #$20                ; pad with spaces
_pbuf1          iny
                sta (buf),y
                cpy sp
                bcc _pbuf1
_pbret          ldy sp
                lda curch
                sta (buf),y
                lda #$ff
                sta dirtyf
                jsr dspbuf
                jmp scrlrt

_pbuf2          ldx insert
                beq _pbret
    ; move buf right one char
_movert         adc #0                  ; really 1, carry set
                sta (buf),y
                tay
_mrt1           dey
                lda (buf),y
                iny
                sta (buf),y
                dey
                cpy sp
                bne _mrt1
                beq _pbret
                .endproc


;    InsrtSp()
;    ---------
insrtsp         .proc
                lda insert
                pha
                lda #$20
                sta insert
                sta curch
                jsr insrtch
                pla
                sta insert
                jmp scrlft
                .endproc


;    Insrt()
;    -------
insrt           .proc
                jsr clnln
                jsr nextup
                sta cur+1               ; tricky
                jsr insert2
                lda #0
                jmp newpage.npage1

insert2         lda #0
                tay
insert3         sta (buf),y
                iny
                sty dirty
                jmp instb
                .endproc


csret           .proc
    ; handle pad if any
                jsr insrtsp
                jsr delch

                ldy #0
                lda (buf),y
                pha
                jsr setsp
                sta dirtyf              ; always non-zero
                sec
                sbc #1
                sta (buf),y
                jsr clnln

                pla
                sta arg1
                inc arg1
                lda #0
                sta arg0
                beq _csr2
_csr1           lda (buf),y
                inc arg0
                ldy arg0
                sta (buf),y
                inc sp
_csr2           ldy sp
                cpy arg1
                bcc _csr1

                ldy #0
                lda arg0
                jsr insrt.insert3
                jsr nextup
                jsr refresh
                jmp return.ret2
                .endproc


;    Return()
;    --------
return          .proc
                ldx insert
                bne csret
                jsr chkdwn
                bne ret2
_ret1           jsr insrt.insert2
                jsr nextup
                jsr ldbuf
ret2            jsr scrldwn
ret3            jmp front
                .endproc


;    Delete()
;    --------
delete          .proc
                jsr clnln
                lda delbuf
                ldx delbuf+1
                stx dirty
                ldy lastch
                cpy #$9c
                beq _del1
                jsr delfree
_del1           sta arg3
                stx arg4
                jsr instbuf
                jsr chkdwn              ; last line ?
                bne _del2               ; no, delete it
                tay
                sta (buf),y
                iny
                sty dirtyf
                bne return.ret3         ; uncond.

_del2           jsr delcur
                beq _del3
                jsr nextdwn
_del3           jsr chkcur
                lda #0
                jmp newpage.npage1
                .endproc


;    DelTop()
;    --------
deltop          .proc
                lda delbuf+4
                ldx delbuf+5
                sta delnxt
                stx delnxt+1
                .endproc


;    DelEnd(ptr)
;    -----------
delend          .proc
                cmp #<delbuf
                bne de1
                cpx #>delbuf
de1             rts
                .endproc


;    DelFree(bot)
;    ------------
delfree         .proc
                jsr delend
                beq delend.de1
                jsr delln
                bne delfree             ; uncond.
                .endproc


;    DelNext()
;    ---------
delnext         .proc
                ldy #5
                lda (delnxt),y
                tax
                dey
                lda (delnxt),y
                sta delnxt
                stx delnxt+1
                jmp delend
                .endproc


undo            .proc
                jsr ldbuf
                jmp front
                .endproc


;    DeleteCh()
;    ----------
delch           .proc
                jsr chkcol
                bcc chkdwn.cdwn1
                ldy #0
                lda (buf),y
                sta dirtyf
                sec
                sbc #1
                sta (buf),y
                ldy sp
_dch1           iny
                lda (buf),y
                dey
                sta (buf),y
                iny
                cpy dirtyf
                bcc _dch1               ; really checking for =
                .endproc


;    RfrshBuf()
;    ----------
rfrshbuf        .proc
                jsr dspbuf
                jmp rstcol.lftrt
                .endproc


;    ChkDwn()
;    --------
chkdwn          .proc
                jsr clnln
                beq cdwn1
                ldy #5
                lda (cur),y
cdwn1           rts
                .endproc


;    BackSp()
;    --------
backsp          .proc
                jsr setsp
                cmp #2
                bcc chkdwn.cdwn1
bsp1            jsr scrllft
                jsr setsp
                tay
                lda #$20
                sta (buf),y
                sta dirtyf
                lda insert
                bne delch
                jmp rfrshbuf
                .endproc


csbs            .proc
                jsr setsp
                cmp #2
                bcs backsp.bsp1
                jsr chkcur
                beq chkdwn.cdwn1        ; no lines at all!
                ldy #1
                lda (cur),y
                beq chkdwn.cdwn1        ; no line to merge with
    ; merge
                jsr scrlup
                jsr back
                jsr nextdwn
                sta dirtyf
                jsr curstr
                clc
                ldy #0
                lda (buf),y
                sta arg2
                adc (arg0),y
                sta (buf),y
                lda (arg0),y
                beq _cb2
                sta arg3
_cb1            iny
                sty arg4
                lda (arg0),y
                inc arg2
                ldy arg2
                sta (buf),y
                ldy arg4
                cpy arg3
                bne _cb1
_cb2            jsr delcur
                jmp refresh
                .endproc


;         EDIT.INI

; Copyright 1983 by Action Computer Services
; All rights reserved.

; last modified May 19, 1983


minit           .proc                   ; initialize memory
                lda memlo
                sta afbase
                lda memlo+1
                sta afbase+1
                lda #0
                tay
                sta (afbase),y
                iny
                sta (afbase),y
                sec
                lda memtop
                sbc afbase
                iny
                sta (afbase),y
                lda memtop+1
                sbc afbase+1
                iny
                sta (afbase),y

                lda #0                  ; allocate 2 pages of
                ldx #2                  ; spare memory
                jsr allocate
                lda afcur
                sta sparem
                ldx afcur+1
                stx sparem+1
miret           rts
                .endproc


zerow           .proc                   ; initialize window
                lda #0
                ldx #15
_zw1            dex                     ; zero page0 window table
                sta sp,x
                bne _zw1
                sta dirtyf
                sta inbuf
                tay
                sta (buf),y
                rts
                .endproc


w2init          .proc                   ; W2Init()
                jsr ctrln
                lda wsize
                sta nlines
                sta cmdln
                jsr savworld
                lda #w2-w1
                sta numwd
                sta curwdw
                jsr zerow
                ldy wsize
                iny
                sty ytop
                sec
                lda #23
                sbc wsize
                sta nlines
                bne einit.fcmsg         ; uncond.
                .endproc


einit           .proc
                jsr minit

                lda #0
                ldx #1
                jsr allocate            ; get edit buffer
                lda afcur
                sta buf
                ldx afcur+1
                stx buf+1

                lda #$40
                sta chcvt

                lda #<delbuf
                sta delbuf
                sta delbuf+4
                lda #>delbuf
                sta delbuf+1
                sta delbuf+5

winit           jsr zerow

winit1          lda #23
                sta nlines
                sta cmdln
                lda #0
                sta curwdw
                sta ytop

fcmsg           jsr ctrln
fcmsg1          lda #<editc
                ldx #>editc
                jmp cmdmsg

editc           .text 19,"ACTION! (c)1983 ACS"
                .endproc


;         EDIT.IO

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 29, 1983



;    GetStr(prompt, str, invert)
;    ---------------------------
getstr          .proc
                jsr dspstr
_gs1            jsr getkey
                tax

                cpx #$7e
                beq _gs3                ; backspace

                cpx #$7d
                beq _gs3                ; clear

_gs1a           ldy #0
                clc
                lda (arg12),y
                adc #1

                cpx #$1b                ; ESC
                beq _gs1b

                cpx #eol
                beq _gs2

                cpy arg3                ; first char?
                beq _gs3                ; yes, clear line

                stx arg3
                ldx colcrs
                cpx rmargin
                bcs _gs1                ; don't go off screen

                sta (arg12),y
                tay
                lda arg3
                sta (arg12),y
                eor arg2
                jsr scrch
                jmp _gs1

_gs1b           lda #0
                sta curch
                sta (arg12),y
                iny
                tya
_gs2            tay
                txa                     ; EOL
                sta (arg12),y
                rts

_gs3            stx arg3
_gs4            ldy #0
                lda (arg12),y
                beq _gs5
                sec
                sbc #1
                sta (arg12),y

                jsr scrlft

                lda #$20
                eor arg2
                jsr scrch

                jsr scrlft
                ldx arg3
                cpx #$7e
                bne _gs4
                beq _gs1

_gs5            cpx #$7d
                beq _gs1
                bne _gs1a
                .endproc


;    FRead()
;    -------
fread           .proc
                lda #0
                sta inbuf
                lda #<rdmsg
                ldx #>rdmsg
                ldy #4
                jsr fopen

_frd1           lda #1
                jsr rdbuf
                bmi _fr3
                jsr instb
                lda allocerr
                beq _frd1
                ldy #22                 ; file to big
                bne _fr4

_fr3            cpy #$88                ; EOF
                beq _fr5
_fr4            jsr syserr
_fr5            jsr fwrite._fw2
                jmp ctrln

rdmsg           .text 6,"Read? "
                .endproc


;    FWrite()
;    --------
fwrite          .proc
                lda #<wrtmsg
                ldx #>wrtmsg
                ldy #8
                jsr fopen

                jsr chkcur._ldtop
                beq _fw3
_fw1            jsr ldbuf

    ; INC COLOR4 ; let user know we're here
                nop
                nop
                nop

                lda #1
                jsr wrtbuf
                bmi _fw3
                jsr nextdwn
                bne _fw1

                lda #0
                sta dirty
_fw2            lda #1
                jsr close
                jsr rstcur
                jmp dspon

_fw3            jsr syserr
                jmp _fw2

wrtmsg          .text 7,"Write? "
                .endproc


;    FOpen(prompt, mode)
;    -------------------
fopen           .proc
                sta arg10
                stx arg11
                sty opmode

    ; JSR ClnLn ; in SaveWd
                jsr savewd
                jsr rstcsr

                ldy #<inbuf
                lda #>inbuf
                sta arg3
                lda arg10
                ldx arg11
                jsr cmdstr

                lda #1
                jsr close
                ldy inbuf
                beq _fo7
                ldx opmode
                lda #':'
                cmp inbuf+2
                beq _fo2
                cmp inbuf+3
                beq _fo2
                iny
                iny
                sty inbuf
_fo1            lda inbuf,y
                sta inbuf+2,y
                dey
                bne _fo1
                lda #':'
                sta inbuf+2
                bne _fo3                ; uncond.

_fo2            lda inbuf+1
                cmp #'?'                ; read directory?
                bne _fo4                ; no
                ldx #6
_fo3            lda #'D'
                sta inbuf+1

_fo4            stx arg3
                jsr dspoff
                lda #1
                sta arg4                ; clear high bit for cassette
                ldx #<inbuf
                ldy #>inbuf
                jsr open
                bmi _fo6
                lda arg3                ; see if directory
                eor #6
                bne _fo5
                sta inbuf               ; clear inbuf
_fo5            rts

_fo6            pla
                pla                     ; pop return
                jmp syserr

_fo7            pla
                pla
                rts
                .endproc

initkeys        .proc
                lda #7
                jsr close

                lda #4
                sta arg3                ; read only
                lda #7
                ldx #<keybd
                ldy #>keybd
                jmp open

keybd           .text 2,"K:"
                .endproc


gotkey          .proc
    ; Test if key in buffer
                lda ch                  ; key down?
                eor #$ff
                rts
                .endproc


;         EDIT.WND

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 6, 1983


;    Wind1()
;    -------
wind1           .proc
                lda curwdw
                beq savworld.wdret
                lda #0
                pha
                .endproc


;    SwapWd()
;    --------
swapwd          .proc
                jsr savworld
                pla
                jmp rstworld
                .endproc


;    Wind2()
;    -------
wind2           .proc
                lda curwdw
                bne savworld.wdret
                lda numwd
                bne _w2
                jmp w2init

_w2             lda #w2-w1
                pha
                bne swapwd              ; uncond.
                .endproc


;    SavWorld()
;    ----------
savworld        .proc
                jsr clnln
                jsr savecol
                jsr rstcsr
                jsr setsp
                jmp savewd

wdret           rts
                .endproc


;    Clear()
;    -------
clear           .proc
                jsr alarm
                lda #<delwd.clearmsg
                ldx #>delwd.clearmsg
                jsr yesno
                bne savworld.wdret
clr0            jsr clnln
                lda dirty
                beq clr1
    ; JSR Alarm
                lda #<delwd.dirtymsg
                ldx #>delwd.dirtymsg
                jsr yesno
                bne savworld.wdret
clr1            jsr freetags            ; get rid of tags
                lda bot
                ldx bot+1
_clr2           jsr delln
                bne _clr2
                stx cur+1
                stx dirty
                stx dirtyf
                stx inbuf
                jmp newpage
                .endproc


;    RstWorld(window)
;    ----------------
rstworld        .proc
                sta curwdw
                jsr rstwd
                jsr ldbuf
                jmp rstcol
                .endproc


;    DelWd()
;    -------
delwd           .proc
                lda numwd
                beq savworld.wdret
                jsr alarm
                lda #<delmsg
                ldx #>delmsg
                jsr yesno
                bne savworld.wdret
_dw1            jsr clear.clr0
                lda dirty
                bne savworld.wdret
                ldy #0
                sty numwd
                cpy curwdw
                bne delwd2
                ldy #w2-w1
delwd2          sty curwdw
                jsr rstwd
                jmp einit.winit1


clearmsg        .text 7,"CLEAR? "
delmsg          .text 15,"Delete window? "
dirtymsg        .text 19,"Not saved, Delete? "
                .endproc


;    GetTemp(msg)
;    ------------
gettemp         .proc
                ldy #0
gett1           sty tempbuf
                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf
                .endproc


;    CmdStr(msg, buf)
;    ----------------
cmdstr          .proc
                sta arg0
                sty arg2
                jsr cmdcol
                lda #$80
                sta arg4
                lda arg0
                ldy arg2
                jsr getstr
                jsr rstcsr
                jmp rstcol
                .endproc


;    YesNo(msg)
;    ----------
yesno           .proc
                jsr gettemp
                ldy tempbuf
                bne _yn1
                iny
                rts

_yn1            lda tempbuf+1
                ora #$20
                cmp #'y'
                rts
                .endproc


;         EDIT.DSP

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 29, 1983


;    CmdMsg(msg)
;    -----------
cmdmsg          .proc
                sta arg0
                jsr cmdcol
                lda #0
                sta arg3
                lda arg0
                ldy #$80
                jsr putstr
                jmp rstcol
                .endproc


;    ClnLn()
;    -------
clnln           .proc
                jsr chkcur
                lda dirtyf
                beq _cll1
                sta dirty
                lda #0
                sta dirtyf
                jsr delcur
                jsr instb
_cll1           jmp chkcur
                .endproc


;    SaveWd()
;    --------
savewd          .proc
                jsr clnln
savwd1          clc
                lda #14
                tax
                adc curwdw
                tay
_sw1            lda sp,x
                sta w1,y
                dey
                dex
                bpl _sw1
                rts
                .endproc


;    RstWd() restore window
;    -------
rstwd           .proc
                clc
                lda #14
                tax
                adc curwdw
                tay
_rw1            lda w1,y
                sta sp,x
                dey
                dex
                bpl _rw1
rw2             rts
                .endproc


endln           .proc
                jsr clnln
                lda bot
                sta cur
                lda bot+1
                sta cur+1
    ; falls into CtrLn
                .endproc


;    CtrLn() center line
;    -------------------
ctrln           .proc
                lda #0
                sta temps
                jsr clnln
                beq _cl0
                jsr nextup
                beq _cl0
                inc temps
                jsr nextup
                beq _cl0
                inc temps
_cl0            jsr newpage
_cl1            lda temps
                beq rstwd.rw2
                jsr scrldwn
                dec temps
                jmp _cl1
                .endproc


;    TopLn()
;    -------
topln           .proc
                jsr clnln
                jsr chkcur._ldtop
    ; falls into NewPage
                .endproc


;    NewPage()
;    ---------
newpage         .proc
                lda #0
                sta lnum
npage1          sta choff
                jsr rstcsr              ; for command line
                lda lmargin
                sta colcrs
    ; JMP Refresh ; do all the work
                .endproc


;    Refresh()
;    ---------
refresh         .proc
                clc
                lda ytop
                adc lnum
                sta rowcrs
                jsr savecol
                jsr savewd
                inc rowcrs
                jsr nextdwn
                sta arg9
                clc
                lda nlines
                sbc lnum
                sta arg10
                beq _cl4

_cl1            ldy #0
                lda indent
                sta arg3
                ldx arg9
                beq _cl5
                jsr curstr

_cl2            jsr putstr
                lda arg9
                bne _cl3
                tay
                sta (arg0),y
_cl3            inc rowcrs
                jsr nextdwn
                sta arg9
                dec arg10
                bne _cl1

_cl4            jsr rstcur
                jsr rstcol
                jmp rfrshbuf

_cl5            lda #<zero
                ldx #>zero
                bne _cl2                ; uncond.
                .endproc


;         EDIT.CMD

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified March 10, 1983


;    Front()
;    -------
front           .proc
                sec
                lda #0
                sbc indent
                sta choff
                jsr dspbuf
                lda lmargin
                jmp rstcol+6
                .endproc


;    Back()
;    ------
back            .proc
                ldy #0
                lda (buf),y
back0           pha
                clc
                adc lmargin
                sec
                sbc rmargin
                bcs _back1
                lda #1
_back1          sbc indent
                sta choff
                jsr dspbuf
                sec
                pla
                sbc indent
                sec
                sbc choff
                clc
                adc lmargin
                jmp rstcol+6
                .endproc


;    PgUp()
;    ------
pgup            .proc
                sec
                lda lnum
                sbc #2
                ldy #1
                bne page
                .endproc


;    PgDwn()
;    -------
pgdwn           .proc
                ldy #5
                sec
                lda #2
                sbc lnum
                .endproc


page            .proc
                clc
                adc nlines
                sta arg14
                dec arg14
                beq _page2
                sty arg13
                jsr clnln
_page1          ldy arg13
                jsr next
                dec arg14
                bne _page1
_page2          jmp ctrln
                .endproc


;    Paste()
;    -------
paste           .proc
                jsr deltop
                beq _pret
                stx dirty
                jsr clnln
                jsr nextup
                sta cur+1               ; tricky, fake out top
                jsr savewd.savwd1

                jsr deltop
_p1             jsr strptr
                jsr ldbuf.ldbuf1
                jsr instb
                lda allocerr
                bne _p2                 ; check for out of memory
                jsr delnext
                bne _p1

_p2             jsr rstcur
                ldy curwdw
                lda w1+wcur+1,y
                beq _p3
                jsr nextdwn
_p3             lda #0
                jmp newpage.npage1

_pret           rts
                .endproc


;    old IndentL()
;        ---------
indntl          .proc
                lda indent
                beq scrlinit.putrtn
                dec indent
                jmp ctrln
                .endproc


;    old IndentR()
;        ---------
indntr          .proc
                lda indent
                bmi scrlinit.putrtn
                inc indent
                jmp ctrln
                .endproc


;    InsrtT() insert/replace toggle
;    --------
insrtt          .proc            ; was InsertT
                lda #<_rmsg
                ldx #>_rmsg
                inc insert
                beq _it1
                lda #$ff
                sta insert
                lda #<_imsg
                ldx #>_imsg
_it1            jmp cmdmsg

_imsg           .text 6,"INSERT"
_rmsg           .text 7,"REPLACE"
                .endproc


scrlinit        .proc
                sty arg13
                jsr clnln
                beq _siret

                ldy arg13
                jsr next
                beq _siret              ; EOF

                lda colcrs
                sta x
    ; LDA choff
    ; BEQ _SI1
                lda #0
                sta choff
                jsr dspbuf
_si1            jmp ldbuf

_siret          pla
                pla
putrtn          rts
                .endproc


;    ScrlUp()
;    --------
scrlup          .proc
                ldy #1
                jsr scrlinit
                dec lnum
                bmi _su2
                jmp scrup

_su2            inc lnum
                lda ytop
                sta y
                jsr botln
                lda nlines
                jsr movedwn
                jsr rstcol
                jmp rfrshbuf
                .endproc


;    ScrlDwn()
;    ---------
scrldwn         .proc
                ldy #5
                jsr scrlinit
                ldx lnum
                inx
                cpx nlines
                beq _sd2
                stx lnum
                jmp scrdwn

_sd2            jsr botln
                stx y

                lda nlines
                ldx ytop
                jsr moveup

                jsr rstcol
                jsr dspbuf
                jmp rstcol
                .endproc


;    BotLn()
;    -------
botln           .proc
                clc
                lda ytop
                adc nlines
                tax
                dex
escape          rts
                .endproc


;    ChkCol()
;    --------
chkcol          .proc
                jsr setsp
                ldy #0
                lda (buf),y
                cmp sp
                bcs chkc1
                jsr back
                jsr setsp
                clc
chkc1           rts
                .endproc


;    ScrlLft()
;    ---------
scrllft         .proc
                jsr chkcol
                lda lmargin
                cmp colcrs
                bcc _sl1

                clc
                lda choff
                adc indent
                beq chkcol.chkc1

                dec choff
                jsr dspbuf
                jsr scrrt
_sl1            jmp scrlft
                .endproc


;    ScrlRt()
;    --------
scrlrt          .proc
                jsr chkcol
                bcc chkcol.chkc1

                lda colcrs
                cmp rmargin
                bcc _sr2

                inc choff
                jsr dspbuf
                jsr scrlft
_sr2            jmp scrrt
                .endproc


;    SetSp()
;    -------
setsp           .proc
                sec
                lda indent
                adc choff
                clc
                adc colcrs
                sec
                sbc lmargin
                sta sp
                rts
                .endproc


;    MoveDwn(cnt, row)
;    -----------------
movedwn         .proc
                ldy #+0-40
                sty arg5
                ldy #$ff
                bne move
                .endproc


;    MoveUp(cnt, row)
;    ----------------
moveup          .proc
                ldy #40
                sty arg5
                ldy #0
                .endproc


move            .proc
                sty arg6
                sta arg4
                stx rowcrs
                jsr rstcsr
                jsr dsploc              ; get display address

                ldx arg4
                dex
_mu1            lda arg0
                sta arg2
                clc
                adc arg5
                sta arg0
                lda arg1
                sta arg3
                adc arg6
                sta arg1

                ldy #39
_mu2            lda (arg0),y
                sta (arg2),y
                dey
                bpl _mu2

                dex
                bne _mu1
                rts
                .endproc


;         EDIT.TAG

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified March 16, 1983


;    SetTag()
;    --------
settag          .proc
                jsr tagid

                lda tempbuf
                beq notag

                jsr clnln
                lda tempbuf+1
                jsr gettag
                bne _st1                ; tag already exists

    ; get a new tag
                lda #8
                jsr allocate
                ldy #1
                lda taglist+1
                sta (afcur),y
                dey
                lda taglist
                sta (afcur),y
                lda afcur
                sta taglist
                ldx afcur+1
                stx taglist+1

_st1            ldy #4
                lda tempbuf+1
                sta (afcur),y
                iny
                lda cur
                sta (afcur),y
                iny
                lda cur+1
                sta (afcur),y
                iny
                jsr setsp
                sta (afcur),y
    ; flag line as taged
                ldy #3
                lda (cur),y
                ora #$80
                sta (cur),y
                rts
                .endproc


;    NoTag()
;    -------
notag           .proc
                lda #<_ntmsg
                ldx #>_ntmsg
                jmp cmdmsg

_ntmsg          .text 11,"tag not set"
                .endproc


;    TagId()
;    -------
tagid           .proc
                lda #<_stmsg
                ldx #>_stmsg
                jmp gettemp

_stmsg          .text 8,"tag id: "
                .endproc


;    LocTag()
;    --------
loctag          .proc
                jsr tagid
                lda tempbuf
                beq gettag._ltret
                jsr clnln
                lda tempbuf+1
                jsr gettag
                beq notag
                ldy #6
                lda (afcur),y
                tax
                dey
                lda (afcur),y
                jsr findln
                beq notag
                ldy #3
                lda (arg2),y
                bpl notag
                ldy #7
                lda (afcur),y
                sta sp
                lda arg2
                sta cur
                ldx arg3
                stx cur+1
                jmp found
                .endproc


;    GetTag PROC ; GetTag(tag)
;                  -----------
gettag          .proc
                sta arg0
                lda taglist
                ldx taglist+1
                bne _gt2
_ltret          rts

_gt1            ldy #4
                lda (afcur),y
                cmp arg0
                beq _gt3
                ldy #1
                lda (afcur),y
                tax
                dey
                lda (afcur),y
_gt2            sta afcur
                stx afcur+1
                txa
                bne _gt1
_gt3            ldx afcur+1
                rts
                .endproc


;    FreeTags()
;    ----------
freetags        .proc
                lda taglist
                ldx taglist+1
                beq _ft2
_ft1            sta afbest
                stx afbest+1
                ldy #0
                lda (afbest),y
                sta arg0
                iny
                lda (afbest),y
                sta arg1
                jsr free.free1
                lda arg0
                ldx arg1
                bne _ft1
                stx taglist+1
_ft2            rts
                .endproc


;    FindLn(line)
;    ------------
findln          .proc
                sta arg0
                stx arg1
                lda top
                ldx top+1
                bne _fl2
                rts

_fl1            ldy #5
                lda (arg2),y
                tax
                dey
                lda (arg2),y
_fl2            sta arg2
                stx arg3
                cmp arg0
                bne _fl3
                cpx arg1
                beq _fl4
_fl3            txa
                bne _fl1
_fl4            ldx arg3
                rts
                .endproc
