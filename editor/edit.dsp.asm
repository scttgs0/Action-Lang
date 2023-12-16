
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.dsp.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   CmdMsg(msg)
;======================================
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


;======================================
;   ClnLn()
;======================================
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


;======================================
;   SaveWd()
;======================================
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


;======================================
;   RstWd() restore window
;======================================
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


;======================================
;
;======================================
endln           .proc
                jsr clnln

                lda bot
                sta cur
                lda bot+1
                sta cur+1

    ; falls into CtrLn
                .endproc


;======================================
;   CtrLn() center line
;======================================
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


;======================================
;   TopLn()
;======================================
topln           .proc
                jsr clnln
                jsr chkcur._ldtop

    ; falls into NewPage
                .endproc


;======================================
;   NewPage()
;======================================
newpage         .proc
                lda #0
                sta lnum
npage1          sta choff
                jsr rstcsr              ; for command line

                lda lmargin
                sta colcrs

    ; JMP Refresh ; do all the work
                .endproc


;======================================
;   Refresh()
;======================================
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
                bne _cl2                ; [unc]

                .endproc
