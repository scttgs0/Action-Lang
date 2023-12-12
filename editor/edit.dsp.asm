
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
                beq _XIT

                sta dirty

                lda #0
                sta dirtyf

                jsr delcur
                jsr instb

_XIT            jmp chkcur

                .endproc


;======================================
;   SaveWd()
;======================================
savewd          .proc
                jsr clnln

_ENTRY1         clc
                lda #14
                tax
                adc curwdw

                tay
_next1          lda sp,X
                sta w1,Y

                dey
                dex
                bpl _next1

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
_next1          lda w1,Y
                sta sp,X

                dey
                dex
                bpl _next1

_XIT             rts
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

                .endproc

                ;[fall-through]


;======================================
;   CtrLn() center line
;======================================
ctrln           .proc
                lda #0
                sta temps

                jsr clnln
                beq _1

                jsr nextup
                beq _1

                inc temps

                jsr nextup
                beq _1

                inc temps

_1              jsr newpage

_next1          lda temps
                beq rstwd._XIT

                jsr scrldwn

                dec temps

                jmp _next1

                .endproc


;======================================
;   TopLn()
;======================================
topln           .proc
                jsr clnln
                jsr chkcur._ENTRY1

                .endproc

                ;[fall-through]


;======================================
;   NewPage()
;======================================
newpage         .proc
                lda #0
                sta lnum

_ENTRY1         sta choff

                jsr rstcsr              ; for command line

                lda lmargin
                sta colcrs

;               jmp Refresh             ; do all the work

                .endproc

                ;[fall-through]


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
                beq _2

_next1          ldy #0
                lda indent
                sta arg3

                ldx arg9
                beq _3

                jsr curstr

_next2          jsr putstr

                lda arg9
                bne _1

                tay
                sta (arg0),Y

_1              inc rowcrs

                jsr nextdwn

                sta arg9

                dec arg10
                bne _next1

_2              jsr rstcur
                jsr rstcol

                jmp rfrshbuf

_3              lda #<zero
                ldx #>zero

                bne _next2                ; [unc]

                .endproc
