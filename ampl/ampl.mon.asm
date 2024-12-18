
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.mon.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
; Monitor for ACTION!
;======================================
monitor         .proc
                jsr savworld

                lda delbuf              ; delete buffer bottom
                ldx delbuf+1
                jsr delfree             ; get rid of delete buf

                lda top+1
                sta top1

_mon1           jsr scrinit

                ldx #1
                stx rowcrs
                stx mpc

                dex
                stx cmdln
                stx device

                jsr splsetup

_mloop          jsr initkeys

                lda dindex              ; display mode
                beq _mon2

                jsr scrinit             ; get Graphics(0)

_mon2           jsr alarm
                jsr rstcsr

                lda #<monitorPrompt
                ldx #>monitorPrompt
                jsr gettemp

                ldy tempbuf
                beq _mloop

                lda #0
                sta top+1
                sta chan

                lda #<tempbuf
                ldx #>tempbuf
                ldy sp
                iny                     ; make sure non-zero
                jsr lexexpand.lexp1
                jsr getnext

                lda tempbuf+1
                ora #$20
                ldx #<mcmd
                ldy #>mcmd
                jsr lookup

                jmp _mloop

                .endproc


;--------------------------------------
;
;--------------------------------------
mquit           .proc
                ldy #0
                sty mpc
                sty subbuf
                sty findbuf
                sty dirtyf

                .endproc

                ;[fall-through]


;======================================
;   RSTwnd()
;======================================
rstwnd          .proc
                lda #23
                sta cmdln

                lda numwd
                beq _rw1

                lda wsize
                sta cmdln

                lda #w2-w1
                jsr paintw

                lda #0
_rw1            jsr paintw
                jsr einit.fcmsg1

                jmp floop

                .endproc


;======================================
;   PaintW(window)
;======================================
paintw          .proc
                sta curwdw

                jsr rstwd

                jmp found

                .endproc


;======================================
;   MDump()
;======================================
mdump           .proc
                jsr mprint

_md1            inc arg11
                bne _md2

                inc arg12

_md2            lda arg11
                ldx arg12
                jsr mprint._mp1
                jsr gotkey
                beq _md1

                ldx #$ff
                stx CH_
                cmp #$de
                bne _md1

                rts
                .endproc


;======================================
;   MPrint()
;======================================
mprint          .proc
                jsr mpsave

_mp1            jsr printc

                ldy #','
                jsr putchar

                lda arg11
                ldx arg12
                jsr printh
                jsr putsp

                ldy #'='
                jsr putchar
                jsr putsp
                jsr mpload

                tay
                jsr putchar
                jsr putsp
                jsr mpload
                jsr printh
                jsr putsp
                jsr mpload

                ldx #0
                jsr printc
                jsr putsp
                jsr mpload
                jsr printc

                jmp puteol

                .endproc


;======================================
;   MpLoad()
;======================================
mpload          .proc
                ldy #1
                lda (arg11),Y
                tax

                dey
                lda (arg11),Y

                rts
                .endproc


;======================================
;   MpSave()
;======================================
mpsave          .proc
                jsr mnum

                sta arg11
                stx arg12

                rts
                .endproc


;======================================
;   Boot()
;======================================
boot            .proc
                lda #<_bmsg
                ldx #>_bmsg
                jsr yesno
                bne mrun._mwrt1

                jmp start.cold

;--------------------------------------

_bmsg           .text 6,"Boot? "

                .endproc


;======================================
;   MRun()
;======================================
mrun            .proc
                lda nxttoken
                cmp #eofid
                beq _mr1

                cmp #quote              ; compile and go?
                bne _mr2                ;   no

                jsr comp

_mr1            lda INITAD
                ldx INITAD+1
                bne _mr3

_mwrt1          rts

_mr2            jsr mnum
_mr3            jsr run

                lda #0
                sta device

                rts
                .endproc


;======================================
;   MWrite()
;======================================
mwrite          .proc                   ; write object file
                lda nxttoken
                cmp #quote
                bne mrun._mwrt1         ; no output file!

                lda INITAD+1
                beq mrun._mwrt1         ; no program!!

                lda #1
                sta chan

                lda #8                  ; output
                jsr openchan

    ; write header
                lda #6
                sta arg9

                lda #$FF
                sta arg10               ; $FF
                sta arg11               ; $FF

                clc
                lda codebase            ; starting address
                adc codeoff
                sta arg12

                lda codebase+1
                adc codeoff+1
                sta arg13
                tax

                clc                     ; ending address
                lda arg12
                adc codesize
                sta arg14
                bne _mw2

                dex
_mw2            dec arg14

                txa
                adc codesize+1
                sta arg15

                jsr mwout

    ; write the qcode
                ldx #$10
                lda #$0B                ; output command
                sta IOCB0+ICCOM,X

                lda codebase
                sta IOCB0+ICBAL,X       ; buffer address
                lda codebase+1
                sta IOCB0+ICBAH,X

                lda codesize
                sta IOCB0+ICBLL,X       ; size
                lda codesize+1
                sta IOCB0+ICBLH,X

                jsr CIOV
                bmi mwout._mwerr

    ; save start address
                ldx #4
_mw3            lda _mwinit,X
                sta arg9,X

                dex
                bpl _mw3

                lda INITAD
                sta arg14
                lda INITAD+1
                sta arg15

                jsr mwout

    ; close file
                lda #1
                jmp close

;--------------------------------------

_mwinit         .byte 6
                .word INITAD
                .word INITAD+1

                .endproc


;======================================
;   MWOut()
;======================================
mwout           .proc
                lda #1
                ldx #arg9
                ldy #0
                jsr output

                bmi _mwerr

                rts


;======================================
;
;======================================
_mxerr          ldy #ender

_mwerr          jmp splerr


;======================================
;
;======================================
_mx                                     ; execute command line
                lda #0
                sta codeoff
                sta codeoff+1

                lda qcode
                pha
                lda qcode+1
                pha

                jsr getnext
                jsr cstmtlst

                cmp #eofid
                bne _mxerr

                lda #$60                ; RTS
                ldy #0
                sta (qcode),Y

                pla
                tax
                pla

                jmp run

                .endproc


;======================================
;   Comp()
;======================================
comp            .proc
                jsr splsetup
                jsr dspoff
                jsr compile

                jmp dspon

                .endproc


; see MAIN.BNK now
;Dret PROC ; Dret() go to DOS
;               lda DOSVEC
;               ldx DOSVEC+1
;               jmp JSRInd


;======================================
;   Proceed()
;======================================
proceed         .proc
                ldx procsp
                beq _XIT

;               lda #<_pmsg
;               ldx #>_pmsg
;               jsr YesNo
;               bne _XIT

;               ldx procSP              ; break stack pointer

                lda #0
                sta procsp

                txs

                jmp lproceed

_XIT            rts
                .endproc

;:Pmsg DEFMSG "Proceed? "


;======================================
;   PrintH(num)
;======================================
printh          .proc
                sta arg0
                stx arg1

                lda #4
                sta arg2

                ldy #'$'
                jsr putchar

_ph1            lda #0
                ldx #4
_ph2            asl arg0
                rol arg1
                rol a

                dex
                bne _ph2

                ; clc
                adc #'0'
                cmp #':'
                bmi _ph3

                adc #6
_ph3            tay
                jsr putchar

                dec arg2
                bne _ph1

                rts
                .endproc


;--------------------------------------
;--------------------------------------

mcmd            .word disptb+9          ; unknown cmd
                .byte 35                ; table size
                .word boot
                .byte 'b'
                .word comp
                .byte 'c'
                .word dret
                .byte 'd'
                .word mquit
                .byte 'e'

; .WORD Format
; .BYTE 'f

                .word options
                .byte 'o'
                .word proceed
                .byte 'p'
                .word mrun
                .byte 'r'
                .word mwrite
                .byte 'w'
                .word mwout._mx
                .byte 'x'
                .word mprint
                .byte '?'
                .word mdump
                .byte '*'

monitorPrompt   .byte 1,">"
