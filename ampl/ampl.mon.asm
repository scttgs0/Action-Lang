;======================================
;   FILE: ampl.mon.asm
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
; Monitor for ACTION!
;======================================
monitor         .proc
                jsr SaveWorld

                lda delbuf              ; delete buffer bottom
                ldx delbuf+1
                jsr DeleteFree             ; get rid of delete buf

                lda top+1
                sta top1
_mon1           jsr scrinit

                ldx #1
                stx ROWCRS
                stx isMonitorLive
                dex
                stx cmdln
                stx device
                jsr splsetup

_mloop          jsr InitKeys

                lda $03_0057 ;!! DINDEX              ; display mode
                beq _mon2

                jsr scrinit             ; get Graphics(0)
_mon2           jsr alarm
                jsr rstcsr

                lda #<monitorPrompt
                ldx #>monitorPrompt
                jsr GetTemp

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

_mquit          ldy #0
                sty isMonitorLive
                sty subbuf
                sty findbuf
                sty isDirty
                .endproc


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
                jsr EditorInit.fcmsg1

                jmp floop

                .endproc


;======================================
;   PaintW(window)
;======================================
paintw          .proc
                sta currentWindow
                jsr RestoreWindow

                jmp Found

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
                jsr GotKey
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
                lda (arg11),y
                tax
                dey
                lda (arg11),y
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
                jsr YesNo

                bne mrun._mwrt1

                jmp Start.cold

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
                bne _mr2                ; no

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
                lda #$ff
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
                lda #$0b                ; output command
                sta $03_0342,x ;!! IOCB0+ICCOM,x

                lda codebase
                sta $03_0344,x ;!! IOCB0+ICBAL,x       ; buffer address
                lda codebase+1
                sta $03_0345,x ;!! IOCB0+ICBAH,x

                lda codesize
                sta $03_0348,x ;!! IOCB0+ICBLL,x       ; size
                lda codesize+1
                sta $03_0349,x ;!! IOCB0+ICBLH,x

                jsr $03_E456 ;!! CIOV
                bmi mwout._mwerr

    ; save start address
                ldx #4
_mw3            lda _mwinit,x
                sta arg9,x
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
                .addr INITAD
                .addr INITAD+1
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
                sta (qcode),y
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
;     lda $03_000A    ;!! DOSVEC
;     ldx $03_000A+1  ;!! DOSVEC+1
;     jmp JSRInd


;======================================
;   Proceed()
;======================================
proceed         .proc
                ldx procsp
                beq _p1

        ; lda #<_pmsg
        ; ldx #>_pmsg
        ; jsr YesNo
        ; bne _p1
        ; ldx procSP  ; break stack pointer

                lda #0
                sta procsp
                txs
                jmp lproceed

_p1             rts
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

    ; CLC
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

mcmd            .addr disptb+9          ; unknown cmd
                .byte 35                ; table size
                .addr boot
                .byte 'b'
                .addr comp
                .byte 'c'
                .addr dret
                .byte 'd'
                .addr monitor._mquit
                .byte 'e'

; .WORD Format
; .BYTE 'f

                .addr options
                .byte 'o'
                .addr proceed
                .byte 'p'
                .addr mrun
                .byte 'r'
                .addr mwrite
                .byte 'w'
                .addr mwout._mx
                .byte 'x'
                .addr mprint
                .byte '?'
                .addr mdump
                .byte '*'

monitorPrompt   .byte 1,">"
