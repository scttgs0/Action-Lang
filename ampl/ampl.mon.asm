
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
                jsr savworld

                lda delbuf              ; delete buffer bottom
                ldx delbuf+1
                jsr delfree             ; get rid of delete buf

                lda top+1
                sta top1

_ENTRY1         jsr scrinit

                ldx #1
                stx rowcrs
                stx mpc

                dex
                stx cmdln
                stx device

                jsr splsetup

_ENTRY2
_next1          jsr initkeys

                lda dindex              ; display mode
                beq _1

                jsr scrinit             ; get Graphics(0)

_1              jsr alarm
                jsr rstcsr

                lda #<monitorPrompt
                ldx #>monitorPrompt
                jsr gettemp

                ldy tempbuf
                beq _next1

                lda #0
                sta top+1
                sta chan

                lda #<tempbuf
                ldx #>tempbuf
                ldy sp
                iny                     ; make sure non-zero
                jsr lexexpand._ENTRY1
                jsr getnext

                lda tempbuf+1
                ora #$20
                ldx #<mcmd
                ldy #>mcmd
                jsr lookup

                jmp _next1

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
                beq _1

                lda wsize
                sta cmdln

                lda #w2-w1
                jsr paintw

                lda #0
_1              jsr paintw
                jsr einit._ENTRY3

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

_next1          inc arg11
                bne _1

                inc arg12

_1              lda arg11
                ldx arg12
                jsr mprint._ENTRY1
                jsr gotkey
                beq _next1

                ldx #$FF
                stx CH_
                cmp #$DE
                bne _next1

                rts
                .endproc


;======================================
;   MPrint()
;======================================
mprint          .proc
                jsr mpsave

_ENTRY1         jsr printc

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
                jsr yesno
                bne mrun._XIT

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
                beq _1

                cmp #quote              ; compile and go?
                bne _2                  ;   no

                jsr comp

_1              lda INITAD
                ldx INITAD+1
                bne _3

_XIT            rts

_2              jsr mnum
_3              jsr run

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
                bne mrun._XIT           ; no output file!

                lda INITAD+1
                beq mrun._XIT           ; no program!!

                lda #1
                sta chan

                lda #8                  ; output
                jsr openchan

;   write header
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
                bne _1

                dex

_1              dec arg14

                txa
                adc codesize+1
                sta arg15

                jsr mwout

;   write the qcode
                ldx #$10
                lda #$0B                ; output command
                sta IOCB0+ICCOM,x

                lda codebase
                sta IOCB0+ICBAL,x       ; buffer address
                lda codebase+1
                sta IOCB0+ICBAH,x

                lda codesize
                sta IOCB0+ICBLL,x       ; size
                lda codesize+1
                sta IOCB0+ICBLH,x

                jsr CIOV
                bmi mwout._mwerr

;   save start address
                ldx #4
_next1          lda _mwinit,x
                sta arg9,x

                dex
                bpl _next1

                lda INITAD
                sta arg14
                lda INITAD+1
                sta arg15

                jsr mwout

;   close file
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

_ENTRY1         lda #0                  ; execute command line
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

_next1          lda #0
                ldx #4
_next2          asl arg0
                rol arg1
                rol a

                dex
                bne _next2

                ; clc
                adc #'0'
                cmp #':'
                bmi _1

                adc #6

_1              tay
                jsr putchar

                dec arg2
                bne _next1

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
                .word mwout._ENTRY1
                .byte 'x'
                .word mprint
                .byte '?'
                .word mdump
                .byte '*'

monitorPrompt   .byte 1,">"
