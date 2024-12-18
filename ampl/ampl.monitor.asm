
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.monitor.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;======================================
;   ACTION! Monitor
;======================================
Monitor         .proc
                jsr SaveWorld

                lda delbuf              ; delete buffer bottom
                ldx delbuf+1
                jsr DeleteFree          ; get rid of delete buf

                lda top+1
                sta top1

_ENTRY1         jsr scrinit

                ldx #1
                stx ROWCRS
                stx isMonitorLive

                dex
                stx cmdln
                stx device

                jsr SPLsetup

_ENTRY2
_next1          jsr InitKeys

                lda DINDEX              ; display mode
                beq _1

                jsr scrinit             ; get Graphics(0)

_1              jsr jt_alarm
                jsr rstcsr

                lda #<monitorPrompt
                ldx #>monitorPrompt
                jsr GetTemp

                ldy tempbuf
                beq _next1

                lda #0
                sta top+1
                sta Channel

                lda #<tempbuf
                ldx #>tempbuf
                ldy sp
                iny                     ; make sure non-zero
                jsr lexexpand._ENTRY1
                jsr GetNext

                lda tempbuf+1
                ora #$20
                ldx #<monitorCmd
                ldy #>monitorCmd
                jsr lookup

                jmp _next1

                .endproc


;--------------------------------------
;
;--------------------------------------
MonQuit         .proc
                ldy #0
                sty isMonitorLive
                sty subbuf
                sty findbuf
                sty isDirty

                .endproc

                ;[fall-through]


;======================================
;   RSTwnd()
;======================================
RSTwnd          .proc
                lda #23
                sta cmdln

                lda numwd
                beq _1

                lda jt_wsize
                sta cmdln

                lda #w2-w1
                jsr PaintW

                lda #0
_1              jsr PaintW
                jsr EditorInit._ENTRY3

                jmp floop

                .endproc


;======================================
;   PaintW(window)
;======================================
PaintW          .proc
                sta currentWindow

                jsr RestoreWindow

                jmp Found

                .endproc


;======================================
;   MDump()
;======================================
MDump           .proc
                jsr MPrint

_next1          inc arg11
                bne _1

                inc arg12

_1              lda arg11
                ldx arg12
                jsr MPrint._ENTRY1
                jsr GotKey
                beq _next1

                ldx #$FF
                stx KEYCHAR             ; reset last keypress

                cmp #$DE
                bne _next1

                rts
                .endproc


;======================================
;   MPrint()
;======================================
MPrint          .proc
                jsr MpSave

_ENTRY1         jsr printc

                ldy #','
                jsr putchar

                lda arg11
                ldx arg12
                jsr PrintH
                jsr putsp

                ldy #'='
                jsr putchar
                jsr putsp
                jsr MpLoad

                tay
                jsr putchar
                jsr putsp
                jsr MpLoad
                jsr PrintH
                jsr putsp
                jsr MpLoad

                ldx #0
                jsr printc
                jsr putsp
                jsr MpLoad
                jsr printc

                jmp puteol

                .endproc


;======================================
;   MpLoad()
;======================================
MpLoad          .proc
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
MpSave          .proc
                jsr mnum

                sta arg11
                stx arg12

                rts
                .endproc


;======================================
;   Boot()
;======================================
ReBoot          .proc
                lda #<_bmsg
                ldx #>_bmsg
                jsr YesNo
                bne MRun._XIT

                jmp START.cold

;--------------------------------------

_bmsg           .ptext "Boot? "

                .endproc


;======================================
;   MRun()
;======================================
MRun            .proc
                lda nxttoken
                cmp #tokEOF
                beq _1

                cmp #tokQuote           ; compile and go?
                bne _2                  ;   no

                jsr Comp

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
MWrite          .proc                   ; write object file
                lda nxttoken
                cmp #tokQuote
                bne MRun._XIT           ; no output file!

                lda INITAD+1
                beq MRun._XIT           ; no program!!

                lda #1
                sta Channel

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

                jsr MWOut

;   write the QCODE
                ldx #$10
                lda #$0B                ; output command
                ;!!sta IOCB0+ICCOM,X

                lda codebase
                ;!!sta IOCB0+ICBAL,X       ; buffer address
                lda codebase+1
                ;!!sta IOCB0+ICBAH,X

                lda codesize
                ;!!sta IOCB0+ICBLL,X       ; size
                lda codesize+1
                ;!!sta IOCB0+ICBLH,X

                ;!!jsr CIOV
                bmi MWOut._mwerr

;   save START address
                ldx #4
_next1          lda _mwinit,X
                sta arg9,X

                dex
                bpl _next1

                lda INITAD
                sta arg14
                lda INITAD+1
                sta arg15

                jsr MWOut

;   close file
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
MWOut           .proc
                lda #1
                ldx #arg9
                ldy #0
                jsr output

                bmi _mwerr

                rts


;======================================
;
;======================================
_mxerr          ldy #endERR

_mwerr          jmp splerr

_ENTRY1         lda #0                  ; execute command line
                sta codeoff
                sta codeoff+1

                lda QCODE
                pha
                lda QCODE+1
                pha

                jsr GetNext
                jsr cstmtlst

                cmp #tokEOF
                bne _mxerr

                lda #$60                ; RTS
                ldy #0
                sta (QCODE),Y

                pla
                tax
                pla

                jmp run

                .endproc


;======================================
;   Comp()
;======================================
Comp            .proc
                jsr SPLsetup
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
Proceed         .proc
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
PrintH          .proc
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

monitorCmd      .addr jt_disptb+9       ; unknown cmd
                .byte 35                ; table size

                .addr ReBoot
                .text 'b'               ; BOOT
                .addr Comp
                .text 'c'               ; COMPILE
                .addr dret
                .text 'd'               ; DOS
                .addr MonQuit
                .text 'e'               ; EDITOR

;               .addr Format
;               .text 'f'

                .addr options
                .text 'o'               ; OPTIONS
                .addr Proceed
                .text 'p'               ; PROCEED (continue after Break)
                .addr MRun
                .text 'r'               ; MEMORY RUN
                .addr MWrite
                .text 'w'               ; MEMORY WRITE
                .addr MWOut._ENTRY1
                .text 'x'               ; EXECUTE
                .addr MPrint
                .text '?'               ; PRINT
                .addr MDump
                .text '*'               ; MEMORY DUMP

monitorPrompt   .ptext '>'
