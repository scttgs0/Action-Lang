
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

_ENTRY1         jsr screenInit

                ldx #$01
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

                jsr screenInit          ; get Graphics(0)

_1              jsr jt_alarm
                jsr ioRestoreCursorChar

                lda #<monitorPrompt
                ldx #>monitorPrompt
                jsr GetTemp

                ldy tempbuf
                beq _next1

                lda #$00
                sta top+1
                sta Channel

                lda #<tempbuf
                ldx #>tempbuf
                ldy sp
                iny                     ; make sure non-zero
                jsr LexExpand._ENTRY1
                jsr LexGetNext

                lda tempbuf+1
                ora #$20
                ldx #<monitorCmd
                ldy #>monitorCmd
                jsr mscLookup

                jmp _next1

                .endproc


;--------------------------------------
;
;--------------------------------------
MonQuit         .proc
                ldy #$00
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
                lda #$17
                sta cmdln

                lda numwd
                beq _1

                lda jt_wsize
                sta cmdln

                lda #w2-w1
                jsr PaintW

                lda #$00
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

_ENTRY1         jsr ioPrintCard

                ldy #','
                jsr ioPutChar

                lda arg11
                ldx arg12
                jsr PrintH
                jsr ioPutSpace

                ldy #'='
                jsr ioPutChar
                jsr ioPutSpace
                jsr MpLoad

                tay
                jsr ioPutChar
                jsr ioPutSpace
                jsr MpLoad
                jsr PrintH
                jsr ioPutSpace
                jsr MpLoad

                ldx #$00
                jsr ioPrintCard
                jsr ioPutSpace
                jsr MpLoad
                jsr ioPrintCard

                jmp ioPutEOL

                .endproc


;======================================
;   MpLoad()
;======================================
MpLoad          .proc
                ldy #$01
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
                jsr mscMNum

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

_2              jsr mscMNum
_3              jsr bankRun

                lda #$00
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

                lda #$01
                sta Channel

                lda #$08                ; output
                jsr ioOpenChannel

;   write header
                lda #$06
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
                bmi MWOut._mwerr

;   save START address
                ldx #$04
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
                lda #$01
                jmp ioClose

;--------------------------------------

_mwinit         .byte 6
                .addr INITAD
                .addr INITAD+1

                .endproc


;======================================
;   MWOut()
;======================================
MWOut           .proc
                lda #$01
                ldx #arg9
                ldy #$00
                jsr ioOutput

                bmi _mwerr

                rts


;======================================
;
;======================================
_mxerr          ldy #endERR

_mwerr          jmp bankSPLErr

_ENTRY1         lda #$00                ; execute command line
                sta codeoff
                sta codeoff+1

                lda QCODE
                pha
                lda QCODE+1
                pha

                jsr LexGetNext
                jsr bankCStmtList

                cmp #tokEOF
                bne _mxerr

                lda #$60                ; RTS
                ldy #$00
                sta (QCODE),Y

                pla
                tax
                pla

                jmp bankRun

                .endproc


;======================================
;   Comp()
;======================================
Comp            .proc
                jsr SPLsetup
                jsr ioDisplayOff
                jsr bankCompile

                jmp ioDisplayOn

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

                lda #$00
                sta procsp

                txs

                jmp bankLProceed

_XIT            rts
                .endproc

;:Pmsg DEFMSG "Proceed? "


;======================================
;   PrintH(num)
;======================================
PrintH          .proc
                sta arg0
                stx arg1

                lda #$04
                sta arg2

                ldy #'$'
                jsr ioPutChar

_next1          lda #$00
                ldx #$04
_next2          asl arg0
                rol arg1
                rol a

                dex
                bne _next2

                ; clc
                adc #'0'
                cmp #':'
                bmi _1

                adc #$06

_1              tay
                jsr ioPutChar

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
                .addr bankDRet
                .text 'd'               ; DOS
                .addr MonQuit
                .text 'e'               ; EDITOR

;               .addr Format
;               .text 'f'

                .addr bankOptions
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
