
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: ampl.monitor.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


monitor         .namespace

;======================================
;   ACTION! Monitor
;======================================
Monitor         .proc
                jsr editor.window.SaveWorld

                lda delbuf                  ; delete buffer bottom
                ldx delbuf+1
                jsr editor.chr.DeleteFree   ; get rid of delete buf

                lda top+1
                sta cacheTop_HI

_ENTRY1         jsr screen.Init

                ldx #$01
                stx CURSOR_Y    ;!!ROWCRS
                stx isMonitorLive

                dex
                stx cmdln
                stx device

                jsr ampl.init.SetupSPL

_ENTRY2
_next1          jsr editor.io.InitKeys

                lda DINDEX              ; display mode
                beq _1

                jsr screen.Init          ; get Graphics(0)

_1              jsr jt_vecAlarm
                ;!! jsr mainio.RestoreCursorChar ; unnecessary

                lda #<prompt
                ldx #>prompt
                jsr editor.window.GetTemp

                ldy tempbuf
                beq _next1

                lda #$00
                sta top+1
                sta ioChnnl

                lda #<tempbuf
                ldx #>tempbuf
                ldy sp
                iny                     ; make sure non-zero
                jsr compiler.lexicon.Expand._ENTRY1
                jsr compiler.lexicon.GetNext

                lda tempbuf+1
                ora #$20
                ldx #<tblMonitorCmd
                ldy #>tblMonitorCmd
                jsr mainmsc.Lookup

                jmp _next1

                .endproc


;--------------------------------------
;
;--------------------------------------
Quit            .proc
                ldy #$00
                sty isMonitorLive
                sty subbuf
                sty findbuf
                sty isDirty

                .endproc

                ;[fall-through]


;======================================
; ResetWindow()
;======================================
ResetWindow     .proc
                lda #$1D        ;;#59
                sta cmdln

                lda is2Windows          ; single window?
                beq _skipWin2           ;   yes

                lda jt_wsize
                sta cmdln

;   paint window2
                lda #win2Base-win1Base
                jsr PaintWindow

;   paint window1
                lda #$00
_skipWin2       jsr PaintWindow

                jsr editor.init.EditorInit._ENTRY3
                jmp editor.main.Loop

                .endproc


;======================================
; PaintWindow(window)
;======================================
PaintWindow     .proc
                sta currentWindow

                jsr editor.display.RestoreWindow
                jmp editor.find.Found

                .endproc


;======================================
; MemDump()
;======================================
MemDump         .proc
                jsr Print

_next1          inc arg11
                bne _1

                inc arg12

_1              lda arg11
                ldx arg12
                jsr Print._ENTRY1
                jsr editor.io.GotKey
                beq _next1

                ldx #$FF
                stx KEYCHAR             ; reset last keypress

                cmp #$DE
                bne _next1

                rts
                .endproc


;======================================
; Print()
;======================================
Print           .proc
                jsr SaveParams

_ENTRY1         jsr mainio.PrintCard

                ldy #','
                jsr mainio.PutChar

                lda arg11
                ldx arg12
                jsr PrintHex
                jsr mainio.PutSpace

                ldy #'='
                jsr mainio.PutChar
                jsr mainio.PutSpace
                jsr LoadParams

                tay
                jsr mainio.PutChar
                jsr mainio.PutSpace
                jsr LoadParams

                jsr PrintHex
                jsr mainio.PutSpace
                jsr LoadParams

                ldx #$00
                jsr mainio.PrintCard
                jsr mainio.PutSpace
                jsr LoadParams

                jsr mainio.PrintCard
                jmp mainio.PutEOL

                .endproc


;======================================
; LoadParams()
;======================================
LoadParams      .proc
                ldy #$01
                lda (arg11),Y
                tax

                dey
                lda (arg11),Y

                rts
                .endproc


;======================================
; SaveParams()
;======================================
SaveParams      .proc
                jsr mainmsc.MNum

                sta arg11
                stx arg12

                rts
                .endproc


;======================================
; Boot()
;======================================
Boot            .proc
                lda #<_bmsg
                ldx #>_bmsg
                jsr editor.window.YesNo

                bne MemRun._XIT
                jmp editor.cartridge.START._cold

;--------------------------------------

_bmsg           .ptext "Boot? "

                .endproc


;======================================
; MemRun()
;--------------------------------------
; execute from memory
;======================================
MemRun          .proc
                lda nxttoken
                cmp #tokEOF
                beq _1

                cmp #tokQuote           ; compile and go?
                bne _2                  ;   no

                jsr Compile

_1              lda INITAD
                ldx INITAD+1
                bne _3

_XIT            rts

_2              jsr mainmsc.MNum
_3              jsr mainbank.Run

                lda #$00
                sta device

                rts
                .endproc


;======================================
; MemWrite()
;======================================
MemWrite        .proc                   ; write object file
                lda nxttoken
                cmp #tokQuote
                bne MemRun._XIT         ; no output file!

                lda INITAD+1
                beq MemRun._XIT         ; no program!!

                lda #$01
                sta ioChnnl

                lda #$08                ; output
                jsr mainio.OpenChannel

;   write header
                lda #$06
                sta arg9

                lda #$FF
                sta arg10               ; $FF
                sta arg11               ; $FF

                clc
                lda codeBase            ; starting address
                adc codeoff
                sta arg12

                lda codeBase+1
                adc codeoff+1
                sta arg13
                tax

                clc                     ; ending address
                lda arg12
                adc codeSize
                sta arg14
                bne _1

                dex

_1              dec arg14

                txa
                adc codeSize+1
                sta arg15

                jsr WOut

;   write the QCODE
                ldx #$10
                lda #$0B                ; PUTCHR
                sta IOCB0+ICCOM,X

                lda codeBase
                sta IOCB0+ICBAL,X       ; buffer address
                lda codeBase+1
                sta IOCB0+ICBAH,X

                lda codeSize
                sta IOCB0+ICBLL,X       ; size
                lda codeSize+1
                sta IOCB0+ICBLH,X

                jsr CIOV
                bmi WOut._mwerr

;   save start address
                ldx #$04
_next1          lda _mwinit,X
                sta arg9,X

                dex
                bpl _next1

                lda INITAD
                sta arg14
                lda INITAD+1
                sta arg15

                jsr WOut

;   close file
                lda #$01
                jmp mainio.Close

;--------------------------------------

_mwinit         .byte 6
                .addr INITAD
                .addr INITAD+1

                .endproc


;======================================
; WOut()
;======================================
WOut            .proc
                lda #$01
                ldx #arg9
                ldy #$00
                jsr mainio.Output

                bmi _mwerr

                rts


;--------------------------------------
;
;--------------------------------------
_mxerr          ldy #endERR

_mwerr          jmp mainbank.SPLErr

                .endproc


;--------------------------------------
;
;--------------------------------------
Execute         .proc
                lda #$00                ; execute command line
                sta codeoff
                sta codeoff+1

                lda QCODE
                pha
                lda QCODE+1
                pha

                jsr compiler.lexicon.GetNext
                jsr mainbank.CStmtList

                cmp #tokEOF
                bne WOut._mxerr

                lda #$60                ; RTS
                ldy #$00
                sta (QCODE),Y

                pla
                tax
                pla

                jmp mainbank.Run

                .endproc


;======================================
; Compile()
;======================================
Compile         .proc
                jsr ampl.init.SetupSPL
                jsr mainio.DisplayOff
                jsr mainbank.Compile

                jmp mainio.DisplayOn

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
                ldx procSP
                beq _XIT

;               lda #<_pmsg
;               ldx #>_pmsg
;               jsr editor.window.YesNo
;               bne _XIT

;               ldx procSP              ; break stack pointer

                lda #$00
                sta procSP

                txs

                jmp mainbank.LProceed

_XIT            rts

;_pmsg          DEFMSG "Proceed? "

                .endproc


;======================================
; PrintHex(num)
;======================================
PrintHex        .proc
                sta arg0
                stx arg1

                lda #$04
                sta arg2

                ldy #'$'
                jsr mainio.PutChar

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
                jsr mainio.PutChar

                dec arg2
                bne _next1

                rts
                .endproc


;--------------------------------------
;--------------------------------------

tblMonitorCmd   .addr jt_vecDispTb+9    ; default routine
                .byte $23               ; table size (#entries*3 - 1)

                .addr Boot
                .text 'b'               ; BOOT
                .addr Compile
                .text 'c'               ; COMPILE
                .addr mainbank.DosRet
                .text 'd'               ; DOS
                .addr Quit
                .text 'e'               ; EDITOR
                .addr mainbank.Options
                .text 'o'               ; OPTIONS
                .addr Proceed
                .text 'p'               ; PROCEED (continue after BRK)
                .addr MemRun
                .text 'r'               ; MEMORY RUN
                .addr MemWrite
                .text 'w'               ; MEMORY WRITE
                .addr Execute
                .text 'x'               ; EXECUTE
                .addr Print
                .text '?'               ; PRINT
                .addr MemDump
                .text '*'               ; MEMORY DUMP

; - - - - - - - - - - - - - - - - - - -

prompt          .ptext '>'

                .endnamespace
