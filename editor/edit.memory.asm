
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.memory.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


memory          .namespace

;======================================
; Get(size)
;======================================
Get             .proc
                clc
                adc #$04
                sta zpAllocSize
                bcc _1

                inx
_1              stx zpAllocSize+1

_ENTRY1         jsr Allocate._ENTRY

                ldx zpAllocCurrent+1
                beq GeneralErr          ; no memory allocated !

                clc
                lda zpAllocCurrent
                adc #$04
                bcc _XIT

                inx

_XIT            rts
                .endproc


;======================================
; General Memory Error
;======================================
GeneralErr      .proc
                ldy #$00
                jsr ioSystemError

                lda sparem
                ldx sparem+1
                ldy allocerr
                bne Punt                ; really out of memory

                inc allocerr

                jsr Free

                jmp Get._ENTRY1         ; retry


;--------------------------------------
;
;--------------------------------------
Punt            jsr editor.display.SaveWindow   ; we're in big trouble

                jmp ampl.monitor.ResetWindow

                .endproc


;======================================
; Free(addr)
;======================================
Free            ;.proc
                sec
                sbc #$04
                bcs _XIT

                dex

_XIT            jmp Free

                ;.endproc


;======================================
; InsertByte()
;======================================
InsertByte      .proc
                lda cur
                sta arg3
                lda cur+1
                sta arg4

                jsr InsertBuffer

                sta cur
                stx cur+1

                rts
                .endproc


;======================================
; InsertBuffer(,,up)
;======================================
InsertBuffer    .proc
                ldy #$00
                lda (buf),Y
                ldx buf
                ldy buf+1

                .endproc

                ;[fall-through]


;======================================
; InsertLine(sze,sloc,up)
;======================================
InsertLine      ;.proc
                sta arg0                ; save sze
                stx arg1                ; save sloc
                sty arg2

                clc
                adc #$03
                ldx #$00

                jsr Get

                clc
                adc #$02
                sta arg5

                txa
                adc #$00
                sta arg6

                ldy arg0
                beq _1

_next1          lda (arg1),Y
                sta (arg5),Y

                dey
                bne _next1

_1              lda arg0
                sta (arg5),Y

                lda arg4
                bne _3                  ; up # 0

                lda top                 ; down _= top
                sta arg5

                ldy #$04                ; AFcur(2) _= down
                sta (zpAllocCurrent),Y

                lda top+1
                sta arg6

                iny
                sta (zpAllocCurrent),Y

                lda zpAllocCurrent      ; top _= AFcur
                sta top
                lda zpAllocCurrent+1
                sta top+1

                ldy #$00                ; AFcur(0) _= 0
                tya
                sta (zpAllocCurrent),Y

                iny
                sta (zpAllocCurrent),Y

_next2          lda arg6
                bne _2                  ; down # 0

                lda zpAllocCurrent      ; bot _= AFcur
                sta bot
                ldx zpAllocCurrent+1
                stx bot+1

                rts

_2              ldy #$01
                lda zpAllocCurrent+1    ; @down _= AFcur
                sta (arg5),Y

                dey
                lda zpAllocCurrent
                sta (arg5),Y

                ldx zpAllocCurrent+1

                rts

_3              ldy #$04
                lda (arg3),Y
                sta arg5                ; down _= Next(up)
                sta (zpAllocCurrent),Y  ; AFcur(2) _= down

                lda zpAllocCurrent
                sta (arg3),Y            ; up(2) _= AFcur

                iny
                lda (arg3),Y
                sta arg6
                sta (zpAllocCurrent),Y

                lda zpAllocCurrent+1
                sta (arg3),Y

                ldy #$00
                lda arg3
                sta (zpAllocCurrent),Y

                iny
                lda arg4
                sta (zpAllocCurrent),Y

                jmp _next2

                ;.endproc


;======================================
; DeleteCurrentLine()
;======================================
DeleteCurrentLine .proc
                lda cur
                ldx cur+1
                jsr DeleteLine

                sta cur
                stx cur+1

_XIT            rts
                .endproc


;======================================
; DeleteLine(lineptr)
;======================================
DeleteLine      .proc
                cpx #$00
                beq DeleteCurrentLine._XIT

                sta arg0
                stx arg1

                ldy #$04
                lda (arg0),Y
                sta arg4                ; down _= Next(ptr)

                iny
                lda (arg0),Y
                sta arg5

                ldy #$00
                lda (arg0),Y
                sta arg2                ; up _= Prev(ptr)

                iny
                lda (arg0),Y
                sta arg3
                bne _1                  ; up # 0

                lda arg4
                sta top                 ; top _= down
                lda arg5
                sta top+1

                jmp _2

_1              ldy #$04
                lda arg4
                sta (arg2),Y            ; up(2) _= down

                iny
                lda arg5
                sta (arg2),Y

_2              lda arg5
                bne _3                  ; down # 0

                lda arg2
                sta bot                 ; bot _= up
                lda arg3
                sta bot+1

                jmp _4

_3              ldy #$00
                lda arg2
                sta (arg4),Y            ; down(0) _= up

                iny
                lda arg3
                sta (arg4),Y

_4              lda arg0
                ldx arg1
                jsr Free

                lda arg2
                ldx arg3

                rts
                .endproc

                .endnamespace
