
; SPDX-FileName: edit.mem.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   GetMemory(size)
;======================================
GetMemory       .proc
                clc
                adc #4
                sta afsize
                bcc _gm0

                inx
_gm0            stx afsize+1
_gm1            jsr Allocate._ENTRY

                ldx afcur+1
                beq GeneralMemErr       ; no memory allocated !

                clc
                lda afcur
                adc #4
                bcc _gm2

                inx
_gm2            rts
                .endproc


;======================================
; General Memory Error
;======================================
GeneralMemErr   .proc
                ldy #0
                jsr syserr

                lda sparem
                ldx sparem+1
                ldy allocerr
                bne Punt                ; really out of memory

                inc allocerr
                jsr Free

                jmp GetMemory._gm1      ; retry

Punt            jsr SaveWindow          ; we're in big trouble

                jmp RSTwnd

                .endproc


;======================================
;   FreeMem(addr)
;======================================
FreeMemory      ;.proc
                sec
                sbc #4
                bcs freem1
                dex
freem1          jmp Free

                ;.endproc


;======================================
;   InsertByte()
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
;   InsertBuffer(,,up)
;======================================
InsertBuffer    .proc
                ldy #0
                lda (buf),y
                ldx buf
                ldy buf+1
                .endproc


;======================================
;   InsertLine(sze,sloc,up)
;======================================
InsertLine      ;.proc
                sta arg0                ; save sze
                stx arg1                ; save sloc
                sty arg2
                clc
                adc #3
                ldx #0
                jsr GetMemory

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

                ;.endproc


;======================================
;   DeleteCurrentLine()
;======================================
DeleteCurrentLine .proc
                lda cur
                ldx cur+1
                jsr DeleteLine

                sta cur
                stx cur+1
dln1            rts
                .endproc


;======================================
;   DeleteLine(lineptr)
;======================================
DeleteLine      .proc
                cpx #0
                beq DeleteCurrentLine.dln1

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
                jsr Free

                lda arg2
                ldx arg3
                rts
                .endproc
