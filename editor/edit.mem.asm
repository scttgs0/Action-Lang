
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.mem.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   GetMem(size)
;======================================
getmem          .proc
                clc
                adc #4
                sta afsize
                bcc _1

                inx
_1              stx afsize+1

_ENTRY1         jsr allocate._ENTRY

                ldx afcur+1
                beq gmerr               ; no memory allocated !

                clc
                lda afcur
                adc #4
                bcc _XIT

                inx

_XIT            rts
                .endproc


;======================================
; General Memory Error
;======================================
gmerr           .proc
                ldy #0
                jsr syserr

                lda sparem
                ldx sparem+1
                ldy allocerr
                bne punt                ; really out of memory

                inc allocerr

                jsr free

                jmp getmem._ENTRY1      ; retry


;--------------------------------------
;
;--------------------------------------
punt            jsr savewd              ; we're in big trouble

                jmp rstwnd

                .endproc


;======================================
;   FreeMem(addr)
;======================================
freemem         ;.proc
                sec
                sbc #4
                bcs _XIT

                dex

_XIT            jmp free

                ;.endproc


;======================================
;   InstB()
;======================================
instb           .proc
                lda cur
                sta arg3
                lda cur+1
                sta arg4

                jsr instbuf

                sta cur
                stx cur+1

                rts
                .endproc


;======================================
;   InstBuf(,,up)
;======================================
instbuf         .proc
                ldy #0
                lda (buf),Y
                ldx buf
                ldy buf+1

                .endproc

                ;[fall-through]


;======================================
;   InstLn(sze,sloc,up)
;======================================
instln          ;.proc
                sta arg0                ; save sze
                stx arg1                ; save sloc
                sty arg2

                clc
                adc #3
                ldx #0

                jsr getmem

                clc
                adc #2
                sta arg5

                txa
                adc #0
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

                ldy #4                  ; AFcur(2) _= down
                sta (afcur),Y

                lda top+1
                sta arg6

                iny
                sta (afcur),Y

                lda afcur               ; top _= AFcur
                sta top
                lda afcur+1
                sta top+1

                ldy #0                  ; AFcur(0) _= 0
                tya
                sta (afcur),Y

                iny
                sta (afcur),Y

_next2          lda arg6
                bne _2                  ; down # 0

                lda afcur               ; bot _= AFcur
                sta bot
                ldx afcur+1
                stx bot+1

                rts

_2              ldy #1
                lda afcur+1             ; @down _= AFcur
                sta (arg5),Y

                dey
                lda afcur
                sta (arg5),Y

                ldx afcur+1

                rts

_3              ldy #4
                lda (arg3),Y
                sta arg5                ; down _= Next(up)
                sta (afcur),Y           ; AFcur(2) _= down

                lda afcur
                sta (arg3),Y            ; up(2) _= AFcur

                iny
                lda (arg3),Y
                sta arg6
                sta (afcur),Y

                lda afcur+1
                sta (arg3),Y

                ldy #0
                lda arg3
                sta (afcur),Y

                iny
                lda arg4
                sta (afcur),Y

                jmp _next2

                ;.endproc


;======================================
;   DelCur()
;======================================
delcur          .proc
                lda cur
                ldx cur+1
                jsr delln

                sta cur
                stx cur+1

_XIT            rts
                .endproc


;======================================
;   DelLn(lineptr)
;======================================
delln           .proc
                cpx #0
                beq delcur._XIT

                sta arg0
                stx arg1

                ldy #4
                lda (arg0),Y
                sta arg4                ; down _= Next(ptr)

                iny
                lda (arg0),Y
                sta arg5

                ldy #0
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

_1              ldy #4
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

_3              ldy #0
                lda arg2
                sta (arg4),Y            ; down(0) _= up

                iny
                lda arg3
                sta (arg4),Y

_4              lda arg0
                ldx arg1
                jsr free

                lda arg2
                ldx arg3

                rts
                .endproc
