
; SPDX-FileName: edit.chr.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


;======================================
;   InsertChar() char in curCh
;======================================
InsertChar      .proc
                jsr SetSpacing

_padbuf         ldy #0
                lda (buf),y
                cmp jt_linemax
                bcc _pbuf0              ; test line too long

                jsr scrbell

                ldy #0
                lda (buf),y
_pbuf0          cmp sp
                bcs _pbuf2

                sta arg0
                lda sp
                sta (buf),y
                ldy arg0
                lda #$20                ; pad with spaces
_pbuf1          iny
                sta (buf),y
                cpy sp
                bcc _pbuf1

_pbret          ldy sp
                lda curch
                sta (buf),y
                lda #$ff
                sta isDirty
                jsr dspbuf

                jmp ScrollRight

_pbuf2          ldx insert
                beq _pbret

    ; move buf right one char
_movert         adc #0                  ; really 1, carry set
                sta (buf),y
                tay
_mrt1           dey
                lda (buf),y
                iny
                sta (buf),y
                dey
                cpy sp
                bne _mrt1
                beq _pbret

                .endproc


;======================================
;   InsrtSp()
;======================================
InsertSpace     .proc
                lda insert
                pha
                lda #$20
                sta insert
                sta curch
                jsr InsertChar

                pla
                sta insert
                jmp scrlft

                .endproc


;======================================
;   Insert_()
;======================================
Insert_         .proc
                jsr CleanLine
                jsr nextup

                sta cur+1               ; tricky
                jsr insert2

                lda #0
                jmp NewPage.npage1

insert2         lda #0
                tay
insert3         sta (buf),y
                iny
                sty dirty
                jmp InsertByte

                .endproc


;======================================
;
;======================================
csret           .proc
    ; handle pad if any
                jsr InsertSpace
                jsr DeleteChar

                ldy #0
                lda (buf),y
                pha
                jsr SetSpacing

                sta isDirty             ; always non-zero
                sec
                sbc #1
                sta (buf),y
                jsr CleanLine

                pla
                sta arg1
                inc arg1
                lda #0
                sta arg0
                beq _csr2

_csr1           lda (buf),y
                inc arg0
                ldy arg0
                sta (buf),y
                inc sp
_csr2           ldy sp
                cpy arg1
                bcc _csr1

                ldy #0
                lda arg0
                jsr Insert_.insert3
                jsr nextup
                jsr Refresh

                jmp Return_.ret2

                .endproc


;======================================
;   Return_()
;======================================
Return_         .proc
                ldx insert
                bne csret

                jsr CheckDown
                bne ret2

_ret1           jsr Insert_.insert2
                jsr nextup
                jsr ldbuf
ret2            jsr ScrollDown

ret3            jmp Front

                .endproc


;======================================
;   Delete_()
;======================================
Delete_         .proc
                jsr CleanLine

                lda delbuf
                ldx delbuf+1
                stx dirty
                ldy lastch
                cpy #$9c
                beq _del1

                jsr DeleteFree

_del1           sta arg3
                stx arg4
                jsr InsertBuffer
                jsr CheckDown           ; last line ?
                bne _del2               ; no, delete it

                tay
                sta (buf),y
                iny
                sty isDirty
                bra Return_.ret3

_del2           jsr DeleteCurrentLine
                beq _del3

                jsr nextdwn
_del3           jsr chkcur

                lda #0

                jmp NewPage.npage1
                .endproc


;======================================
;   DeleteTop()
;======================================
DeleteTop       .proc
                lda delbuf+4
                ldx delbuf+5
                sta delnxt
                stx delnxt+1
                .endproc


;======================================
;   DeleteEnd(ptr)
;======================================
DeleteEnd       .proc
                cmp #<delbuf
                bne _XIT

                cpx #>delbuf
_XIT            rts
                .endproc


;======================================
;   DeleteFree(bot)
;======================================
DeleteFree      .proc
                jsr DeleteEnd
                beq DeleteEnd._XIT

                jsr DeleteLine
                bra DeleteFree
                .endproc


;======================================
;   DeleteNext()
;======================================
DeleteNext      .proc
                ldy #5
                lda (delnxt),y
                tax
                dey
                lda (delnxt),y
                sta delnxt
                stx delnxt+1
                jmp DeleteEnd

                .endproc


;======================================
;   Undo()
;======================================
Undo            .proc
                jsr ldbuf

                jmp Front

                .endproc


;======================================
;   DeleteChar()
;======================================
DeleteChar      .proc
                jsr CheckColumn
                bcc CheckDown._XIT

                ldy #0
                lda (buf),y
                sta isDirty
                sec
                sbc #1
                sta (buf),y
                ldy sp
_dch1           iny
                lda (buf),y
                dey
                sta (buf),y
                iny
                cpy isDirty
                bcc _dch1               ; really checking for =

                .endproc


;======================================
;   RefreshBuf()
;======================================
RefreshBuf      .proc
                jsr dspbuf

                jmp rstcol.lftrt

                .endproc


;======================================
;   CheckDown()
;======================================
CheckDown       .proc
                jsr CleanLine
                beq _XIT

                ldy #5
                lda (cur),y
_XIT            rts
                .endproc


;======================================
;   BackSpc()
;======================================
BackSpc         .proc
                jsr SetSpacing

                cmp #2
                bcc CheckDown._XIT

bsp1            jsr ScrollLeft
                jsr SetSpacing

                tay
                lda #$20
                sta (buf),y
                sta isDirty
                lda insert
                bne DeleteChar

                jmp RefreshBuf

                .endproc


;======================================
;
;======================================
csbs            .proc
                jsr SetSpacing

                cmp #2
                bcs BackSpc.bsp1

                jsr chkcur
                beq CheckDown._XIT      ; no lines at all!

                ldy #1
                lda (cur),y
                beq CheckDown._XIT      ; no line to merge with

    ; merge
                jsr ScrollUp
                jsr Back
                jsr nextdwn

                sta isDirty
                jsr curstr

                clc
                ldy #0
                lda (buf),y
                sta arg2
                adc (arg0),y
                sta (buf),y
                lda (arg0),y
                beq _cb2

                sta arg3
_cb1            iny
                sty arg4
                lda (arg0),y
                inc arg2
                ldy arg2
                sta (buf),y
                ldy arg4
                cpy arg3
                bne _cb1

_cb2            jsr DeleteCurrentLine

                jmp Refresh

                .endproc
