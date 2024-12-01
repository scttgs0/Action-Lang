
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.chr.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
;   InsrtCh() char in curCh
;======================================
insrtch         .proc
                jsr setsp

_padbuf         ldy #0
                lda (buf),Y
                cmp linemax
                bcc _pbuf0              ; test line too long

                jsr scrbell

                ldy #0
                lda (buf),Y
_pbuf0          cmp sp
                bcs _pbuf2

                sta arg0
                lda sp
                sta (buf),Y
                ldy arg0
                lda #$20                ; pad with spaces
_pbuf1          iny
                sta (buf),Y
                cpy sp
                bcc _pbuf1

_pbret          ldy sp
                lda curch
                sta (buf),Y
                lda #$ff
                sta dirtyf
                jsr dspbuf

                jmp scrlrt

_pbuf2          ldx insert
                beq _pbret

    ; move buf right one char
_movert         adc #0                  ; really 1, carry set
                sta (buf),Y
                tay
_mrt1           dey
                lda (buf),Y
                iny
                sta (buf),Y
                dey
                cpy sp
                bne _mrt1
                beq _pbret

                .endproc


;======================================
;   InsrtSp()
;======================================
insrtsp         .proc
                lda insert
                pha
                lda #$20
                sta insert
                sta curch
                jsr insrtch

                pla
                sta insert
                jmp scrlft

                .endproc


;======================================
;   Insrt()
;======================================
insrt           .proc
                jsr clnln
                jsr nextup

                sta cur+1               ; tricky
                jsr insert2

                lda #0
                jmp newpage.npage1

insert2         lda #0
                tay
insert3         sta (buf),Y
                iny
                sty dirty
                jmp instb

                .endproc


;======================================
;
;======================================
csret           .proc
    ; handle pad if any
                jsr insrtsp
                jsr delch

                ldy #0
                lda (buf),Y
                pha
                jsr setsp

                sta dirtyf              ; always non-zero
                sec
                sbc #1
                sta (buf),Y
                jsr clnln

                pla
                sta arg1
                inc arg1
                lda #0
                sta arg0
                beq _csr2

_csr1           lda (buf),Y
                inc arg0
                ldy arg0
                sta (buf),Y
                inc sp
_csr2           ldy sp
                cpy arg1
                bcc _csr1

                ldy #0
                lda arg0
                jsr insrt.insert3
                jsr nextup
                jsr refresh

                jmp return.ret2

                .endproc


;======================================
;   Return()
;======================================
return          .proc
                ldx insert
                bne csret

                jsr chkdwn
                bne ret2

_ret1           jsr insrt.insert2
                jsr nextup
                jsr ldbuf
ret2            jsr scrldwn

ret3            jmp front

                .endproc


;======================================
;   Delete()
;======================================
delete          .proc
                jsr clnln

                lda delbuf
                ldx delbuf+1
                stx dirty
                ldy lastch
                cpy #$9c
                beq _del1

                jsr delfree

_del1           sta arg3
                stx arg4
                jsr instbuf
                jsr chkdwn              ; last line ?
                bne _del2               ; no, delete it

                tay
                sta (buf),Y
                iny
                sty dirtyf
                bne return.ret3         ; [unc]

_del2           jsr delcur
                beq _del3

                jsr nextdwn
_del3           jsr chkcur

                lda #0

                jmp newpage.npage1
                .endproc


;======================================
;   DelTop()
;======================================
deltop          .proc
                lda delbuf+4
                ldx delbuf+5
                sta delnxt
                stx delnxt+1
                .endproc


;======================================
;   DelEnd(ptr)
;======================================
delend          .proc
                cmp #<delbuf
                bne de1

                cpx #>delbuf
de1             rts
                .endproc


;======================================
;   DelFree(bot)
;======================================
delfree         .proc
                jsr delend
                beq delend.de1

                jsr delln
                bne delfree             ; [unc]
                .endproc


;======================================
;   DelNext()
;======================================
delnext         .proc
                ldy #5
                lda (delnxt),Y
                tax
                dey
                lda (delnxt),Y
                sta delnxt
                stx delnxt+1
                jmp delend

                .endproc


;======================================
;
;======================================
undo            .proc
                jsr ldbuf

                jmp front

                .endproc


;======================================
;   DeleteCh()
;======================================
delch           .proc
                jsr chkcol
                bcc chkdwn.cdwn1

                ldy #0
                lda (buf),Y
                sta dirtyf
                sec
                sbc #1
                sta (buf),Y
                ldy sp
_dch1           iny
                lda (buf),Y
                dey
                sta (buf),Y
                iny
                cpy dirtyf
                bcc _dch1               ; really checking for =

                .endproc


;======================================
;   RfrshBuf()
;======================================
rfrshbuf        .proc
                jsr dspbuf

                jmp rstcol.lftrt

                .endproc


;======================================
;   ChkDwn()
;======================================
chkdwn          .proc
                jsr clnln
                beq cdwn1

                ldy #5
                lda (cur),Y
cdwn1           rts
                .endproc


;======================================
;   BackSp()
;======================================
backsp          .proc
                jsr setsp

                cmp #2
                bcc chkdwn.cdwn1

bsp1            jsr scrllft
                jsr setsp

                tay
                lda #$20
                sta (buf),Y
                sta dirtyf
                lda insert
                bne delch

                jmp rfrshbuf

                .endproc


;======================================
;
;======================================
csbs            .proc
                jsr setsp

                cmp #2
                bcs backsp.bsp1

                jsr chkcur
                beq chkdwn.cdwn1        ; no lines at all!

                ldy #1
                lda (cur),Y
                beq chkdwn.cdwn1        ; no line to merge with

    ; merge
                jsr scrlup
                jsr back
                jsr nextdwn

                sta dirtyf
                jsr curstr

                clc
                ldy #0
                lda (buf),Y
                sta arg2
                adc (arg0),Y
                sta (buf),Y
                lda (arg0),Y
                beq _cb2

                sta arg3
_cb1            iny
                sty arg4
                lda (arg0),Y
                inc arg2
                ldy arg2
                sta (buf),Y
                ldy arg4
                cpy arg3
                bne _cb1

_cb2            jsr delcur

                jmp refresh

                .endproc
