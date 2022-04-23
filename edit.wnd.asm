;======================================
;   FILE: edit.wnd.asm
;======================================

; Copyright 1983 by Action Computer Services
; All Rights Reserved

; last modified July 6, 1983


;    Wind1()
;    -------
wind1           .proc
                lda curwdw
                beq savworld.wdret
                lda #0
                pha
                .endproc


;    SwapWd()
;    --------
swapwd          .proc
                jsr savworld
                pla
                jmp rstworld
                .endproc


;    Wind2()
;    -------
wind2           .proc
                lda curwdw
                bne savworld.wdret
                lda numwd
                bne _w2
                jmp w2init

_w2             lda #w2-w1
                pha
                bne swapwd              ; uncond.
                .endproc


;    SavWorld()
;    ----------
savworld        .proc
                jsr clnln
                jsr savecol
                jsr rstcsr
                jsr setsp
                jmp savewd

wdret           rts
                .endproc


;    Clear()
;    -------
clear           .proc
                jsr alarm
                lda #<delwd.clearmsg
                ldx #>delwd.clearmsg
                jsr yesno
                bne savworld.wdret
clr0            jsr clnln
                lda dirty
                beq clr1
    ; JSR Alarm
                lda #<delwd.dirtymsg
                ldx #>delwd.dirtymsg
                jsr yesno
                bne savworld.wdret
clr1            jsr freetags            ; get rid of tags
                lda bot
                ldx bot+1
_clr2           jsr delln
                bne _clr2
                stx cur+1
                stx dirty
                stx dirtyf
                stx inbuf
                jmp newpage
                .endproc


;    RstWorld(window)
;    ----------------
rstworld        .proc
                sta curwdw
                jsr rstwd
                jsr ldbuf
                jmp rstcol
                .endproc


;    DelWd()
;    -------
delwd           .proc
                lda numwd
                beq savworld.wdret
                jsr alarm
                lda #<delmsg
                ldx #>delmsg
                jsr yesno
                bne savworld.wdret
_dw1            jsr clear.clr0
                lda dirty
                bne savworld.wdret
                ldy #0
                sty numwd
                cpy curwdw
                bne delwd2
                ldy #w2-w1
delwd2          sty curwdw
                jsr rstwd
                jmp einit.winit1


clearmsg        .text 7,"CLEAR? "
delmsg          .text 15,"Delete window? "
dirtymsg        .text 19,"Not saved, Delete? "
                .endproc


;    GetTemp(msg)
;    ------------
gettemp         .proc
                ldy #0
gett1           sty tempbuf
                ldy #>tempbuf
                sty arg3
                ldy #<tempbuf
                .endproc


;    CmdStr(msg, buf)
;    ----------------
cmdstr          .proc
                sta arg0
                sty arg2
                jsr cmdcol
                lda #$80
                sta arg4
                lda arg0
                ldy arg2
                jsr getstr
                jsr rstcsr
                jmp rstcol
                .endproc


;    YesNo(msg)
;    ----------
yesno           .proc
                jsr gettemp
                ldy tempbuf
                bne _yn1
                iny
                rts

_yn1            lda tempbuf+1
                ora #$20
                cmp #'y'
                rts
                .endproc
