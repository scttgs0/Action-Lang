;======================================
;   FILE: main.bnk.asm
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


en0             .text 5,"Error",$c0
                .addr error
                .byte 3,138,138,138

en1             .text 3,"EOF",$9a
                .addr eof

en2             .text 5,"color",$8a
                .addr $03_02FD ;!! FILDAT

en3             .text 4,"LIST",$8a
                .addr list

en4             .text 6,"device",$8a
                .word device

en5             .text 5,"TRACE",$8a
                .addr trace


;======================================
;   CStrt()
;======================================
cstart          .proc
                ldy #$03 ;!! ebank
                sty curbank
                sty $03_D503 ;!! bank+ebank

    ; initialize memory boundries
                lda #<$03_7FFF
                sta MEMTOP
                lda #>$03_7FFF
                sta MEMTOP+1

                lda #<$03_0600
                sta MEMLO
                lda #>$03_0600
                sta MEMLO+1

    ; initialize left and right margins
                stz LMARGN
                lda #97
                sta RMARGN

    ; initialize cursor location
                stz COLCRS
                stz ROWCRS

    ; initialize Tab positions
                ldy #$00
_nextBatch      ldx #$00
_nextTab        lda InitialTabs,x
                sta TABMAP,y

                iny
                inx
                cpx #$03
                bne _nextTab

                cpy #$10
                bne _nextBatch

                lda #<$037FE0
                sta INITAD
                lda #>$037FE0
                sta INITAD+1

                stz WARMST
                jmp Start

;--------------------------------------

InitialTabs     .byte %00100100
                .byte %10010010
                .byte %01001001

                .endproc


;======================================
;   GetName(char)
;======================================
getname         .proc
                sta $03_D500 ;!! bank+lbank
                jsr lgetname

                .endproc


;======================================
;   RstBank()
;======================================
rstbank         .proc
                php
                pha
                tya
                ldy curbank
rbank1          sta bank,y
                tay
                pla
                plp
                rts
                .endproc


;======================================
;   Run(address)
;======================================
run             .proc
    ; reset Error routine
                ldy #<splerr
                sty error+1
                ldy #>splerr
                sty error+2

                jsr lproceed
                jsr jsrind

                jmp editbank

                .endproc


;======================================
;   Compile()
;======================================
compile         .proc
                ldy #$09 ;!! cbank
                sty curbank
                sty $03_D509 ;!! bank+cbank
                jsr ccompile

                .endproc


;======================================
;   EditBank()
;======================================
editbank        .proc
                php
                pha
                tya
                ldy #$03 ;!! ebank
                sty curbank
                jmp rstbank.rbank1

                .endproc


;======================================
;   GetAlias()
;======================================
getalias        .proc
                lda #1
                jsr getprop

                cpx #0
                beq _gal1

                sta addr
                stx addr+1
                sta $03_D500 ;!! bank+lbank
                lda #0
                jsr getprop

                sta token
                jmp rstbank

_gal1           jmp mnum._varerr

                .endproc


;======================================
;   GNlocal()
;======================================
gnlocal         .proc
                sta $03_D500 ;!! bank+lbank
                jsr lgetname.lgnlocal

                jmp rstbank

                .endproc


;======================================
;   CStmtList()
;======================================
cstmtlst        .proc
                ldy #$09 ;!! cbank
                sty curbank
                sta $03_D509 ;!! bank+cbank
                jsr stmtlist

                jmp editbank

                .endproc


;======================================
;
;======================================
mgett1          .proc
                jsr editbank
                jsr GetTemp.gett1

                .endproc


;======================================
;   LProceed()
;======================================
lproceed        .proc
                ldy #$00 ;!! lbank
                sty curbank
                sty $03_D500 ;!! bank+lbank
                rts
                .endproc


;======================================
;
;======================================
options         .proc
                jsr lproceed
                jsr setopts

                jmp editbank
                .endproc


;======================================
;
;======================================
getkey          .proc
                sta $03_D500 ;!! bank+lbank
                jsr lgetkey

                jmp rstbank

                .endproc


;======================================
;
;======================================
splerr          .proc
                sta $03_D500 ;!! bank+lbank
                jmp lsplerr

                .endproc


;======================================
;
;======================================
emloop          .proc
                jsr editbank

                jmp monitor._mloop

                .endproc


;======================================
;
;======================================
getargs         .proc
                pha                     ; save arg type load flag
                sty $03_D500 ;!! bank+lbank
                lda #1
                jsr getprop

                sta addr
                stx addr+1

                pla
                bne _ga2                ; don't load arg types

                sta abt                 ; A=0
                sta abt+1               ; flag as temp args
                sta abt+2               ; (default)

                ldy #2
                lda (props),y
                sta numargs
                beq _ga2

                tax
                cpx #9
                bcs _ga2

_ga1            iny
                lda (props),y
                dex
                sta argtypes,x          ; args inverted
                bne _ga1

_ga2            jmp rstbank

                .endproc


;======================================
;
;======================================
prth            .proc                   ; call only from LBANK!
                sty $03_D503 ;!! bank+ebank
                jsr printh

                sty $03_D500 ;!! bank+lbank
                jmp chkerr

                .endproc


;======================================
; go directly to DOS, do NOT pass GO,
; do NOT collect $200, but setup LIB
;======================================
dret            .proc                   ; Dret()
                jsr lproceed

                jmp ($0A) ;!! (DOSVEC)

                .endproc
