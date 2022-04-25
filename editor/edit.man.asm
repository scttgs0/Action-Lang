;======================================
;   FILE: edit.man.asm
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


;======================================
; Main program for EDIT/FLASH
;======================================
floop           .proc
                lda allocerr
                beq _fm1

                lda #<outmem
                ldx #>outmem
                jsr cmdmsg

_fm1            lda curch
                sta lastch
                jsr getkey

                jsr einit.fcmsg1

                lda curch
                ldy $03_02F2 ;!! CH1
                cpy #$c0                ; Ctrl-Shft
                bcs _fmcs

                ldy lastch
                cpy #$1b                ; escape
                bne _fmch

                cmp #eol
                beq floop

                jsr insrtch

                jmp floop

_fmcs           ldx #<fmcscmd
                ldy #>fmcscmd
                bne _fmlu

_fmch           ldx #<fmcmd
                ldy #>fmcmd

_fmlu           jsr lookup

                jmp floop

                .endproc

;--------------------------------------
;--------------------------------------

fmcmd           .addr disptb            ; default routine
                .byte 50                ; table size
                .addr scrlup
                .byte $1c
                .addr scrldwn
                .byte $1d
                .addr scrlrt
                .byte $1f
                .addr scrllft
zap2            .byte $1e
                .addr delch
zap3            .byte $fe
                .addr backsp
zap4            .byte $7e
                .addr insrtch
                .byte $60
                .addr insrtsp
                .byte $ff
                .addr return
                .byte eol
                .addr tab
                .byte $7f
                .addr delete
                .byte $9c
                .addr botln.escape
                .byte $1b
                .addr clear
                .byte $7d
                .addr insrt
                .byte $9d
                .addr settab
                .byte $9f
                .addr clrtab
                .byte $9e

fmcscmd         .addr disptb+3          ; default
                .byte 71                ; table size
                .addr front
                .byte $f6
                .addr back
                .byte $f7
                .addr pgup
                .byte $ce
                .addr pgdwn
                .byte $cf
                .addr indntl
                .byte $e0
                .addr indntr
                .byte $e2
                .addr fread
                .byte $e8
                .addr fwrite
                .byte $ee
                .addr paste
                .byte $ca
                .addr insrtt
                .byte $cd
                .addr monitor
                .byte $e5
                .addr find
                .byte $f8
                .addr subst
                .byte $fe
                .addr wind1
                .byte $df
                .addr wind2
                .byte $de
                .addr delwd
                .byte $fa
                .addr csbs
                .byte $f4
                .addr csret
                .byte $cc
                .addr undo
                .byte $cb
                .addr topln
                .byte $f9
                .addr endln
                .byte $ea
                .addr settag
                .byte $ed
                .addr loctag
                .byte $fd

outmem          .text 14," "
        .enc "atari-screen-inverse"
                .text "Out"
        .enc "none"

                .text " "
        .enc "atari-screen-inverse"
                .text "of"
        .enc "none"

                .text " "
        .enc "atari-screen-inverse"
                .text "Memory"
        .enc "none"
