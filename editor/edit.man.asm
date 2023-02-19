
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
                jsr CommandMsg

_fm1            lda curch
                sta lastch
                jsr GetKey

                jsr EditorInit.fcmsg1

                lda curch
                ldy CH1
                cpy #$c0                ; Ctrl-Shft
                bcs _fmcs

                ldy lastch
                cpy #$1b                ; escape
                bne _fmch

                cmp #eol
                beq floop

                jsr InsertChar

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

fmcmd           .addr jt_disptb         ; default routine
                .byte 50                ; table size
                .addr ScrollUp
                .byte $1c
                .addr ScrollDown
                .byte $1d
                .addr ScrollRight
                .byte $1f
                .addr ScrollLeft
zap2            .byte $1e
                .addr DeleteChar
zap3            .byte $fe
                .addr BackSpc
zap4            .byte $7e
                .addr InsertChar
                .byte $60
                .addr InsertSpace
                .byte $ff
                .addr Return_
                .byte eol
                .addr Tab_
                .byte $7f
                .addr Delete_
                .byte $9c
                .addr BottomLine._XIT
                .byte $1b
                .addr Clear_
                .byte $7d
                .addr Insert_
                .byte $9d
                .addr SetTab
                .byte $9f
                .addr ClearTab
                .byte $9e

fmcscmd         .addr jt_disptb+3       ; default
                .byte 71                ; table size
                .addr Front
                .byte $f6
                .addr Back
                .byte $f7
                .addr PageUp
                .byte $ce
                .addr PageDown
                .byte $cf
                .addr IndentLeft
                .byte $e0
                .addr IndentRight
                .byte $e2
                .addr FRead
                .byte $e8
                .addr FWrite
                .byte $ee
                .addr Paste
                .byte $ca
                .addr InsertToggle
                .byte $cd
                .addr Monitor
                .byte $e5
                .addr Find
                .byte $f8
                .addr Substitute
                .byte $fe
                .addr Window1
                .byte $df
                .addr Window2
                .byte $de
                .addr DeleteWindow
                .byte $fa
                .addr csbs
                .byte $f4
                .addr csret
                .byte $cc
                .addr Undo
                .byte $cb
                .addr TopLine
                .byte $f9
                .addr EndLine
                .byte $ea
                .addr SetTag
                .byte $ed
                .addr LocateTag
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
