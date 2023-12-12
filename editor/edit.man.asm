
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.man.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;======================================
; Main program for EDIT/FLASH
;======================================
floop           .proc
                lda allocerr
                beq _1

                lda #<outmem
                ldx #>outmem
                jsr cmdmsg

_1              lda curch
                sta lastch

                jsr getkey
                jsr einit._ENTRY3

                lda curch
                ldy CH1
                cpy #$C0                ; Ctrl-Shft
                bcs _2

                ldy lastch
                cpy #$1B                ; escape
                bne _3

                cmp #eol
                beq floop

                jsr insrtch

                jmp floop

_2              ldx #<fmcscmd
                ldy #>fmcscmd
                bne _4

_3              ldx #<fmcmd
                ldy #>fmcmd

_4              jsr lookup

                jmp floop

                .endproc


;--------------------------------------
;--------------------------------------

fmcmd           .word disptb            ; default routine
                .byte 50                ; table size
                .word scrlup
                .byte $1c
                .word scrldwn
                .byte $1d
                .word scrlrt
                .byte $1f
                .word scrllft
zap2            .byte $1e
                .word delch
zap3            .byte $fe
                .word backsp
zap4            .byte $7e
                .word insrtch
                .byte $60
                .word insrtsp
                .byte $ff
                .word return
                .byte eol
                .word tab
                .byte $7f
                .word delete
                .byte $9c
                .word botln._XIT
                .byte $1b
                .word clear
                .byte $7d
                .word insrt
                .byte $9d
                .word settab
                .byte $9f
                .word clrtab
                .byte $9e

fmcscmd         .word disptb+3          ; default
                .byte 71                ; table size
                .word front
                .byte $f6
                .word back
                .byte $f7
                .word pgup
                .byte $ce
                .word pgdwn
                .byte $cf
                .word indntl
                .byte $e0
                .word indntr
                .byte $e2
                .word fread
                .byte $e8
                .word fwrite
                .byte $ee
                .word paste
                .byte $ca
                .word insrtt
                .byte $cd
                .word monitor
                .byte $e5
                .word find
                .byte $f8
                .word subst
                .byte $fe
                .word wind1
                .byte $df
                .word wind2
                .byte $de
                .word delwd
                .byte $fa
                .word csbs
                .byte $f4
                .word csret
                .byte $cc
                .word undo
                .byte $cb
                .word topln
                .byte $f9
                .word endln
                .byte $ea
                .word settag
                .byte $ed
                .word loctag
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
