
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.main.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


main           .namespace

;======================================
; Main program for EDIT/FLASH
;======================================
Loop           .proc
                lda allocerr
                beq _1

                lda #<outmem
                ldx #>outmem
                jsr editor.display.CommandMsg

_1              lda curch
                sta lastch

                jsr bankGetKey
                jsr editor.init.EditorInit._ENTRY3

                lda curch
                ldy CH1
                cpy #$C0                ; Ctrl-Shft?
                bcs _2                  ;   yes

                ldy lastch
                cpy #$1B                ; escape
                bne _3

                cmp #EOL
                beq Loop

                jsr editor.chr.InsertChar

                jmp Loop

_2              ldx #<fmcscmd
                ldy #>fmcscmd
                bne _4

_3              ldx #<fmcmd
                ldy #>fmcmd

_4              jsr mscLookup

                jmp Loop

                .endproc


;--------------------------------------
;--------------------------------------

fmcmd           .addr jt_disptb         ; default routine
                .byte 50                ; table size
                .addr editor.command.ScrollUp
                .byte $1c
                .addr editor.command.ScrollDown
                .byte $1d
                .addr editor.command.ScrollRight
                .byte $1f
                .addr editor.command.ScrollLeft
zap2            .byte $1e
                .addr editor.chr.DeleteChar
zap3            .byte $fe
                .addr editor.chr.BackSpc
zap4            .byte $7e
                .addr editor.chr.InsertChar
                .byte $60
                .addr editor.chr.InsertSpace
                .byte $ff
                .addr editor.chr.Return
                .byte EOL
                .addr editor.tab.Tab
                .byte $7f
                .addr editor.chr.Delete
                .byte $9c
                .addr editor.command.BottomLine._XIT
                .byte $1b
                .addr editor.window.Clear
                .byte $7d
                .addr editor.chr.Insert_2
                .byte $9d
                .addr editor.tab.Set
                .byte $9f
                .addr editor.tab.Clear
                .byte $9e

fmcscmd         .addr jt_disptb+3       ; default
                .byte 71                ; table size
                .addr editor.command.Front
                .byte $f6
                .addr editor.command.Back
                .byte $f7
                .addr editor.command.PageUp
                .byte $ce
                .addr editor.command.PageDown
                .byte $cf
                .addr editor.command.IndentLeft
                .byte $e0
                .addr editor.command.IndentRight
                .byte $e2
                .addr editor.io.FRead
                .byte $e8
                .addr editor.io.FWrite
                .byte $ee
                .addr editor.command.Paste
                .byte $ca
                .addr editor.command.InsertToggle
                .byte $cd
                .addr ampl.monitor.Monitor
                .byte $e5
                .addr editor.find.Find
                .byte $f8
                .addr editor.Substitute
                .byte $fe
                .addr editor.window.Window1
                .byte $df
                .addr editor.window.Window2
                .byte $de
                .addr editor.window.Delete
                .byte $fa
                .addr editor.chr.CSBS
                .byte $f4
                .addr editor.chr.CSRet
                .byte $cc
                .addr editor.chr.Undo
                .byte $cb
                .addr editor.display.TopLine
                .byte $f9
                .addr editor.display.EndLine
                .byte $ea
                .addr editor.tag.Set
                .byte $ed
                .addr editor.tag.Locate
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

            .endnamespace
