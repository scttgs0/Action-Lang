
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
Loop            .proc
                lda allocErr
                beq _1

                lda #<msgOutOfMem
                ldx #>msgOutOfMem
                jsr editor.display.CommandMsg

_1              .mba curCH,lastCH

                jsr mainbank.GetKey
                jsr editor.init.EditorInit._ENTRY3

                lda curCH
                ldy CH1                 ; prior key pressed
                cpy #$C0                ; Ctrl-Shft?
                bcs _2                  ;   yes

                ldy lastCH
                cpy #$1B                ; escape
                bne _3

                cmp #EOL
                beq Loop

                jsr editor.chr.InsertChar
                jmp Loop

; - - - - - - - - - - - - - - - - - - -

_2              ldx #<tblEditCtrlShft
                ldy #>tblEditCtrlShft
                bne _4

; - - - - - - - - - - - - - - - - - - -

_3              ldx #<tblEditCmd
                ldy #>tblEditCmd

_4              jsr mainmsc.Lookup
                jmp Loop

                .endproc


;--------------------------------------
;--------------------------------------

tblEditCmd      .addr jt_vecDispTb      ; default routine
                .byte $32               ; table size (#entries*3 - 1)

                .addr editor.command.ScrollUp
                .byte $1C
                .addr editor.command.ScrollDown
                .byte $1D
                .addr editor.command.ScrollRight
                .byte $1F
                .addr editor.command.ScrollLeft
zapScrlLft      .byte $1E
                .addr editor.chr.DeleteChar
zapDelChr       .byte $FE
                .addr editor.chr.BackSpc
zapBckSpc       .byte $7E
                .addr editor.chr.InsertChar
                .byte $60
                .addr editor.chr.InsertSpace
                .byte $FF
                .addr editor.chr.Return
                .byte EOL
                .addr editor.tab.Tab
                .byte $7F
                .addr editor.chr.Delete
                .byte $9C
                .addr editor.command.BottomLine._XIT
                .byte $1B
                .addr editor.window.Clear
                .byte $7D
                .addr editor.chr.Insert_2
                .byte $9D
                .addr editor.tab.Set
                .byte $9F
                .addr editor.tab.Clear
                .byte $9E

; - - - - - - - - - - - - - - - - - - -

tblEditCtrlShft .addr jt_vecDispTb+3    ; default routine
                .byte $47               ; table size (#entries*3 - 1)

                .addr editor.command.Front
                .byte $F6
                .addr editor.command.Back
                .byte $F7
                .addr editor.command.PageUp
                .byte $CE
                .addr editor.command.PageDown
                .byte $CF
                .addr editor.command.IndentLeft
                .byte $E0
                .addr editor.command.IndentRight
                .byte $E2
                .addr editor.io.FRead
                .byte $E8
                .addr editor.io.FWrite
                .byte $EE
                .addr editor.command.Paste
                .byte $CA
                .addr editor.command.InsertToggle
                .byte $CD
                .addr ampl.monitor.Monitor
                .byte $E5
                .addr editor.find.Find
                .byte $F8
                .addr editor.Substitute
                .byte $FE
                .addr editor.window.Window1
                .byte $DF
                .addr editor.window.Window2
                .byte $DE
                .addr editor.window.Delete
                .byte $FA
                .addr editor.chr.CSBS
                .byte $F4
                .addr editor.chr.CSRet
                .byte $CC
                .addr editor.chr.Undo
                .byte $CB
                .addr editor.display.TopLine
                .byte $F9
                .addr editor.display.EndLine
                .byte $EA
                .addr editor.tag.Set
                .byte $ED
                .addr editor.tag.Locate
                .byte $FD

; - - - - - - - - - - - - - - - - - - -

msgOutOfMem     .text 14," "
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
