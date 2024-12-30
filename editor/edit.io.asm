
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.io.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


io              .namespace

;======================================
; GetString(prompt, str, invert)
;======================================
GetString       .proc
                jsr ioDisplayStr
_next1          jsr bankGetKey

                tax
                cpx #$7E
                beq _3                  ; backspace

                cpx #$7D
                beq _3                  ; clear

_next2          ldy #$00
                clc
                lda (arg12),Y
                adc #$01

                cpx #$1B                ; ESC
                beq _1

                cpx #EOL
                beq _2

                cpy arg3                ; first char?
                beq _3                  ; yes, clear line

                stx arg3

                ldx CURSOR_X    ;!!COLCRS
                cpx RMARGN
                bcs _next1              ; don't go off screen

                sta (arg12),Y

                tay
                lda arg3
                sta (arg12),Y

                eor arg2
                jsr screen.PutChar

                jmp _next1

_1              lda #$00
                sta curCH
                sta (arg12),Y

                iny
                tya

_2              tay
                txa                     ; EOL
                sta (arg12),Y

                rts

_3              stx arg3

_next3          ldy #$00
                lda (arg12),Y
                beq _4

                sec
                sbc #$01
                sta (arg12),Y

                jsr screen.CursorLeft

                lda #$20
                eor arg2

                jsr screen.PutChar
                jsr screen.CursorLeft

                ldx arg3
                cpx #$7E
                bne _next3
                bra _next1

_4              cpx #$7D
                beq _next1
                bra _next2

                .endproc


;======================================
; FRead()
;======================================
FRead           .proc
                lda #$00
                sta inbuf

                lda #<rdmsg
                ldx #>rdmsg
                ldy #$04
                jsr FOpen

_next1          lda #$01
                jsr ioReadBuffer
                bmi _1

                jsr editor.memory.InsertByte

                lda allocErr
                beq _next1

                ldy #$16                ; file too big
                bne _2

_1              cpy #$88                ; EOF
                beq _3

_2              jsr ioSystemError
_3              jsr FWrite._ENTRY1

                jmp editor.display.CenterLine

;--------------------------------------

rdmsg           .ptext "Read? "

                .endproc


;======================================
; FWrite()
;======================================
FWrite          .proc
                lda #<wrtmsg
                ldx #>wrtmsg
                ldy #$08
                jsr FOpen

                jsr ioChkCursor._ENTRY1
                beq _1

_next1          jsr ioLoadBuffer

                ; inc COLOR4            ; let user know we're here

                nop
                nop
                nop

                lda #$01
                jsr ioWriteBuffer
                bmi _1

                jsr mscNextDown
                bne _next1

                lda #$00
                sta dirty

_ENTRY1         lda #$01
                jsr ioClose
                jsr ioResetCursor
                jmp ioDisplayOn

_1              jsr ioSystemError
                jmp _ENTRY1

;--------------------------------------

wrtmsg          .ptext "Write? "

                .endproc


;======================================
; FOpen(prompt, mode)
;======================================
FOpen           .proc
                sta arg10
                stx arg11
                sty opmode

;               jsr ClnLn               ; in SaveWd
                jsr editor.display.SaveWindow
                ;!! jsr ioRestoreCursorChar ; unnecessary

                ldy #<inbuf
                lda #>inbuf
                sta arg3

                lda arg10
                ldx arg11
                jsr editor.window.CommandString

                lda #$01
                jsr ioClose

                ldy inbuf
                beq _5

                ldx opmode
                lda #':'
                cmp inbuf+2
                beq _1

                cmp inbuf+3
                beq _1

                iny
                iny
                sty inbuf

_next1          lda inbuf,Y
                sta inbuf+2,Y

                dey
                bne _next1

                lda #':'
                sta inbuf+2
                bra _2

_1              lda inbuf+1
                cmp #'?'                ; read directory?
                bne _3                  ;   no

                ldx #$06
_2              lda #'D'
                sta inbuf+1

_3              stx arg3
                jsr ioDisplayOff

                lda #$01
                sta arg4                ; clear high bit for cassette

                ldx #<inbuf
                ldy #>inbuf
                jsr ioOpen
                bmi _4

                lda arg3                ; see if directory
                eor #$06
                bne _XIT

                sta inbuf               ; clear inbuf

_XIT            rts

_4              pla
                pla                     ; pop return

                jmp ioSystemError

_5              pla
                pla

                rts
                .endproc


;======================================
; InitKeys()
;======================================
InitKeys        .proc
                lda #$07
                jsr ioClose

                lda #$04
                sta arg3                ; read only

                lda #$07
                ldx #<keybd
                ldy #>keybd

                jmp ioOpen

;--------------------------------------

keybd           .ptext "K:"

                .endproc


;======================================
; Test if key in buffer
;======================================
GotKey          .proc
                lda KEYCHAR             ; key down?
                eor #$FF                ; flip the bits

                rts
                .endproc

                .endnamespace
