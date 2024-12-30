
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: lib.str.asm
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


str             .namespace

;======================================
; INT FUNC SCompare(STRING a,b)
;--------------------------------------
; result returned is:
;   =0 if a=b
;   low 0 if a<b
;   high 0 if a>b
;======================================
SCompare        .proc
                sta arg4
                stx arg5
                sty arg2

                ldy #$00
                sty args
                sty args+1

                lda (arg4),Y
                cmp (arg2),Y
                beq _1

                jsr _3

_1              cmp #$00
                bne _2

                rts

_2              sta arg6

_next1          iny
                lda (arg4),Y
                cmp (arg2),Y
                bne _3

                cpy arg6
                bcc _next1

                rts

_3              ldx #$FF
                stx args
                bcc _4

                lda (arg2),Y
                inx
_4              stx args+1

                rts
                .endproc


;======================================
; PROC SCopy(STRING dest, src)
;--------------------------------------
; dest = src
;======================================
SCopy           .proc
                sta arg0
                stx arg1
                sty arg2

                ldy #$00
                lda (arg2),Y
_ENTRY1         sta (arg0),Y
                beq _XIT

_ENTRY2         tay
_next1          lda (arg2),Y
                sta (arg0),Y

                dey
                bne _next1

_XIT            rts
                .endproc


;======================================
; PROC SCopyS(STRING dest, src, BYTE start, stop)
;--------------------------------------
; if LEN(src)<stop then stop=LEN(src)
; dest = src(start, stop)
;======================================
SCopyS          .proc
                sta arg0
                stx arg1
                sty arg2

                ldy #$00
                lda (arg2),Y
                cmp arg5
                bcs _1

                sta arg5

_1              dec arg4

                clc
                lda arg2
                adc arg4
                sta arg2
                bcc _2

                inc arg3

_2              sec
                lda arg5
                sbc arg4
                bcs _XIT

                lda #$00

_XIT            jmp SCopy._ENTRY1

                .endproc


;======================================
; PROC SAssign(STRING dest, src, BYTE start, stop)
;--------------------------------------
; IF stop-start+1>LEN(src) THEN
;   stop = LEN(src)+start-1
; IF LEN(dest)<stop THEN
;   LEN(dest) = stop
; dest(start, stop) = src
;======================================
SAssign         .proc
                sta arg0
                stx arg1
                sty arg2

                ldy #$00
                lda (arg2),Y
                beq _XIT1

                sta arg6

                dec arg4

                sec
                lda arg5
                sbc arg4
                beq _XIT1
                bcs _1

_XIT1           rts

_1              tax
                cmp  arg6
                bcc _2

                clc
                lda arg6
                tax
                adc arg4
                sta arg5

_2              lda arg5
                cmp (arg0),Y
                bcc _3

                sta (arg0),Y

                clc
_3              lda arg0
                adc arg4
                sta arg0
                bcc _4

                inc arg1

_4              txa
                jmp SCopy._ENTRY2

                .endproc


;--------------------------------------
; symbol table
;--------------------------------------

;:EN6           .byte 6,"PrintF",200
;               .addr PrtF ; #117
;               .byte 6,17,12,12,12,12,12

_en7            .ptext "Open"
                .byte 200
                .addr lib.io.Open       ; #96
                .byte 4,138,17,138,138
_en8            .ptext "PrintE"
                .byte 200
                .addr lib.io.PrintE     ; #116
                .byte 1,17
_en9            .ptext "PrintDE"
                .byte 200
                .addr lib.io.PrintDE    ; #75
                .byte 2,138,17
_en10           .ptext "Close"
                .byte 200
                .addr lib.io.Close      ; #253
                .byte 1,138
_en11           .ptext "Print"
                .byte 200
                .addr lib.io.Print      ; #135
                .byte 1,17
_en12           .ptext "PrintD"
                .byte 200
                .addr lib.io.PrintD     ; #115
                .byte 2,138,17
_en13           .ptext "InputS"
                .byte 200
                .addr lib.io.InputS     ; #249
                .byte 1,17
_en14           .ptext "InputSD"
                .byte 200
                .addr lib.io.InputSD    ; #87
                .byte 2,138,17
_en15           .ptext "InputMD"
                .byte 200
                .addr lib.io.InputMD    ; #75
                .byte 3,138,17,138
_en16           .ptext "GetD"
                .byte 202
                .addr lib.io.GetD       ; #138
                .byte 1,138
_en17           .ptext "PutE"
                .byte 200
                .addr lib.io.PutE       ; #162
                .byte 0
_en18           .ptext "Put"
                .byte 200
                .addr lib.io.Put        ; #158
                .byte 1,137
_en19           .ptext "PutD"
                .byte 200
                .addr lib.io.PutD       ; #161
                .byte 2,138,137
_en20           .ptext "PutDE"
                .byte 200
                .addr lib.io.PutDE      ; #168
                .byte 1,138
_en21           .ptext "XIO"
                .byte 200
                .addr lib.io.XIO        ; #225
                .byte 6,138,138,138,138,138,17
_en22           .ptext "PrintB"
                .byte 200
                .addr lib.io.PrintB     ; #113
                .byte 1,138
_en23           .ptext "PrintBE"
                .byte 200
                .addr lib.io.PrintBE    ; #71
                .byte 1,138

;:EN24          .byte 7,"PrintBD",200
;               .addr PrtBD ; #70
;               .byte 2,138,138

_en25           .ptext "PrintBDE"
                .byte 200
                .addr lib.io.PrintBDE   ; #241
                .byte 2,138,138
_en26           .ptext "PrintC"
                .byte 200
                .addr lib.io.PrintC     ; #114
                .byte 1,12
_en27           .ptext "PrintCE"
                .byte 200
                .addr lib.io.PrintCE    ; #73
                .byte 1,12
_en28           .ptext "PrintCD"
                .byte 200
                .addr lib.io.PrintCD    ; #72
                .byte 2,138,12
_en29           .ptext "PrintCDE"
                .byte 200
                .addr lib.io.PrintCDE   ; #245
                .byte 2,138,12
_en30           .ptext "PrintI"
                .byte 200
                .addr lib.io.PrintI     ; #120
                .byte 1,11
_en31           .ptext "PrintID"
                .byte 200
                .addr lib.io.PrintID    ; #84
                .byte 2,138,11
_en32           .ptext "PrintIE"
                .byte 200
                .addr lib.io.PrintIE    ; #85
                .byte 1,11
_en33           .ptext "PrintIDE"
                .byte 200
                .addr lib.io.PrintIDE   ; #13
                .byte 2,138,11
_en34           .ptext "InputB"
                .byte 202
                .addr lib.io.InputB     ; #232
                .byte 0
_en35           .ptext "InputBD"
                .byte 202
                .addr lib.io.InputBD    ; #53
                .byte 1,138
_en36           .ptext "InputI"
                .byte 203
                .addr lib.io.InputI     ; #239
                .byte 0
_en37           .ptext "InputID"
                .byte 203
                .addr lib.io.InputID    ; #67
                .byte 1,138
_en38           .ptext "InputC"
                .byte 204
                .addr lib.io.InputC     ; #233
                .byte 0
_en39           .ptext "InputCD"
                .byte 204
                .addr lib.io.InputCD    ; #55
                .byte 1,138
_en40           .ptext "ValB"
                .byte 202
                .addr lib.io.ValB       ; #207
                .byte 1,17
_en41           .ptext "ValI"
                .byte 203
                .addr lib.io.ValI       ; #214
                .byte 1,17
_en42           .ptext "ValC"
                .byte 204
                .addr lib.io.ValC       ; #208
                .byte 1,17
_en43           .ptext "StrB"
                .byte 200
                .addr lib.io.StrB       ; #223
                .byte 2,138,17
_en44           .ptext "StrI"
                .byte 200
                .addr lib.io.StrI       ; #230
                .byte 2,11,17
_en45           .ptext "StrC"
                .byte 200
                .addr lib.io.StrC       ; #224
                .byte 2,12,17
_en46           .ptext "Note"
                .byte 200
                .addr lib.io.Note       ; #89
                .byte 3,138,20,18
_en47           .ptext "Point"
                .byte 200
                .addr lib.io.Point      ; #110
                .byte 3,138,12,138
_en48           .ptext "Graphics"
                .byte 200
                .addr lib.gr.Graphics   ; #108
                .byte 1,138
_en49           .ptext "DrawTo"
                .byte 200
                .addr lib.gr.DrawTo     ; #231
                .byte 2,12,138
_en50           .ptext "Position"
                .byte 200
                .addr lib.gr.Position   ; #94
                .byte 2,12,138
_en51           .ptext "Locate"
                .byte 202
                .addr lib.gr.Locate     ; #97
                .byte 2,12,138
_en52           .ptext "Plot"
                .byte 200
                .addr lib.gr.Plot       ; #131
                .byte 2,12,138
_en53           .ptext "SetColor"
                .byte 200
                .addr lib.gr.SetColor   ; #6
                .byte 3,138,138,138
_en54           .ptext "Fill"
                .byte 200
                .addr lib.gr.Fill       ; #122
                .byte 2,12,138
_en55           .ptext "Rand"
                .byte 202
                .addr lib.msc.Rand      ; #117
                .byte 1,138
_en56           .ptext "Sound"
                .byte 200
                .addr lib.msc.Sound     ; #31
                .byte 4,138,138,138,138
_en57           .ptext "SndRst"
                .byte 200
                .addr lib.msc.SndRst    ; #73
                .byte 0
_en58           .ptext "Paddle"
                .byte 202
                .addr Paddle            ; #254
                .byte 1,138
_en59           .ptext "PTrig"
                .byte 202
                .addr lib.msc.PTrig     ; #164
                .byte 1,138
_en60           .ptext "Stick"
                .byte 202
                .addr lib.msc.Stick     ; #8
                .byte 1,138
_en61           .ptext "STrig"
                .byte 202
                .addr STrig             ; #52
                .byte 1,138

;:EN62          .byte 4,"Peek",202
;               .addr Peek              ; #73
;               .byte 1,12
;:EN63          .byte 5,"PeekC",204
;               .addr PeekC             ; #245
;               .byte 1,12

_en64           .ptext "Poke"
                .byte 200
                .addr lib.msc.Poke      ; #120
                .byte 2,12,138
_en65           .ptext "PokeC"
                .byte 200
                .addr lib.msc.PokeC     ; #83
                .byte 2,12,12
_en66           .ptext "Zero"
                .byte 200
                .addr lib.msc.Zero      ; #88
                .byte 2,18,12
_en67           .ptext "SetBlock"
                .byte 200
                .addr lib.msc.SetBlock  ; #203
                .byte 3,18,12,138

;:EN68          .byte 9,"MoveBlock",200
;               .word MoveBlock         ; #85
;               .byte 3,18,18,12

_en69           .ptext "Break"
                .byte 200
                .addr lib.msc.Break     ; #183
_en70           .ptext "SCompare"
                .byte 203
                .addr SCompare          ; #92
                .byte 2,17,17
_en71           .ptext "SCopy"
                .byte 200
                .addr SCopy             ; #192
                .byte 2,17,17
_en72           .ptext "SCopyS"
                .byte 200
                .addr SCopyS            ; #244
                .byte 4,17,17,138,138
_en73           .ptext "SAssign"
                .byte 200
                .addr SAssign           ; #23
                .byte 4,17,17,138,138


;--------------------------------------
; hash table
;--------------------------------------

libst           .byte 0                     ; 1
                .byte >mainbank.en1         ; EOF #1
                .byte >mainbank.en5         ; TRACE #2
                .byte 0,0,0                 ; 3
                .byte >_en53                ; SetColor #6
                .byte 0                     ; 1
                .byte >_en60                ; Stick #8
                .byte 0,0,0,0               ; 4
                .byte >_en33                ; PrintIDE #13
                .byte >mainbank.en2         ; color #14
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte >_en73                ; SAssign #23
                .byte 0,0,0,0,0,0,0         ; 7
                .byte >_en56                ; Sound #31
                .byte >mainbank.en3         ; LIST #32
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0,0     ; 19
                .byte >_en61                ; STrig #52
                .byte >_en35                ; InputBD #53
                .byte 0                     ; 1
                .byte >_en39                ; InputCD #55
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0                     ; 11
                .byte >_en37                ; InputID #67
                .byte 0,0                   ; 2
                .byte >_en24                ; PrintBD #70
                .byte >_en23                ; PrintBE #71
                .byte >_en28                ; PrintCD #72
                .byte >_en27                ; PrintCE #73
                .byte >_en57                ; SndRst #74
                .byte >_en9                 ; PrintDE #75
                .byte >_en15                ; InputMD #76
                .byte >_en62                ; Peek #77
                .byte 0,0,0,0,0             ; 5
                .byte >_en65                ; PokeC #83
                .byte >_en31                ; PrintID #84
                .byte >_en32                ; PrintIE #85
                .byte >_en68                ; MoveBlock #86
                .byte >_en14                ; InputSD #87
                .byte >_en66                ; Zero #88
                .byte >_en46                ; Note #89
                .byte 0,0                   ; 2
                .byte >mainbank.en4         ; device #92
                .byte >_en70                ; SCompare #93
                .byte >_en50                ; Position #94
                .byte 0                     ; 1
                .byte >_en7                 ; Open #96
                .byte >_en51                ; Locate #97
                .byte 0,0,0,0,0,0,0,0,0,0   ; 10
                .byte >_en48                ; Graphics #108
                .byte 0                     ; 1
                .byte >_en47                ; Point #110
                .byte 0,0                   ; 2
                .byte >_en22                ; PrintB #113
                .byte >_en26                ; PrintC #114
                .byte >_en12                ; PrintD #115
                .byte >_en8                 ; PrintE #116
                .byte >_en6                 ; PrintF #117
                .byte >_en55                ; Rand #118
                .byte 0                     ; 1
                .byte >_en30                ; PrintI #120
                .byte >_en64                ; Poke #121
                .byte >_en54                ; Fill #122
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte >_en52                ; Plot #131
                .byte 0,0,0                 ; 3
                .byte >_en11                ; Print #135
                .byte 0,0                   ; 2
                .byte >_en16                ; GetD #138
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0       ; 18
                .byte >mainbank.en0         ; Error #157
                .byte >_en18                ; Put #158
                .byte 0,0                   ; 2
                .byte >_en19                ; PutD #161
                .byte >_en17                ; PutE #162
                .byte 0                     ; 1
                .byte >_en59                ; PTrig #164
                .byte 0,0,0                 ; 3
                .byte >_en20                ; PutDE #168
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0               ; 14
                .byte >_en69                ; Break #183
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte >_en71                ; SCopy #192
                .byte 0,0,0,0,0,0,0,0,0,0   ; 10
                .byte >_en67                ; SetBlock #203
                .byte 0,0,0                 ; 3
                .byte >_en40                ; ValB #207
                .byte >_en42                ; ValC #208
                .byte 0,0,0,0,0             ; 5
                .byte >_en41                ; ValI #214
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte >_en43                ; StrB #223
                .byte >_en45                ; StrC #224
                .byte >_en21                ; XIO #225
                .byte 0,0,0,0               ; 4
                .byte >_en44                ; StrI #230
                .byte >_en49                ; DrawTo #231
                .byte >_en34                ; InputB #232
                .byte >_en38                ; InputC #233
                .byte 0,0,0,0,0             ; 5
                .byte >_en36                ; InputI #239
                .byte 0                     ; 1
                .byte >_en25                ; PrintBDE #241
                .byte 0,0                   ; 2
                .byte >_en72                ; SCopyS #244
                .byte >_en29                ; PrintCDE #245
                .byte >_en63                ; PeekC #246
                .byte 0,0                   ; 2
                .byte >_en13                ; InputS #249
                .byte 0,0,0                 ; 3
                .byte >_en10                ; Close #253
                .byte >_en58                ; Paddle #254
                .byte 0                     ; 1

                .byte 0                     ; 1
                .byte <mainbank.en1
                .byte <mainbank.en5
                .byte 0,0,0                 ; 3
                .byte <_en53
                .byte 0                     ; 1
                .byte <_en60
                .byte 0,0,0,0               ; 4
                .byte <_en33
                .byte <mainbank.en2
                .byte 0                     ; 1

;               .byte 0,0,0,0,0,0,0,0       ; 7


;======================================
;   STrig()
;======================================
STrig           .proc
                tax
                ;!!lda TRIG0,X
                sta args

                rts
                .endproc


;--------------------------------------
;--------------------------------------

                .byte <_en73
;               .byte 0,0,0,0,0,0,0     ; 7


;======================================
; Paddle()
;======================================
Paddle          .proc
                tax
                ;!!lda PADDL0,X
                lda #$1F    ; HACK:
                sta args

                rts
                .endproc

;--------------------------------------
;--------------------------------------

                .byte <_en56
                .byte <mainbank.en3
                .byte 0,0                   ; 2

;               .byte 0,0,0,0,0,0,0,0,0,0
;               .byte 0,0,0,0,0,0,0,0,0     ; 17

_en6            .ptext "PrintF"
                .byte 200
                .addr lib.io.PrintF         ; #117
                .byte 6,17,12,12,12,12,12

                .byte <_en61
                .byte <_en35
                .byte 0                     ; 1
                .byte <_en39

;               .byte 0,0,0,0,0,0,0,0,0,0
;               .byte 0                     ; 11

_en63           .ptext "PeekC"
                .byte 204
                .addr lib.msc.PeekC         ; #245
                .byte 1,12

                .byte <_en37
                .byte 0,0                   ; 2
                .byte <_en24
                .byte <_en23
                .byte <_en28
                .byte <_en27
                .byte <_en57
                .byte <_en9
                .byte <_en15
                .byte <_en62
                .byte 0,0,0,0,0             ; 5
                .byte <_en65
                .byte <_en31
                .byte <_en32
                .byte <_en68
                .byte <_en14
                .byte <_en66
                .byte <_en46
                .byte 0,0                   ; 2
                .byte <mainbank.en4
                .byte <_en70
                .byte <_en50
                .byte 0                     ; 1
                .byte <_en7
                .byte <_en51


;               .byte 0,0,0,0,0,0,0,0,0,0   ; 10

copyright       .byte 8,99,9,17,25,24,19,33,35,51   ; (c)1983ACS (in internal char. QCODE)

                .byte <_en48
                .byte 0                     ; 1
                .byte <_en47
                .byte 0,0                   ; 2
                .byte <_en22
                .byte <_en26
                .byte <_en12
                .byte <_en8
                .byte <_en6
                .byte <_en55
                .byte 0                     ; 1
                .byte <_en30
                .byte <_en64
                .byte <_en54
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte <_en52
                .byte 0,0,0                 ; 3
                .byte <_en11
                .byte 0,0                   ; 2
                .byte <_en16
                .byte 0                     ; 1


;               .byte 0,0,0,0,0,0,0,0,0,0
;               .byte 0,0,0,0,0,0,0,0       ; 17

_en68           .ptext "MoveBlock"
                .byte 200
                .addr lib.msc.MoveBlock     ; #85
                .byte 3,18,18,12

                .byte <mainbank.en0
                .byte <_en18
                .byte 0,0                   ; 2
                .byte <_en19
                .byte <_en17
                .byte 0                     ; 1
                .byte <_en59
                .byte 0,0,0                 ; 3
                .byte <_en20


;               .byte 0,0,0,0,0,0,0,0,0,0
;               .byte 0,0,0,0               ; 14

_en24           .ptext "PrintBD"
                .byte 200
                .addr lib.io.PrintBD        ; #70
                .byte 2,138,138

                .byte <_en69
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte <_en71


;               .byte 0,0,0,0,0,0,0,0,0,0   ; 10

_en62           .ptext "Peek"
                .byte 202
                .addr lib.msc.Peek          ; #73
                .byte 1,12

                .byte <_en67
                .byte 0,0,0                 ; 3
                .byte <_en40
                .byte <_en42
                .byte 0,0,0,0,0             ; 5
                .byte <_en41
                .byte 0,0,0,0,0,0,0,0       ; 8
                .byte <_en43
                .byte <_en45
                .byte <_en21
                .byte 0,0,0,0               ; 4
                .byte <_en44
                .byte <_en49
                .byte <_en34
                .byte <_en38
                .byte 0,0,0,0,0             ; 5
                .byte <_en36
                .byte 0                     ; 1
                .byte <_en25
                .byte 0,0                   ; 2
                .byte <_en72
                .byte <_en29
                .byte <_en63
                .byte 0,0                   ; 2
                .byte <_en13
                .byte 0,0,0                 ; 3
                .byte <_en10
                .byte <_en58

;               .byte 0                     ; 1

                .endnamespace
