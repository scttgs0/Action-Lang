
;======================================
;   FILE: lib.str.asm
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


;lstr            .proc


;======================================
;INT FUNC SCompare(STRING a,b)
; result returned is:
;   =0 if a=b
;   low 0 if a<b
;   high 0 if a>b
;======================================
scompare        .proc
                sta arg4
                stx arg5
                sty arg2
                ldy #0
                sty args
                sty args+1
                lda (arg4),y
                cmp (arg2),y
                beq _sc1

                jsr _sc4

_sc1            cmp #0
                bne _sc2

                rts

_sc2            sta arg6
_sc3            iny
                lda (arg4),y
                cmp (arg2),y
                bne _sc4

                cpy arg6
                bcc _sc3

                rts

_sc4            ldx #$ff
                stx args
                bcc _sc5

                lda (arg2),y
                inx
_sc5            stx args+1
                rts
                .endproc


;======================================
;PROC SCopy(STRING dest, src)
; dest = src
;======================================
scopy           .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
_scopy1         sta (arg0),y
                beq _scp2

_scopy2         tay
_scp1           lda (arg2),y
                sta (arg0),y
                dey
                bne _scp1

_scp2           rts
                .endproc


;======================================
;PROC SCopyS(STRING dest, src, BYTE start, stop)
; if LEN(src)<stop then stop=LEN(src)
; dest = src(start, stop)
;======================================
scopys          .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
                cmp arg5
                bcs _scs1

                sta arg5
_scs1           dec arg4
                clc
                lda arg2
                adc arg4
                sta arg2
                bcc _scs2

                inc arg3
_scs2           sec
                lda arg5
                sbc arg4
                bcs _scs3

                lda #0
_scs3           jmp scopy._scopy1

                .endproc


;======================================
;PROC SAssign(STRING dest, src, BYTE start, stop)
; IF stop-start+1>LEN(src) THEN
;   stop = LEN(src)+start-1
; IF LEN(dest)<stop THEN
;   LEN(dest) = stop
; dest(start, stop) = src
;======================================
sassign         .proc
                sta arg0
                stx arg1
                sty arg2
                ldy #0
                lda (arg2),y
                beq _sa1

                sta arg6
                dec arg4
                sec
                lda arg5
                sbc arg4
                beq _sa1
                bcs _sa2

_sa1            rts

_sa2            tax
                cmp  arg6
                bcc  _sa3

                clc
                lda arg6
                tax
                adc arg4
                sta arg5
_sa3            lda arg5
                cmp (arg0),y
                bcc _sa4

                sta (arg0),y
                clc
_sa4            lda arg0
                adc arg4
                sta arg0
                bcc _sa5

                inc arg1
_sa5            txa
                jmp scopy._scopy2

                .endproc

                ;.endproc


;======================================
            ; symbol table
;======================================

;:EN6 .BYTE 6,"PrintF",200
; .WORD PrtF ; #117
; .BYTE 6,17,12,12,12,12,12

_en7            .text 4,"Open",200
                .word opn               ; #96
                .byte 4,138,17,138,138
_en8            .text 6,"PrintE",200
                .word prte              ; #116
                .byte 1,17
_en9            .text 7,"PrintDE",200
                .word prtde             ; #75
                .byte 2,138,17
_en10           .text 5,"Close",200
                .word clos              ; #253
                .byte 1,138
_en11           .text 5,"Print",200
                .word prt               ; #135
                .byte 1,17
_en12           .text 6,"PrintD",200
                .word prtd              ; #115
                .byte 2,138,17
_en13           .text 6,"InputS",200
                .word ins               ; #249
                .byte 1,17
_en14           .text 7,"InputSD",200
                .word insd              ; #87
                .byte 2,138,17
_en15           .text 7,"InputMD",200
                .word inmd              ; #75
                .byte 3,138,17,138
_en16           .text 4,"GetD",202
                .word getd              ; #138
                .byte 1,138
_en17           .text 4,"PutE",200
                .word pute              ; #162
                .byte 0
_en18           .text 3,"Put",200
                .word put               ; #158
                .byte 1,137
_en19           .text 4,"PutD",200
                .word putd              ; #161
                .byte 2,138,137
_en20           .text 5,"PutDE",200
                .word putde             ; #168
                .byte 1,138
_en21           .text 3,"XIO",200
                .word xio               ; #225
                .byte 6,138,138,138,138,138,17
_en22           .text 6,"PrintB",200
                .word prtb              ; #113
                .byte 1,138
_en23           .text 7,"PrintBE",200
                .word prtbe             ; #71
                .byte 1,138

;:EN24 .BYTE 7,"PrintBD",200
; .WORD PrtBD ; #70
; .BYTE 2,138,138

_en25           .text 8,"PrintBDE",200
                .word prtbde            ; #241
                .byte 2,138,138
_en26           .text 6,"PrintC",200
                .word prtc              ; #114
                .byte 1,12
_en27           .text 7,"PrintCE",200
                .word prtce             ; #73
                .byte 1,12
_en28           .text 7,"PrintCD",200
                .word prtcd             ; #72
                .byte 2,138,12
_en29           .text 8,"PrintCDE",200
                .word prtcde            ; #245
                .byte 2,138,12
_en30           .text 6,"PrintI",200
                .word prti              ; #120
                .byte 1,11
_en31           .text 7,"PrintID",200
                .word prtid             ; #84
                .byte 2,138,11
_en32           .text 7,"PrintIE",200
                .word prtie             ; #85
                .byte 1,11
_en33           .text 8,"PrintIDE",200
                .word prtide            ; #13
                .byte 2,138,11
_en34           .text 6,"InputB",202
                .word input_b           ; #232
                .byte 0
_en35           .text 7,"InputBD",202
                .word inbd              ; #53
                .byte 1,138
_en36           .text 6,"InputI",203
                .word input_i           ; #239
                .byte 0
_en37           .text 7,"InputID",203
                .word inid              ; #67
                .byte 1,138
_en38           .text 6,"InputC",204
                .word input_c           ; #233
                .byte 0
_en39           .text 7,"InputCD",204
                .word incd              ; #55
                .byte 1,138
_en40           .text 4,"ValB",202
                .word valb              ; #207
                .byte 1,17
_en41           .text 4,"ValI",203
                .word vali              ; #214
                .byte 1,17
_en42           .text 4,"ValC",204
                .word valc              ; #208
                .byte 1,17
_en43           .text 4,"StrB",200
                .word strb              ; #223
                .byte 2,138,17
_en44           .text 4,"StrI",200
                .word stri              ; #230
                .byte 2,11,17
_en45           .text 4,"StrC",200
                .word strc              ; #224
                .byte 2,12,17
_en46           .text 4,"Note",200
                .word note              ; #89
                .byte 3,138,20,18
_en47           .text 5,"Point",200
                .word point             ; #110
                .byte 3,138,12,138
_en48           .text 8,"Graphics",200
                .word graphics          ; #108
                .byte 1,138
_en49           .text 6,"DrawTo",200
                .word drawto            ; #231
                .byte 2,12,138
_en50           .text 8,"Position",200
                .word position          ; #94
                .byte 2,12,138
_en51           .text 6,"Locate",202
                .word locate            ; #97
                .byte 2,12,138
_en52           .text 4,"Plot",200
                .word plot              ; #131
                .byte 2,12,138
_en53           .text 8,"SetColor",200
                .word setcolor          ; #6
                .byte 3,138,138,138
_en54           .text 4,"Fill",200
                .word fill              ; #122
                .byte 2,12,138
_en55           .text 4,"Rand",202
                .word rand              ; #117
                .byte 1,138
_en56           .text 5,"Sound",200
                .word sound             ; #31
                .byte 4,138,138,138,138
_en57           .text 6,"SndRst",200
                .word sndrst            ; #73
                .byte 0
_en58           .text 6,"Paddle",202
                .word paddle            ; #254
                .byte 1,138
_en59           .text 5,"PTrig",202
                .word ptrig             ; #164
                .byte 1,138
_en60           .text 5,"Stick",202
                .word stick             ; #8
                .byte 1,138
_en61           .text 5,"STrig",202
                .word strig             ; #52
                .byte 1,138

;:EN62 .BYTE 4,"Peek",202
; .WORD Peek ; #73
; .BYTE 1,12
;:EN63 .BYTE 5,"PeekC",204
; .WORD PeekC ; #245
; .BYTE 1,12

_en64           .text 4,"Poke",200
                .word poke              ; #120
                .byte 2,12,138
_en65           .text 5,"PokeC",200
                .word pokec             ; #83
                .byte 2,12,12
_en66           .text 4,"Zero",200
                .word mzero             ; #88
                .byte 2,18,12
_en67           .text 8,"SetBlock",200
                .word setblock          ; #203
                .byte 3,18,12,138

;:EN68 .BYTE 9,"MoveBlock",200
; .WORD MoveBlock ; #85
; .BYTE 3,18,18,12

_en69           .text 5,"Break",200
                .word break             ; #183
_en70           .text 8,"SCompare",203
                .word scompare          ; #92
                .byte 2,17,17
_en71           .text 5,"SCopy",200
                .word scopy             ; #192
                .byte 2,17,17
_en72           .text 6,"SCopyS",200
                .word scopys            ; #244
                .byte 4,17,17,138,138
_en73           .text 7,"SAssign",200
                .word sassign           ; #23
                .byte 4,17,17,138,138


;======================================
; hash table
;======================================
libst           .byte 0                 ; 1
                .byte >en1              ; EOF #1
                .byte >en5              ; TRACE #2
                .byte 0,0,0             ; 3
                .byte >_en53            ; SetColor #6
                .byte 0                 ; 1
                .byte >_en60            ; Stick #8
                .byte 0,0,0,0           ; 4
                .byte >_en33            ; PrintIDE #13
                .byte >en2              ; color #14
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en73            ; SAssign #23
                .byte 0,0,0,0,0,0,0     ; 7
                .byte >_en56            ; Sound #31
                .byte >en3              ; LIST #32
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0,0 ; 19
                .byte >_en61            ; STrig #52
                .byte >_en35            ; InputBD #53
                .byte 0                 ; 1
                .byte >_en39            ; InputCD #55
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0                 ; 11
                .byte >_en37            ; InputID #67
                .byte 0,0               ; 2
                .byte >_en24            ; PrintBD #70
                .byte >_en23            ; PrintBE #71
                .byte >_en28            ; PrintCD #72
                .byte >_en27            ; PrintCE #73
                .byte >_en57            ; SndRst #74
                .byte >_en9             ; PrintDE #75
                .byte >_en15            ; InputMD #76
                .byte >_en62            ; Peek #77
                .byte 0,0,0,0,0         ; 5
                .byte >_en65            ; PokeC #83
                .byte >_en31            ; PrintID #84
                .byte >_en32            ; PrintIE #85
                .byte >_en68            ; MoveBlock #86
                .byte >_en14            ; InputSD #87
                .byte >_en66            ; Zero #88
                .byte >_en46            ; Note #89
                .byte 0,0               ; 2
                .byte >en4              ; device #92
                .byte >_en70            ; SCompare #93
                .byte >_en50            ; Position #94
                .byte 0                 ; 1
                .byte >_en7             ; Open #96
                .byte >_en51            ; Locate #97
                .byte 0,0,0,0,0,0,0,0,0,0 ; 10
                .byte >_en48            ; Graphics #108
                .byte 0                 ; 1
                .byte >_en47            ; Point #110
                .byte 0,0               ; 2
                .byte >_en22            ; PrintB #113
                .byte >_en26            ; PrintC #114
                .byte >_en12            ; PrintD #115
                .byte >_en8             ; PrintE #116
                .byte >_en6             ; PrintF #117
                .byte >_en55            ; Rand #118
                .byte 0                 ; 1
                .byte >_en30            ; PrintI #120
                .byte >_en64            ; Poke #121
                .byte >_en54            ; Fill #122
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en52            ; Plot #131
                .byte 0,0,0             ; 3
                .byte >_en11            ; Print #135
                .byte 0,0               ; 2
                .byte >_en16            ; GetD #138
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0,0,0,0,0   ; 18
                .byte >en0              ; Error #157
                .byte >_en18            ; Put #158
                .byte 0,0               ; 2
                .byte >_en19            ; PutD #161
                .byte >_en17            ; PutE #162
                .byte 0                 ; 1
                .byte >_en59            ; PTrig #164
                .byte 0,0,0             ; 3
                .byte >_en20            ; PutDE #168
                .byte 0,0,0,0,0,0,0,0,0,0
                .byte 0,0,0,0           ; 14
                .byte >_en69            ; Break #183
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte >_en71            ; SCopy #192
                .byte 0,0,0,0,0,0,0,0,0,0 ; 10
                .byte >_en67            ; SetBlock #203
                .byte 0,0,0             ; 3
                .byte >_en40            ; ValB #207
                .byte >_en42            ; ValC #208
                .byte 0,0,0,0,0         ; 5
                .byte >_en41            ; ValI #214
                .byte 0,0,0,0,0,0,0,0 ; 8
                .byte >_en43            ; StrB #223
                .byte >_en45            ; StrC #224
                .byte >_en21            ; XIO #225
                .byte 0,0,0,0           ; 4
                .byte >_en44            ; StrI #230
                .byte >_en49            ; DrawTo #231
                .byte >_en34            ; InputB #232
                .byte >_en38            ; InputC #233
                .byte 0,0,0,0,0         ; 5
                .byte >_en36            ; InputI #239
                .byte 0                 ; 1
                .byte >_en25            ; PrintBDE #241
                .byte 0,0               ; 2
                .byte >_en72            ; SCopyS #244
                .byte >_en29            ; PrintCDE #245
                .byte >_en63            ; PeekC #246
                .byte 0,0               ; 2
                .byte >_en13            ; InputS #249
                .byte 0,0,0             ; 3
                .byte >_en10            ; Close #253
                .byte >_en58            ; Paddle #254
                .byte 0                 ; 1

                .byte 0                 ; 1
                .byte <en1
                .byte <en5
                .byte 0,0,0             ; 3
                .byte <_en53
                .byte 0                 ; 1
                .byte <_en60
                .byte 0,0,0,0           ; 4
                .byte <_en33
                .byte <en2
                .byte 0                 ; 1


; .BYTE 0,0,0,0,0,0,0,0 ; 7


;======================================
;
;======================================
strig           .proc
                tax
                lda TRIG0,x
                sta args
                rts
                .endproc


                .byte <_en73


; .BYTE 0,0,0,0,0,0,0 ; 7

;======================================
;
;======================================
paddle          .proc
                tax
                lda PADDL0,x
                sta args
                rts
                .endproc

;--------------------------------------
;--------------------------------------

                .byte <_en56
                .byte <en3
                .byte 0,0               ; 2

; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0,0,0,0,0,0               ; 17

_en6            .text 6,"PrintF",200
                .word prtf        ; #117
                .byte 6,17,12,12,12,12,12
;
                .byte <_en61
                .byte <_en35
                .byte 0                 ; 1
                .byte <_en39

; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0                               ; 11

_en63           .text 5,"PeekC",204
                .word peekc             ; #245
                .byte 1,12

                .byte <_en37
                .byte 0,0               ; 2
                .byte <_en24
                .byte <_en23
                .byte <_en28
                .byte <_en27
                .byte <_en57
                .byte <_en9
                .byte <_en15
                .byte <_en62
                .byte 0,0,0,0,0         ; 5
                .byte <_en65
                .byte <_en31
                .byte <_en32
                .byte <_en68
                .byte <_en14
                .byte <_en66
                .byte <_en46
                .byte 0,0               ; 2
                .byte <en4
                .byte <_en70
                .byte <_en50
                .byte 0                 ; 1
                .byte <_en7
                .byte <_en51


; .BYTE 0,0,0,0,0,0,0,0,0,0             ; 10
; (c)1983ACS in internal char. qcode

copyright       .byte 8,99,9,17,25,24,19,33,35,51


                .byte <_en48
                .byte 0                 ; 1
                .byte <_en47
                .byte 0,0               ; 2
                .byte <_en22
                .byte <_en26
                .byte <_en12
                .byte <_en8
                .byte <_en6
                .byte <_en55
                .byte 0                 ; 1
                .byte <_en30
                .byte <_en64
                .byte <_en54
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en52
                .byte 0,0,0             ; 3
                .byte <_en11
                .byte 0,0               ; 2
                .byte <_en16
                .byte 0                 ; 1


; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0,0,0,0,0 ; 17

_en68           .text 9,"MoveBlock",200
                .word moveblock         ; #85
                .byte 3,18,18,12

                .byte <en0
                .byte <_en18
                .byte 0,0               ; 2
                .byte <_en19
                .byte <_en17
                .byte 0                 ; 1
                .byte <_en59
                .byte 0,0,0             ; 3
                .byte <_en20


; .BYTE 0,0,0,0,0,0,0,0,0,0
; .BYTE 0,0,0,0 ; 14

_en24           .text 7,"PrintBD",200
                .word prtbd             ; #70
                .byte 2,138,138

                .byte <_en69
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en71


; .BYTE 0,0,0,0,0,0,0,0,0,0 ; 10

_en62           .text 4,"Peek",202
                .word peek              ; #73
                .byte 1,12

                .byte <_en67
                .byte 0,0,0             ; 3
                .byte <_en40
                .byte <_en42
                .byte 0,0,0,0,0         ; 5
                .byte <_en41
                .byte 0,0,0,0,0,0,0,0   ; 8
                .byte <_en43
                .byte <_en45
                .byte <_en21
                .byte 0,0,0,0           ; 4
                .byte <_en44
                .byte <_en49
                .byte <_en34
                .byte <_en38
                .byte 0,0,0,0,0         ; 5
                .byte <_en36
                .byte 0                 ; 1
                .byte <_en25
                .byte 0,0               ; 2
                .byte <_en72
                .byte <_en29
                .byte <_en63
                .byte 0,0               ; 2
                .byte <_en13
                .byte 0,0,0             ; 3
                .byte <_en10
                .byte <_en58

; .BYTE 0 ; 1
