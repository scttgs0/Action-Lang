
; SPDX-FileName: edit.def.asm
; SPDX-FileComment: Action! Programming Language
; SPDX-FileCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later


ramzap          = 0                     ; to zap or not to zap, that is the question?


; ACTION! vars ($480 - $57D)
;----------------------------
w1              = $0480              ; window 1 data
top1            = $048F
chrConvert      = $0490              ; char convert
codebase        = $0491              ; 2 bytes
codesize        = $0493              ; 2 bytes
SymTblSizePages = $0495              ; symbol table size (pages)
isMonitorLive   = $0496              ; edit/mon flag
gbase           = $0497              ; 2 bytes
type            = $0499
list            = $049A              ; listing flag
numargs         = $049B
cmdln           = $049C              ; command line offset
param           = $049D
opmode          = $049E
currentWindow   = $049F              ; window memory offset
cury            = $04A0              ; current Y reg (0/1/unknown)
lastch          = $04A1              ; last char
curch           = $04A2              ; current char
sparem          = $04A3              ; -> spare mem
numwd           = $04A5              ; number of windows
allocerr        = $04A6              ; INC on Alloc error
delbuf          = $04A7              ; 6 bytes
FirstChar       = $04AD              ; used for big symbol table
taglist         = $04AE              ; 2 bytes
chrConvert1     = $04B0              ; char convert
w2              = $04B1              ; window 2 data
bckgrnd         = $04C0              ; background color
procsp          = $04C1              ; Break stack pointer
argbytes        = $04C2
trace           = $04C3              ; trace flag
isBigSymTbl     = $04C4              ; big symbol table flag

        ; note: $4C5 available

jt_jmps         = $04C6              ; see EDIT.CAR

; Jump table goes to $4FF
;-------------------------
jt_segend       = $04C6
jt_curbank      = $04C9
jt_stmask       = $04CA
jt_error        = $04CB
jt_wsize        = $04CE
jt_linemax      = $04CF
jt_chrConvert2  = $04D0              ; char convert
jt_expend       = $04D1
jt_dclend       = jt_expend+3
jt_cgend        = jt_dclend+3
jt_arend        = jt_cgend+3
jt_splend       = jt_arend+3
jt_alarm        = jt_splend+3
jt_eolch        = jt_alarm+3
jt_lsh          = jt_eolch+1

jt_chrConvert3  = $04F0              ; char convert
jt_tvdisp       = $04F1
jt_disptb       = $04F2
jt_smtend       = $04F8
jt_stmradr      = $04FE

subbuf          = $0500              ; 40 bytes     ; rowSize
findbuf         = $0528              ; 40 bytes     ; rowSize
numbuf          = $0550              ; 24 bytes
stkbase         = $0577

opstack         = $0580

; $580 TO ? is used for output of
; FASC routine.  Should not exceed
; 16 bytes, $58F (worst case?).
;---------------------------------
tempbuf         = $0590              ; 40 bytes     ; rowSize
argtypes        = $05B8              ;  8 bytes
eof             = $05C0              ;  8 bytes
inbuf           = $05C8              ; 36 bytes
abt             = $05EC              ;  4 bytes
temps           = $05F0              ; 16 bytes


; system vars
;-------------
qcode           = $0E            ; ap. high mem.
csrch           = $5D

lbuff           = $0580              ; fp ASCII buf


; window record offsets
;-----------------------
wsp             = 0
wch             = 1
wlnum           = 2
wdirty          = 3
wtop            = 4
wbot            = 6
wcur            = 8
wx              = 10
wy              = 11
wnlns           = 12                    ; num lines
wytop           = 13
windent         = 14


; system defs
;-------------
eol             = $9B
esc             = $1B
