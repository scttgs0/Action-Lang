;======================================
;   FILE: edit.def.asm
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


ramzap          = 0                     ; to zap or not to zap, that is the question?


; ACTION! vars ($480 - $57D)
;----------------------------
w1              = $03_0480              ; window 1 data
top1            = $03_048F
chrConvert      = $03_0490              ; char convert
codebase        = $03_0491              ; 2 bytes
codesize        = $03_0493              ; 2 bytes
SymTblSizePages = $03_0495              ; symbol table size (pages)
isMonitorLive   = $03_0496              ; edit/mon flag
gbase           = $03_0497              ; 2 bytes
type            = $03_0499
list            = $03_049A              ; listing flag
numargs         = $03_049B
cmdln           = $03_049C              ; command line offset
param           = $03_049D
opmode          = $03_049E
currentWindow   = $03_049F              ; window memory offset
cury            = $03_04A0              ; current Y reg (0/1/unknown)
lastch          = $03_04A1              ; last char
curch           = $03_04A2              ; current char
sparem          = $03_04A3              ; -> spare mem
numwd           = $03_04A5              ; number of windows
allocerr        = $03_04A6              ; INC on Alloc error
delbuf          = $03_04A7              ; 6 bytes
FirstChar       = $03_04AD              ; used for big symbol table
taglist         = $03_04AE              ; 2 bytes
chrConvert1     = $03_04B0              ; char convert
w2              = $03_04B1              ; window 2 data
bckgrnd         = $03_04C0              ; background color
procsp          = $03_04C1              ; Break stack pointer
argbytes        = $03_04C2
trace           = $03_04C3              ; trace flag
isBigSymTbl     = $03_04C4              ; big symbol table flag

        ; note: $4C5 available

jt_jmps         = $03_04C6              ; see EDIT.CAR

; Jump table goes to $4FF
;-------------------------
jt_segend       = $03_04C6
jt_curbank      = $03_04C9
jt_stmask       = $03_04CA
jt_error        = $03_04CB
jt_wsize        = $03_04CE
jt_linemax      = $03_04CF
jt_chrConvert2  = $03_04D0              ; char convert
jt_expend       = $03_04D1
jt_dclend       = jt_expend+3
jt_cgend        = jt_dclend+3
jt_arend        = jt_cgend+3
jt_splend       = jt_arend+3
jt_alarm        = jt_splend+3
jt_eolch        = jt_alarm+3
jt_lsh          = jt_eolch+1

jt_chrConvert3  = $03_04F0              ; char convert
jt_tvdisp       = $03_04F1
jt_disptb       = $03_04F2
jt_smtend       = $03_04F8
;jt_stmradr      = $03_04FE

subbuf          = $03_0500              ; 40 bytes     ; rowSize
findbuf         = $03_0528              ; 40 bytes     ; rowSize
numbuf          = $03_0550              ; 24 bytes
stkbase         = $03_0577

opstack         = $03_0580

; $580 TO ? is used for output of
; FASC routine.  Should not exceed
; 16 bytes, $58F (worst case?).
;---------------------------------
tempbuf         = $03_0590              ; 40 bytes     ; rowSize
argtypes        = $03_05B8              ;  8 bytes
eof             = $03_05C0              ;  8 bytes
inbuf           = $03_05C8              ; 36 bytes
abt             = $03_05EC              ;  4 bytes
temps           = $03_05F0              ; 16 bytes


; system vars
;-------------
qcode           = DPBASE+$0E                   ; ap. high mem.
csrch           = $5D

lbuff           = $03_0580              ; fp ASCII buf


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
