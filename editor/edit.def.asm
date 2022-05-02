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


ramzap          = 1                     ; to zap or not to zap, that is the question?

; Page Zero defs
;----------------
; Alloc/Free defs, also used by compiler
afbase          = $80
aflast          = $82
afcur           = $84
afsize          = $86
afbest          = $88
props           = $88
afbsze          = $8A
op              = $8A
lsttoken        = $8B                   ; prev. token value

; current window data / comp. vars
;----------------------------------
sp              = $8C
choff           = $8D
lnum            = $8E
curproc         = $8E                   ; 2 bytes
dirty           = $8F
top             = $90                   ; -> top line
bot             = $92                   ; -> bottom line
whaddr          = $92                   ; -> current DO frame
cur             = $94                   ; -> current line
x               = $96                   ; don't use in comp.
y               = $97                   ; don't use in comp.
nlines          = $98
chan            = $98                   ; current input file
ytop            = $99
defflg          = $99
indent          = $9A
stackptr        = $9A                   ; op stack offset

buf             = $9B                   ; -> edit buf
insert          = $9D                   ; insert/replace flag
delnxt          = $9E                   ; ->, USED BY LEX

; arguments and temps
;---------------------
args            = $A0
arg0            = $A0
arg1            = $A1
arg2            = $A2
arg3            = $A3
arg4            = $A4
arg5            = $A5
arg6            = $A6
arg7            = $A7
arg8            = $A8
arg9            = $A9
arg10           = $AA
arg11           = $AB
arg12           = $AC
arg13           = $AD
arg14           = $AE
arg15           = $AF

; compiler vars
;---------------
stbase          = $B0                   ; symbol table base (page)
symTblGlobal    = $B1                   ; qglobal hash table
symTblLocal     = $B3                   ; local hash table
codeoff         = $B5                   ; relocation offset
device          = $B7                   ; default device
qglobal         = $B8                   ; qglobal/local flag
stack           = $B9                   ; value stack
frame           = $BB                   ; -> current frame
symtab          = $BD                   ; -> next symbol table entry
stmax           = $BF                   ; symbol table top (page)
addr            = $C0                   ; token address
token           = $C2                   ; token value

; following defs can be used during
; program execution (user program)
;-----------------------------------
isDirty         = $C3
spln            = $C4                   ; error char
curln           = $C5                   ; error line
curnxt          = $C7                   ; next error line
nxtaddr         = $C9                   ; next token address

        ; note: $CA-$CD never referenced

bigSymTblGlobal = $CE                   ; only used if big symbol table
arrayptr        = $D0                   ; array mem. list
spnxt           = $D2                   ; next error char
nxttoken        = $D3                   ; next token value


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
frstchar        = $03_04AD              ; used for big symbol table
taglist         = $03_04AE              ; 2 bytes
chrConvert1     = $03_04B0              ; char convert
w2              = $03_04B1              ; window 2 data
bckgrnd         = $03_04C0              ; background color
procsp          = $03_04C1              ; Break stack pointer
argbytes        = $03_04C2
trace           = $03_04C3              ; trace flag
isBigSymTbl     = $03_04C4              ; big symbol table flag

        ; note: $4C5 available

jmps            = $03_04C6              ; see EDIT.CAR

; Jump table goes to $4FF
;-------------------------
segend          = $03_04C6
curbank         = $03_04C9
stmask          = $03_04CA
error           = $03_04CB
wsize           = $03_04CE
linemax         = $03_04CF
chrConvert2     = $03_04D0              ; char convert
expend          = $03_04D1
dclend          = expend+3
cgend           = dclend+3
arend           = cgend+3
splend          = arend+3
alarm           = splend+3
eolch           = alarm+3
lsh             = eolch+1

chrConvert3     = $03_04F0              ; char convert
tvdisp          = $03_04F1
disptb          = $03_04F2
smtend          = $03_04F8
stmradr         = $04FE

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
qcode           = $0E                   ; ap. high mem.
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


; bank switching defs
;---------------------
bank            = $03_D500
