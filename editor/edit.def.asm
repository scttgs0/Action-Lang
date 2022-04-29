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
afbsze          = $8a
op              = $8a
lsttoken        = $8b                   ; prev. token value

; current window data / comp. vars
;----------------------------------
sp              = $8c
choff           = $8d
lnum            = $8e
curproc         = $8e                   ; 2 bytes
dirty           = $8f
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
indent          = $9a
stackptr        = $9a                   ; op stack offset

buf             = $9b                   ; -> edit buf
insert          = $9d                   ; insert/replace flag
delnxt          = $9e                   ; ->, USED BY LEX

; arguments and temps
;---------------------
args            = $a0
arg0            = $a0
arg1            = $a1
arg2            = $a2
arg3            = $a3
arg4            = $a4
arg5            = $a5
arg6            = $a6
arg7            = $a7
arg8            = $a8
arg9            = $a9
arg10           = $aa
arg11           = $ab
arg12           = $ac
arg13           = $ad
arg14           = $ae
arg15           = $af

; compiler vars
;---------------
stbase          = $b0                   ; s.t. base (page)
stglobal        = $b1                   ; qglobal hash table
stlocal         = $b3                   ; local hash table
codeoff         = $b5                   ; relocation offset
device          = $b7                   ; default device
qglobal         = $b8                   ; qglobal/local flag
stack           = $b9                   ; value stack
frame           = $bb                   ; -> current frame
symtab          = $bd                   ; -> next s.t. entry
stmax           = $bf                   ; s.t. top (page)
addr            = $c0                   ; token address
token           = $c2                   ; token value

; following defs can be used during
; program execution (user program)
;-----------------------------------
dirtyf          = $c3
spln            = $c4                   ; error char
curln           = $c5                   ; error line
curnxt          = $c7                   ; next error line
nxtaddr         = $c9                   ; next token address

        ; note: $CA-$CD never referenced

stg2            = $ce                   ; only used if big s.t.
arrayptr        = $d0                   ; array mem. list
spnxt           = $d2                   ; next error char
nxttoken        = $d3                   ; next token value


; ACTION! vars ($480 - $57D)
;----------------------------
w1              = $03_0480                 ; window 1 data
top1            = $03_048f
chcvt           = $03_0490                 ; char convert
codebase        = $03_0491                 ; 2 bytes
codesize        = $03_0493                 ; 2 bytes
stsp            = $03_0495                 ; s.t. size (pages)
mpc             = $03_0496                 ; edit/mon flag
gbase           = $03_0497                 ; 2 bytes
type            = $03_0499
list            = $03_049a                 ; listing flag
numargs         = $03_049b
cmdln           = $03_049c                 ; command line offset
param           = $03_049d
opmode          = $03_049e
curwdw          = $03_049f                 ; window memory offset
cury            = $03_04a0                 ; current Y reg (0/1/unknown)
lastch          = $03_04a1                 ; last char
curch           = $03_04a2                 ; current char
sparem          = $03_04a3                 ; -> spare mem
numwd           = $03_04a5                 ; number of windows
allocerr        = $03_04a6                 ; INC on Alloc error
delbuf          = $03_04a7                 ; 6 bytes
frstchar        = $03_04ad                 ; used for big s.t.
taglist         = $03_04ae                 ; 2 bytes
chcvt1          = $03_04b0
w2              = $03_04b1                 ; window 2 data
bckgrnd         = $03_04c0                 ; background color
procsp          = $03_04c1                 ; Break stack pointer
argbytes        = $03_04c2
trace           = $03_04c3                 ; trace flag
bigst           = $03_04c4                 ; big s.t. flag

        ; note: $4C5 available

jmps            = $03_04c6                 ; see EDIT.CAR

; Jump table goes to $4FF
;-------------------------
segend          = $03_04c6
curbank         = $03_04c9
stmask          = $03_04ca
error           = $03_04cb
wsize           = $03_04ce
linemax         = $03_04cf
chcvt2          = $03_04d0
expend          = $03_04d1
dclend          = expend+3
cgend           = dclend+3
arend           = cgend+3
splend          = arend+3
alarm           = splend+3
eolch           = alarm+3
lsh             = eolch+1

chcvt3          = $03_04f0
tvdisp          = $03_04f1
disptb          = $03_04f2
smtend          = $03_04f8
stmradr         = $04fe

subbuf          = $03_0500                 ; 40 bytes     ; rowSize
findbuf         = $03_0528                 ; 40 bytes     ; rowSize
numbuf          = $03_0550                 ; 24 bytes
stkbase         = $03_0577

opstack         = $03_0580

; $580 TO ? is used for output of
; FASC routine.  Should not exceed
; 16 bytes, $58F (worst case?).
;---------------------------------
tempbuf         = $03_0590                 ; 40 bytes     ; rowSize
argtypes        = $03_05b8                 ;  8 bytes
eof             = $03_05c0                 ;  8 bytes
inbuf           = $03_05c8                 ; 36 bytes
abt             = $03_05ec                 ;  4 bytes
temps           = $03_05f0                 ; 16 bytes


; system vars
;-------------
warmst          = $08
dosvec          = $0a
dosini          = $0c
qcode           = $0e                   ; ap. high mem.
brkkey          = $11
rtclok          = $12
adress          = $64
lmargin         = $52
rmargin         = $53
rowcrs          = $54
colcrs          = $55
dindex          = $57
savmsc          = $58
oldrow          = $5a
oldcol          = $5b
oldadr          = $5e
oldchr          = $5d
csrch           = $5d
ramtop          = $6a
fr0             = $d4
fr1             = $e0
cix             = $f2
inbuff          = $f3
flptr           = $fc

lbuff           = $03_0580                 ; fp ASCII buf


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
eol             = $9b
esc             = $1b


; bank switching defs
;---------------------
bank            = $03_d500
