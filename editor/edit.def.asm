
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: edit.def.asm
; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


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
w1              = $0480                 ; window 1 data
top1            = $048f
chcvt           = $0490                 ; char convert
codebase        = $0491                 ; 2 bytes
codesize        = $0493                 ; 2 bytes
stsp            = $0495                 ; s.t. size (pages)
mpc             = $0496                 ; edit/mon flag
gbase           = $0497                 ; 2 bytes
type            = $0499
list            = $049a                 ; listing flag
numargs         = $049b
cmdln           = $049c                 ; command line offset
param           = $049d
opmode          = $049e
curwdw          = $049f                 ; window memory offset
cury            = $04a0                 ; current Y reg (0/1/unknown)
lastch          = $04a1                 ; last char
curch           = $04a2                 ; current char
sparem          = $04a3                 ; -> spare mem
numwd           = $04a5                 ; number of windows
allocerr        = $04a6                 ; INC on Alloc error
delbuf          = $04a7                 ; 6 bytes
frstchar        = $04ad                 ; used for big s.t.
taglist         = $04ae                 ; 2 bytes
chcvt1          = $04b0
w2              = $04b1                 ; window 2 data
bckgrnd         = $04c0                 ; background color
procsp          = $04c1                 ; Break stack pointer
argbytes        = $04c2
trace           = $04c3                 ; trace flag
bigst           = $04c4                 ; big s.t. flag

        ; note: $4C5 available

jmps            = $04c6                 ; see EDIT.CAR

; Jump table goes to $4FF
;-------------------------
segend          = $04c6
curbank         = $04c9
stmask          = $04ca
error           = $04cb
wsize           = $04ce
linemax         = $04cf
chcvt2          = $04d0
expend          = $04d1
dclend          = expend+3
cgend           = dclend+3
arend           = cgend+3
splend          = arend+3
alarm           = splend+3
eolch           = alarm+3
lsh             = eolch+1

chcvt3          = $04f0
tvdisp          = $04f1
disptb          = $04f2
smtend          = $04f8
stmradr         = $04fe

subbuf          = $0500                 ; 40 bytes
findbuf         = $0528                 ; 40 bytes
numbuf          = $0550                 ; 24 bytes
stkbase         = $0577

opstack         = $0580

; $580 TO ? is used for output of
; FASC routine.  Should not exceed
; 16 bytes, $58F (worst case?).
;---------------------------------
tempbuf         = $0590                 ; 40 bytes
argtypes        = $05b8                 ;  8 bytes
eof             = $05c0                 ;  8 bytes
inbuf           = $05c8                 ; 36 bytes
abt             = $05ec                 ;  4 bytes
temps           = $05f0                 ; 16 bytes


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

lbuff           = $0580                 ; fp ASCII buf


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
EOL             = $9b
esc             = $1b


; bank switching defs
;---------------------
ml              = $b000
el              = $a000
ll              = $a000
cl              = $a000
bank            = $d500
ebank           = 3
lbank           = 0
cbank           = 9
