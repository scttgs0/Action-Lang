;======================================
; Direct-page variables
;======================================

DPBASE          = $0800
WARMST          = DPBASE+$00            ; 0=cold powerup; 255=warm RESET
DOSVEC          = DPBASE+$01            ; [3-byte] jump to DOS
                ; DPBASE+$03
RMARGN          = DPBASE+$04            ; [2-bytes]
                ; DPBASE+$05
LMARGN          = DPBASE+$06
OLDCHR          = DPBASE+$07            ; character under the cursor
                                        ; used to restore that character when the cursor moves
COLCRS          = DPBASE+$08            ; [2-byte] Column of cursor
                ; DPBASE+$09
ROWCRS          = DPBASE+$0A            ; [2-byte] Row of cursor
                ; DPBASE+$0B
DINDEX          = DPBASE+$0C            ; display mode/current screen mode
ADRESS          = DPBASE+$0D            ; [3-byte] temporary storage / line buffer
                ; DPBASE+$0F
BRKKEY          = DPBASE+$10            ; 0=the BREAK key is pressed; 1=not pressed


; Floating-Point ======================
FR0             = DPBASE+$20            ; [6-byte]
                ; DPBASE+$25
FR1             = DPBASE+$26            ; [6-byte]
                ; DPBASE+$2B
CIX             = DPBASE+$2C
INBUFF          = DPBASE+$2D            ; [3-byte]
                ; DPBASE+$2F


; Page Zero defs
;----------------
; Alloc/Free defs, also used by compiler
afbase          = DPBASE+$80
aflast          = DPBASE+$82
afcur           = DPBASE+$84
afsize          = DPBASE+$86
afbest          = DPBASE+$88
props           = DPBASE+$88
afbsze          = DPBASE+$8A
op              = DPBASE+$8A
lsttoken        = DPBASE+$8B            ; prev. token value

; current window data / comp. vars
;----------------------------------
sp              = DPBASE+$8C
choff           = DPBASE+$8D
lnum            = DPBASE+$8E
curproc         = DPBASE+$8E            ; 2 bytes
dirty           = DPBASE+$8F
top             = DPBASE+$90            ; -> top line
bot             = DPBASE+$92            ; -> bottom line
whaddr          = DPBASE+$92            ; -> current DO frame
cur             = DPBASE+$94            ; -> current line
x               = DPBASE+$96            ; don't use in comp.
y               = DPBASE+$97            ; don't use in comp.
nlines          = DPBASE+$98
Channel         = DPBASE+$98            ; current input file
ytop            = DPBASE+$99
defflg          = DPBASE+$99
indent          = DPBASE+$9A
stackptr        = DPBASE+$9A            ; op stack offset
buf             = DPBASE+$9B            ; -> edit buf
insert          = DPBASE+$9D            ; insert/replace flag
delnxt          = DPBASE+$9E            ; ->, USED BY LEX

; arguments and temps
;---------------------
args            = DPBASE+$A0
arg0            = DPBASE+$A0
arg1            = DPBASE+$A1
arg2            = DPBASE+$A2
arg3            = DPBASE+$A3
arg4            = DPBASE+$A4
arg5            = DPBASE+$A5
arg6            = DPBASE+$A6
arg7            = DPBASE+$A7
arg8            = DPBASE+$A8
arg9            = DPBASE+$A9
arg10           = DPBASE+$AA
arg11           = DPBASE+$AB
arg12           = DPBASE+$AC
arg13           = DPBASE+$AD
arg14           = DPBASE+$AE
arg15           = DPBASE+$AF

; compiler vars
;---------------
stbase          = DPBASE+$B0            ; symbol table base (page)
symTblGlobal    = DPBASE+$B1            ; qglobal hash table
symTblLocal     = DPBASE+$B3            ; local hash table
codeoff         = DPBASE+$B5            ; relocation offset
device          = DPBASE+$B7            ; default device
qglobal         = DPBASE+$B8            ; qglobal/local flag
stack           = DPBASE+$B9            ; value stack
frame           = DPBASE+$BB            ; -> current frame
symtab          = DPBASE+$BD            ; -> next symbol table entry
stmax           = DPBASE+$BF            ; symbol table top (page)
addr            = DPBASE+$C0            ; token address
token           = DPBASE+$C2            ; token value

; following defs can be used during
; program execution (user program)
;-----------------------------------
isDirty         = DPBASE+$C3
spln            = DPBASE+$C4            ; error char
curln           = DPBASE+$C5            ; error line
curnxt          = DPBASE+$C7            ; next error line
nxtaddr         = DPBASE+$C9            ; next token address

        ; note: $CA-$CD never referenced

bigSymTblGlobal = DPBASE+$CE            ; only used if big symbol table
arrayptr        = DPBASE+$D0            ; array mem. list
spnxt           = DPBASE+$D2            ; next error char
nxttoken        = DPBASE+$D3            ; next token value
