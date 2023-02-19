
;--------------------------------------
; Zero-page variables
;--------------------------------------

ZPBASE          = $00
WARMST          = ZPBASE+$00            ; 0=cold powerup; 255=warm RESET
DOSVEC          = ZPBASE+$01            ; [3-byte] jump to DOS
                ; ZPBASE+$03
RMARGN          = ZPBASE+$04            ; [2-bytes]
                ; ZPBASE+$05
LMARGN          = ZPBASE+$06
OLDCHR          = ZPBASE+$07            ; character under the cursor
                                        ; used to restore that character when the cursor moves
COLCRS          = ZPBASE+$08            ; [2-byte] Column of cursor
                ; ZPBASE+$09
ROWCRS          = ZPBASE+$0A            ; [2-byte] Row of cursor
                ; ZPBASE+$0B
DINDEX          = ZPBASE+$0C            ; display mode/current screen mode
ADRESS          = ZPBASE+$0D            ; [3-byte] temporary storage / line buffer
                ; ZPBASE+$0F
BRKKEY          = ZPBASE+$10            ; 0=the BREAK key is pressed; 1=not pressed


; Floating-Point ======================
FR0             = ZPBASE+$20            ; [6-byte]
                ; ZPBASE+$25
FR1             = ZPBASE+$26            ; [6-byte]
                ; ZPBASE+$2B
CIX             = ZPBASE+$2C
INBUFF          = ZPBASE+$2D            ; [3-byte]
                ; ZPBASE+$2F


; Page Zero defs
;----------------
; Alloc/Free defs, also used by compiler
afbase          = ZPBASE+$80
aflast          = ZPBASE+$82
afcur           = ZPBASE+$84
afsize          = ZPBASE+$86
afbest          = ZPBASE+$88
props           = ZPBASE+$88
afbsze          = ZPBASE+$8A
op              = ZPBASE+$8A
lsttoken        = ZPBASE+$8B            ; prev. token value

; current window data / comp. vars
;----------------------------------
sp              = ZPBASE+$8C
choff           = ZPBASE+$8D
lnum            = ZPBASE+$8E
curproc         = ZPBASE+$8E            ; 2 bytes
dirty           = ZPBASE+$8F
top             = ZPBASE+$90            ; -> top line
bot             = ZPBASE+$92            ; -> bottom line
whaddr          = ZPBASE+$92            ; -> current DO frame
cur             = ZPBASE+$94            ; -> current line
x               = ZPBASE+$96            ; don't use in comp.
y               = ZPBASE+$97            ; don't use in comp.
nlines          = ZPBASE+$98
Channel         = ZPBASE+$98            ; current input file
ytop            = ZPBASE+$99
defflg          = ZPBASE+$99
indent          = ZPBASE+$9A
stackptr        = ZPBASE+$9A            ; op stack offset
buf             = ZPBASE+$9B            ; -> edit buf
insert          = ZPBASE+$9D            ; insert/replace flag
delnxt          = ZPBASE+$9E            ; ->, USED BY LEX

; arguments and temps
;---------------------
args            = ZPBASE+$A0
arg0            = ZPBASE+$A0
arg1            = ZPBASE+$A1
arg2            = ZPBASE+$A2
arg3            = ZPBASE+$A3
arg4            = ZPBASE+$A4
arg5            = ZPBASE+$A5
arg6            = ZPBASE+$A6
arg7            = ZPBASE+$A7
arg8            = ZPBASE+$A8
arg9            = ZPBASE+$A9
arg10           = ZPBASE+$AA
arg11           = ZPBASE+$AB
arg12           = ZPBASE+$AC
arg13           = ZPBASE+$AD
arg14           = ZPBASE+$AE
arg15           = ZPBASE+$AF

; compiler vars
;---------------
stbase          = ZPBASE+$B0            ; symbol table base (page)
symTblGlobal    = ZPBASE+$B1            ; qglobal hash table
symTblLocal     = ZPBASE+$B3            ; local hash table
codeoff         = ZPBASE+$B5            ; relocation offset
device          = ZPBASE+$B7            ; default device
qglobal         = ZPBASE+$B8            ; qglobal/local flag
stack           = ZPBASE+$B9            ; value stack
frame           = ZPBASE+$BB            ; -> current frame
symtab          = ZPBASE+$BD            ; -> next symbol table entry
stmax           = ZPBASE+$BF            ; symbol table top (page)
addr            = ZPBASE+$C0            ; token address
token           = ZPBASE+$C2            ; token value

; following defs can be used during
; program execution (user program)
;-----------------------------------
isDirty         = ZPBASE+$C3
spln            = ZPBASE+$C4            ; error char
curln           = ZPBASE+$C5            ; error line
curnxt          = ZPBASE+$C7            ; next error line
nxtaddr         = ZPBASE+$C9            ; next token address

        ; note: $CA-$CD never referenced

bigSymTblGlobal = ZPBASE+$CE            ; only used if big symbol table
arrayptr        = ZPBASE+$D0            ; array mem. list
spnxt           = ZPBASE+$D2            ; next error char
nxttoken        = ZPBASE+$D3            ; next token value