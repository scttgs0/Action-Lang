
; SPDX-PackageSummary: Action! Programming Language
; SPDX-PackageOriginator: Clinton W Parker
; SPDX-PackageCopyrightText: Copyright 1983 by Clinton W Parker
; SPDX-License-Identifier: GPL-3.0-or-later

; SPDX-FileName: compiler.inc
; SPDX-FileCopyrightText: Copyright 2023-2024 Scott Giese


;--------------------------------------
;    Compiler tokens
;--------------------------------------

tokPLUS         = 1
tokMINUS        = 2
tokMULT         = 3
tokDIVD         = 4
tokOR           = 5
tokAND          = 6
tokEQU          = 7
tokNOTEQU       = 8
tokGRTR         = 9
tokGRTREQU      = 10
tokLESS         = 11
tokLESSEQU      = 12
tokREM          = 13
tokXOR          = 14
tokLSH          = 15
tokRSH          = 16
tokUMINUS       = 17
tokAT           = 18

tokSColon       = 21
tokSQuote       = 22
tokPeriod       = 23
tokRightParen   = 24
tokLeftParen    = 25
tokComma        = 26
tokDef          = 27
tokDigit        = 28+$80
tokHex          = 29
tokQuote        = 30

tokCHAR         = 32
tokBYTE         = 33
tokINT          = 34
tokCARD         = 35
tokSTRING       = 36
tokREAL         = 37
tokDEFINE       = 38
tokRECORD       = 39

tokARRAY        = 64
tokFUNC         = 65
tokPROC         = 66
tokGET          = 67
tokSET          = 68
tokPOINTER      = 69
tokTYPE         = 70

tokIF           = 80
tokWHILE        = 81
tokRET          = 82
tokEXIT         = 83
tokFOR          = 84
tokCASE         = 85
tokCode         = 86
tokMOD          = 87
tokUNTIL        = 88

tokLBracket     = 91
tokRBracket     = 93
tokUpArrow      = 94

tokTHEN         = 96
tokELSE         = 97
tokDO           = 98
tokFI           = 99
tokOD           = 100
tokTO           = 101
tokSTEP         = 102
tokOF           = 103
tokESAC         = 104
tokEDOC         = 105
tokELSEIF       = 106
tokDOWNTO       = 107

tokTYPE_t       = $70                   ; 112

tokEOF          = 127

tokCONST_t      = $80
tokVAR_t        = $88
tokARRAY_t      = $90
tokTEMP_t       = $A8
tokFUNC_t       = $C0
tokCOND_t       = $48

;    types
;--------------------------------------
tokCHAR_t       = 1
tokBYTE_t       = 2
tokINT_t        = 3
tokCARD_t       = 4
tokSTR_t        = 5
tokREAL_t       = 6

tokUNDEC        = $88


;--------------------------------------
;    Error codes
;--------------------------------------

;   0 - out of system memory
strERR          = 1                     ; missing " at end
dfnERR          = 2                     ; nested defines
symtblERR       = 3                     ; too many qglobal st
;   4 - too many local st entries
setERR          = 5                     ; SET syntax error
declERR         = 6                     ; declaration error
argERR          = 7                     ; bad argument list
varERR          = 8                     ; var not declared
constERR        = 9                     ; not int const
assgnERR        = 10                    ; bad assignment
endERR          = 11                    ; unknown error
thenERR         = 12                    ; missing THEN
fiERR           = 13                    ; missing FI
qcodeERR        = 14                    ; out of QCODE space
doERR           = 15                    ; missing DO
toERR           = 16                    ; missing TO
exprERR         = 17                    ; bad expression
parenthERR      = 18                    ; unmatched ()s
odERR           = 19                    ; missing OD
allocateERR     = 20                    ; can't alloc mem.
arrayERR        = 21                    ; bad array ref.
;   22 - file too large on input
condtERR        = 23                    ; illegal cond. exp.
forERR          = 24                    ; illegal FOR stmt
exitERR         = 25                    ; no loop for EXIT
nestERR         = 26                    ; nesting level too deep
typERR          = 27                    ; illegal type ref.
retrnERR        = 28                    ; illegal RETURN stmt.
;   61 - out of st space
brkERR          = $80                   ; Break key depressed
