WARMST          = $03_0008
LMARGN          = $03_0052
RMARGN          = $03_0053
ROWCRS          = $03_0054              ; 1-byte TODO:
COLCRS          = $03_0055              ; 2-byte
OLDROW          = $03_005A              ; 1-byte TODO
OLDCOL          = $03_005B              ; 2-byte
OLDCHR          = $03_005D              ; character under the cursor
                                        ; used to restore that character when the cursor moves
OLDADR          = $03_005E              ; 2-bytes TODO
                                        ; current cursor location (for restore)

TABMAP          = $03_02A3              ; 15-byte TODO:
                ; $03_02B1

INITAD          = $03_02E2              ; 2-byte
MEMTOP          = $03_02E5
MEMLO           = $03_02E7

CH1             = $03_02F2              ; Prior keyboard qcode
CH_             = $03_02FC              ; character buffer
