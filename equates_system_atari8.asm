CH_             = $02FC                 ; character buffer
DSPFLG          = $02FE                 ; display flag

IOCB0           = $0340
ICCOM       = $02
ICBAL       = $04
ICBAH       = $05
ICBLL       = $08
ICBLH       = $09
ICAX1       = $0A
ICAX2       = $0B
ICAX3       = $0C
ICAX4       = $0D
ICAX5       = $0E

SRTIMR          = $022B
SDMCTL          = $022F
SSKCTL          = $0232
PADDL0          = $0270
TXTMSC          = $0294
TABMAP          = $02A3
INVFLG          = $02B6
SHFLOC          = $02BE
COLOR0          = $02C4
COLOR4          = $02C8
INITAD          = $02E2
MEMTOP          = $02E5
MEMLO           = $02E7
CH1             = $02F2                 ; Prior keyboard qcode (CH1 in OS listing)
FILDAT          = $02FD                 ; fill color

TRIG0           = $D010
COLPF0          = $D016
CONSOL          = $D01F

POT0            = $D200
AUDF1           = $D200
AUDC1           = $D201
RANDOM          = $D20A
SKCTL           = $D20F

PORTA           = $D300

DMACTL          = $D400
WSYNC           = $D40A

;    floating point routines
;    -----------------------
AFP             = $D800
FASC            = $D8E6
IFP             = $D9AA
FPI             = $D9D2

CIOV            = $E456
