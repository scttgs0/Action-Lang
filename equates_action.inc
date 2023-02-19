
SRTIMR          = $022B                 ; key repeat delay [default:$30 -- reset with each keystroke]
SDMCTL          = $022F
SSKCTL          = $0232                 ; Serial port control register
                                        ;   bit 0 =     Enable debouce
                                        ;       1 =     Enable keyboard scanning
                                        ;       2 =     The pot counter completes a read within two scan lines
                                        ;       3 =     POKEY two-tone mode
                                        ;     4-6 =     Serial port mode control
                                        ;       7 =     Force break; serial output to zero
PADDL0          = $0270
TABMAP          = $02A3                 ; 15-byte TODO:
                ; $02B1
INVFLG          = $02B6                 ; Inverse character flag; 0=normal; 128=inverse
SHFLOK          = $02BE                 ; Flag for the shift and control keys; 0=lowercase; 64=uppercase; 128=control
COLOR0          = $02C4
COLOR4          = $02C8                 ; background color
INITAD          = $02E2                 ; 2-byte
MEMTOP          = $02E5
MEMLO           = $02E7

CH1             = $02F2                 ; Prior keyboard qcode
CH_             = $02FC                 ; character buffer
FILDAT          = $02FD                 ; Color data for the fill region in the XIO FILL command
DSPFLG          = $02FE                 ; display flag -- 0=normal ASCII control code behavior; 1=display ASCII control codes (e.g. 27-31)

TRIG0           = $D010
COLPF0          = $D016
CONSOL          = $D01F                 ; Function Keys [default:8]

AUDF1           = $D200                 ; (W) Audio channel one frequency
AUDC1           = $D201                 ; (W) Audio channel one control
RANDOM          = $D20A
SKCTL           = $D20F                 ; Serial port control (w);  255=no key press; 251=most keys pressed; 247=SHIFT key pressed

PORTA           = $D300                 ; joystick port

DMACTL          = $D400
WSYNC           = $D40A


; --- IOCB ----------------------------
CIOV            = $E456
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


; --- Floating-Point ------------------
AFP             = $D800
FASC            = $D8E6
IFP             = $D9AA
FPI             = $D9D2


; --- Bank-switching ------------------

bank            = $D500
lbank       = $00                       ; local bank
ebank       = $03                       ; editor bank
cbank       = $09                       ; compiler bank

;--------------------------------------
;--------------------------------------

ScreenResX      = 800
ScreenResY      = 600
CharResX        = 100
CharResY        = 75