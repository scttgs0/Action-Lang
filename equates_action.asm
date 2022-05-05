
SRTIMR          = $03_022B              ; key repeat delay [default:$30 -- reset with each keystroke]
SDMCTL          = $03_022F
SSKCTL          = $03_0232              ; Serial port control register
                                        ;   bit 0 =     Enable debouce
                                        ;       1 =     Enable keyboard scanning
                                        ;       2 =     The pot counter completes a read within two scan lines
                                        ;       3 =     POKEY two-tone mode
                                        ;     4-6 =     Serial port mode control
                                        ;       7 =     Force break; serial output to zero
PADDL0          = $03_0270
TABMAP          = $03_02A3              ; 15-byte TODO:
                ; $03_02B1
INVFLG          = $03_02B6              ; Inverse character flag; 0=normal; 128=inverse
SHFLOK          = $03_02BE              ; Flag for the shift and control keys; 0=lowercase; 64=uppercase; 128=control
COLOR0          = $03_02C4
COLOR4          = $03_02C8              ; background color
INITAD          = $03_02E2              ; 2-byte
MEMTOP          = $03_02E5
MEMLO           = $03_02E7

CH1             = $03_02F2              ; Prior keyboard qcode
CH_             = $03_02FC              ; character buffer
FILDAT          = $03_02FD              ; Color data for the fill region in the XIO FILL command
DSPFLG          = $03_02FE              ; display flag -- 0=normal ASCII control code behavior; 1=display ASCII control codes (e.g. 27-31)

TRIG0           = $03_D010
COLPF0          = $03_D016
CONSOL          = $03_D01F              ; Function Keys [default:8]

AUDF1           = $03_D200              ; (W) Audio channel one frequency
AUDC1           = $03_D201              ; (W) Audio channel one control
RANDOM          = $03_D20A
SKCTL           = $03_D20F              ; Serial port control (w);  255=no key press; 251=most keys pressed; 247=SHIFT key pressed

PORTA           = $03_D300              ; joystick port

DMACTL          = $03_D400
WSYNC           = $03_D40A


; IOCB ================================
CIOV            = $03_E456
IOCB0           = $03_0340
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


; Floating-Point ======================
AFP             = $03_D800
FASC            = $03_D8E6
IFP             = $03_D9AA
FPI             = $03_D9D2


; Bank-switching ======================
bank            = $03_D500
lbank       = $00                       ; local bank
ebank       = $03                       ; editor bank
cbank       = $09                       ; compiler bank

;======================================
;======================================

ScreenResX      = 800
ScreenResY      = 600
CharResX        = 100
CharResY        = 75
