;---------------------------------------
; System Equates for Foenix C256
;---------------------------------------

BORDER_CTRL_REG         = $AF_0004              ; Bit[0] - Enable (1 by default)
bcEnable            = $01
                                                ; Bit[4..6]: X Scroll Offset (Will scroll Left)
BORDER_COLOR_B          = $AF_0005
BORDER_COLOR_G          = $AF_0006
BORDER_COLOR_R          = $AF_0007
BORDER_X_SIZE           = $AF_0008              ; Values: 0 - 32 (Default: 32)
BORDER_Y_SIZE           = $AF_0009              ; Values: 0 - 32 (Default: 32)

KBD_INPT_BUF            = $AF_1803
irqKBD              = $01                       ; keyboard Interrupt
KEYBOARD_SCAN_CODE      = $AF_115F

JOYSTICK0               = $AF_E800              ; (R) Joystick 0 - J7 (next to SD Card)
SIO_JOY                 = $AF_1200

FONT_MEMORY_BANK0       = $AF_8000              ; $AF8000 - $AF87FF

MASTER_CTRL_REG_L	    = $AF_0000
mcTextOn            = $01                       ; Enable the Text Mode
mcOverlayOn         = $02                       ; Enable the Overlay of the text mode on top of Graphic Mode (the Background Color is ignored)
mcGraphicsOn        = $04                       ; Enable the Graphic Mode
mcBitmapOn          = $08                       ; Enable the Bitmap Module In Vicky
mcTileMapOn         = $10                       ; Enable the Tile Module in Vicky
mcSpriteOn          = $20                       ; Enable the Sprite Module in Vicky
mcDisableVideo      = $80                       ; This will disable the Scanning of the Video hence giving 100% bandwith to the CPU

MASTER_CTRL_REG_H       = $AF_0001
mcVideoMode640      = $00                       ; 0 - 640x480 (Clock @ 25.175Mhz)
mcVideoMode800      = $01                       ; 1 - 800x600 (Clock @ 40Mhz)
mcVideoMode320      = $02                       ; 2 - 320x240 pixel-doubling (Clock @ 25.175Mhz)
mcVideoMode400      = $03                       ; 3 - 400x300 pixel-doubling (Clock @ 40Mhz)

C256F_MODEL_MAJOR       = $AF_070B
C256F_MODEL_MINOR       = $AF_070C

SID_FREQ1               = $AF_E400              ; [word]
SID_PULSE1              = $AF_E402              ; [word]
SID_CTRL1               = $AF_E404
SID_ATDCY1              = $AF_E405
SID_SUREL1              = $AF_E406

SID_FREQ2               = $AF_E407              ; [word]
SID_PULSE2              = $AF_E409              ; [word]
SID_CTRL2               = $AF_E40B
SID_ATDCY2              = $AF_E40C
SID_SUREL2              = $AF_E40D

SID_FREQ3               = $AF_E40E              ; [word]
SID_PULSE3              = $AF_E410              ; [word]
SID_CTRL3               = $AF_E412
SID_ATDCY3              = $AF_E413
SID_SUREL3              = $AF_E414

SID_CUTOFF              = $AF_E415              ; [word]
SID_RESON               = $AF_E417
SID_SIGVOL              = $AF_E418
SID_RANDOM              = $AF_E41B
SID_ENV3                = $AF_E41C

;--------------------------------------

SP_CONTROL_REG          = $AF_0C00
scEnable            = $01

scLUT0              = $00
scLUT1              = $02
scLUT2              = $04
scLUT3              = $06
scLUT4              = $08
scLUT5              = $0A
scLUT6              = $0C
scLUT7              = $0E

scDEPTH0            = $00
scDEPTH1            = $10
scDEPTH2            = $20
scDEPTH3            = $30
scDEPTH4            = $40
scDEPTH5            = $50
scDEPTH6            = $60

;SP_ADDR                 = $AF_0C01      ; [long]
;SP_X_POS                = $AF_0C04      ; [word]
;SP_Y_POS                = $AF_0C06      ; [word]
