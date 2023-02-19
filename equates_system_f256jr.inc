
;---------------------------------------
; System Equates for Foenix C256jr
;---------------------------------------

MMU_CTRL                = $0000
mmuPage0            = $00
mmuPage1            = $01
mmuPage2            = $02
mmuPage3            = $03
mmuEditPage0        = $00
mmuEditPage1        = $10
mmuEditPage2        = $20
mmuEditPage3        = $30
mmuEditMode         = $80

; - - - - - - - - - - - - - - - - - - -

MMU_Block0              = $0008
MMU_Block1              = $0009
MMU_Block2              = $000A
MMU_Block3              = $000B
MMU_Block4              = $000C
MMU_Block5              = $000D
MMU_Block6              = $000E
MMU_Block7              = $000F

;---------------------------------------

IOPAGE_CTRL             = $0001
iopPage0            = $00
iopPage1            = $01
iopPage2            = $02
iopPage3            = $03
iopDisable          = $04

;---------------------------------------

MASTER_CTRL_L           = $D000
mcTextOn            = $01               ; Enable Text Mode
mcOverlayOn         = $02               ; Overlay Text on top of Graphics (Text Background Color is transparent)
mcGraphicsOn        = $04               ; Enable Graphic Mode
mcBitmapOn          = $08               ; Enable the Bitmap Module
mcTileMapOn         = $10               ; Enable the Tile Module
mcSpriteOn          = $20               ; Enable the Sprite Module
mcGammaOn           = $40               ; Enable Gamma Correction

MASTER_CTRL_H           = $D001
mcVideoMode240      = $00               ; 0 - 80x60 (Clock @ 60Mhz)                    - Text Mode
                                        ; 0 - 320x240 w/pixel-doubling (Clock @ 60Mhz) - Graphics Mode
mcVideoMode200      = $01               ; 1 - 80x50 (Clock @ 70Mhz)                    - Text Mode
                                        ; 1 - 320x200 w/pixel-doubling (Clock @ 70Mhz) - Graphics Mode
mcTextDoubleX       = $02               ; X Pixel Doubling - Text Mode only
mcTextDoubleY       = $04               ; Y Pixel Doubling - Text Mode only
mcDisableVideo      = $08               ; [rev B] Disable the scanning of the video (hence giving 100% bandwidth to the CPU)
mcTransparent       = $10               ; [rev B]
mcFontSet1          = $20

;---------------------------------------

LAYER_ORDER_CTRL_0      = $D002
locLayer0_BM0       = $00
locLayer0_BM1       = $01
locLayer0_BM2       = $02
locLayer0_TL0       = $04
locLayer0_TL1       = $05
locLayer0_TL2       = $06
locLayer1_BM0       = $00
locLayer1_BM1       = $10
locLayer1_BM2       = $20
locLayer1_TL0       = $40
locLayer1_TL1       = $50
locLayer1_TL2       = $60

LAYER_ORDER_CTRL_1      = $D003
locLayer2_BM0       = $00
locLayer2_BM1       = $01
locLayer2_BM2       = $02
locLayer2_TL0       = $04
locLayer2_TL1       = $05
locLayer2_TL2       = $06

;---------------------------------------

BORDER_CTRL             = $D004         ; Bit[0] - Enable (1 by default)
bcEnable            = $01               ; Bit[4..6]: X Scroll Offset (Will scroll Left)

BORDER_COLOR_B          = $D005
BORDER_COLOR_G          = $D006
BORDER_COLOR_R          = $D007

BORDER_SIZE_X           = $D008         ; Values: 0 - 31 (Default: 31)
BORDER_SIZE_Y           = $D009         ; Values: 0 - 31 (Default: 31)

;---------------------------------------

BACKGROUND_COLOR_B      = $D00D         ; When in Graphic Mode, if a pixel is "0" then the background pixel is chosen
BACKGROUND_COLOR_G      = $D00E
BACKGROUND_COLOR_R      = $D00F

;---------------------------------------

CURSOR_CTRL             = $D010         ; Bit[0] enable, Bit[1..2] flash rate, Bit[3] no flash
ccEnable            = $01
ccFlashRate1000     = $00
ccFlashRate500      = $02
ccFlashRate250      = $04
ccFlashRate200      = $06
ccNoFlash           = $08

CURSOR_CHAR_CODE        = $D012
CURSOR_X                = $D014         ; [word]
CURSOR_Y                = $D016         ; [word]

;---------------------------------------

; Line Interrupt (SOL)
LINE_IRQ_CTRL           = $D018         ; [W] Bit[0] - Enable
licEnable       = $01

LINE_IRQ_VALUE          = $D019         ; [W] [word] Write Only Bits[11:0]

RASTER_X                = $D018         ; [R] [word] pixel displacement on raster line Bits[11:0]
RASTER_Y                = $D01A         ; [R] [word] raster line Bits[11:0]

;---------------------------------------

BITMAP0_CTRL            = $D100
bmcEnable           = $01
bmcLUT0             = $00
bmcLUT1             = $02
bmcLUT2             = $04
bmcLUT3             = $06

BITMAP0_ADDR            = $D101         ; [long] Bits[18:0]

BITMAP1_CTRL            = $D108
BITMAP1_ADDR            = $D109         ; [long] Bits[18:0]

BITMAP2_CTRL            = $D110
BITMAP2_ADDR            = $D111         ; [long] Bits[18:0]

;---------------------------------------

TILE0_CTRL              = $D200
tcEnable            = $01
tcSmallTiles        = $08
TILE0_ADDR              = $D201         ; [long] Bits[18:0]
TILE0_SIZE_X            = $D204         ; [word]
TILE0_SIZE_Y            = $D206         ; [word]

TILE0_SCROLL_X          = $D208         ; [word] Bits[13:4] Tile #
                                        ;        Bits[3:0] Scroll Position (16x16 tiles)
                                        ;        Bits[3:1] Scroll Position (8x8 tiles)
                                        ;        Bit[15] Scroll Direction

TILE0_SCROLL_Y          = $D20A         ; [word] Bits[13:4] Tile #
                                        ;        Bits[3:0] Scroll Position (16x16 tiles)
                                        ;        Bits[3:1] Scroll Position (8x8 tiles)
                                        ;        Bit[15] Scroll Direction

TILE1_CTRL              = $D20C
TILE1_ADDR              = $D20D
TILE1_SIZE_X            = $D210
TILE1_SIZE_Y            = $D212
TILE1_SCROLL_X          = $D214
TILE1_SCROLL_Y          = $D216

TILE2_CTRL              = $D218
TILE2_ADDR              = $D219
TILE2_SIZE_X            = $D21C
TILE2_SIZE_Y            = $D21E
TILE2_SCROLL_X          = $D220
TILE2_SCROLL_Y          = $D222

TILESET0_ADDR           = $D280         ; [long] Bits[18:0]
TILESET0_ADDR_CFG       = $D283
tsVertical          = $00
tsSquare            = $08

TILESET1_ADDR           = $D284         ; [long] Bits[18:0]
TILESET1_ADDR_CFG       = $D287

TILESET2_ADDR           = $D288         ; [long] Bits[18:0]
TILESET2_ADDR_CFG       = $D28B

TILESET3_ADDR           = $D28C         ; [long] Bits[18:0]
TILESET3_ADDR_CFG       = $D28F

TILESET4_ADDR           = $D290         ; [long] Bits[18:0]
TILESET4_ADDR_CFG       = $D293

TILESET5_ADDR           = $D294         ; [long] Bits[18:0]
TILESET5_ADDR_CFG       = $D297

TILESET6_ADDR           = $D298         ; [long] Bits[18:0]
TILESET6_ADDR_CFG       = $D29B

TILESET7_ADDR           = $D29C         ; [long] Bits[18:0]
TILESET7_ADDR_CFG       = $D29F

;---------------------------------------

XY_BASE_ADDR            = $D301         ; [long] [W] Bits[17:0]
XY_POS_X                = $D304
XY_POS_Y                = $D306

XY_OFFSET               = $D308         ; [word] [R]
XY_BANK                 = $D30A         ; [R]
XY_ADDR                 = $D30B         ; [long] [R] Bits[17:0]

;---------------------------------------

SID1_BASE               = $D400
SID1_FREQ1              = $D400         ; [word]
SID1_PULSE1             = $D402         ; [word]
SID1_CTRL1              = $D404
SID1_ATDCY1             = $D405
SID1_SUREL1             = $D406

SID1_FREQ2              = $D407         ; [word]
SID1_PULSE2             = $D409         ; [word]
SID1_CTRL2              = $D40B
SID1_ATDCY2             = $D40C
SID1_SUREL2             = $D40D

SID1_FREQ3              = $D40E         ; [word]
SID1_PULSE3             = $D410         ; [word]
SID1_CTRL3              = $D412
SID1_ATDCY3             = $D413
SID1_SUREL3             = $D414

SID1_CUTOFF             = $D415         ; [word]
SID1_RESON              = $D417
SID1_SIGVOL             = $D418
SID1_RANDOM             = $D41B
SID1_ENV3               = $D41C

SID2_BASE               = $D500
SID2_FREQ1              = $D500         ; [word]
SID2_PULSE1             = $D502         ; [word]
SID2_CTRL1              = $D504
SID2_ATDCY1             = $D505
SID2_SUREL1             = $D506

SID2_FREQ2              = $D507         ; [word]
SID2_PULSE2             = $D509         ; [word]
SID2_CTRL2              = $D50B
SID2_ATDCY2             = $D50C
SID2_SUREL2             = $D50D

SID2_FREQ3              = $D50E         ; [word]
SID2_PULSE3             = $D510         ; [word]
SID2_CTRL3              = $D512
SID2_ATDCY3             = $D513
SID2_SUREL3             = $D514

SID2_CUTOFF             = $D515         ; [word]
SID2_RESON              = $D517
SID2_SIGVOL             = $D518
SID2_RANDOM             = $D51B
SID2_ENV3               = $D51C

;---------------------------------------

PSG1_BASE               = $D600     ; TODO:
;PSG1_T1_FREQ            = $D600
;PSG1_T1_ATTN            = $D601
;PSG1_T2_FREQ            = $D602
;PSG1_T2_ATTN            = $D603
;PSG1_T3_FREQ            = $D604
;PSG1_T3_ATTN            = $D605
;PSG1_NOISE_CTRL         = $D606
;PSG1_NOISE_ATTN         = $D607

PSG_DUAL_BASE           = $D608

PSG2_BASE               = $D610
;PSG2_T1_FREQ            = $D610
;PSG2_T1_ATTN            = $D611
;PSG2_T2_FREQ            = $D612
;PSG2_T2_ATTN            = $D613
;PSG2_T3_FREQ            = $D614
;PSG2_T3_ATTN            = $D615
;PSG2_NOISE_CTRL         = $D616
;PSG2_NOISE_ATTN         = $D617

;---------------------------------------

UART_DATA               = $D630
UART_IER                = $D631
UART_ISR                = $D632
UART_FCR                = $D632
UART_LCR                = $D633
UART_MCR                = $D634
UART_LSR                = $D635
UART_MSR                = $D636
UART_SPR                = $D637

UART_DLL                = $D630
UART_DLH                = $D631
UART_PSD                = $D632

;---------------------------------------

PS2_CTRL                = $D640
ps2KeybdWrite       = $02
ps2MouseWrite       = $08
ps2KeybdClear       = $10
ps2MouseClear       = $20

PS2_OUT                 = $D641
PS2_KEYBD_IN            = $D642
PS2_MOUSE_IN            = $D643

PS2_STATUS              = $D644
ps2KeybdEmpty       = $01
ps2MouseEmpty       = $02
ps2MouseErr         = $10
ps2MouseAck         = $20
ps2KeybdErr         = $40
ps2KeybdAck         = $80

;---------------------------------------

INT_PENDING_REG0        = $D660
INT_PENDING_REG1        = $D661
INT_PENDING_REG2        = $D662 ; NOT USED
INT_PENDING_REG3        = $D663 ; NOT USED
FNX0_INT00_SOF      = $01
FNX0_INT01_SOL      = $02

INT_EDGE_REG0           = $D668
INT_EDGE_REG1           = $D669
INT_EDGE_REG2           = $D66A ; NOT USED

INT_MASK_REG0           = $D66C
INT_MASK_REG1           = $D66D
INT_MASK_REG2           = $D66E ; NOT USED

FNX1_INT00_KBD      = $01
FNX1_INT01_COL0     = $02               ; collision detection
FNX1_INT02_COL1     = $04
FNX1_INT03_COL2     = $08

;---------------------------------------

RTC_SEC                 = $D690
RTC_MIN                 = $D692
RTC_HRS                 = $D694
RTC_DAY                 = $D696

;---------------------------------------

MACHINE_ID              = $D6A7
macC256_FMX         = $00
macC256_U           = $01
macF256_JR          = $02
macC256_GENX        = $04
macC256_UPLUS       = $05
macA2560U_PLUS      = $06
macA2560X           = $08
macA2560U           = $09
macA2560M           = $0A
macA2560K           = $0B
macF256K            = $12

;---------------------------------------

MOUSE_CTRL              = $D6E0
mcEnable            = $01
mcLegacyMode        = $02

MOUSE_X                 = $D6E2         ; [word]
MOUSE_Y                 = $D6E4         ; [word]
MOUSE_PACKET            = $D6E6         ; [3-byte] [legacy mode only]

;---------------------------------------

C256F_MODEL_MAJOR       = $D70B
C256F_MODEL_MINOR       = $D70C

;--- IO PAGE 0 -------------------------

FG_CHAR_LUT_PTR         = $D800         ; 16 entries = ARGB     $008060FF (medium slate blue)
                                        ;                       $00108020 (la salle green)
BG_CHAR_LUT_PTR		    = $D840         ; 16 entries = ARGB

;---------------------------------------

SP00_CTRL               = $D900
scEnable            = $01
scLUT0              = $00
scLUT1              = $02
scLUT2              = $04
scLUT3              = $06

scDEPTH0            = $00
scDEPTH1            = $08
scDEPTH2            = $10
scDEPTH3            = $18

scSIZE_32           = $00
scSIZE_24           = $20
scSIZE_16           = $40
scSIZE_8            = $60

SP00_ADDR               = $D901      ; [long] Bits[18:0]
SP00_X                  = $D904      ; [word]
SP00_Y                  = $D906      ; [word]

SP01_CTRL               = $D908
SP01_ADDR               = $D909
SP01_X                  = $D90C
SP01_Y                  = $D90E

SP02_CTRL               = $D910
SP02_ADDR               = $D911
SP02_X                  = $D914
SP02_Y                  = $D916

SP03_CTRL               = $D918
SP03_ADDR               = $D919
SP03_X                  = $D91C
SP03_Y                  = $D91E

;...

; SP63_CTRL             = $DAF8
; SP63_ADDR             = $DAF9
; SP63_X                = $DAFC
; SP63_Y                = $DAFE

;---------------------------------------
;---------------------------------------
;   IO Bank 01
;---------------------------------------

FONT_MEMORY_BANK0       = $C000         ; [C000:C7FF]

;--------------------------------------

GRPH_LUT0_PTR	        = $D000         ; [D000:D3FF]
GRPH_LUT1_PTR	        = $D400         ; [D400:D7FF]
GRPH_LUT2_PTR	        = $D800         ; [D800:DBFF]
GRPH_LUT3_PTR	        = $DC00         ; [DC00:DFFF]


;---------------------------------------
;---------------------------------------
;   IO Bank 02
;---------------------------------------

CS_TEXT_MEM_PTR         = $C000         ; [C000:DFFF]   ascii code for text character


;---------------------------------------
;---------------------------------------
;   IO Bank 02
;---------------------------------------
CS_COLOR_MEM_PTR        = $C000         ; [C000:DFFF]   HiNibble = Foreground; LoNibble = Background
                                        ;               0-15 = index into the CHAR_LUT tables


;--------------------------------------

JOYSTICK0               = $DC00         ; (R) Joystick 0
JOYSTICK1               = $DC01
JOY_UP              = $01               ;           1110            bit-0   UP
JOY_DOWN            = $02               ;      1010   |   0110      bit-1   DOWN
JOY_LEFT            = $04               ;          \  |  /          bit-2   LEFT
JOT_RIGHT           = $08               ;   1011----1111----0111    bit-3   RIGHT
JOT_BUTTON0         = $10               ;          /  |  \
JOY_BUTTON1         = $20               ;      1001   |   0101
JOY_BUTTON2         = $40               ;           1101

LUTBkColor          = 0
LUTPfColor0         = 1
LUTPfColor1         = 2
LUTPfColor2         = 3
LUTPfColor3         = 4
LUTPfColor4         = 5
LUTPfColor5         = 6
LUTPfColor6         = 7
LUTPfColor7         = 8
LUTSprColor0        = 9
LUTSprColor1        = 10
LUTSprColor2        = 11
LUTSprColor3        = 12
LUTSprColor4        = 13
LUTSprColor5        = 14
LUTSprColor6        = 15
LUTSprColor7        = 16

;--------------------------------------

; READ
RNG_DAT_LO              = $D6A4         ; Low-byte of 16Bit RNG Generator
RNG_DAT_HI              = $D6A5         ; Hi-byte of 16Bit RNG Generator

; WRITE
RNG_SEED_LO             = $D6A4         ; Low-byte of 16Bit RNG Generator
RNG_SEED_HI             = $D6A5         ; Hi-byte of 16Bit RNG Generator

; WRITE
RNG_CTRL                = $D6A6
rcEnable        = $01                   ; Enable the LFSR BLOCK_LEN
rcDV            = $02                   ; After Setting the Seed Value, Toggle that Bit for it be registered

;---------------------------------------

DMA_CTRL                = $DF00         ; DMA Control Register
dmaDISABLE      = $00
dmaENABLE       = $01                   ; DMA engine is enabled
dma2D           = $02                   ; DMA is 2D operation (otherwise it is 1D)
dmaFILL         = $04                   ; DMA is a fill operation (otherwise DMA is a copy)
dmaINT_EN       = $08                   ; Enable DMA interrupts
dmaSTART        = $80                   ; Start the DMA operation

DMA_STATUS              = $DF01         ; DMA status register (Read Only)
dmaBUSY         = $80                   ; DMA engine is busy with an operation

DMA_FILL_VAL            = $DF01         ; Byte value to use for fill operations

DMA_SRC_ADDR            = $DF04         ; Source address (system bus) for copy operations
DMA_DST_ADDR            = $DF08         ; Destination address (system bus) for fill and copy operations

DMA_COUNT               = $DF0C         ; Number of bytes to fill or copy (1D operations, 24 bit value)
DMA_WIDTH               = $DF0C         ; Width of rectangle to fill or copy (2D operations, 16 bit value)
DMA_HEIGHT              = $DF0E         ; Height of rectangle to fill or copy (2D operations, 16 bit value)
DMA_STRIDE_SRC          = $DF10         ; Width of the source bitmap image in bytes (2D operations, 16 bit value)
DMA_STRIDE_DST          = $DF12         ; Width of the destination bitmap image in bytes (2D operations, 16 bit value)

;---------------------------------------

vecCOP                  = $FFF4
vecABORT                = $FFF8
vecNMI                  = $FFFA
vecRESET                = $FFFC
vecIRQ_BRK              = $FFFE
