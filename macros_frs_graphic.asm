graphics        .macro
                lda #\1
                sta MASTER_CTRL_REG_L

                lda #\2
                sta MASTER_CTRL_REG_H
                .endmacro

graphics_s      .macro
                pha
                php

                .m8
                .graphics \@

                plp
                pla
                .endmacro

border_off      .macro
                stz BORDER_CTRL_REG
                stz BORDER_X_SIZE
                stz BORDER_Y_SIZE
                .endmacro

border_off_s    .macro
                php

                .m8
                .border_off

                plp
                .endmacro

border_on       .macro color, xSize, ySize
                php

                lda #$01
                sta BORDER_CTRL_REG

                lda #\xSize
                sta BORDER_X_SIZE

                lda #\ySize
                sta BORDER_Y_SIZE

                .m16
                lda #\color
                sta BORDER_COLOR_B-1

                plp
                .endmacro
