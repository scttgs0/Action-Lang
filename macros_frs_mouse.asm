mouse_off       .macro
                stz MOUSE_PTR_CTRL_REG_L
                .endmacro

mouse_off_s     .macro
                php

                .m8
                .mouse_off

                plp
                .endmacro

mouse_on        .macro
                lda #1
                sta MOUSE_PTR_CTRL_REG_L
                .endmacro

mouse_on_s      .macro
                pha
                php

                .m8
                .mouse_on

                plp
                pla
                .endmacro
