
spr_addr        .sfunction num, $AF_0C01+(num*8)
spr_xpos        .sfunction num, $AF_0C04+(num*8)
spr_ypos        .sfunction num, $AF_0C06+(num*8)

sta_spr_xpos    .function num
                sta $AF_0C04+(num*8)
                .endfunction

sta_spr_ypos    .function num
                sta $AF_0C06+(num*8)
                .endfunction

sta_ix_spr_xpos .macro
                pha
                phx

                txa
                asl
                asl
                asl
                tax
                sta $AF_0C04,x

                plx
                pla
                .endmacro
