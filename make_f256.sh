
mkdir -p obj/

# -------------------------------------

64tass  --m65816 \
        --c256-pgz \
        --output-exec=BOOT_ \
        --long-address \
        -D PGZ=1 \
        -o obj/action.pgz \
        --list=obj/action.lst \
        --labels=obj/action.lbl \
        action.asm
