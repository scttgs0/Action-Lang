
mkdir -p obj/

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/action.bin \
        --list=obj/action.lst \
        --labels=obj/action.lbl \
        action.asm
