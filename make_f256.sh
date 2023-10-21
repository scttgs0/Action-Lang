
mkdir -p obj/

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/actionJR.bin \
        --list=obj/actionJR.lst \
        --labels=obj/actionJR.lbl \
        action.asm
