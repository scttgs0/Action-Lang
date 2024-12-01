
mkdir -p obj/

# -------------------------------------

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -D ZAPRAM=1 \
        -o obj/action.rom \
        --list=obj/action_rom.lst \
        --labels=obj/action_rom.lbl \
        action.asm
