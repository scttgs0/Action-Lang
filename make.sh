64tass  --m65xx \
        --atari-xex \
        -b \
        -o action!.rom \
        --list=action_rom.lst \
        --labels=action_rom.lbl \
        action.asm

hd -v action.rom >action.rom.new.txt
