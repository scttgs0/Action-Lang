64tass  --m65816 \
        --long-address \
        --flat \
        --nostart \
        -o action.pgx \
        --list=action_pgx.lst \
        --labels=action_pgx.lbl \
        action.asm

cp ./action.pgx /media/scott/ExternData/src/GameEngines/C256Foenix/bin/SDCARD/
