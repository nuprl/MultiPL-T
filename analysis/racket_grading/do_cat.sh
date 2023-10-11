#!/bin/bash
cd select_for_grading
for model_dir in */; do
    if [ -d "$model_dir" ]; then
        cd "$model_dir"
        for prob_dir in */; do
            if [ -d "$prob_dir" ]; then
                cd "$prob_dir"
                echo "Inside prob dir"
                echo "$PWD"
                for filename in "$PWD"/*program*.rkt; do
                    # cut filename
                    echo ";;`expr match "$filename" '.*select_for_grading\(.*\)'`"
                    cat "$filename"
                    echo
                done > "$PWD"/all.rkt
                cd ..
            fi
        done
        cd ..
    fi
done