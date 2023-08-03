python3 proc.py \
    --bench="humaneval" \
    --model="bigcode_15b_1000m" \
    --temp="0.2" \
    --config="reworded" \
    --output-csv="starcoderbase_0p2_reworded.csv" 

Rscript pass_matrix.r
