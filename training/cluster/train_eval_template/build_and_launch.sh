EXP_ROOT=experiments
python3 fill_template.py --exp-root $EXP_ROOT

for dir in $EXP_ROOT/*; do
  if [ -d "$dir" ]; then
    echo "Launching experiment $dir"
    cd $dir
    ./launch.sh
    cd ..
  fi
done