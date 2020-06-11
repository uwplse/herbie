#!/bin/bash


report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")
rm -rf reports
mkdir -p reports


echo "Converting user submitted data into benchmark suite"
rm -rf "bench/demo"
mkdir "bench/demo"
racket infra/convert-demo.rkt "bench/demo" "infra/v10.json" "infra/v11.json" "infra/v13.json"


function run {
    bench="$1"; shift
    name="$1"; shift
    
    echo "Running herbie sampling on $name"
    seed=$(date "+%Y%j")
    racket "src/herbie.rkt" report  \
	   --num-iters 0 \
	   --note "demo" \
	   --profile \
	   --debug \
	   --seed "$seed" \
	   --threads 1 \
	   "$bench" "reports/$name"
}

for demofile in bench/demo/*; do
    name=$(basename "$demofile" .fpcore)
    run "$demofile" "$name"
done

