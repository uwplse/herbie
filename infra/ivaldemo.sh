#!/bin/bash

CORES=4

report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")



function convert {
    echo "Converting user submitted data into benchmark suite"
    rm -rf "bench/demo"
    mkdir "bench/demo"
    racket infra/convert-demo.rkt "bench/demo" "infra/v10.json" "infra/v11.json" "infra/v12.json" "infra/v13.json"

    racket infra/sort-fpbench-exprs.rkt "bench/demo/v10.fpcore" > "bench/demo/v10-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v11.fpcore" > "bench/demo/v11-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v12.fpcore" > "bench/demo/v12-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v13.fpcore" > "bench/demo/v13-s.fpcore"
    rm bench/demo/v10.fpcore bench/demo/v11.fpcore bench/demo/v12.fpcore bench/demo/v13.fpcore
}


function run {
  bench="$1"; shift
  name="$1"; shift
  seed=$(date "+%Y%j")
  
  echo "Running $name test with flags $@"
  rm -rf "reports/$name"
  racket "src/herbie.rkt" report \
      --num-iters 0 \
      --note "$name" \
      --profile \
      --debug \
      --seed "$seed" \
      --threads "$CORES" \
      "$@" \
      "$bench" "reports/$name"
}


function demonormal {
    rm -rf reports
    mkdir -p reports
    convert
    for demofile in bench/demo/*; do
	name=$(basename "$demofile" .fpcore)
	run "$demofile" "$name" --enable setup:search
    done
    mv reports demonormalreports
}

demonormal




