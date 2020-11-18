#!/bin/bash


CORES=4

report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")

mkdir -p rival-eval

function convertdemo {
    echo "Converting user submitted data into benchmark suite"
    rm -rf "bench/demo"
    mkdir "bench/demo"
    racket infra/convert-demo.rkt "bench/demo" "infra/v10.json" "infra/v11.json" "infra/v12.json" "infra/v13.json"

    racket infra/sort-fpbench-exprs.rkt "bench/demo/v10.fpcore" "bench/demo/v10-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v11.fpcore" "bench/demo/v11-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v12.fpcore" "bench/demo/v12-s.fpcore"
    racket infra/sort-fpbench-exprs.rkt "bench/demo/v13.fpcore" "bench/demo/v13-s.fpcore"
    rm bench/demo/v10.fpcore bench/demo/v11.fpcore bench/demo/v12.fpcore bench/demo/v13.fpcore
}


function convertfpbench {
    rm -rf FPBench
    git clone https://github.com/FPBench/FPBench
    mkdir FPBench/converted
    for bench in FPBench/benchmarks/*; do
	name=$(basename "$bench" .fpcore)
	echo "Converting FPBench file $name"
	racket FPBench/transform.rkt --unroll 5 --skip-loops --expand-while* --expand-let* "$bench" "FPBench/converted/$name.fpcore"
	racket infra/sort-fpbench-exprs.rkt "FPBench/converted/$name.fpcore" "FPBench/converted/$name-s.fpcore"
	rm "FPBench/converted/$name.fpcore"
    done
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


function runAll {
    benchDir="$1"; shift
    outputDir="$1"; shift
    rm -rf reports
    mkdir -p reports
    
    dirs=""
    for bench in $benchDir; do
	name=$(basename "$bench" .fpcore)
	run "$bench" "$name" "$@"
	if [ "$?" -eq 0 ]; then
	    dirs="$dirs reports/$name";
	fi
    done
    
    racket infra/nightly.rkt reports/ $dirs
    echo "moving reports to rival-eval/$outputDir"
    rm -rf "rival-eval/$outputDir"
    mv reports "rival-eval/$outputDir"
}

function demo {
    convertdemo
    runAll "bench/demo/*" "$@"
}

function nightly {
    rm -rf "bench/demo"
    runAll "bench/*" "$@"
}


function fpbench {
    convertfpbench
    runAll "FPBench/converted/*" "$@"
}

function demoSearchDisabled {
    demo demoSearchDisabled --disable setup:search
}

function demoSearchEnabled {
    demo demoSearchEnabled --enable setup:search
}

function nightlySearchDisabled {
    nightly nightlySearchDisabled --disable setup:search
}

function nightlySearchEnabled {
    nightly nightlySearchEnabled --enable setup:search
}

function fpbenchSearchDisabled {
    fpbench fpbenchSearchDisabled --disable setup:search
}

function fpbenchSearchEnabled {
    fpbench fpbenchSearchEnabled --enable setup:search
}


function all {
    demoSearchDisabled
    demoSearchEnabled
    nightlySearchDisabled
    nightlySearchEnabled
    fpbenchSearchDisabled
    fpbenchSearchEnabled
}

for cmd in $@; do
    echo "Running $cmd"
    $cmd
done



