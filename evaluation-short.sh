#!/bin/bash -e

OUTPUTDIR=/output
HOOGLE_DIR=/home/hoogle_plus

SHORT_BENCHMARKS=benchmark/suites/aec-short.yml

cd $HOOGLE_DIR
mkdir -p $OUTPUTDIR
rm -rf tmp/*

stack exec -- hplus generate --preset=popl2020 || (echo "failed to create database" && exit 1)
./scripts/run_each_benchmark.py --benchmarks $SHORT_BENCHMARKS || (echo "failed to run benchmarks" && exit 1)
./scripts/run_quality.py --benchmarks $SHORT_BENCHMARKS || (echo "failed to run result quality collection" && exit 1)
./scripts/collect_tsvs.py || (echo "gathering TSVs failed" && exit 1)
./scripts/collect_quality.py || (echo "failed to gather solutions" && exit 1)
mv bounded_variants.pdf $OUTPUTDIR/bounded_variants.pdf
mv major_variants.pdf $OUTPUTDIR/major_variants.pdf
mv quality.csv $OUTPUTDIR/quality.csv
mv table_results.tex $OUTPUTDIR/table_results.tex
cp table.tex $OUTPUTDIR/table.tex
echo "In another terminal, please navigate to your output directory and run 'pdflatex table.tex'"