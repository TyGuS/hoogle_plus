#!/bin/bash -e

OUTPUTDIR=/output
HOOGLE_DIR=/home/hoogle_plus

cd $HOOGLE_DIR
mkdir -p $OUTPUTDIR

stack exec -- hplus generate --preset=icfppartial || (echo "failed to create database" && exit 1)
./scripts/run_each_benchmark.py || (echo "failed to run benchmarks" && exit 1)
./scripts/run_quality.py || (echo "failed to run result quality collection" && exit 1)
./scripts/collect_tsvs.py || (echo "gathering TSVs failed" && exit 1)
./scripts/collect_quality.py || (echo "failed to gather solutions" && exit 1)
mv bounded_variants.pdf $OUTPUTDIR/bounded_variants.pdf
mv major_variants.pdf $OUTPUTDIR/major_variants.pdf
mv quality.csv $OUTPUTDIR/quality.csv
pdflatex table.tex
mv table.pdf $OUTPUTDIR/table.pdf