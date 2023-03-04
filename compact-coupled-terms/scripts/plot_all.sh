python scripts/plot.py --files \
    results_stackoverflow_default_0.csv \
    results_stackoverflow_default_1.csv \
    results_stackoverflow_default_2.csv \
    ../hoogle_plus/output/result_ecta_0.tsv \
    ../hoogle_plus/output/result_ecta_1.tsv \
    ../hoogle_plus/output/result_ecta_2.tsv \
    --type stackoverflow \
    --output stackoverflow_loop.pdf

python scripts/plot.py --files \
    results_hplus_default_0.csv \
    results_hplus_default_1.csv \
    results_hplus_default_2.csv \
    ../hoogle_plus/output/result_0.tsv \
    ../hoogle_plus/output/result_1.tsv \
    ../hoogle_plus/output/result_2.tsv \
    --type hplus-cactus \
    --output hplus_loop_cactus.pdf

python scripts/plot.py --files \
    results_hplus_default_0.csv \
    results_hplus_default_1.csv \
    results_hplus_default_2.csv \
    ../hoogle_plus/output/result_0.tsv \
    ../hoogle_plus/output/result_1.tsv \
    ../hoogle_plus/output/result_2.tsv \
    --type baseline-zoomin \
    --output hplus_loop_scatter.pdf

python scripts/plot.py --files \
    results_hplus_default_0.csv \
    results_hplus_default_1.csv \
    results_hplus_default_2.csv \
    results_hplus_noEnumeration_0.csv \
    results_hplus_noEnumeration_1.csv \
    results_hplus_noEnumeration_2.csv \
    results_hplus_noReduction_0.csv \
    results_hplus_noReduction_1.csv \
    results_hplus_noReduction_2.csv \
    --type ablation \
    --output ablation_loop.pdf