all: main_figures main_tables supp_figures supp_tables README.md dag
main_figures: results/img/cap_always_subsidized.pdf
main_tables: results/tab/not_sub.tex results/tab/loglin_not_sub.tex results/tab/fuel_left.tex results/tab/fuel_right.tex
supp_figures:
supp_tables: results/tab/supp_not_sub.tex results/tab/supp_loglin_not_sub.tex results/tab/hours_left.tex results/tab/hours_right.tex
dag: makefile-dag.png workflow.png

# draw makefile dag
makefile-dag.png: Makefile
	make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o makefile-dag.png

workflow.png: Makefile
	LANG=C make -p | python3 make_p_to_json.py | python3 json_to_dot.py | dot -Tpng >| workflow.png

README.md: scripts/make_README.r makefile-dag.png
	cd $(<D); Rscript $(<F)

## MAIN FIGURES ################################################################
results/img/cap_always_subsidized.pdf: scripts/content/plot_cap_always_subsidized.R
	cd $(<D); Rscript $(<F)

## MAIN TABLES #################################################################
results/tab/not_sub.tex: scripts/04_analysis/01_shrimp_price_elasticity.R
	cd $(<D); Rscript $(<F)

results/tab/supp_not_sub.tex: scripts/04_analysis/01_shrimp_price_elasticity.R
	cd $(<D); Rscript $(<F)

results/tab/fuel_left.tex: scripts/04_analysis/02_shrimp_subsidy_effect.R
	cd $(<D); Rscript $(<F)

results/tab/fuel_right.tex: scripts/04_analysis/02_shrimp_subsidy_effect.R
	cd $(<D); Rscript $(<F)

results/tab/salience_test.tex: scripts/04_analysis/03_shrimp_salience_test.R
	cd $(<D); Rscript $(<F)

## SUPPLEMENTARY FIGURES #######################################################


## SUPPLEMENTARY TABLES ########################################################
results/tab/loglin_not_sub.tex: scripts/04_analysis/01_shrimp_price_elasticity.R
	cd $(<D); Rscript $(<F)

results/tab/supp_loglin_not_sub.tex: scripts/04_analysis/01_shrimp_price_elasticity.R
	cd $(<D); Rscript $(<F)

results/tab/hours_left.tex: scripts/04_analysis/02_shrimp_subsidy_effect.R
	cd $(<D); Rscript $(<F)

results/tab/hours_right.tex: scripts/04_analysis/02_shrimp_subsidy_effect.R
	cd $(<D); Rscript $(<F)

results/tab/supp_salience_test.tex: scripts/04_analysis/03_shrimp_salience_test.R
	cd $(<D); Rscript $(<F)