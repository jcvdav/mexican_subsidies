all: main_figures main_tables supp_figures supp_tables dag

main_figures: content/figures/fig_elasticity.pdf content/figures/fig_extensive.pdf content/figures/fig_levels.pdf content/figures/fig_semi_elasticity.pdf content/figures/fig_enter.pdf content/figures/event_study_2020_reform_exit.pdf content/figures/fig_temporal_attribution.pdf content/figures/fig_spatial_attribution.pdf

main_tables: content/tables/tab_elasticity.tex content/tables/tab_main_entry_exit.tex content/tables/tab_reform.tex

supp_figures: content/figures/fig_elasticity_by_frequency.pdf content/figures/fig_ext_by_frequency.pdf content/figures/fig_levels_by_frequency.pdf content/figures/fig_semi_elasticity_by_frequency.pdf content/figures/fig_example_int_ext.pdf content/figures/fig_example_int_ext_diff.pdf content/figures/fig_example_int_ext_ref_map.pdf content/figures/fig_vms_pipeline_example.pdf content/figures/fig_vms_pipeline_example_1.pdf content/figures/fig_vms_pipeline_example_2.pdf content/figures/fig_vms_pipeline_example_3.pdf content/figures/fig_vms_pipeline_example_4.pdf content/figures/fig_vms_pipeline_example_5.pdf content/figures/fig_vms_pipeline_example_6.pdf content/figures/fig_adjustment_factor.pdf content/figures/fig_mdl_plot.pdf content/figures/fig_N_times_subsidized_histogram.pdf content/figures/fig_subsidy_vs_time.pdf content/figures/fig_total_hours_spatial_attribution.pdf content/figures/fig_subsidized_hours_spatial_attribution.pdf content/figures/fig_relative_spatial_attribution.pdf content/figures/fig_rank_spatial_attribution.pdf

supp_tables: content/tables/tab_elasticity_all_estimates.tex content/tables/tab_ext_all_estimates.tex content/tables/tab_levels_all_estimates.tex content/tables/tab_semi_elasticity_all_estimates.tex content/tables/tab_reform_p_exit.tex

.PHONY: dag

dag: makefile-dag.png workflow.png

# draw makefile dag
makefile-dag.png: Makefile
	make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o makefile-dag.png

workflow.png: Makefile
	LANG=C make -np | python3 make_p_to_json.py | python3 json_to_dot.py | dot -Tpng >| workflow.png

## MAIN FIGURES ################################################################

# Entry/Exit figures
content/figures/fig_extensive.pdf: scripts/05_content/01_entry_exit/make_fig_semi_elasticity.R
	cd $(<D); Rscript $(<F)

content/figures/fig_levels.pdf: scripts/05_content/01_entry_exit/make_fig_semi_elasticity.R
	cd $(<D); Rscript $(<F)

content/figures/fig_semi_elasticity.pdf: scripts/05_content/01_entry_exit/make_fig_semi_elasticity.R
	cd $(<D); Rscript $(<F)

content/figures/fig_enter.pdf: scripts/05_content/make_fig_enter.R
	cd $(<D); Rscript $(<F)

# Elasticity figures
content/figures/fig_elasticity.pdf: scripts/05_content/02_change_in_subsidy/make_fig_elasticity.R
	cd $(<D); Rscript $(<F)

# Reform figures
content/figures/event_study_2020_reform_exit.pdf: scripts/05_content/03_reform/make_fig_event_study.R
	cd $(<D); Rscript $(<F)

# Implications figures
content/figures/fig_temporal_attribution.pdf: scripts/04_implications/01_temporal.R
	cd $(<D); Rscript $(<F)

content/figures/fig_spatial_attribution.pdf: scripts/04_implications/02_spatial.R
	cd $(<D); Rscript $(<F)

## SUPPLEMENTARY FIGURES #######################################################

# Entry/Exit by frequency
content/figures/fig_ext_by_frequency.pdf: scripts/05_content/01_entry_exit/make_fig_entry_exit_by_frequency.R
	cd $(<D); Rscript $(<F)

content/figures/fig_levels_by_frequency.pdf: scripts/05_content/01_entry_exit/make_fig_entry_exit_by_frequency.R
	cd $(<D); Rscript $(<F)

content/figures/fig_semi_elasticity_by_frequency.pdf: scripts/05_content/01_entry_exit/make_fig_entry_exit_by_frequency.R
	cd $(<D); Rscript $(<F)

# Elasticity by frequency
content/figures/fig_elasticity_by_frequency.pdf: scripts/05_content/02_change_in_subsidy/make_fig_elasticity_by_frequency.R
	cd $(<D); Rscript $(<F)

# Example intensive/extensive margin
content/figures/fig_example_int_ext.pdf: scripts/05_content/make_fig_example_int_ext.R
	cd $(<D); Rscript $(<F)

content/figures/fig_example_int_ext_diff.pdf: scripts/05_content/make_fig_example_int_ext.R
	cd $(<D); Rscript $(<F)

content/figures/fig_example_int_ext_ref_map.pdf: scripts/05_content/make_fig_example_int_ext.R
	cd $(<D); Rscript $(<F)

# VMS pipeline examples
content/figures/fig_vms_pipeline_example.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_1.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_2.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_3.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_4.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_5.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

content/figures/fig_vms_pipeline_example_6.pdf: scripts/05_content/make_fig_vms_pipeline_examples.R
	cd $(<D); Rscript $(<F)

# Other supplementary figures
content/figures/fig_adjustment_factor.pdf: scripts/05_content/make_fig_adjustment_factor.R
	cd $(<D); Rscript $(<F)

content/figures/fig_mdl_plot.pdf: scripts/05_content/make_fig_mdl_plot.R
	cd $(<D); Rscript $(<F)

content/figures/fig_N_times_subsidized_histogram.pdf: scripts/05_content/make_fig_N_times_subsidized_histogram.R
	cd $(<D); Rscript $(<F)

content/figures/fig_subsidy_vs_time.pdf: scripts/content/fig_subsidy_vs_time.R
	cd $(<D); Rscript $(<F)

# Spatial attribution supplementary figures
content/figures/fig_total_hours_spatial_attribution.pdf: scripts/04_implications/02_spatial.R
	cd $(<D); Rscript $(<F)

content/figures/fig_subsidized_hours_spatial_attribution.pdf: scripts/04_implications/02_spatial.R
	cd $(<D); Rscript $(<F)

content/figures/fig_relative_spatial_attribution.pdf: scripts/04_implications/02_spatial.R
	cd $(<D); Rscript $(<F)

content/figures/fig_rank_spatial_attribution.pdf: scripts/04_implications/02_spatial.R
	cd $(<D); Rscript $(<F)

## MAIN TABLES #################################################################

content/tables/tab_elasticity.tex: scripts/05_content/02_change_in_subsidy/make_tab_elasticity.R
	cd $(<D); Rscript $(<F)

content/tables/tab_main_entry_exit.tex: scripts/05_content/01_entry_exit/make_tab_entry_exit.R
	cd $(<D); Rscript $(<F)

content/tables/tab_reform.tex: scripts/05_content/03_reform/make_tab_reform.R
	cd $(<D); Rscript $(<F)

## SUPPLEMENTARY TABLES ########################################################

content/tables/tab_elasticity_all_estimates.tex: scripts/05_content/02_change_in_subsidy/make_tab_elasticity.R
	cd $(<D); Rscript $(<F)

content/tables/tab_ext_all_estimates.tex: scripts/05_content/01_entry_exit/make_tab_entry_exit.R
	cd $(<D); Rscript $(<F)

content/tables/tab_levels_all_estimates.tex: scripts/05_content/01_entry_exit/make_tab_entry_exit.R
	cd $(<D); Rscript $(<F)

content/tables/tab_semi_elasticity_all_estimates.tex: scripts/05_content/01_entry_exit/make_tab_entry_exit.R
	cd $(<D); Rscript $(<F)

content/tables/tab_reform_p_exit.tex: scripts/05_content/03_reform/make_tab_reform.R
	cd $(<D); Rscript $(<F)
