data/economic_unit_subsidy_panel.rds: data/all_fuel_clean.rds scripts/03_processing/01_create_economic_unit_subsidy_panel.R
	cd scripts;Rscript 00_setup.R
	cd scripts/03_processing;Rscript 01_create_economic_unit_subsidy_panel.R
	
data/all_fuel_clean.rds: scripts/01_cleaning/01_clean_fuel.R
	cd scripts;Rscript 00_setup.R
	cd scripts/01_cleaning;Rscript 01_clean_fuel.R
	
data/landings_clean.rds: scripts/01_cleaning/02_clean_landings.R
	cd scripts;Rscript 00_setup.R
	cd scripts/01_cleaning;Rscript 02_clean_landings.R
