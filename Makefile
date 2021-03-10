data/all_fuel_clean.rds: scripts/01_cleaning/01_clean_fuel.R
	cd scripts/01_cleaning;Rscript 01_clean_fuel.R
	
data/landings_clean.rds: scripts/01_cleaning/02_clean_landings.R
	cd scripts/01_cleaning;Rscript 02_clean_landings.R