######################################################
#  clean_vessel_subsidy_panel  #
######################################################
#
# Clean CONAPESCA's version of the subsidy panel
#
# Data come from: https://datos.gob.mx/busca/dataset/energeticos-pesqueros-y-acuicolas
#
######################################################

## SET UP ######################################################################

# Load packages
library(tidyverse)

# Define function
fix_rnpa <- function(rnpa){
  lengths <- str_length(rnpa)
  missing <- pmax(8 - lengths, 0)
  zeroes <- map_chr(missing, ~paste(numeric(length = .x), collapse = ""))
  out <- paste0(zeroes, rnpa)
  return(out)
}


## PROCESSING ##################################################################
# Load data
vessel_subsidy_panel_raw <- read_csv(file.path(project_path, "data", "raw_data", "PADRON_DIESEL_MARINO.csv"))
  
vessel_subsidy_panel_clean <- vessel_subsidy_panel_raw %>% 
  janitor::clean_names() %>% 
  select(year = ejercicio_fiscal,
         program = programa,
         component = componente,
         admin_unit = unidad_administrativa,
         state = entidad_ventanilla_de_reinscripcion,
         budget_code = clave_presupuestaria,
         vessel_rnpa = rnp_activo,
         subsidy_cap_l = volumen_asignado_en_litros,
         price_subsidy_mxp_per_l = concepto_del_apoyo_en_mxn_por_litro) %>% 
  mutate(vessel_rnpa = fix_rnpa(vessel_rnpa))

## EXPORT ######################################################################

write_csv(x = vessel_subsidy_panel_clean,
          file = file.path(project_path, "data", "processed_data", "vessel_subsidy_panel.csv"))
