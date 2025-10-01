################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Load data -------------------------------------------------------------------
modernization <- read_csv("data/raw/B2_modernizacion.csv")

# PROCESSING ###################################################################

## Filter data -----------------------------------------------------------------
# I will retain vessels that received a new industrial vessel and that fished
# for shrimp between 2011 and 2019
modernized <- modernization |> 
  filter(concepto_apoyo == "EMBARCACION (MAYOR)",
         pesqueria_estandar == "CAMARONES",
         between(año, 2011, 2019)) |> 
  # Standardize the EU rnpa so they are 10-digits long and we can match them
  mutate(rnpa = ifelse(str_length(rnpa) == 10, rnpa, paste0("0", rnpa)),
         modernized = 1) |> 
  group_by(rnpa) |> 
  summarize(year = min(año)) |> 
  drop_na() |> 
  mutate(modernized = 1)

# EXPORT #######################################################################

## Export it -------------------------------------------------------------------
write_rds(x = modernized,
          file = here("data/processed/economic_unit_modernized_vessels.rds"))
  
