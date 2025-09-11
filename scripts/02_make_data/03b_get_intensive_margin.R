################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Combines the panel on subsidy allocations with the
# one on activity
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

## Read data -------------------------------------------------------------------
# Vessel activity
vessel_activity_raw <- readRDS(
  file = here("data", "processed", "vms_annual_vessel_activity.rds"))

## PROCESSING ##################################################################
# For each economic unit, identify the fishing capacity (# vessels and total hp)
# owned in the first year they are active
capacity_by_eu <- vessel_activity_raw |> 
  group_by(eu_rnpa) |> 
  slice_min(year) |> 
  group_by(eu_rnpa) |> 
  summarize(n_vessels = n_distinct(vessel_rnpa),
            total_hp = sum(main_engine_power_hp)) |> 
  arrange(desc(n_vessels))

# Test that the number of EUs in the capacity table matches the number of EUs in data
test1 <- length(unique(capacity_by_eu$eu_rnpa)) == length(unique(vessel_activity_raw$eu_rnpa))
cat("Testing for sample number of EUs in both data sets")
if(test1){
  cat("Pass: The number of EUs is", length(unique(capacity_by_eu$eu_rnpa)), "in both")
} else {
  cat("Number of EUS don't match, with", length(unique(capacity_by_eu$eu_rnpa)),
      "and", length(unique(vessel_activity_raw$eu_rnpa)))
}


# Summarize vessel activity by economic unit -----------------------------------
eu_activity_panel <- vessel_activity_raw |>
  group_by(year, state, eu_rnpa, fleet, fuel_type) |> 
  summarize(hours = sum(hours, na.rm = T),
            .groups = "drop") |> 
  left_join(capacity_by_eu, by = "eu_rnpa") |> 
  mutate(state = str_to_sentence(state)) |> 
  mutate(region = case_when(state %in% c("Baja california", "Baja california sur", "Sinaloa", "Sonora", "Nayarit") ~ "GoC",
                            state %in% c("Campeche", "Tamaulipas", "Veracruz", "Quintana roo", "Yucatan") ~ "GoM",
                            state %in% c("Chiapas", "Oaxaca") ~ "Pacific")) 

# Test that the number of EUs in the capacity table matches the number of EUs in data
test2 <- length(unique(eu_activity_panel$eu_rnpa)) == length(unique(vessel_activity_raw$eu_rnpa))
cat("Testing for sample number of EUs in both data sets")
if(test1){
  cat("Pass: The number of EUs is", length(unique(eu_activity_panel$eu_rnpa)), "in both")
} else {
  cat("Number of EUS don't match, with", length(unique(eu_activity_panel$eu_rnpa)),
      "and", length(unique(vessel_activity_raw$eu_rnpa)))
}

## EXPORT ######################################################################
saveRDS(object = eu_activity_panel,
        file = here("data", "processed", "intensive_margin.rds"))


