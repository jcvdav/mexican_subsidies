################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# The comented portion provides a reference for how to query the data-base
# from within R, but requries authentication. I query the data and export it
# directly so that it can be read as an RDS file.
#
# The code then counts the number of vessels for wich engine power was imputed
# and then proceeds to build a plot relating engine power to length overall
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Query data from GoogleBigQuery -----------------------------------------------
# Packages to connect
# library(DBI)
# library(bigrquery)
#
# # Authenticate using local token 
# bq_auth("juancarlos@ucsb.edu")
# 
# # Establish a connection to BigQuery
# mex_fisheries <- dbConnect(
#   bigquery(),
#   project = "emlab-gcp",
#   dataset = "mex_fisheries",
#   billing = "emlab-gcp",
#   use_legacy_sql = FALSE,
#   allowLargeResults = TRUE
# )
# 
# # Query the vessel registry
# shrimp_registry <- tbl(mex_fisheries, "vessel_info_v_20220912") %>% 
#   group_by(vessel_rnpa) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n == 1,
#          fleet == "large scale",
#          shrimp == 1,
#          tuna == 0,
#          sardine == 0,
#          others == 0) %>% 
#   collect()
#
# Export as an RDS to share with PEW
# saveRDS(object = shrimp_registry,
#         file = "shrimp_registry.rds")

# Load data --------------------------------------------------------------------
shrimp_registry <- readRDS(file = "shrimp_registry.rds")

# How many vessels have an imputed engine power (because it was missing)?
sum(as.logical(shrimp_registry$imputed_engine_power) == TRUE) # 53

## VISUALIZE ###################################################################

# Plot length overall and engine power (log-scales) ----------------------------
p <- ggplot(shrimp_registry,
            aes(x = log(vessel_length_m),
                y = log(engine_power_hp),
                fill = imputed_engine_power)) +
  geom_point() +
  labs(x = "log(Length Overall [m])",
       y = "log(Engine Power [HP])",
       fill = "Imputed") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

p

# EXPORT #######################################################################
# Commented out for PEW
# ggsave(plot = p,
#        filename = here::here("results", "img", "imputed_engine_power.pdf"),
#        width = 6,
#        height = 4.5)

# END OF SCRIPT ################################################################