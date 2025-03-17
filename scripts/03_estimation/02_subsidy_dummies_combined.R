################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(fixest)
library(modelsummary)
library(tidyverse)

# Load functions ---------------------------------------------------------------
source(here("scripts", "00_setup.R"))

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(file = here("data", "estimation_panels", "shrimp_estimation_panel.rds")) %>% 
  mutate(tl = treated * left,
         tr = treated * right,
         nt = 1 * !treated)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
m1 <- feols(log(fuel_consumption_l) ~ treated | eu + year, data = shrimp_panel)
m2 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l | eu + year, data = shrimp_panel)
m3 <- feols(log(fuel_consumption_l) ~ tl + tr  + left| eu + year, data = shrimp_panel)
m4 <- feols(log(fuel_consumption_l) ~ tl + tr + left + total_hp + predicted_subsidy_cap_l | eu + year, data = shrimp_panel)

# Build tables -----------------------------------------------------------------
m <- list(m1, m2, m3, m4)
names(m) <- rep("log(L)", length(m))

renames <- c("treated" = "Subsidized",
             "tl" = "Subsidized X Left",
             "tr" = "Subsidized X Right")

controls <- c("Controls", "", "X", "", "X") %>% 
  t() %>% 
  as.data.frame()

## EXPORT ######################################################################

# Export table -----------------------------------------------------------------
modelsummary(models = m,
             title = "Effect of subsidy on fuel consumption.",
             # output = here("results", "tab", "subsidy_dummies_combined.tex"),
             stars = T,
             gof_omit = "IC|Std",
             coef_rename = renames,
             coef_omit = "total_hp|predicted_subsidy_cap_l",
             add_rows = controls, 
             notes = list("Controls are: Total engine power (HP) and imputed kink (L)",
                          "Standard errors are clustered at the economic-unit-level"))
