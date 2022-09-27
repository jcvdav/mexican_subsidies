######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(fixest)
library(modelsummary)
library(tidyverse)

source(here("scripts", "00_setup.R"))

## Load data ###################################################################

shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds"))


## ANALYSIS ####################################################################

# Price elasticity of demand on never subsidized
# e(p) = dQ/Q / dP/P
#Q = quantity of demanded good
#P = price of demanded good

#log-log
nev0 <- feols(log(fuel_consumption_l) ~ log(p), data = shrimp_panel, subset = ~!treated, cluster = "eu")
nev1 <- feols(log(fuel_consumption_l) ~ log(p) | eu, data = shrimp_panel, subset = ~!treated)
nev2 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp + nino34_m | eu, data = shrimp_panel, subset = ~!treated)
nev3 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp + nino34_m + year | eu, data = shrimp_panel, subset = ~!treated)
nev4 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp + nino34_m | eu, shrimp_panel, subset = ~!treated & year >= 2017)
nev5 <- feols(log(fuel_consumption_l) ~ log(p_stat) + total_hp + nino34_m | eu, data = shrimp_panel, subset = ~!treated & year >= 2017)
nev6 <- feols(log(fuel_consumption_l) ~ log(p_stat) + total_hp | eu + year, data = shrimp_panel, subset = ~!treated & year >= 2017)


nev_short <- list(nev0,
                  nev1,
                  nev2,
                  nev3)
names(nev_short) <- rep("log(L)", length(nev_short))

nino <- c("Controls", "", "", "X", "X") %>% 
  t() %>% 
  as.data.frame()

year <- c("Time trend", "", "", "", "X") %>% 
  t() %>% 
  as.data.frame()

controls <- rbind(nino, year)


modelsummary(nev_short,
             output = here("results", "tab", "not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",
             title = "Price elasticity of demand for vessels that are not subsidized.",
             coef_rename = c("log(p)" = "log(Fuel price [MXP / L])"),
             coef_omit = "(Intercept)|total_hp|nino|year",
             add_rows = controls)

# Appendix table

nev <- list(nev2,
            nev4,
            nev5,
            nev6)
names(nev) <- rep("log(L)", length(nev))

state_p <- c("Prices", rep("National", 2), rep("State", 2)) %>% 
  t() %>% 
  as.data.frame()

modelsummary(nev, 
             output = here("results", "tab", "supp_not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",
             add_rows = state_p,
             title = "Price elasticity of demand for vessels that are not subsidized.
             Columns 1-2 use mean national fuel prices, columns 3-5 use mean state-level prices.
             Column 1 is our full specification in the main text.
             Columns 2 - 5 use 2017 - 2019 data.",
             coef_rename = c("log(p)" = "log(Fuel price [MXP / L])",
                             "log(p_stat)" = "log(Fuel price [MXP / L])",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "NINO 3.4"))

#log-linear

loglin_nev0 <- feols(log(fuel_consumption_l) ~ p, data = shrimp_panel, subset = ~!treated, cluster = "eu")
loglin_nev1 <- feols(log(fuel_consumption_l) ~ p | eu, data = shrimp_panel, subset = ~!treated)
loglin_nev2 <- feols(log(fuel_consumption_l) ~ p + total_hp + nino34_m | eu, data = shrimp_panel, subset = ~!treated)
loglin_nev3 <- feols(log(fuel_consumption_l) ~ p + total_hp + nino34_m + year | eu, data = shrimp_panel, subset = ~!treated)
loglin_nev4 <- feols(log(fuel_consumption_l) ~ p + total_hp + nino34_m | eu, shrimp_panel, subset = ~!treated & year >= 2017)
loglin_nev5 <- feols(log(fuel_consumption_l) ~ p_stat + total_hp + nino34_m | eu, data = shrimp_panel, subset = ~!treated & year >= 2017)
loglin_nev6 <- feols(log(fuel_consumption_l) ~ p_stat + total_hp | eu + year, data = shrimp_panel, subset = ~!treated & year >= 2017)

loglin_nev_short <- list(loglin_nev0,
                         loglin_nev1,
                         loglin_nev2,
                         loglin_nev3)

names(loglin_nev_short) <- rep("log(L)", length(loglin_nev_short))

modelsummary(loglin_nev_short, 
             output = here("results", "tab", "loglin_not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",
             title = "Price semi-elasticity of demand for vessels that are not subsidized.",
             coef_rename = c("p" = "Fuel price (MXN / L)",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "NINO 3.4",
                             "year" = "Year"),
             coef_omit = "(Intercept)|total_hp|nino|year",
             add_rows = controls)


# Appendix
loglin_nev <- list(loglin_nev2,
                   loglin_nev4,
                   loglin_nev5,
                   loglin_nev6)

names(loglin_nev) <- rep("log(L)", length(loglin_nev))

modelsummary(loglin_nev, 
             output = here("results", "tab", "supp_loglin_not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             add_rows = state_p,
             title = "Price semi-elasticity of demand for vessels that are not subsidized.
             Columns 1-2 use mean national fuel prices, columns 3-4 use mean state-level prices.
             Column 1 is our full specification in the main text, columns 2 - 4 use 2017 - 2019 data.",
             coef_rename = c("p" = "Fuel price (MXN / L)",
                             "p_stat" = "Fuel price (MXN / L)",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "NINO 3.4"))
