######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(fixest)
library(regrrr)
library(modelsummary)
library(tidyverse)

source(here("scripts", "00_setup.R"))

## Load data ###################################################################

shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds"))

always_sub <- shrimp_panel %>% 
  group_by(eu) %>% 
  summarize(n_obs = n(),
            n_sub = sum(treated)) %>% 
  filter(n_sub == n_obs) %>% 
  pull(eu)


# DEFINE FUNCTIONS
my_test <- function(model) {
  dif <- model$coefficients[1] - model$coefficients[2]
  result <- test_coef_equality(model, names(model$coefficients)[1], names(model$coefficients)[2],
                               v = sandwich::vcovHAC(model))
  paste0(round(dif, 3), " (", round(result, 3),  ")")
}

omits <- "Adj|IC|Lo|Ps|RMSE"


## ANALYSIS ####################################################################
s0 <- feols(log(fuel_consumption_l) ~ ph + delta, shrimp_panel, subset = ~left == 1, cluster = "eu")
s1 <- feols(log(fuel_consumption_l) ~ ph + delta | eu, shrimp_panel, subset = ~left == 1)
s2 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp + nino34_m | eu, shrimp_panel, subset = ~left == 1)
s3 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp + nino34_m | eu, data = shrimp_panel %>% filter(!eu %in% always_sub), subset = ~left == 1)
s4 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp + nino34_m | eu, shrimp_panel, subset = ~left == 1 & year >= 2017)
s5 <- feols(log(fuel_consumption_l) ~ p_stat + delta + total_hp + nino34_m | eu, shrimp_panel, subset = ~left == 1 & year >= 2017)
s6 <- feols(log(fuel_consumption_l) ~ p_stat + delta + total_hp | eu + year, shrimp_panel, subset = ~left == 1 & year >= 2017)

s_short <- list(s0,
                s1,
                s2,
                s3)
names(s_short) <- rep("log(L)", length(s_short))

Htest_short <- c("H0: delta = alpha", map_chr(s_short[1:4], my_test)) %>%
  t() %>%
  as.data.frame() %>% 
  set_names(nm = paste0("V", 1:5))

modelsummary(s_short,
             output = here("results", "tab", "salience_test.tex"),
             title = "Salience test on vessels to the left of the kink.
             The last column excludes vessels that were always subsidized.",
             stars = T,
             add_rows = Htest_short,
             gof_omit = omits,
             coef_rename = c("ph" = "P",
                             "delta" = "Delta",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "Nino 3.4"),
             coef_omit = "(Intercept)")



## SUPPLEMENTARY ###############################################################

s <- list(s2,
          s4,
          s5,
          s6)
names(s) <- rep("log(L)", length(s))


state_p <- c("Prices", rep("National", 1), rep("State", 3)) %>% 
  t() %>% 
  as.data.frame()

Htest <- c("H0: delta = alpha", map_chr(s[1:4], my_test)) %>%
  t() %>%
  as.data.frame() %>% 
  set_names(nm = paste0("V", 1:5)) %>% 
  rbind(state_p)


modelsummary(s,
             output = here("results", "tab", "supp_salience_test.tex"),
             title = "Salience test on vessels to the left.
             Columns 1-2 use mean national fuel prices, columns 3-4 use mean state-level prices.
             Column 1 is our full specification in the main text, columns 2 - 4 use 2017 - 2019 data.",
             stars = T,
             add_rows = Htest,
             gof_omit = omits,
             coef_rename = c("ph" = "P",
                             "p_stat" = "P",
                             "delta" = "Delta",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "Nino 3.4"),
             coef_omit = "(Intercept)")
