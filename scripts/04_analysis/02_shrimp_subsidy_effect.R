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

always_sub <- shrimp_panel %>% 
  group_by(eu) %>% 
  summarize(n_obs = n(),
            n_sub = sum(treated)) %>% 
  filter(n_sub == n_obs) %>% 
  pull(eu)


## ANALYSIS ####################################################################
# Subsidy dummy regression
# Log-linear of fuel consumtpuion and hours on a treatment dummy,
# separately ran for vessels to the right and to the left.

# The m# objects contain four models each. First one is for vessels to the right and fuel, then right and hours, left ,, and fuel, left and hours.
m1 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated, data = shrimp_panel, split = ~left, cluster = "eu")
m2 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated | eu, data = shrimp_panel, split = ~left)
m3 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated | eu + year, data = shrimp_panel, split = ~left)
m4 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp | eu + year, data = shrimp_panel, split = ~left)
m5 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp | eu + year, data = shrimp_panel %>% filter(!eu %in% always_sub), split = ~left)



## Extract to the right and fuel
rmods <- list(m1[[1]],
              m2[[1]],
              m3[[1]],
              m4[[1]],
              m5[[1]])
names(rmods) <- rep("log(L)", length(rmods))

## Extract to the left and fuel
lmods <- list(m1[[3]],
              m2[[3]],
              m3[[3]],
              m4[[3]],
              m5[[3]])
names(lmods) <- rep("log(L)", length(lmods))


# Now we regress only on subsidy status
s1 <- feols(log(fuel_consumption_l) ~ treated | eu + year, data = shrimp_panel)
s2 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l | eu + year, data = shrimp_panel)


## BUILD TABLES ################################################################

# Set up table defaults
renames <- c("treated" = "Subsidized",
             "dist" = "Distance from kink (1,000 L)",
             "total_hp" = "Total Capacity (HP)")

omits <- "Adj|IC|Lo|Ps|Std."

modelsummary(rmods,
             title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_right.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames,
             coef_omit = "(Intercept)")

modelsummary(lmods, 
             title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames,
             coef_omit = "(Intercept)")

# This table is not exported
list(s1, s2, lmods[[3]], lmods[[4]], rmods[[3]], rmods[[4]]) %>%
  set_names(c("Naive", "Naive", "Left", "Left", "Right", "Right")) %>% 
  modelsummary(stars = T,
               gof_omit = omits,
               coef_rename = renames,
               coef_omit = "(Intercept)|total_hp|predicted",
               add_rows = c("Controls", "", "X", "", "X", "", "X") %>% 
                 t() %>% 
                 as.data.frame())



## BUILD SUPPLEMENTARY TABLES ##################################################
## Extract to the right and hours
hrmods <- list(m1[[2]],
               m2[[2]],
               m3[[2]],
               m4[[2]],
               m5[[2]])
names(hrmods) <- rep("log(H)", length(hrmods))

## Extract to the left and hours
hlmods <- list(m1[[4]],
               m2[[4]],
               m3[[4]],
               m4[[4]],
               m5[[4]])
names(hlmods) <- rep("log(H)", length(hlmods))

modelsummary(hrmods,
             title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_right.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames,
             coef_omit = "(Intercept)")

modelsummary(hlmods,
             title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_left.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames,
             coef_omit = "(Intercept)")
