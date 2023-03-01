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
shrimp_panel <- readRDS(file = here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

## PROCESSING ##################################################################

# Filter for vessels that are always subsidized --------------------------------
always_sub <- shrimp_panel %>%
  group_by(eu) %>%
  summarize(n_obs = n(),
            n_sub = sum(treated)) %>%
  filter(n_sub == n_obs) %>%
  pull(eu)

# Subsidy dummy regression -----------------------------------------------------
# Log-linear of fuel consumtpuion and hours on a treatment dummy,
# separately ran for vessels to the right and to the left.

# The m# objects contain four models each. The first one is for vessels to the
# right and fuel, then right and hours, left and fuel, left and hours.
m1 <-
  feols(
    c(log(fuel_consumption_l), log(hours)) ~ treated,
    data = shrimp_panel,
    split = ~ left,
    cluster = "eu"
  )
m2 <-
  feols(c(log(fuel_consumption_l), log(hours)) ~ treated |
          eu,
        data = shrimp_panel,
        split = ~ left)
m3 <-
  feols(c(log(fuel_consumption_l), log(hours)) ~ treated |
          eu + year,
        data = shrimp_panel,
        split = ~ left)
m4 <-
  feols(c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp |
          eu + year,
        data = shrimp_panel,
        split = ~ left)
m5 <-
  feols(
    c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp |
      eu + year,
    data = shrimp_panel %>% filter(!eu %in% always_sub),
    split = ~ left
  )



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


## BUILD TABLES ################################################################

# Set up table defaults
renames <- c("treated" = "Subsidized",
             "dist" = "Distance from kink (1,000 L)",
             "total_hp" = "Total Capacity (HP)")

omits <- "Adj|IC|Lo|Ps|Std.|RMSE"

modelsummary(
  rmods,
  title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
  output = here("results", "tab", "fuel_right.tex"),
  stars = T,
  gof_omit = omits,
  coef_rename = renames,
  coef_omit = "(Intercept)"
)

modelsummary(
  lmods,
  title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
  output = here("results", "tab", "fuel_left.tex"),
  stars = T,
  gof_omit = omits,
  coef_rename = renames,
  coef_omit = "(Intercept)"
)


# Pooled regressions -----------------------------------------------------------
s1 <-
  feols(log(fuel_consumption_l) ~ treated |
          eu + year, data = shrimp_panel)
s2 <-
  feols(log(fuel_consumption_l) ~ treated + total_hp |
          eu + year, data = shrimp_panel)

# This table is not exported
grouped_models <- list(s1, s2, lmods[[3]], lmods[[4]], rmods[[3]], rmods[[4]]) %>%
  set_names(c("Pooled", "Pooled", "Left", "Left", "Right", "Right"))

rsq <- c("R2", map_dbl(grouped_models, ~round(
  r2(.x, type = "r2")
  , 3))) %>%
  t() %>%
  as.data.frame()

controls <- c("Controls", "", "X", "", "X", "", "X") %>%
  t() %>%
  as.data.frame() %>% 
  set_names(c("V1", "Pooled", "Pooled", "Left", "Left", "Right", "Right"))

expo <- c("exp(Beta)", map_dbl(grouped_models, ~round(exp(coef(.x)["treated"][[1]]), 3))) %>%
  t() %>%
  as.data.frame()

pct <- c("(exp(Beta) - 1) * 100)", map_dbl(grouped_models, ~round(((exp(coef(.x)["treated"][[1]]) - 1) * 100), 3))) %>%
  t() %>%
  as.data.frame()

extra <- rbind(rsq,
               controls,
               expo,
               pct)

modelsummary(
  grouped_models,
  title = 'Effect of MFSP on economic-unit level annual fuel consumption.
  The dependent variable is log(fuel consumption). The first two columns use all data.
  Columns 2 and 3 restrict the sample to economic units ``to the left" of their subsidy cap,
  columns 5 and 6 restrict the sample to economic units ``to the right" of the kink.
  Since this is a log-linear regression, we must exponentiate the coefficient of interest to interpret it.
  These values are shown at the bottom of the table. When subsidized, economic units
  ``to the left" increase their fishing effort by a factor of factor of 2.24, or 124\\% compared to similar unsubsidized fishers.
  We do not find evidence that economic units ``to the right" increase their effort.',
  output = here("results", "tab", "fuel_on_dummies_pooled_left_right.tex"),
  stars = T,
  gof_omit = omits,
  coef_rename = renames,
  coef_omit = "(Intercept)|total_hp",
  add_rows = extra,
  notes = "All standard errors are clustered at the economic-unit level"
)





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

modelsummary(
  hrmods,
  title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
  output = here("results", "tab", "hours_right.tex"),
  stars = T,
  gof_omit = omits,
  coef_rename = renames,
  coef_omit = "(Intercept)"
)

modelsummary(
  hlmods,
  title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
  output = here("results", "tab", "hours_left.tex"),
  stars = T,
  gof_omit = omits,
  coef_rename = renames,
  coef_omit = "(Intercept)"
)
