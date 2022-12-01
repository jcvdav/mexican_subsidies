################################################################################
# Supplementary code for "Fuel Subsidies and Overfishing (for Pew)
################################################################################
#
# Christopher Costello, Olivier Deschenes, Katherine Millage, and Juan Carlos
# Villaseñor-Derbez
# 
# For questions about the code, contact JCVD at juancvd@stanford.edu
#
# Description ------------------------------------------------------------------
# The code first sets up the environment with the required packages. It then
# loads the data and proceeds to perform three analysis:
#
# Part 1 estimates the effect of fuel subsidies on fuel consumption
# Part 2 estimates salience
# Part 3 estimates the price elasticity of demand for fuel
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(regrrr)                                                                 # Hypothesis testing of regression coefficients
library(fixest)                                                                 # For fast fixed effects estimation
library(modelsummary)                                                           # To build tables
library(tidyverse)                                                              # General data management

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS("supplementary_data.rds")

## ANALYSIS ####################################################################

# ==============================================================================
# Part 1 - What is the effect of fuel subsidies on fuel consumption?
# ==============================================================================

# Naive dummy regression -------------------------------------------------------
# We begin with a naive regression of log fuel consumption on subsidy status,
# without distinguiwhing weather a vesse lies to the left or to the right of the
# subsidy cap.

# Two-way fixed effects regression
s1 <- feols(
  log(fuel_consumption_l) ~ treated |                                           # Regress log(fuel) on subsidy status dummy
    eu + year,                                                                  # With fixed effects by year end EU
  data = shrimp_panel                                                           # Specify data source
)

# Two-way fixed effects regression with controls
s2 <-feols(
  log(fuel_consumption_l) ~ treated + total_hp |                                # Regress log(fuel) on subsidy status dummy, and add controls
    eu + year,                                                                  # With fixed effects by year end EU
  data = shrimp_panel                                                           # Specify data source
)


# Dividing sample by left or right of kink -------------------------------------
# We now run the same specification as above, but run it for vessels to the
# right of the kink (called "Sample: 0" in the model object) and to the left
# (called "Sample: 1" in the model object)

# The m# objects contain two models each. The first one is for vessels to the
# right and and the secon done is for vessels to the left

# Two-way fixed effects regression
m1 <- feols(
  fml = log(fuel_consumption_l) ~ treated |                                     # Regress log(fuel) on subsidy status dummy
    eu + year,                                                                  # With fixed effects by year end EU
  data = shrimp_panel,                                                          # Specify data source
  split = ~ left                                                                # Specify the column name that indicates wheather a vessel is right or left of the kink
)
# Two-way fixed effects regression with controls
m2 <- feols(
  fml = log(fuel_consumption_l) ~ treated + total_hp|                           # Regress log(fuel) on subsidy status dummy and control variable of total horsepower
    eu + year,                                                                  # With fixed effects by year end EU
  data = shrimp_panel,                                                          # Specify data source
  split = ~ left                                                                # Specify the column name that indicates wheather a vessel is right or left of the kink
)

## BUILD TABLES ################################################################

# Put model objects together in a list -----------------------------------------
grouped_models <-
  list(s1, s2, m1[[2]], m2[[2]], m1[[1]], m2[[1]]) %>%
  set_names(c("Pooled", "Pooled", "Left", "Left", "Right", "Right"))


# Create extra rows ------------------------------------------------------------
# A row that indicates the use of controls for each model
controls <- c("Controls", "", "X", "", "X", "", "X") %>%
  t() %>%
  as.data.frame() %>%
  set_names(c("V1", "Pooled", "Pooled", "Left", "Left", "Right", "Right"))

# A row that contains the exponentiated coefficient, round to the third digit
expo <-
  c("Relative change",
    map_dbl(
      grouped_models, ~ round(exp(coef(.x)["treated"][[1]]), 3))) %>%
  t() %>%
  as.data.frame()

# A row that contains the % change, round to the third digit
pct <-
  c("\\% Change", map_dbl(grouped_models, ~ round(((
    exp(coef(.x)["treated"][[1]]) - 1
  ) * 100), 3))) %>%
  t() %>%
  as.data.frame()

# Combine all rows together
extra <- rbind(controls,
               expo,
               pct)

# Make table -------------------------------------------------------------------
modelsummary(
  grouped_models,
  title = 'Effect of MFSP on economic-unit level annual fuel consumption.
  The dependent variable is log(fuel consumption). The first two columns use all
  data. Columns 2 and 3 restrict the sample to economic units ``to the left" of
  their subsidy cap, columns 5 and 6 restrict the sample to economic units
  ``to the right" of the kink. Since this is a log-linear regression, we must
  exponentiate the coefficient of interest to interpret it. These values are
  shown at the bottom of the table. When subsidized, economic units ``to the
  left" increase their fishing effort by a factor of factor of 2.24, or 124\\%
  compared to similar unsubsidized fishers (Column 4). We do not find evidence
  that economic units ``to the right" increase their effort.',
  stars = T,                                                                    # Add asteriscs indicating significance
  gof_omit = "Adj|IC|Lo|Ps|Std.|With",                                          # Ommit boilerplate goodness-of-fit measures
  coef_rename = c("treated" = "Subsidized"),                                    # Rename coefficients
  coef_omit = "(Intercept)|total_hp",                                           # Don't display these coefficients (clutter)
  add_rows = extra,                                                             # Add rows indicating controls, and changes
  notes = list("All standard errors are clustered at the economic-unit level.",
               "Controls are total engine capacity by vessel-year.",
               "Relative change is calculated as exp(Beta); Percent change is
               calculated as (exp(Beta) - 1) * 100).")
)


# ==============================================================================
# Part 2 - Estimate salience
# ==============================================================================

# We will regress log fuel consumption on the price (ph) and a variable called
# delta, which takes the value of 0 (for unsubsidized vessels) or -2 for vessels
# that are subsidized. All regressions will have fixed effects by economic unit.
# Since the test above (subsidy effect on fuel consumption) shows that only 
# vessels to the left of the kink respond to a subsidy, we restrict the sample
# to vessels to the left of the kink

# Regress consumption on market price
s1 <- feols(
  log(fuel_consumption_l) ~ ph |
    eu,
  data = shrimp_panel,
  subset = ~ left == 1                                                          # Restrict sample to vessels to the left
)

# Regress consumption on market price and delta
s2 <- feols(
  log(fuel_consumption_l) ~ ph + delta |
    eu,
  data = shrimp_panel,
  subset = ~ left == 1                                                          # Restrict sample to vessels to the left
)

# Regress consumption on market price and delta, adding controls
s3 <- feols(
  log(fuel_consumption_l) ~ ph + delta + nino34_m + total_hp |
    eu,
  data = shrimp_panel,
  subset = ~ left == 1                                                          # Restrict sample to vessels to the left
)

# Regress consumption on market price and delta, adding controls and a time trend
s4 <- feols(
  log(fuel_consumption_l) ~ ph + delta + nino34_m + total_hp + year |
    eu,
  data = shrimp_panel,
  subset = ~ left == 1                                                          # Restrict sample to vessels to the left
)


## BUILD TABLE #################################################################
# Combine all model objects into a list ----------------------------------------
s_short <- list(s1,
                s2,
                s3,
                s4)

names(s_short) <- rep("log(L)", length(s_short))                                # Add names to the list


# Add rows to the table --------------------------------------------------------
# Row of controls
controls <- c("Controls", "", "", "X", "X") %>%
  t() %>%
  as.data.frame()

# Row of time trend
year <- c("Time trend", "", "", "", "X") %>%
  t() %>%
  as.data.frame()



# We also want to test hhether the coefficient on price is different from the
# coefficient on delta (salience). We therefore build a function that performs a
# hypothesis test.
# Define hypiothesis testing function
my_test <- function(model) {
  # Calculate the difference between coefficients
  dif <- model$coefficients[1] - model$coefficients[2]
  
  # Perform a hypothesis test
  result <- regrrr::test_coef_equality(
    model = model,
    var1.name = names(model$coefficients)[1],
    var2.name = names(model$coefficients)[2],
    v = sandwich::vcovHAC(model))
  
  # Nicely format the result keeping 3 digits
  result <- paste0(round(dif, 3), " (", round(result, 3),  ")")
  
  # The first model doesn't have delta, so we can't test. This captures that
  if(is.na(dif)){
    return("-")
  } else {
    return(result)
  }
  
}

# Call that functipon to create one last row
Htest_short <- c("H0: delta = P", map_chr(s_short, my_test)) %>%
  t() %>%
  as.data.frame() %>%
  set_names(nm = paste0("V", 1:5))

# Combine all extra rows together
rows <- rbind(controls,
              year,
              Htest_short)

# Make the table ---------------------------------------------------------------
modelsummary(
  s_short,
  title = "Salience test on vessels to the left of the kink.",
  stars = T,                                                                    # Add asterisks for significance
  gof_omit = "Adj|IC|Lo|Ps|Std.|With",                                          # Ommit boilerplate goodness-of-fit measures
  coef_rename = c("ph" = "p",                                                   # Rename coefficients
                  "delta" = "delta"),
  coef_omit = "(Intercept)|year|nino|total_hp",                                 # Ommit other coeffiecients to reduce clutter
  add_rows = rows,                                                              # Add extra rows
  notes = list(
    "Controls are: Total engine capacity and annual mean of NINO3.4 index",
    "Standard errors are clustered at the economic-unit-level"
  )
)



# ==============================================================================
# Part 3 - Price elasticity of demand
# ==============================================================================

# We now proceed to estimate the price elasticity of demand. We will remove
# subsidized vessels from the sample and retain vessels that are not subsidized
# so that we can ask, who does fuel consumption change when the market price
# changes?
# 
# We will first perform log-log regressions to estimate leasticities, and then
# we will perform log-linear regressions to estimate semi-elasticities that we 
# can compare to thelog-linear regressions in the previous two sections

# log-log models to estimate elasticity ----------------------------------------
# Begin with regression of consumption on prices
loglog1 <- feols(
  log(fuel_consumption_l) ~ log(ph),                                            # Regress log(fuel consumption) on log(price)
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated,                                                          # Filter for vessels that are not subsidized
  cluster = "eu"                                                                # Specify to cluster standard errors at the EU level
)

# Regression of consumption on prices, adding economic-unit fixed effects
loglog2 <- feols(
  log(fuel_consumption_l) ~ log(ph) |                                           # Regress log(fuel consumption) on log(price)
    eu,                                                                         # Add economic-unit fixed effects
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated                                                           # Filter for vessels that are not subsidized
)

# Add controls
loglog3 <- feols(
  log(fuel_consumption_l) ~ log(ph) + total_hp + nino34_m |                     # Regress log(fuel consumption) on log(price)
    eu,                                                                         # Add economic-unit fixed effects
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated                                                           # Filter for vessels that are not subsidized
)


# log-linear models to estimate semi-elasticity --------------------------------
# Begin with regression of consumption on prices
loglin1 <- feols(
  log(fuel_consumption_l) ~ ph,                                                 # Regress log(fuel consumption) on price
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated,                                                          # Filter for vessels that are not subsidized
  cluster = "eu"                                                                # Specify to cluster standard errors at the EU level
)

# Regression of consumption on prices, adding economic-unit fixed effects
loglin2 <- feols(
  log(fuel_consumption_l) ~ ph |                                                # Regress log(fuel consumption) on price
    eu,                                                                         # Add economic-unit fixed effects
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated                                                           # Filter for vessels that are not subsidized
)

# Add controls
loglin3 <- feols(
  log(fuel_consumption_l) ~ ph + total_hp + nino34_m |                          # Regress log(fuel consumption) on lprice
    eu,                                                                         # Add economic-unit fixed effects
  data = shrimp_panel,                                                          # Specify data source
  subset = ~ !treated                                                           # Filter for vessels that are not subsidized
)

## BUILD TABLES ################################################################

# Combine models into a single list object -------------------------------------
loglog <- list(loglog1,
               loglog2,
               loglog3)

loglin <- list(loglin1,
               loglin2,
               loglin3)

names(loglog) <- rep("log(L)", length(loglog))                                  # Assign names
names(loglin) <- rep("log(L)", length(loglin))                                  # Assign names


# Build additional rows --------------------------------------------------------
# Rows indicating controls
controls <- c("Controls", "", "", "X") %>%
  t() %>%
  as.data.frame()

# Produce panel A of table 3 ---------------------------------------------------
modelsummary(
  loglog,
  title = "Price elasticity of demand for vessels that are not subsidized.
  Panel A shows the results for log-log regressions and coefficients are
  elasticities. Panel B shows the results for log-linear regressions where
  coefficients are interpreted as semi-elasticities.",
  stars = T,                                                                    # Add stars of significance
  gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",                                            # Remove redundant goodness of fit measures
  coef_rename = c("log(ph)" = "log(Fuel price [MXP / L])"),                     # Rename coefficients of interest
  coef_omit = "(Intercept)|total_hp|nino|year",                                 # Remove coefficients to reduce clutter
  add_rows = controls                                                           # Add extra rows
)

# Produce panel B of table 3 ---------------------------------------------------
modelsummary(
  loglin,
  stars = T,                                                                    # Add stars of significance
  gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",                                            # Remove redundant goodness of fit measures
  coef_rename = c("ph" = "Fuel price (MXN / L)"),                               # Rename coefficients of interest
  coef_omit = "(Intercept)|total_hp|nino|year",                                 # Remove coefficients to reduce clutter
  add_rows = controls,                                                          # Add extra rows
  notes = list(
    "Controls are: Total engine capacity and annual mean of NINO3.4 index",
    "Standard errors are clustered at the economic-unit-level"
  )
)

# END OF SCRIPT ################################################################

