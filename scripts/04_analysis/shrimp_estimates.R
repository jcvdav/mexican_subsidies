######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(regrrr)
library(fixest)
library(modelsummary)
library(tidyverse)

cpi_t <- readRDS(file.path(project_path, "data", "processed_data", "cpi_t_rates.rds"))

state_prices <- readRDS(
  file.path(
    project_path,
    "data",
    "processed_data",
    "annual_state_diesel_prices.rds")) %>% 
  rename(p_stat = mean_diesel_price_mxn_l) %>% 
  left_join(cpi_t, by = "year") %>% 
  mutate(p_stat = p_stat * rate) %>% 
  select(-rate)

nino <- readRDS(file.path(project_path, "data", "processed_data", "annual_nino34.rds"))

shrimp <- read_csv(
  file = file.path(
  project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  rename(eu = eu_rnpa) %>% 
  left_join(state_prices, by = c("year", "state")) %>% 
  left_join(nino, by = "year") %>% 
  mutate(delta = -2 * treated ,
         dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1e3,
         norm_dist = dist / fuel_consumption_l,
         left = 1 * (fuel_consumption_l <= predicted_subsidy_cap_l),
         right = 1 - left,
         treated = 1 * treated) %>% 
  mutate(phi = subsidy_cap_l / fuel_consumption_l,
         D = phi < 1L,
         R = 2,
         term1 = pl + (D * R),
         term2 = phi * R  * D) 

# Create subsamples ############################################################
# Vessels that are subsidized every time we observe them
always_sub <- shrimp %>% 
  group_by(eu) %>% 
  summarize(n_obs = n(),
            n_sub = sum(treated)) %>% 
  filter(n_sub == n_obs) %>% 
  pull(eu)

# Vessels that are not subsidized
not_sub <- shrimp %>% 
  filter(!treated)

# Vessels to the left, according to the predicted cap
left_pred <- shrimp %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l) #1489 observations
# Vessels to the right, according to the predicted cap
right_pred <- shrimp %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l) #1520 observations

# Vessels to the left, according to the granted cap
left_grant <- shrimp %>% 
  filter((treated & (fuel_consumption_l <= subsidy_cap_l)) |
           (!treated & (fuel_consumption_l <= predicted_subsidy_cap_l))) %>% #1378
  mutate(dist = fuel_consumption_l - subsidy_cap_l)
# Vessels to the right, according to the granted cap
right_grant <- shrimp %>% 
  filter((treated & (fuel_consumption_l > subsidy_cap_l)) |
           (!treated & (fuel_consumption_l > predicted_subsidy_cap_l))) %>%  #1631
  mutate(dist = fuel_consumption_l - subsidy_cap_l,
         norm_dist = dist / fuel_consumption_l)

## ANALYSIS ####################################################################

# Price elasticity of demand on never subsidized
# e(p) = dQ/Q / dP/P
#Q = quantity of demanded good
#P = price of demanded good

#log-log
nev1 <- feols(log(fuel_consumption_l) ~ log(p) | eu, data = not_sub)
nev2 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp + nino34_m | eu, data = not_sub)
nev3 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp | eu, not_sub %>% filter(year >= 2017))
nev4 <- feols(log(fuel_consumption_l) ~ log(p_stat) + total_hp | eu, data = not_sub)
nev5 <- feols(log(fuel_consumption_l) ~ log(p_stat) + total_hp + nino34_m | eu, data = not_sub)
nev6 <- feols(log(fuel_consumption_l) ~ log(p_stat) + total_hp | eu + year, data = not_sub)


nev <- list(nev1,
            nev2,
            nev3,
            nev4,
            nev5,
            nev6)

names(nev) <- rep("log(L)", length(nev))

state_p <- c("Prices", rep("National", 3), rep("State", 3)) %>% 
  t() %>% 
  as.data.frame()

modelsummary(nev, 
             output = here("results", "tab", "not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             add_rows = state_p,
             title = "Price elasticity of demand for vessels that are not subsidized.
             Columns 1-3 use mean national fuel prices from 2012-2019.
             Columns 4-6 use mean annual state-level prices from 2017-2019.",
             coef_rename = c("log(p)" = "log(Fuel price [MXP / L])",
                             "log(p_stat)" = "log(Fuel price [MXP / L])",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "NINO 3.4"))

#log-linear

loglin_nev1 <- feols(log(fuel_consumption_l) ~ p | eu, data = not_sub)
loglin_nev2 <- feols(log(fuel_consumption_l) ~ p + total_hp + nino34_m | eu, data = not_sub)
loglin_nev3 <- feols(log(fuel_consumption_l) ~ p + total_hp | eu, not_sub %>% filter(year >= 2017))
loglin_nev4 <- feols(log(fuel_consumption_l) ~ p_stat + total_hp | eu, data = not_sub)
loglin_nev5 <- feols(log(fuel_consumption_l) ~ p_stat + total_hp + nino34_m | eu, data = not_sub)
loglin_nev6 <- feols(log(fuel_consumption_l) ~ p_stat + total_hp | eu + year, data = not_sub)


loglin_nev <- list(loglin_nev1,
                   loglin_nev2,
                   loglin_nev3,
                   loglin_nev4,
                   loglin_nev5,
                   loglin_nev6)

names(loglin_nev) <- rep("log(L)", length(loglin_nev))

modelsummary(loglin_nev, 
             output = here("results", "tab", "loglin_not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             add_rows = state_p,
             title = "Price elasticity of demand for vessels that are not subsidized.
             Columns 1-3 use mean national fuel prices from 2012-2019.
             Columns 4-6 use mean annual state-level prices from 2017-2019.",
             coef_rename = c("p" = "Fuel price (MXN / L)",
                             "p_stat" = "Fuel price (MXN / L)",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "NINO 3.4"))


#######################################################################################################
# Subsidy dummy regression
# Log-linear of fuel consumtpuion and hours on a treatment dummy,
# separately ran for vessels to the right and to the left.

# The m# objects contain four models each. First one is for vessels to the right and fuel, then right and hours, left ,, and fuel, left and hours.
m1 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated, data = shrimp, split = ~left)
m2 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated | eu, data = shrimp, split = ~left)
m3 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated | eu + year, data = shrimp, split = ~left)
m4 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp | eu + year, data = shrimp, split = ~left)
m5 <- feols(c(log(fuel_consumption_l), log(hours)) ~ treated + total_hp | eu + year, data = shrimp %>% filter(!eu %in% always_sub), split = ~left)

# Set up table defaults
renames <- c("treated" = "Subsidized",
             "dist" = "Distance from kink (1,000 L)",
             "total_hp" = "Total Capacity (HP)")

omits <- "Adj|IC|Lo|Ps"

## Extract to the right and fuel
rmods <- list(m1[[1]],
              m2[[1]],
              m3[[1]],
              m4[[1]],
              m5[[1]])
names(rmods) <- rep("log(L)", length(rmods))
modelsummary(rmods,
             title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_right.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames)

## Extract to the right and hours
hrmods <- list(m1[[2]],
               m2[[2]],
               m3[[2]],
               m4[[2]],
               m5[[2]])
names(hrmods) <- rep("log(H)", length(hrmods))
modelsummary(hrmods,
             title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_right.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames)


## Extract to the left and fuel
lmods <- list(lm1[[3]],
              lm2[[3]],
              lm3[[3]],
              lm4[[3]],
              lm5[[3]])
names(lmods) <- rep("log(L)", length(lmods))
modelsummary(lmods, 
             title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames)

## Extract to the left and fuel
hlmods <- list(m1[[4]],
               m2[[4]],
               m3[[4]],
               m4[[4]],
               m5[[4]])
names(hlmods) <- rep("log(H)", length(hlmods))
modelsummary(hlmods,
             title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_left.tex"),
             stars = T,
             gof_omit = omits,
             coef_rename = renames)

## Salience test ################################################################################################

s1 <- feols(log(fuel_consumption_l) ~ ph + delta | eu, shrimp, subset = ~left == 1)
s2 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp + nino34_m | eu, shrimp, subset = ~left == 1)
s3 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp | eu, shrimp %>% filter(year >= 2017), subset = ~left == 1)
s4 <- feols(log(fuel_consumption_l) ~ p_stat + delta + total_hp | eu, shrimp, subset = ~left == 1)
s5 <- feols(log(fuel_consumption_l) ~ p_stat + delta + total_hp + nino34_m | eu, shrimp, subset = ~left == 1)
s6 <- feols(log(fuel_consumption_l) ~ p_stat + delta + total_hp | eu + year, shrimp, subset = ~left == 1)

s <- list(s1,
          s2,
          s3,
          s4,
          s5,
          s6)
names(s) <- rep("log(L)", length(s))

my_test <- function(model) {
  dif <- model$coefficients[1] - model$coefficients[2]
  result <- test_coef_equality(model, names(model$coefficients)[1], names(model$coefficients)[2],
                               v = sandwich::vcovHAC(model))
  paste0(round(dif, 3), " (", round(result, 3),  ")")
}

Htest <- c("H0: delta = alpha", map_chr(s[1:6], my_test)) %>%
          t() %>%
          as.data.frame() %>% 
          set_names(nm = paste0("V", 1:7)) %>% 
  rbind(state_p)

modelsummary(s,
             output = here("results", "tab", "salience_test.tex"),
             title = "Salience test on vessels to the left.
             Columns 1-3 use mean national fuel prices from 2012-2019.
             Columns 4-6 use mean annual state-level prices from 2017-2019.",
             stars = T,
             add_rows = Htest,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("ph" = "P",
                             "p_stat" = "P",
                             "delta" = "Delta",
                             "total_hp" = "Total Capacity (HP)",
                             "nino34_m" = "Nino 3.4"))



















# NOW WITH ALL OF THE DATA ######################################################################################
###########
#L/R S/U plot




# Olivier Deschenes  
# Table 1 & 2 look great, thanks!  For Table 3, let me propose the following model:
# y = a + b1*SUB*LEFT + b2*SUB*RIGHT (plus the same FEs and controls)

cm1 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right, shrimp)
cm2 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right | eu , shrimp)
cm3 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right | eu + year, shrimp)
cm4 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right + total_hp | eu + year, shrimp)
cm5 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right + total_hp | eu + year, shrimp %>% filter(!eu %in% always_sub))

cm <- list(cm1,
           cm2,
           cm3,
           cm4,
           cm5)

names(cm) <- rep("log(L)", length(cm))

modelsummary(cm,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("left" = "Left",
                             "left:treated" = "Subsidized X Left",
                             "treated:right" = "Subsidized X Right",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


# Now with hours
h_cm1 <- feols(log(hours) ~ left + treated:left + treated:right, shrimp)
h_cm2 <- feols(log(hours) ~ left + treated:left + treated:right | eu , shrimp)
h_cm3 <- feols(log(hours) ~ left + treated:left + treated:right | eu + year, shrimp)
h_cm4 <- feols(log(hours) ~ left + treated:left + treated:right + total_hp | eu + year, shrimp)
h_cm5 <- feols(log(hours) ~ left + treated:left + treated:right + total_hp | eu + year, shrimp %>% filter(!eu %in% always_sub))

h_cm <- list(h_cm1,
             h_cm2,
             h_cm3,
             h_cm4,
             h_cm5)

names(h_cm) <- rep("log(Hours)", length(h_cm))

modelsummary(h_cm,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("left" = "Left",
                             "left:treated" = "Subsidized X Left",
                             "treated:right" = "Subsidized X Right",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))



# Pew summary table

sum <- list(nev2,
            loglin_nev2,
            m4[[1]],
            m4[[3]],
            s2)


modelsummary(sum)









# Run using only the real caps

shrimp2 <- shrimp %>% 
  mutate(left = treated * (fuel_consumption_l <= subsidy_cap_l),
         right = treated * (fuel_consumption_l > subsidy_cap_l),
         ihs = log(subsidy_cap_l + sqrt(subsidy_cap_l ^ 2 + 1)),
         subsidy_cap_l = subsidy_cap_l / 1e5,
         cl = ihs * left,
         cr = ihs * right)

real1 <- feols(log(fuel_consumption_l) ~ treated, data = shrimp2)
real2 <- feols(log(fuel_consumption_l) ~ treated | eu , data = shrimp2)
real3 <- feols(log(fuel_consumption_l) ~ treated | eu + year, data = shrimp2)
real4 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu + year, data = shrimp2)
real5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu + year, data = shrimp2)
real6 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu + year, data = shrimp2 %>% filter(!eu %in% unique(always_sub)))

real <- list(real1,
             real2,
             real3,
             real4,
             real5,
             real6)

names(real) <- rep("log(L)", length(real))


modelsummary(real,
             title = "Using the real subsidy cap.",
             output = here("results", "tab", "real_cap.tex"),
             stars = T, 
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("treated" = "Subsidized",
                             "subsidy_cap_l" = "Subsidy cap (L)",
                             "total_hp" = "Total Capacity (HP)",
                             "treated:left" = "Subsidized X Left",
                             "treated:right" = "Subsidized X Right"))







feols(log(fuel_consumption_l) ~ treated + left:subsidy_cap_l + right:subsidy_cap_l + total_hp | eu + year, data = shrimp2) %>% 
  modelsummary(stars = T, 
               gof_omit = "Adj|IC|Lo|Ps|Wi",
               coef_rename = c("treated" = "Subsidized",
                               "left" = "Left",
                               "subsidy_cap_l" = "Subsidy cap (L)",
                               "total_hp" = "Total Capacity (HP)",
                               "left:subsidy_cap_l" = "Subsidy cap X Left",
                               "subsidy_cap_l:right" = "Subsidy cap X Right"))









# Reeurecting term1 and term2

get_alpha <- function(model){
  beta <- coefficients(model)["term1"]
  theta <- coefficients(model)["term2"]
  1  + (theta/beta)
}

perc1 <- feols(fuel_consumption_l ~ term1 + term2 |  year, data = shrimp)
perc2 <- feols(fuel_consumption_l ~ term1 + term2 + total_hp + predicted_subsidy_cap_l | eu + year, data = shrimp)

feols(log(fuel_consumption_l) ~ p + dist:right + dist:right:treated + total_hp + predicted_subsidy_cap_l|  year, data = shrimp)

perc <- list(perc1,
             perc2)

names(perc) <- rep("Liters", length(perc))


modelsummary(perc,
             title = "Weight on marginal",
             # output = here("results", "tab", "weight_marginal.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


#Distance, but old
# Models with binned distances
rm5 <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") | eu + log_dist_dummy, right_pred)
rm5_full <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") + total_hp + n_vessels + predicted_subsidy_cap_l| eu + log_dist_dummy, right_pred)

rm6 <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") | eu + rel_dist_dummy, right_pred)
rm6_full <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") + total_hp + n_vessels + predicted_subsidy_cap_l| eu + rel_dist_dummy, right_pred)

coefplot(list(rm5, rm5_full))
coefplot(list(rm6,rm6_full))

# Make table
rmods2 <- list(rm5, rm5_full, rm6, rm6_full)
names(rmods2) <- rep("log(L)", length(rmods2))

modelsummary(rmods2,
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = F,
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels"))













## TNot sure what the sutff below is, but keeping it there for now

a <- by_numbers %>% 
  mutate(r = fuel_consumption_l > subsidy_cap_l) %>% 
  group_by(eu, r) %>% 
  mutate(n = n()) %>% 
  filter(n < 6)

ito1 <- feols(log(fuel_consumption_l) ~ r + log(p) | eu, data = a)
ito2 <- feols(log(fuel_consumption_l) ~ dist | eu + year , data = by_numbers)
ito3 <- feols(log(fuel_consumption_l) ~ dist + total_hp | eu + year, data = by_numbers)
ito4 <- feols(log(fuel_consumption_l) ~ dist + total_hp + subsidy_cap_l | eu + year, data = by_numbers)

# Make table
itomods <- list(ito1,
                ito2,
                ito3,
                ito4)

names(itomods) <- rep("log(L)", length(itomods))

modelsummary(itomods,
             # output = here("results", "tab", "always_subsidized.tex"),
             title = "Price elasticity of demand for fuel using the 177 economic units that are always subsidized.",
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             coef_rename = c("log(p)" = "log(price)",
                             "subsidy_cap_l" = "Subsidy Cap (L)",
                             "total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink (L)"))


nevers <- shrimp %>% 
  group_by(eu) %>% 
  mutate(n_subs = sum(treated),
         n_obs = n()) %>% 
  ungroup() %>% 
  filter(!treated,
         n_subs == 0)

feols(log(fuel_consumption_l) ~ log(p) | eu , data = nevers)









######### TEST FOR EFFECT OF CAP ON ALWAYS SUBD

s3 <- shrimp %>%
  filter(eu %in% always_sub) %>% 
  mutate(norm_cap = subsidy_cap_l / total_hp)


feols(log(fuel_consumption_l) ~ left:log(subsidy_cap_l) + right:log(subsidy_cap_l) + total_hp | eu + year, data = s3) 
feols(log(fuel_consumption_l) ~ log(ph) + log(p) + total_hp | eu + year, data = s3) 


s4 <- shrimp %>%
  mutate(norm_cap = subsidy_cap_l / total_hp,
         ihs_norm_cap = log(norm_cap + sqrt((norm_cap ^ 2) + 1)))

feols(log(fuel_consumption_l) ~ treated + left:ihs_norm_cap + right:ihs_norm_cap| eu + year, data = s4) 




# Notes for next week
# - Combine tables 7 and 8 into a single table (two panels). They should us only columns 1 - 6 (exclude the ones where fuel prices are mix and match)
# - Tabke 9 replaces table 6
# - Run a new test. Keep only subsidized vessels, and regress Q on a dummy for Right. We expect a negative value.

annual_national_diesel_prices_2011_2020 <- readRDS(file = file.path(
  project_path,
  "data",
  "processed_data",
  "annual_national_diesel_prices_2011_2020.rds")) %>% 
  rename(ph = mean_diesel_price_mxn_l)

s5 <- shrimp %>% 
  filter(treated == 1) %>% 
  select(fuel_consumption_l, subsidy_cap_l, year, eu, total_hp, right, left, treated, delta) %>% 
  left_join(annual_national_diesel_prices_2011_2020, by = "year") %>% 
  left_join(nino, by = "year") %>% 
  mutate(norm_cap = subsidy_cap_l / total_hp)

feols(log(fuel_consumption_l) ~ right + total_hp | eu + year, data = s3) 
feols(log(fuel_consumption_l) ~ right + total_hp | eu + year, data = s5)
feols(log(fuel_consumption_l) ~ right + total_hp, data = s5)
feols(log(fuel_consumption_l) ~ right + right:norm_cap + total_hp | eu + year, data = s5)

feols(log(fuel_consumption_l) ~ log(ph) + log(norm_cap) + nino34_m | eu, data = s5, fsplit = ~right) %>% 
  set_names("Full sample", "Lef tof kink", "Right of kink") %>% 
  modelsummary(stars = T,
               gof_omit = "Adj|IC|Lo|Ps|Wi",
               coef_rename = c("log(norm_cap)" = "Norm. sub [log(L / HP)]"))










