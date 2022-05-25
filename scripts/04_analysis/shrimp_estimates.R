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


shrimp <- read_csv(
  file = file.path(
  project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  mutate(delta = -2 * treated ,
         dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1e3,
         norm_dist = dist / fuel_consumption_l,
         left = 1 * ((treated & fuel_consumption_l <= predicted_subsidy_cap_l) | (!treated & fuel_consumption_l <= predicted_subsidy_cap_l)),
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
  group_by(eu_rnpa) %>% 
  summarize(n_obs = n(),
            n_sub = sum(treated)) %>% 
  filter(n_sub == n_obs) %>% 
  pull(eu_rnpa)

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

# shrimp %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `1` + `0`)
# left_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `1` + `0`)
# right_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `1` + `0`)
# left_grant %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `1` + `0`)
# right_grant %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `1` + `0`)



## ANALYSIS ####################################################################

# Price elasticity of demand on never subsidized
# e(p) = dQ/Q / dP/P
#Q = quantity of demanded good
#P = price of demanded good

nev1 <- feols(log(fuel_consumption_l) ~ p, data = not_sub)
nev2 <- feols(log(fuel_consumption_l) ~ p | eu_rnpa, data = not_sub)
nev3 <- feols(log(fuel_consumption_l) ~ p | eu_rnpa + year, data = not_sub)
nev4 <- feols(log(fuel_consumption_l) ~ p + total_hp | eu_rnpa + year, data = not_sub)
nev5 <- feols(log(fuel_consumption_l) ~ log(p) + total_hp | eu_rnpa + year, data = not_sub)


nev <- list(nev1,
            nev2,
            nev3,
            nev4,
            nev5)

names(nev) <- rep("log(L)", length(nev))

modelsummary(nev, 
             output = here("results", "tab", "not_sub.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             title = "Price elasticity of demand for vessels that are not subsidized (2017 - 2019).",
             coef_rename = c("p" = "Fuel price (MXN / L)",
                             "log(p)" = "log(Fuel price [MXP / L])",
                             "total_hp" = "Total Capacity (HP)"))


# Left of kink
# Fuel consumption
lm1 <- feols(log(fuel_consumption_l) ~ treated, left_pred)
lm2 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa, left_pred)
lm3 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa + year, left_pred)
lm4 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, left_pred)
lm5 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% always_sub))

lmods <- list(lm1,
              lm2,
              lm3,
              lm4,
              lm5)

names(lmods) <- rep("log(L)", length(lmods))

modelsummary(lmods, 
             title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("treated" = "Subsidized",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# Hours

h_lm1 <- feols(log(hours) ~ treated, left_pred)
h_lm2 <- feols(log(hours) ~ treated | eu_rnpa, left_pred)
h_lm3 <- feols(log(hours) ~ treated | eu_rnpa + year, left_pred)
h_lm4 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, left_pred)
h_lm5 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% always_sub))


hlmods <- list(h_lm1,
               h_lm2,
               h_lm3,
               h_lm4,
               h_lm5)

names(hlmods) <- rep("log(H)", length(hlmods))

modelsummary(hlmods,
             title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("treated" = "Subsidized",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


# Models to the Right of the kink
rm1 <- feols(log(fuel_consumption_l) ~ treated, right_pred)
rm2 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa, right_pred)
rm3 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa + year, right_pred)
rm4 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred)
rm5 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% always_sub))

# Make table
rmods <- list(rm1,
              rm2,
              rm3,
              rm4,
              rm5)

names(rmods) <- rep("log(L)", length(rmods))

modelsummary(rmods,
             title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "fuel_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("treated" = "Subsidized",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


h_rm1 <- feols(log(hours) ~ treated, right_pred)
h_rm2 <- feols(log(hours) ~ treated | eu_rnpa, right_pred)
h_rm3 <- feols(log(hours) ~ treated | eu_rnpa + year, right_pred)
h_rm4 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred)
h_rm5 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% always_sub))

# Make table
hrmods <- list(h_rm1,
               h_rm2,
               h_rm3,
               h_rm4,
               h_rm5)

names(hrmods) <- rep("log(H)", length(hrmods))

modelsummary(hrmods,
             title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             output = here("results", "tab", "hours_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("treated" = "Subsidized",
                             "dist" = "Distance from kink (1,000 L)",
                             "treated:dist" = "Subsidized x Distance from kink",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))



# NOW WITH ALL OF THE DATA
###########
#L/R S/U plot




# Olivier Deschenes  
# Table 1 & 2 look great, thanks!  For Table 3, let me propose the following model:
# y = a + b1*SUB*LEFT + b2*SUB*RIGHT (plus the same FEs and controls)

cm1 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right, shrimp)
cm2 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right | eu_rnpa , shrimp)
cm3 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right | eu_rnpa + year, shrimp)
cm4 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right + total_hp | eu_rnpa + year, shrimp)
cm5 <- feols(log(fuel_consumption_l) ~ left + treated:left + treated:right + total_hp | eu_rnpa + year, shrimp %>% filter(!eu_rnpa %in% always_sub))

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
h_cm2 <- feols(log(hours) ~ left + treated:left + treated:right | eu_rnpa , shrimp)
h_cm3 <- feols(log(hours) ~ left + treated:left + treated:right | eu_rnpa + year, shrimp)
h_cm4 <- feols(log(hours) ~ left + treated:left + treated:right + total_hp | eu_rnpa + year, shrimp)
h_cm5 <- feols(log(hours) ~ left + treated:left + treated:right + total_hp | eu_rnpa + year, shrimp %>% filter(!eu_rnpa %in% always_sub))

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



# The whole shebang

full1 <- feols(log(fuel_consumption_l) ~ ph + delta, shrimp)
full2 <- feols(log(fuel_consumption_l) ~ ph + delta | eu_rnpa, shrimp)
full3 <- feols(log(fuel_consumption_l) ~ ph + delta | eu_rnpa + year, shrimp)
full4 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp | eu_rnpa + year, shrimp)
full5 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp | eu_rnpa + year, left_pred)
full6 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp | eu_rnpa + year, right_pred)

full <- list(full1,
             full2,
             full3,
             full4,
             full5,
             full6)

names(full) <- rep("log(L)", length(full))


my_test <- function(model) {
  dif <- model$coefficients["ph"] - model$coefficients["delta"]
  result <- test_coef_equality(model, "delta", "ph",
                     v = sandwich::vcovHAC(model))
  paste0(round(dif, 3), " (", round(result, 3),  ")")
}

Htest <- cbind("H0: delta = alpha", map_dfc(full[1:6], my_test))

modelsummary(full,
             title = "Salience test (2017 - 2019 data only).",
             output = here("results", "tab", "salience.tex"),
             stars = T,
             add_rows = Htest,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             coef_rename = c("ph" = "Fuel price (MXN / L)",
                             "delta" = "Price subsidy",
                             "delta:right" = "Price subsidy X Right",
                             "left:delta" = "Price subsidy X Left",
                             "total_hp" = "Total Capacity (HP)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))



# Run using only the real caps

shrimp2 <- shrimp %>% 
  mutate(left = treated * (fuel_consumption_l <= subsidy_cap_l),
         right = treated * (fuel_consumption_l > subsidy_cap_l),
         ihs = log(subsidy_cap_l + sqrt(subsidy_cap_l ^ 2 + 1)),
         subsidy_cap_l = subsidy_cap_l / 1e5,
         cl = ihs * left,
         cr = ihs * right)

real1 <- feols(log(fuel_consumption_l) ~ treated, data = shrimp2)
real2 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa , data = shrimp2)
real3 <- feols(log(fuel_consumption_l) ~ treated | eu_rnpa + year, data = shrimp2)
real4 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, data = shrimp2)
real5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu_rnpa + year, data = shrimp2)
real6 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, data = shrimp2 %>% filter(!eu_rnpa %in% unique(always_sub)))

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







feols(log(fuel_consumption_l) ~ treated + left:subsidy_cap_l + right:subsidy_cap_l + total_hp | eu_rnpa + year, data = shrimp2) %>% 
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
perc2 <- feols(fuel_consumption_l ~ term1 + term2 + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, data = shrimp)

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
rm5 <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") | eu_rnpa + log_dist_dummy, right_pred)
rm5_full <- feols(log(fuel_consumption_l) ~ i(treated, log_dist_dummy, "15") + total_hp + n_vessels + predicted_subsidy_cap_l| eu_rnpa + log_dist_dummy, right_pred)

rm6 <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") | eu_rnpa + rel_dist_dummy, right_pred)
rm6_full <- feols(log(fuel_consumption_l) ~ i(treated, rel_dist_dummy, "9+") + total_hp + n_vessels + predicted_subsidy_cap_l| eu_rnpa + rel_dist_dummy, right_pred)

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
  group_by(eu_rnpa, r) %>% 
  mutate(n = n()) %>% 
  filter(n < 6)

ito1 <- feols(log(fuel_consumption_l) ~ r + log(p) | eu_rnpa, data = a)
ito2 <- feols(log(fuel_consumption_l) ~ dist | eu_rnpa + year , data = by_numbers)
ito3 <- feols(log(fuel_consumption_l) ~ dist + total_hp | eu_rnpa + year, data = by_numbers)
ito4 <- feols(log(fuel_consumption_l) ~ dist + total_hp + subsidy_cap_l | eu_rnpa + year, data = by_numbers)

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
  group_by(eu_rnpa) %>% 
  mutate(n_subs = sum(treated),
         n_obs = n()) %>% 
  ungroup() %>% 
  filter(!treated,
         n_subs == 0)

feols(log(fuel_consumption_l) ~ log(p) | eu_rnpa , data = nevers)









######### TEST FOR EFFECT OF CAP ON ALWAYS SUBD

s3 <- shrimp %>%
  filter(eu_rnpa %in% always_sub) %>% 
  mutate(norm_cap = subsidy_cap_l / total_hp)


feols(log(fuel_consumption_l) ~ left:log(subsidy_cap_l) + right:log(subsidy_cap_l) + total_hp | eu_rnpa + year, data = s3) 
feols(log(fuel_consumption_l) ~ log(ph) + log(p) + total_hp | eu_rnpa + year, data = s3) 


s4 <- shrimp %>%
  mutate(norm_cap = subsidy_cap_l / total_hp,
         ihs_norm_cap = log(norm_cap + sqrt((norm_cap ^ 2) + 1)))

feols(log(fuel_consumption_l) ~ treated + left:ihs_norm_cap + right:ihs_norm_cap| eu_rnpa + year, data = s4) 




# Notes for next week
# - Combine tables 7 and 8 into a single table (two panels). They should us only columns 1 - 6 (exclude the ones where fuel prices are mix and match)
# - Tabke 9 replaces table 6
# - Run a new test. Keep only subsidized vessels, and regress Q on a dummy for Right. We expect a negative value.

s5 <- shrimp %>% 
  filter(treated == 1) %>% 
  mutate(norm_cap = subsidy_cap_l / total_hp)

feols(log(fuel_consumption_l) ~ right + total_hp | eu_rnpa + year, data = s3) 
feols(log(fuel_consumption_l) ~ right + total_hp | eu_rnpa + year, data = s5)
feols(log(fuel_consumption_l) ~ right + total_hp, data = s5)
feols(log(fuel_consumption_l) ~ right + right:norm_cap + total_hp | eu_rnpa + year, data = s5)

feols(log(fuel_consumption_l) ~ log(ph) + log(norm_cap) | eu_rnpa + year, data = s5, fsplit = ~right) %>% 
  set_names("Full sample", "Lef tof kink", "Right of kink") %>% 
  modelsummary(stars = T)










