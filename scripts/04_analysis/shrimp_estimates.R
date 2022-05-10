
library(here)
library(fixest)
library(modelsummary)
library(ggridges)
library(cowplot)
library(tidyverse)


shrimp <- read_csv(
  file = file.path(
  project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
  filter(between(year, 2012, 2019)) %>% 
  mutate(delta = treated * 2,
         dist = pmax(0, fuel_consumption_l - subsidy_cap_l))


# left of predicted
left_pred <- shrimp %>% 
  filter(fuel_consumption_l <= predicted_subsidy_cap_l) %>% 
  mutate(dist = fuel_consumption_l - predicted_subsidy_cap_l,
         rel_dist = fuel_consumption_l / predicted_subsidy_cap_l,
         ps = 2 * treated)

right_pred <- shrimp %>% 
  filter(fuel_consumption_l > predicted_subsidy_cap_l) %>% 
  mutate(dist = fuel_consumption_l - predicted_subsidy_cap_l,
         log_dist_dummy = round(log(dist)),
         log_dist_dummy = factor(ifelse(log_dist_dummy <= 10, "<=10", log_dist_dummy)),
         log_dist_dummy = fct_reorder(log_dist_dummy, dist, function(x){mean(round(log(x)))}),
         rel_dist = fuel_consumption_l / predicted_subsidy_cap_l,
         rel_dist_dummy = round(rel_dist),
         rel_dist_dummy = factor(ifelse(rel_dist_dummy >= 9, "9+", rel_dist_dummy)),
         dist_quant = factor(cut(rel_dist, quantile(rel_dist, probs = seq(0, 1, by = 0.1)), labels = F)),
         t = 1 * treated)

not_subs <- shrimp %>% 
  filter(!treated)

# shrimp %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
# left_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)
# right_pred %>% count(year, treated) %>% spread(treated, n) %>% mutate(n = `TRUE` + `FALSE`)



## ANALYSIS ####################################################################

# Price elasticity of demand on never subsidized

nev1 <- feols(log(fuel_consumption_l) ~ p, data = not_subs)
nev2 <- feols(log(fuel_consumption_l) ~ p | eu_rnpa, data = not_subs)
nev3 <- feols(log(fuel_consumption_l) ~ p + total_hp | eu_rnpa, data = not_subs)

nev <- list(nev1, nev2, nev3)
names(nev) <- rep("log(L)", length(nev))

modelsummary(nev, 
             # output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Not subsidized",
             coef_rename = c("total_hp" = "Total Capacity (HP)"))


# Left of kink
# Fuel consumption
lm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp, left_pred)
lm2 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa, left_pred)
lm3 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa + year, left_pred)
lm4 <- feols(log(fuel_consumption_l) ~  treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, left_pred)
lm5 <- feols(log(fuel_consumption_l) ~  treated + total_hp + dist | eu_rnpa + year, left_pred)
lm6 <- feols(log(fuel_consumption_l) ~  treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

lmods <- list(lm1, lm2, lm3, lm4, lm5, lm6)
names(lmods) <- rep("log(L)", length(lmods))

modelsummary(lmods, 
             output = here("results", "tab", "fuel_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Left of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# Hours

h_lm1 <- feols(log(hours) ~ treated + total_hp, left_pred)
h_lm2 <- feols(log(hours) ~  treated + total_hp | eu_rnpa, left_pred)
h_lm3 <- feols(log(hours) ~  treated + total_hp | eu_rnpa + year, left_pred)
h_lm4 <- feols(log(hours) ~  treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, left_pred)
h_lm5 <- feols(log(hours) ~  treated + total_hp + dist | eu_rnpa + year, left_pred)
h_lm6 <- feols(log(hours) ~  treated + total_hp | eu_rnpa + year, left_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))


hlmods <- list(h_lm1, h_lm2, h_lm3, h_lm4, h_lm5, h_lm6)
names(hlmods) <- rep("log(H)", length(hlmods))

modelsummary(hlmods,
             output = here("results", "tab", "hours_left.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Left of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))



# Models to the Right of the kink
rm1 <- feols(log(fuel_consumption_l) ~ treated + total_hp, right_pred)
rm2 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa, right_pred)
rm3 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred)
rm4 <- feols(log(fuel_consumption_l) ~ treated + total_hp + predicted_subsidy_cap_l| eu_rnpa + year, right_pred)
rm5 <- feols(log(fuel_consumption_l) ~ treated + dist + total_hp  | eu_rnpa + year, right_pred)
rm6 <- feols(log(fuel_consumption_l) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))


# Make table
rmods <- list(rm1, rm2, rm3, rm4, rm5, rm6)
names(rmods) <- rep("log(L)", length(rmods))

modelsummary(rmods,
             output = here("results", "tab", "fuel_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Right of kink, effect on fuel consumption. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink (L)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

h_rm1 <- feols(log(hours) ~ treated + total_hp, right_pred)
h_rm2 <- feols(log(hours) ~ treated + total_hp | eu_rnpa, right_pred)
h_rm3 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred)
h_rm4 <- feols(log(hours) ~ treated + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, right_pred)
h_rm5 <- feols(log(hours) ~ treated + dist + total_hp  | eu_rnpa + year, right_pred)
h_rm6 <- feols(log(hours) ~ treated + total_hp | eu_rnpa + year, right_pred %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

# Make table
hrmods <- list(h_rm1, h_rm2, h_rm3, h_rm4, h_rm5, h_rm6)
names(hrmods) <- rep("log(H)", length(hrmods))

modelsummary(hrmods,
             output = here("results", "tab", "hours_right.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Right of kink, effect on hours. The last column excludes vessels that were always subsidized.",
             coef_rename = c("treatedTRUE" = "Subsidized",
                             "total_hp" = "Total Capacity (HP)",
                             "n_vessels" = "# Vessels",
                             "dist" = "Distance from kink (L)",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))


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




# NOW WITH ALL OF THE DATA



###########
#L/R S/U plot

full_shrimp <- shrimp %>% 
  mutate(left = 1 * ((treated & fuel_consumption_l <= predicted_subsidy_cap_l) | (!treated & fuel_consumption_l <= predicted_subsidy_cap_l)),
         right = 1 - left,
         treated = 1 * treated)

# Olivier Deschenes  17:23
# Table 1 & 2 look great, thanks!  For Table 3, let me propose the following model:
# y = a + b1*SUB*LEFT + b2*SUB*RIGHT (plus the same FEs and controls)

cm1 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right, full_shrimp)
cm2 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa , full_shrimp)
cm3 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp)
cm4 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu_rnpa + year, full_shrimp)
cm5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, full_shrimp)
cm6 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

cm <- list(cm1, cm2, cm3, cm4, cm5, cm6)
names(cm) <- rep("log(L)", length(cm))

modelsummary(cm,
             output = here("results", "tab", "fuel_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# Now with hours

h_cm1 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right, full_shrimp)
h_cm2 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa , full_shrimp)
h_cm3 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp)
h_cm4 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp | eu_rnpa + year, full_shrimp)
h_cm5 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right + total_hp + predicted_subsidy_cap_l | eu_rnpa + year, full_shrimp)
h_cm6 <- feols(log(fuel_consumption_l) ~ treated:left + treated:right | eu_rnpa + year, full_shrimp %>% filter(!eu_rnpa %in% unique(by_numbers$eu_rnpa)))

h_cm <- list(h_cm1, h_cm2, h_cm3, h_cm4, h_cm5, h_cm6)
names(h_cm) <- rep("log(Hours)", length(h_cm))

modelsummary(h_cm,
             output = here("results", "tab", "hours_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))

# The whole shebang



full1 <- feols(log(fuel_consumption_l) ~ ph + delta, shrimp)
full2 <- feols(log(fuel_consumption_l) ~ ph + delta | eu_rnpa, shrimp)
full3 <- feols(log(fuel_consumption_l) ~ ph + delta + total_hp + predicted_subsidy_cap_l| eu_rnpa, shrimp)

full <- list(full1, full2, full3)
names(full) <- rep("log(L)", length(full))


modelsummary(full,
             # output = here("results", "tab", "hours_all.tex"),
             stars = T,
             gof_omit = "Adj|IC|Lo|Ps|Wi",
             statistic_vertical = T,
             title = "Effect on fuel consumption by all economic units. The last column excludes vessels that were always subsidized.",
             coef_rename = c("total_hp" = "Total Capacity (HP)",
                             "dist" = "Distance from kink",
                             "predicted_subsidy_cap_l" = "Predicted subsidy cap (L)"))























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
             title = "Demand elasticity of fuel using the 177 economic units that are always subsidized.",
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









