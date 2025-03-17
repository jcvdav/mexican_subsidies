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

# cpi_t <- readRDS(file.path(project_path, "data", "processed_data", "cpi_t_rates.rds"))
# 
# state_prices <- readRDS(
#   file.path(
#     project_path,
#     "data",
#     "processed_data",
#     "annual_state_diesel_prices.rds")) %>% 
#   rename(p_stat = mean_diesel_price_mxn_l) %>% 
#   left_join(cpi_t, by = "year") %>% 
#   mutate(p_stat = p_stat * rate) %>% 
#   select(-rate)
# 
# nino <- readRDS(file.path(project_path, "data", "processed_data", "annual_nino34.rds"))
# 
# shrimp <- read_csv(
#   file = file.path(
#     project_path, "data", "processed_data", "imputed_subsidy_economic_unit_annual_shrimp_panel.csv")) %>% 
#   filter(between(year, 2012, 2019)) %>% 
#   rename(eu = eu_rnpa) %>% 
#   left_join(state_prices, by = c("year", "state")) %>% 
#   left_join(nino, by = "year") %>% 
#   mutate(delta = -2 * treated ,
#          dist = (fuel_consumption_l - predicted_subsidy_cap_l) / 1e3,
#          norm_dist = dist / fuel_consumption_l,
#          left = 1 * (fuel_consumption_l <= predicted_subsidy_cap_l),
#          right = 1 - left,
#          treated = 1 * treated) %>% 
#   mutate(phi = subsidy_cap_l / fuel_consumption_l,
#          D = phi < 1L,
#          R = 2,
#          term1 = pl + (D * R),
#          term2 = phi * R  * D)
# 

# Create subsamples ############################################################
# Vessels that are subsidized every time we observe them

# # Vessels that are not subsidized
# not_sub <- shrimp %>% 
#   filter(!treated)
# 
# # Vessels to the left, according to the predicted cap
# left_pred <- shrimp %>% 
#   filter(fuel_consumption_l <= predicted_subsidy_cap_l) #1489 observations
# # Vessels to the right, according to the predicted cap
# right_pred <- shrimp %>% 
#   filter(fuel_consumption_l > predicted_subsidy_cap_l) #1520 observations
# 
# # Vessels to the left, according to the granted cap
# left_grant <- shrimp %>% 
#   filter((treated & (fuel_consumption_l <= subsidy_cap_l)) |
#            (!treated & (fuel_consumption_l <= predicted_subsidy_cap_l))) %>% #1378
#   mutate(dist = fuel_consumption_l - subsidy_cap_l)
# # Vessels to the right, according to the granted cap
# right_grant <- shrimp %>% 
#   filter((treated & (fuel_consumption_l > subsidy_cap_l)) |
#            (!treated & (fuel_consumption_l > predicted_subsidy_cap_l))) %>%  #1631
#   mutate(dist = fuel_consumption_l - subsidy_cap_l,
#          norm_dist = dist / fuel_consumption_l)




#######################################################################################################


## Salience test ################################################################################################





















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










