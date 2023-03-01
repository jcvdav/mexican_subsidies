

shrimp_panel <- readRDS(file.path(project_path, "data", "processed_data", "shrimp_estimation_panel.rds")) %>% 
  filter(year >= 2017,
         !treated)

log_m1 <- feols(log(fuel_consumption_l) ~ log(p_stat) | eu, data = shrimp_panel)
log_m2 <- feols(log(fuel_consumption_l) ~ log(p_stat) | eu + year, data = shrimp_panel)
log_m3 <- feols(log(fuel_consumption_l) ~ log(p_stat)  + total_hp | eu + year, data = shrimp_panel)


modelsummary(list(log_m1, log_m2, log_m3),
             stars = T,
             # output = "elasticity_2017_2019.tex",
             gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",
             title = "Price elasticity of demand for vessels that are not subsidized.",
             coef_rename = c("log(p)" = "log(Fuel price [MXP / L])"),
             coef_omit = "(Intercept)|total_hp|nino|year")

m1 <- feols(log(fuel_consumption_l) ~ p_stat | eu, data = shrimp_panel)
m2 <- feols(log(fuel_consumption_l) ~ p_stat | eu + year, data = shrimp_panel)
m3 <- feols(log(fuel_consumption_l) ~ p_stat  + total_hp | eu + year, data = shrimp_panel)


modelsummary(list(m1, m2, m3),
             stars = T,
             output = "semi_elasticity_2017_2019.tex",
             gof_omit = "Adj|IC|Lo|Ps|Wi|RMSE",
             title = "Price semi-elasticity of demand for vessels that are not subsidized.",
             coef_rename = c("p_stat" = "Fuel price [MXP / L]"),
             coef_omit = "(Intercept)|total_hp|nino|year")
