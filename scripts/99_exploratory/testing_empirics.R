
library(modelsummary)
library(fixest)
library(cowplot)
library(tidyverse)
  

# Define some basic variables
p_market <- 31
p_subsidy <- p_market - 2
# cap <- 50

# Define some functions
get_demand <- function(intercept, slope, q){
  d <- (-intercept/slope) + (1 / slope * q)
  return(d)
}

get_pa <- function(ph, pl, cap, q){
  p <- ph - ((ph - pl) * (cap / q))
  p[q <= cap] <- pl
  return(p)
}


get_pp <- function(ph, pl, cap, q, alpha = 0.7){
  p <- ph - ((1-alpha) * (ph - pl) * (cap / q))
  p[q <= cap] <- pl
  return(p)
}


pm_opt_fun <- function(q, oth){
  intercept <- oth$intercept
  slope <- oth$slope
  cap <- oth$cap
  ph <- oth$ph
  pl <- oth$pl
  
  pm <- ifelse(q <= cap, pl, ph)
  d <- get_demand(intercept = intercept, slope = slope, q = q)
  
  dif <- (pm - d) ^ 2
  return(dif)
}

pa_opt_fun <- function(q, oth){
  intercept <- oth$intercept
  slope <- oth$slope
  cap <- oth$cap
  ph <- oth$ph
  pl <- oth$pl
  
  pa <- get_pa(ph = ph, pl = pl, cap = cap, q = q)
  d <- get_demand(intercept = intercept, slope = slope, q = q)
  
  dif <- (pa - d) ^ 2
  return(dif)
}

pp_opt_fun <- function(q, oth){
  intercept <- oth$intercept
  slope <- oth$slope
  cap <- oth$cap
  ph <- oth$ph
  pl <- oth$pl
  alpha <- oth$alpha
  
  pp <- get_pp(ph = ph, pl = pl, cap = cap, q = q, alpha = alpha)
  d <- get_demand(intercept = intercept, slope = slope, q = q)
  
  dif <- (pp - d) ^ 2
  return(dif)
}

pm_optim_wraper <- function(intercept, slope, cap, ph, pl){
  # opt <- optim(par = cap,
  #              fn = pm_opt_fun,
  #              lower = 0,
  #              upper = 1000,
  #              method = "L-BFGS-B",
  #              oth =  list(intercept = intercept,
  #                          slope = slope,
  #                          cap = cap,
  #                          ph = ph,
  #                          pl = pl))
  
  qm_h <- intercept + (slope * ph)
  qm_l <- intercept + (slope * pl)
  
  qm <- min(cap, qm_l)
  qm <- max(qm_h, qm)
  
  tibble(qm = qm,
         pm = ifelse(qm <= cap, pl, ph))
}

pa_optim_wraper <- function(intercept, slope, cap, ph, pl){
  opt <- nlminb(objective = pa_opt_fun,
                start = 0.5 * cap,
                lower = 0,
                upper = 1e6,
                # method = "L-BFGS-B",
                oth =  list(intercept = intercept,
                            slope = slope,
                            cap = cap,
                            ph = ph,
                            pl = pl))
  
  qa <- opt$par
  
  tibble(qa = qa,
         pa = get_pa(q = qa, cap = cap, ph = ph, pl = pl))
}

pp_optim_wraper <- function(intercept, slope, cap, ph, pl, alpha){
  # browser()
  opt <- optim(par = cap - 1,
               fn = pp_opt_fun,
               lower = 0,
               upper = 1e6,
               method = "L-BFGS-B", 
               oth =  list(intercept = intercept,
                           slope = slope,
                           cap = cap,
                           ph = ph,
                           pl = pl,
                           alpha = alpha))
  
  qm <- opt$par
  pp <- get_pp(q = qm, cap = cap, ph = ph, pl = pl, alpha = alpha)
  
  if(near(qm, cap, 0.001)){
   pp <- get_demand(intercept = intercept, slope = slope, q = qm)
  }

  
  tibble(qp = qm,
         pp = pp)
}






##########################################################################################################
# SIMULATE DATA ##########################################################################################
##########################################################################################################

n <- 200
periods <- 5
set.seed(10)
prices <- p_market + rnorm(n = periods, mean = 0, sd = 6)
caps <- sample(seq(0.3, 0.7, by = 0.1), size = periods, replace = F)

price_tbl <- tibble(period = 1:periods,
                    cap = caps * 80000,
                    ph = prices)

data <- tibble(q = seq(1, 3e5, by = 100)) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 2,
         pm = ifelse(q <= cap, pl, ph)) %>% 
  group_by(ph, cap, period) %>% 
  mutate(pa = cumsum(pm) / 1:length(q),
         pp = (0.7 * pm) + (0.3 * pa)) %>% 
  ungroup()

demands <- tibble(id = 1:n,
                  sub = sample(c(T, F), size = n, replace = T),
                  slope = -5884,
                  intercept = 3.4e5,
                  error = rnorm(n = n, mean = 0, sd = 5e4),
                  int = intercept + error) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 2,
         qu = int + (slope * ph))

# demands_rcap <- demands %>% 
#   mutate(cap = cap + rnorm(cap, mean = 0, sd = 2),
#          marginal_subsidy = pmap(.l = list(intercept = int, slope = slope, cap = cap, ph = ph, pl = pl),
#                                  pm_optim_wraper),
#          average_subsidy = pmap(.l = list(int, slope, cap, ph, pl),
#                                 pa_optim_wraper),
#          perceived_subsidy = pmap(.l = list(int, slope, cap, ph, pl, alpha = 0.7),
#                                   pp_optim_wraper)) %>% 
#   unnest(c(marginal_subsidy, average_subsidy, perceived_subsidy)) %>% 
#   filter(qm > 0, qu > 0, qp > 0)

demands_fcap <- demands %>% 
  mutate(marginal_subsidy = pmap(.l = list(intercept = int, slope = slope, cap = cap, ph = ph, pl = pl),
                                 pm_optim_wraper),
         average_subsidy = pmap(.l = list(int, slope, cap, ph, pl),
                                pa_optim_wraper),
         perceived_subsidy = pmap(.l = list(int, slope, cap, ph, pl, alpha = 0.7),
                                  pp_optim_wraper)) %>% 
  unnest(c(marginal_subsidy, average_subsidy, perceived_subsidy)) %>% 
  filter(qm > 0, qu > 0, qp > 0,
         pa <= pm,
         pp <= pm)

# Plot
ggplot(data = data, aes(x = q, group = period)) +
  geom_step(aes(y = pm), direction = "vh") +
  geom_line(aes(y = pa)) +
  geom_line(aes(y = pp)) +
  geom_abline(data = demands_fcap, aes(intercept = -int / slope, slope = 1/slope)) +
  geom_point(data = demands_fcap, aes(x = qm, y = pm), fill = "red") +
  geom_point(data = demands_fcap, aes(x = qa, y = pa), fill = "blue") +
  geom_point(data = demands_fcap, aes(x = qp, y = pp), fill = "green") +
  lims(y = c(0, 50),
       x = c(0, 3e5)) +
  facet_wrap(~period, scales = "free") +
  geom_vline(aes(xintercept = cap), linetype = "dashed")

d <- demands_fcap_vcon %>%
  mutate(phi = cap / (qp),
         D = phi < 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = phi * R  * D)  %>% 
  mutate(o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  add_count(id) %>% 
  filter(n > 1)


# Average vs Marginal above the kink

ggplot(data = d, aes(x = pm, y = pa)) +
  geom_point() +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0)


ggplot(data = d, aes(x = qu, y = ph)) + 
  geom_point() +
  geom_line(aes(group = id)) +
  geom_rug()

ggplot(data = d, aes(x = qm, y = pm)) + 
  geom_point() +
  geom_line(aes(group = id)) +
  geom_rug()

ggplot(data = d, aes(x = qa, y = pa)) + 
  geom_point() +
  geom_line(aes(group = id)) +
  geom_rug()

ggplot(data = d, aes(x = qp, y = pp)) + 
  geom_point() +
  geom_line(aes(group = id)) +
  geom_rug()

t1 <- ggplot(d, aes(x = term1, y = qp)) +
  geom_path(aes(color = id))

t2 <- ggplot(d, aes(x = term2, y = qp)) +
  geom_path(aes(color = id))


dep_vars <- c("qu", "qm", "qa", "qp")
indep_vars <- c("pm", "pa", "term1 + term2")


mdls <- expand_grid(dep_vars, indep_vars) %>% 
  mutate(fml = paste(dep_vars, "~", indep_vars, " | id"),
         model = map(fml, my_feols, data = d)) %>% 
  filter(dep_vars == "qp")

a <- mdls$model
names(a) <- mdls$dep_vars

alphs <- c("weight on marginal", map_dbl(a, get_alpha)) %>%  
  magrittr::set_names(mdls$dep_vars) %>% 
  t() %>% 
  as.data.frame()

modelsummary(a, stars = T, statistic = "std.error",
             gof_omit = "IC|Adj|Ps|Wi|L|Std",
             add_rows = alphs,
             statistic_vertical = F)


mdls$model %>% 
  map_dfr(broom::tidy) %>% 
  filter(str_detect(term, "p|1")) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_hline(yintercept = -5884, linetype = "dashed") 























my_feols <- function(fml, data){
  fml <- as.formula(fml)
  feols(fml, data)
}

cowplot::plot_grid(t1, t2)

mdl1 <- fixest::feols(qp ~ term1 + term2 | id, data = d)

get_alpha(mdl)

alphs <- c("weight on marginal", map_dbl(mods, get_alpha)) %>% 
  magrittr::set_names(c("a", "b", "c")) %>% 
  t() %>% 
  as.data.frame()

modelsummary(mdl1,
             gof_omit = "IC|Adj|Ps|Wi|L|Nu|Std")

mods %>% 
  map_dfr(fixef, .id = "model") %>% 
  rename(int = vessel) %>% 
  mutate(vessel = c("A", "B", "C", "A", "C"),
         model = paste0("Model", model)) %>% 
  spread(model, int, fill = 0) %>% 
  knitr::kable(format = "pipe")


##########################################################################################################
# Demands ################################################################################################
##########################################################################################################

get_alpha <- function(model){
  beta <- coefficients(model)["term1"]
  theta <- coefficients(model)["term2"]
  
  alpha <- round(1 + (theta/beta), 3)
  return(alpha)
}

demands_fcap_fcon <- demands_fcap %>% 
  filter(qp >= 0,
         qu >= 0) %>% 
  mutate(phi = cap / (qp),
         D = phi <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = phi * R  * D)  %>% 
  mutate(o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  # filter(D) %>%
  add_count(id) %>% 
  filter(n > 1) %>% 
  group_by(id) %>% 
  mutate(all = all(D),
         all2 = all(!D),
         wrong = !all & !all2) %>% 
  filter(!wrong) %>% 
  filter(!near(cap, qp, 0.5))

demands_fcap_vcon <- demands_fcap %>% 
  filter(qp >= 0,
         qu >= 0) %>% 
  mutate(qp = qp + rnorm(qp, 0, 1000),
         phi = cap / (qp),
         D = phi <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = phi * R  * D) %>% 
  mutate(o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  # filter(D) %>%
  add_count(id) %>% 
  filter(n > 1) %>% 
  # group_by(id) %>% 
  # mutate(all = all(D),
         # all2 = all(!D),
         # wrong = !all & !all2) %>% 
  # filter(!wrong) #%>% 
  filter(id %in% unique(demands_fcap_fcon$id))

demands_rcap_fcon <- demands_rcap %>% 
  filter(qp >= 0,
         qu >= 0) %>% 
  mutate(phi = cap / qp,
         D = phi <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = phi * R  * D)  %>% 
  mutate(o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  # filter(D) %>%
  add_count(id) %>% 
  filter(n > 1) %>% 
  # group_by(id) %>% 
  # mutate(all = all(D),
         # all2 = all(!D),
         # wrong = !all & !all2) %>% 
  # filter(!wrong)# %>% 
  filter(id %in% unique(demands_fcap_fcon$id))

demands_rcap_vcon <- demands_rcap %>% 
  filter(qp >= 0,
         qu >= 0) %>% 
  mutate(qp = qp + rnorm(qp, 0, 0.001),
         phi = cap / (qp),
         D = phi <= 1L,
         R = ph - pl,
         term1 = pl + (D * R),
         term2 = phi * R  * D) %>% 
  mutate(o_term1 = (pl * (1 - D)) + (ph * D),
         o_term2 = D * phi * (pl - ph)) %>% 
  # filter(D) %>%
  add_count(id) %>% 
  filter(n > 1)  %>% 
  # group_by(id) %>% 
  # mutate(all = all(D),
         # all2 = all(!D),
         # wrong = !all & !all2) %>% 
  # filter(!wrong) #%>% 
  filter(id %in% unique(demands_fcap_fcon$id))

dat <- tibble(source = c("rcap_fcon", "rcap_vcon", "fcap_fcon", "fcap_vcon"),
               data = list(demands_rcap_fcon, demands_rcap_vcon, demands_fcap_fcon, demands_fcap_vcon)) %>% 
  unnest(data) %>% 
  filter(!near(cap, qp, 0.5))


ggplot(dat, aes(x = term1, y = qp)) +
  # geom_point() +
  geom_path(aes(group = id, color = factor(wrong)), size = 0.1) +
  facet_wrap(~source) #+
  # lims(y = c(9, 17),
       # x = c(0, NA)) +
  # geom_vline(aes(xintercept = cap))
  
ggplot(dat, aes(x = term2, y = qp)) +
  # geom_point() +
  geom_path(aes(group = id, color = factor(wrong)), size = 0.1) +
  facet_wrap(~source) #+


# 
# 
# 
# 
# model1 <- fixest::feols(qp ~ term1 + term2 | factor(id), data = demands2)
# model2 <- fixest::feols(qp ~ o_term1 + o_term2 | factor(id), data = demands2)
# model3 <- fixest::feols(qp ~ term1 + term2 | factor(id), data = demands3)
# model4 <- fixest::feols(qp ~ o_term1 + o_term2 | factor(id), data = demands3)

formulas <- c("qp ~ term1 + term2 | factor(id)",
              "qp ~ o_term1 + o_term2 | factor(id)") %>% 
  map(as.formula)

models <- expand_grid(data = list(demands_fcap_fcon, demands_fcap_vcon, demands_rcap_fcon, demands_rcap_vcon),
                      formula = formulas) %>% 
  mutate(model = map2(formula, data, fixest::feols))

# get_theta <- function(model){
#   beta <- coefficients(model)[1]
#   gamma <- coefficients(model)[2]
#   
#   theta <- 1 - (gamma / beta)
#   return(theta)
# }

# models <- list(model1, model2, model3, model4)

qbar <- c("$\\bar{Q_t} = Q_{t}$", "X", "X", "X", "X", "", "", "", "")
error <- c("$\\epsilon_i \\neq 0$", "", "", "X", "X", "", "", "X", "X")
model <- c("Econometrician", "Chris", "Olivier", "Chris", "Olivier", "Chris", "Olivier", "Chris", "Olivier")
alphas <- c("weight on marginal", map_dbl(models$model, get_alpha))


extra_rows <- rbind(alphas, qbar, error, model) %>% 
  as_tibble()

attr(extra_rows, 'position') <- c(5, 8, 9, 10, 11)

models %>% 
  pull(model) %>% 
  modelsummary::modelsummary(stars = T, 
                             gof_omit = "IC|Adj|Ps|Wi|L|Nu",
                             add_rows = extra_rows,
                             coef_map = c(term1 = "term1",
                                          term2 = "term2",
                                          o_term1 = "term1",
                                          o_term2 = "term2"),
                             notes = "By design, term1 has a slope of -25 and the weight on marginal is 0.7.")

models %>% 
  pull(model) %>% 
  map_dfr(broom::tidy, .id = "model") %>% 
  select(model, term, estimate, std.error) %>% 
  mutate(term = str_remove(term, "o_")) %>% 
  ggplot(aes(x = model, y = estimate, ymin = estimate-std.error, ymax = estimate+std.error)) +
  facet_wrap(~term, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange()

bmarks <- tibble(term = c("alpha", "beta"), val = c(0.7, -25))
  
models %>% 
  pull(model) %>% 
  map_dfr(broom::tidy, .id = "model") %>% 
  mutate(term = str_remove(term, "o_")) %>% 
  select(model, term, estimate, std.error) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, std.error)) %>% 
  mutate(estimate_alpha = round(1 + (abs(estimate_term2) / estimate_term1), 3)) %>% 
  pivot_longer(cols = c(estimate_term1, estimate_term2, estimate_alpha, std.error_term1, std.error_term2),
               names_to = c(".value", "term"),
               names_pattern = "(.+)_(.+)") %>% 
  filter(term %in% c("term1", "alpha")) %>% 
  mutate(term = ifelse(term == "term1", "beta", term)) %>% 
  ggplot(aes(x = model, y = estimate, ymin = estimate-std.error, ymax = estimate+std.error)) +
  facet_wrap(~term, scales = "free") +
  geom_hline(data = bmarks, aes(yintercept = val), linetype = "dashed") +
  geom_pointrange()

