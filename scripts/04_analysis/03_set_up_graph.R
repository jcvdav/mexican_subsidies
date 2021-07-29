
library(modelsummary)
library(cowplot)
library(tidyverse)


# Define some basic variables
p_market <- 12.5
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
  opt <- optim(par = cap,
               fn = pa_opt_fun,
               lower = 0,
               upper = 1000,
               method = "L-BFGS-B",
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
               upper = 1000,
               method = "L-BFGS-B", 
               oth =  list(intercept = intercept,
                           slope = slope,
                           cap = cap,
                           ph = ph,
                           pl = pl,
                           alpha = alpha))
  
  qm <- opt$par
  pp <- get_pp(q = qm, cap = cap, ph = ph, pl = pl, alpha = alpha)
  
  if(near(qm, cap, 0.01)){
    pp <- get_demand(intercept = intercept, slope = slope, q = qm)
  }
  
  
  tibble(qp = qm,
         pp = pp)
}






##########################################################################################################
# SIMULATE DATA ##########################################################################################
##########################################################################################################

n <- 2
periods <- 1
prices <- p_market
caps <- 75 

price_tbl <- tibble(period = 1:periods,
                    cap = caps,
                    ph = prices)

data <- tibble(q = seq(1, 200, by = 1)) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 1,
         pm = ifelse(q <= cap, pl, ph)) %>% 
  group_by(ph, cap, period) %>% 
  mutate(pa = cumsum(pm) / 1:length(q),
         pp = (0.5 * pm) + (0.5 * pa)) %>% 
  ungroup()

demands <- tibble(id = 1:n,
                  slope = -50,
                  int = c(626, 750)) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 1,
         qu = int + (slope * ph))

demands_fcap <- demands %>% 
  # filter(id < 3) %>%
  mutate(marginal_subsidy = pmap(.l = list(intercept = int, slope = slope, cap = cap, ph = ph, pl = pl),
                                 pm_optim_wraper),
         average_subsidy = pmap(.l = list(int, slope, cap, ph, pl),
                                pa_optim_wraper),
         perceived_subsidy = pmap(.l = list(int, slope, cap, ph, pl, alpha = 0.5),
                                  pp_optim_wraper)) %>% 
  unnest(c(marginal_subsidy, average_subsidy, perceived_subsidy)) %>% 
  filter(qm > 0, qu > 0, qp > 0)

# Plot
plot <- ggplot(data = data, aes(x = q, group = period)) +
  geom_step(aes(y = pm), direction = "vh") +
  geom_line(aes(y = pa), linetype = "dashed") +
  geom_line(aes(y = pp), linetype = "dotted") +
  geom_hline(yintercept = p_market, linetype = "dashed") +
  geom_abline(data = demands_fcap, aes(intercept = -int / slope, slope = 1/slope)) +
  geom_point(data = demands_fcap, aes(x = qa, y = pa), fill = "blue") +
  geom_point(data = demands_fcap, aes(x = qp, y = pp), fill = "green") +
  geom_point(data = demands_fcap, aes(x = qm, y = pm), fill = "red") +
  geom_vline(aes(xintercept = cap), linetype = "dashed") +
  labs(x = "Fuel consumption (L)", y = "Fuel price ($/L)") +
  scale_x_continuous(limits = c(0, 200),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(10.5, 13), labels = NULL, expand = c(0, 0)) +
  geom_segment(x = 1, xend = 1, y = 0, yend = p_market, linetype = "dashed") +
  geom_segment(x = 51, xend = 51, y = 0, yend = p_market - 1, linetype = "dashed") +
  geom_segment(x = 125, xend = 125, y = 0, yend = p_market, linetype = "dashed") +
  geom_segment(x = 139, xend = 139, y = 0, yend = 12.2, linetype = "dashed") +
  geom_segment(x = 150, xend = 150, y = 0, yend = 12, linetype = "dashed")


ggsave(plot = plot,
       filename = file.path(project_path, "results", "figures", "vanilla_POM.png"),
       width = 6,
       height = 4,
       units = "in")




