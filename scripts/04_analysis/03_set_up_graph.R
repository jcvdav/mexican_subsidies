
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

demands <- tibble(id = LETTERS[1:n],
                  slope = -50,
                  int = c(630, 750)) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 1,
         qu = int + (slope * ph),
         ql = int + (slope * pl))

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
(
  plot <- ggplot(data = demands_fcap, aes(x = q)) +
    geom_abline(aes(intercept = -int / slope, slope = 1/slope, color = id)) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "None") +
    labs(x = "Fuel consumption (L)", y = "Fuel price ($/L)") +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0), labels = NULL, breaks = NULL) +
    scale_y_continuous(limits = c(10, 13), labels = NULL, breaks = NULL, expand = c(0, 0)) +
    geom_hline(yintercept = p_market, linetype = "dashed") +
    geom_text(x = 20, y = 12.6, label = "Market price")
)

(
  p2 <- plot +
    geom_point(aes(x = qu, y = ph), fill = "black") +
    geom_segment(aes(x = qu, xend = qu, y = 10, yend = ph), linetype = "dashed") +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(Q[au]), expression(Q[bu])), breaks = c(5, 125))
)

(p3 <- p2 +
    geom_hline(yintercept = p_market - 1, linetype = "dashed") +
    geom_text(x = 25, y = 11.6, label = "Subsidized price")
)

(
  p4 <- p3 +
    geom_step(data = data, aes(y = pm), direction = "vh", size = 1) +
    geom_vline(xintercept = caps, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(Q[au]), expression(bar(Q)), expression(Q[bu])), breaks = c(5, caps, 125))
)

(
  p5 <- p4 +
    geom_point(aes(x = qm, y = pm), fill = "black") +
    geom_segment(aes(x = qm, xend = qm, y = 10, yend = pm), linetype = "dashed") +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(Q[au]), expression(Q[as]) ,expression(bar(Q)), expression(Q[bu]~"="~Q[bs])),
                       breaks = c(5, 55, caps, 125))
)

(
  p6 <- demands_fcap %>% 
    filter(int == max(int)) %>% 
    ggplot(aes(x = q)) +
    geom_abline(aes(intercept = -int / slope, slope = 1/slope), color = "steelblue") +
    theme(legend.position = "None") +
    labs(x = "Fuel consumption (L)", y = "Fuel price ($/L)") +
    geom_step(data = data, aes(y = pm), direction = "vh", size = 1) +
    geom_vline(xintercept = caps, linetype = "dashed") +
    geom_point(aes(x = qm, y = pm), fill = "black") +
    geom_segment(aes(x = qm, xend = qm, y = 10, yend = pm)) +
    scale_y_continuous(limits = c(10, 13), labels = NULL, breaks = NULL, expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(bar(Q)), expression(Q[bu]), expression(Q[ba])), breaks = c(75, 125, 150)) +
    geom_line(data = data, aes(y = pa), size = 1, linetype = "dotted") +
    geom_point(aes(x = qa, y = pa), fill = "black") +
    geom_segment(aes(x = qa, xend = qa, y = 10, yend = pa), linetype = "dotted")
)

(
  p7 <- p6 +
    geom_line(data = data, aes(y = pp), size = 1, linetype = "dashed") +
    geom_point(aes(x = qp, y = pp), fill = "black") +
    geom_segment(aes(x = qp, xend = qp, y = 10, yend = pp), linetype = "dashed") +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(bar(Q)), expression(Q[bu]), expression(Q[bp]), expression(Q[ba])),
                       breaks = c(75, 125, 139, 150))
  )

(
  p8 <- ggplot(data = demands_fcap, aes(x = q)) +
    geom_abline(aes(intercept = -int / slope, slope = 1/slope, color = id)) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "None") +
    labs(x = "Fuel consumption (L)", y = "Fuel price ($/L)") +
    geom_step(data = data, aes(y = pm), direction = "vh", size = 1) +
    geom_vline(xintercept = caps, linetype = "dashed") +
    geom_segment(aes(x = qu, xend = qu, y = 10, yend = ph)) +
    geom_point(aes(x = qu, y = ph), fill = "white") +
    scale_y_continuous(limits = c(10, 13), labels = NULL, breaks = NULL, expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0),
                       labels = c(expression(Q[au]), expression(Q[as]), expression(bar(Q)), expression(Q[bu]~"="~Q[bs]), expression(Q[ba])),
                       breaks = c(5, 55, 75, 125, 150)) +
    geom_line(data = data, aes(y = pa), size = 1, linetype = "dotted") +
    geom_point(aes(x = qa, y = pa), fill = "black") +
    geom_segment(aes(x = qa, xend = qa, y = 10, yend = pa), linetype = "dotted") +
    geom_segment(aes(x = qu, xend = qa, y = 10.1, yend = 10.1, group = id),
                 arrow = arrow(lengt = unit(10, "pt"), ends = "first")) +
    geom_text(x = 26, y = 10.35, label = "Additional\nconsumption by A") +
    geom_text(x = 140, y = 10.35, label = "Additional\nconsumption by B")
)

ggsave(plot = plot,
       filename = file.path(project_path, "results", "figures", "mex_prog1.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p2,
       filename = file.path(project_path, "results", "figures", "mex_prog2.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p3,
       filename = file.path(project_path, "results", "figures", "mex_prog3.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p4,
       filename = file.path(project_path, "results", "figures", "mex_prog4.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p5,
       filename = file.path(project_path, "results", "figures", "mex_prog5.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p6,
       filename = file.path(project_path, "results", "figures", "mex_prog6.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p7,
       filename = file.path(project_path, "results", "figures", "mex_prog7.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p8,
       filename = file.path(project_path, "results", "figures", "mex_prog8.png"),
       width = 5,
       height = 3,
       units = "in")




