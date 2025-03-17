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
pacman::p_load(
  here,
  tidyverse
)

# Build data -------------------------------------------------------------------
data <- 
  tibble(x = 0:10 * 100,
         y = 10 - 0.01 * x)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
(base <- ggplot(data = data,
                mapping = aes(x = x,
                              y = y)) +
   geom_line() +
   labs(x = "L",
        y = "$/L") +
   theme(axis.line = element_line(colour = "black"),
         axis.title = element_text(hjust = 1),
         panel.grid.major.y = element_blank()) +
   scale_x_continuous(expand = expansion(0.01, 0),
                      breaks = 0) +
   scale_y_continuous(expand = expansion(0.01, 0),
                      breaks = 0)
)

# Show demand for fuel ---------------------------------------------------------
(d1 <- base + 
  geom_hline(yintercept = 8,
             color = "steelblue") +
  scale_y_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 8),
                     labels = c(0, "P*"))
 )

(d2 <- d1 + 
  geom_point(x = 200, y = 8))

(d3 <- d2 +
  geom_segment(x = 200, xend = 200,
               y = 0, yend = 8,
               color = "steelblue") +
  scale_x_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 200),
                     labels = c(0, "L*")))

d3 +
  geom_rect(xmin = 0, ymin = 0,
            xmax = 200, ymax = 8,
            color = "transparent",
            fill = "gray90") +
  geom_point(x = 200, y = 8)

# Show price subsidy for fuel --------------------------------------------------

(s1 <- d3 +
  geom_hline(yintercept = 7,
             color = "red") +
  scale_y_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 8, 7),
                     labels = c(0, "P*", expression(P^s))))

s1 +
  geom_point(x = 300, y = 7,
             fill = "red") + 
  geom_segment(x = 300, xend = 300,
               y = 0, yend = 7,
               color = "red") +
  scale_x_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 200, 300),
                     labels = c(0, "L*", expression(L^s)))


# Show kink subsidy ------------------------------------------------------------

d3

(k1 <- d3 +
    geom_segment(x = 0, xend = 100,
                 y = 7, yend = 7,
                 color = "orange") +
    geom_segment(x = 100, xend = 100,
                 y = 7, yend = 8,
                 color = "orange") +
    geom_segment(x = 100, xend = Inf,
                 y = 8, yend = 8,
                 color = "orange") +
    scale_y_continuous(expand = expansion(0.01, 0),
                       breaks = c(0, 8, 7),
                       labels = c(0, "P*", expression(P^s))))

(k2 <- k1 + geom_point(x = 200, y = 8,
                fill = "orange") + 
  geom_segment(x = 200, xend = 200,
               y = 0, yend = 8,
               color = "orange") +
  scale_x_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 200),
                     labels = c(0, expression("L*"==L^s))))


d1 + geom_rect(xmin = 0, ymin = 6.05,
               xmax = 145, ymax = 7.95,
               color = "transparent",
               fill = "gray50") +
  geom_hline(yintercept = 8,
             color = "steelblue") +
  geom_segment(x = 0, xend = 100,
               y = 6, yend = 6,
               color = "red") +
  geom_segment(x = 145, xend = 145,
               y = 6, yend = 8,
               color = "red") +
  geom_segment(x = 145, xend = 145,
               y = 0, yend = 6,
               color = "red") +
  scale_y_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 8, 6),
                     labels = c(0, "P", expression(P^s))) +
  scale_x_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 145),
                     labels = c(0, expression(L^s)))

# Average ----------------------------------------------------------------------

avg <- tibble(x = 0:1000,
              y = 8 - (1 * 100 / x)) %>% 
  mutate(y = ifelse(x <= 100, 7, y))

k2 + 
  geom_line(data = avg,
            linetype = "dashed",
            color = "gray") + 
  geom_segment(x = 245,
               xend = 245,
               y = 0,
               yend = 7.6,
               color = "gray") +
  annotate(x = 400,
           y = 7.5,
           geom = "text",
           label = expression(P^p)) +
  scale_x_continuous(expand = expansion(0.01, 0),
                     breaks = c(0, 200, 245),
                     labels = c(0, expression("L*"==L^s), expression(L^p)))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------