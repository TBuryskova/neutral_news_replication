library(dplyr)
library(purrr)
library(ggplot2)

# --------------------------------------------------
# Parameters
# --------------------------------------------------

e <- 0.8
e_nu <- 0.3
n <- 2

p_values <- seq(0.2, 2, by = 0.2)

q_values <- seq(0.01, 0.99, by = 0.0005)

# --------------------------------------------------
# Utility
# --------------------------------------------------

util <- function(belief, p) {
  2^(p-1) * abs(belief - 0.5)^p + 0.5
}

# --------------------------------------------------
# Objective function
# --------------------------------------------------

objective_function <- function(k_alpha, k_beta, k_nu,
                               q, e, e_nu, p) {
  
  total <- (1 - e^k_alpha)*(1 - q) +
    (1 - e^k_beta)*q
  
  sum_term <- 0
  
  for (k_nuA in 0:k_nu) {
    
    A <- e^k_beta * q *
      e_nu^(k_nu - k_nuA) *
      (1 - e_nu)^k_nuA
    
    B <- e^k_alpha * (1 - q) *
      e_nu^k_nuA *
      (1 - e_nu)^(k_nu - k_nuA)
    
    belief <- A / (A + B)
    
    sum_term <- sum_term +
      choose(k_nu, k_nuA) *
      (A + B) *
      util(belief, p)
  }
  
  total + sum_term
}

# --------------------------------------------------
# Feasible allocations
# --------------------------------------------------

k_grid <- expand.grid(
  k_alpha = 0:n,
  k_beta  = 0:n,
  k_nu    = 0:n
) %>%
  filter(k_alpha + k_beta + k_nu == n)

# --------------------------------------------------
# Solve maximization
# --------------------------------------------------

solve_maximization <- function(q, e, e_nu, p) {
  
  vals <- pmap_dbl(
    k_grid,
    ~ objective_function(..1, ..2, ..3,
                         q, e, e_nu, p)
  )
  
  best <- k_grid[which.max(vals), ]
  
  paste0(
    strrep("α", best$k_alpha),
    strrep("ν", best$k_nu),
    strrep("β", best$k_beta)
  )
}

# --------------------------------------------------
# Compute optimal choices
# --------------------------------------------------

results <- expand.grid(
  p = p_values,
  q = q_values
)

results$result <- pmap_chr(
  results,
  ~ solve_maximization(
    q = ..2,
    e = e,
    e_nu = e_nu,
    p = ..1
  )
)
levels_order <- c("ββ","αν","νν","νβ","αα")
results$result <- factor(results$result, levels = levels_order)

# --------------------------------------------------
# Plot
# --------------------------------------------------

ggplot(results,
       aes(x = q,
           y = factor(p),
           fill = result)) +
  
  geom_tile(height = 0.9) +
  
  labs(
    x = "q",
    y = "p - utility function parameter"
   
  ) +
  
  scale_x_continuous(
    breaks = c(0,0.25,0.5,0.75,1)
  ) +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(fill="Optimal source choice") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.line.y = element_blank() ,   # Remove y-axis line
  ) 