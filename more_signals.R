library(dplyr)
library(purrr)
library(tidyverse)
p=1

util <- function(belief) {
#  max(belief, 1-belief)
2^(p-1)*abs(belief-0.5)^p+0.5
}

objective_function <- function(k_alpha, k_beta, k_nu, q, e, e_nu) {
  
  total <- (1 - e^k_alpha)*(1 - q) + (1 - e^k_beta)*q
  
  sum_term <- 0
  
  for (k_nuA in 0:k_nu) {
    
    A <- e^k_beta * q * e_nu^(k_nu - k_nuA) * (1 - e_nu)^k_nuA
    B <- e^k_alpha * (1 - q) * e_nu^k_nuA * (1 - e_nu)^(k_nu - k_nuA)
    
    belief <- A/(A+B)
    bin_coeff <- choose(k_nu, k_nuA)
    
    sum_term <- sum_term + bin_coeff *(A+B) *util(belief)
  }
  
  return(total + sum_term)
}


objective_function(1, 0,2, 0.9, 0.8, 0.1)
objective_function(0, 1,2, 0.9, 0.8, 0.1)
objective_function(0, 0,3, 0.9, 0.8, 0.1)
objective_function(2, 0,1, 0.9, 0.8, 0.1)
objective_function(1, 1,1, 0.9, 0.8, 0.1)




# Function to solve the maximization problem
solve_maximization <- function(q, e, e_nu, n) {
  grid <- expand.grid(k_alpha = 0:n, k_beta = 0:n, k_nu = 0:n)
  k_grid <- subset(grid, k_alpha + k_beta + k_nu == n)
  
  # Apply the function row-wise
  k_grid <- k_grid %>%
    mutate(result = pmap_dbl(list(k_alpha, k_beta, k_nu),
                             objective_function, q = q, e = e, e_nu = e_nu)) %>%
  arrange(-result) %>% slice(1) %>% select(k_alpha, k_beta, k_nu)
  letters <- paste0(paste0(rep("α", as.numeric(k_grid[1,1])), collapse = ""), 
                    paste0(rep("ν", as.numeric(k_grid[1,3])), collapse = ""),
                    paste0(rep("β", as.numeric(k_grid[1,2])), collapse = ""), 
                    collapse = "")
  
  
  return(letters)
}

e_values <- seq(0.05, 0.95, by = 0.1)  # Adjust this range as needed
e_nu_values <- seq(0.05, 0.45, by = 0.1)  # Adjust this range as needed
q_values <- seq(0.01, 0.99, by = 0.01)
# Use expand.grid to generate all combinations of e and e_nu
results <- expand.grid(e = e_values, e_nu = e_nu_values, q=q_values)

# Apply solve_maximization and translate_into_letters to each row
results <- results %>%
  rowwise() %>%
  mutate(
    result = (solve_maximization(q = q, e = e, e_nu = e_nu, n = 5)),
  )

# Create a custom labeller function for the facets
custom_labeller <- function(variable, value) {
  if (variable == "e") {
    return(paste(value))
  } else if (variable == "e_ν") {
    return(paste(value))
  } else {
    return(value)
  }
}
 levels_order <- c("βββββ","ανννν","ννννν","ννννβ","ααααα")
results$result <- factor(results$result, levels = levels_order)

# Plot the results using ggplot2
ggplot(results, aes(x = q, group= result, fill=result)) +
  geom_histogram(binwidth = 0.01,  alpha = 0.7) +
  facet_grid(rows=vars(e), cols=vars((e_nu)), labeller = custom_labeller, switch="y" ) + 
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(fill="Optimal source choice") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +  # Format x-axis labels to 2 decimal places
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.line.y = element_blank() ,   # Remove y-axis line
    panel.spacing = unit(1, "lines")  # Add padding between panels (columns and rows)
  )+
  theme(text = element_text(size = 17),strip.text = element_text(
    size = 11),     strip.background = element_blank()  # Remove background of facet names
  ) 