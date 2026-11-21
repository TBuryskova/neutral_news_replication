# Load ggplot2
library(tidyverse)
library(ggtern)
library(plotly)
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)


set.seed(1997)

########two optimal source choice#############
q <- seq(0.01, 0.99, by = 0.01)
n <- 2
q_repeated <- rep(q, each = n)
e_values <- seq(0.05, 0.95, by = 0.1)
e_ν_values <- seq(0.05, 0.45, by = 0.1)

# Initialize an empty dataframe to store the results
big_data_repeated <- data.frame()
# Loop over all combinations of e and e_ν
for (e in e_values) {
  for (e_ν in e_ν_values) {
    
    # Compute the functions for the current e and e_ν
    f_αα <- pmax(q, e^2 * (1 - q)) + (1 - q - e^2 * (1 - q))
    f_αβ <- e * pmax(q, 1 - q) + (1 - e)
    f_ββ <- pmax(e^2 * q, 1 - q) + (1 - e^2 * q - (1 - q))
    f_νν <- pmax((1 - e_ν)^2 * q, e_ν^2 * (1 - q)) +
      2 * (1 - e_ν) * e_ν * pmax(q, 1 - q) +
      pmax(e_ν^2 * q, (1 - e_ν)^2 * (1 - q))
    
    f_αν <- pmax((1 - e_ν) * q, e * e_ν * (1 - q)) +
      pmax(e_ν * q, e * (1 - e_ν) * (1 - q)) +
      (1 - e) * e_ν * (1 - q) +
      (1 - e_ν) * (1 - e) * (1 - q)
    
    f_βν <- (1 - e) * (1 - e_ν) * q +
      (1 - e) * e_ν * q +
      pmax(e * (1 - e_ν) * q, (1 - q) * e_ν) +
      pmax(e * e_ν * q, (1 - q) * (1 - e_ν))
    
    # Repeat the values for plotting
    f_αα_repeated <- rep(f_αα, each = n)
    f_αβ_repeated <- rep(f_αβ, each = n)
    f_ββ_repeated <- rep(f_ββ, each = n)
    f_αν_repeated <- rep(f_αν, each = n)
    f_νν_repeated <- rep(f_νν, each = n)
    f_βν_repeated <- rep(f_βν, each = n)
    
    # Create a data frame for the current combination of e and e_ν
    data_repeated <- data.frame(
      q = rep(q, each = n),
      value = c(f_αα_repeated, f_αβ_repeated, f_ββ_repeated, f_αν_repeated, f_νν_repeated, f_βν_repeated),
      Source = rep(c("αα", "αβ", "ββ", "αν", "νν", "βν"), each = n * length(q)),
      e = e,      # Add e and e_ν to the data frame
      e_ν = e_ν
    )
    
    # Bind the current data_repeated to the big dataframe
    big_data_repeated <- rbind(big_data_repeated, data_repeated)
  }
}
levels_order <- c("ββ","αν","νν","αβ","βν","αα")



big_data <- big_data_repeated %>%
  group_by(q, e, e_ν) %>%
  mutate(max_source = Source[which.max(value)]) %>%
  ungroup() 

big_data$max_source <- factor(big_data$max_source, levels = levels_order)


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


ggplot(big_data, aes(x=q, group=max_source, fill=max_source)) +

  geom_histogram(binwidth = 0.01,  alpha= 0.7) +
  facet_grid(rows=vars(e), cols=vars((e_ν)), labeller = custom_labeller, switch="y" ) + 
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(fill="Optimal choice") +
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
  ) +
  theme(text = element_text(size = 17),strip.text = element_text(
    size = 11),     strip.background = element_blank()  # Remove background of facet names
) 