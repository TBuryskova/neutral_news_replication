# Load ggplot2
library(tidyverse)
library(ggtern)
library(plotly)
library(readr)
library(ggplot2)
set.seed(1997)
e=0.65
e_nu=0.15

  
  # Define the range for q
  q <- seq(0, 1, by = 0.01)
  
  # Define the six functions
  f_αα <- pmax(q, e^2 * (1 - q)) + (1 - q - e^2 * (1 - q))
  f_αβ <- e * pmax(q, 1 - q) + (1 - e)
  f_ββ <- pmax(e^2 * q, 1 - q) + (1 - e^2 * q - (1 - q))
  f_νν <- pmax((1 - e_nu)^2 * q, e_nu^2 * (1 - q)) +
    2 * (1 - e_nu) * e_nu * pmax(q, 1 - q) +
    pmax(e_nu^2 * q, (1 - e_nu)^2 * (1 - q))
  
  f_αν <- pmax((1 - e_nu) * q, e * e_nu * (1 - q)) +
    pmax(e_nu * q, e * (1 - e_nu) * (1 - q)) +
    (1 - e) * e_nu * (1 - q) +
    (1 - e_nu) * (1 - e) * (1 - q)
  
  f_βν <- (1 - e) * (1 - e_nu) * q +
    (1 - e) * e_nu * q +
    pmax(e * (1 - e_nu) * q, (1 - q) * e_nu) +
    pmax(e * e_nu * q, (1 - q) * (1 - e_nu))
  
  # Create a data frame for plotting
  data <- data.frame(q = rep(q, 6),
                     value = c(f_αα, f_αβ, f_ββ, f_αν, f_νν, f_βν),
                     Source = rep(c("αα", "αβ", "ββ", "αν", "νν", "βν"), each = length(q)))
  
  # Ensure Source is a factor with the desired order
  levels_order <- c("αα", "αβ", "ββ", "αν", "νν", "βν")
  data$Source <- factor(data$Source, levels = levels_order)
  
  # Define color and linetype mappings explicitly
  color_mapping <- c("αα" = "blue", "αβ" = "purple", "ββ" = "red", 
                     "νν" = "black", "αν" = "blue", "βν" = "red")
  linetype_mapping <- c("αα" = "solid", "αβ" = "solid", "ββ" = "solid",
                        "νν" = "dotdash", "αν" = "dotdash", "βν" = "dotdash")
  
  # Plot using ggplot
ggplot(data, aes(x = q, y = value, color = Source, linetype = Source)) +
    geom_line(size = 1) +
    labs(x = "q", y = "Expected utility") +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping)
  




