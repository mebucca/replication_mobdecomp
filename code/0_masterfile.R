
# ========================================================== PREAMBLE ============================================================

# Clear Screen 
cat("\014")
rm(list = ls())

# Load Packages 
library("pacman")
p_load(
  "tidyverse",
  "dplyr",
  "ggplot2",
  "ggtext",
  "grid",
  "here",
  "rstan",
  "stargazer",
  "knitr",
  "kableExtra",
  "haven",
  "RcppRoll",
  "posterior",
  "stringr",
  "patchwork",
  "caret",
  "loo",
  "extrafont",
  "cmdstanr",
  "ggside"
  )


options(xtable.floating = FALSE)
options(xtable.timestamp = "")
rstan_options(auto_write = TRUE)


# Set preferences ggplot theme

# Julia-like color palette
scale_color_julia <- function(...) {
  scale_color_manual(
    values = c(
      "#1f77b4", # blue
      "#ff7f0e", # orange
      "#2ca02c", # green
      "#d62728", # red
      "#9467bd", # purple
      "#8c564b", # brown
      "#e377c2", # pink
      "#7f7f7f", # gray
      "#bcbd22", # olive
      "#17becf"  # cyan
    ),
    ...
  )
}

scale_fill_julia <- function(...) {
  scale_fill_manual(
    values = c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
      "#bcbd22", "#17becf"
    ),
    ...
  )
}

# Julia-like theme
theme_julia <-function(base_size = 7.5, base_family = "DejaVu Sans") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Panel and grids
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.4, linetype = "dotted"),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.3, linetype = "dotted"),
      
      # Axes
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"), # short ticks
      axis.text = element_text(color = "black", size = base_size * 0.9),
      axis.title = element_text(color = "black", size = base_size),
      
      # Facets
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(size = base_size, face = "plain"),
      
      # Legend
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "white", linewidth = 0),
      legend.key = element_rect(fill = "white", color = "white"),
      
      # Plot
      plot.title = element_text(size = base_size * 1.1, face = "plain"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

theme_set(theme_julia())
rstan_options(auto_write = TRUE)

n_chains = 4
n_iter   = 2000
target_ratio = 0.5
n_warmup = max(75, floor(n_iter * target_ratio))

# ======================================================== SET DIRECTORIES =======================================================


# Define path to root directory  

folder <- '/Users/mebucca/Library/Mobile Documents/com~apple~CloudDocs/Research/Mobility Decomposition'
code <- here(folder,"code")
data <- here(folder,"data")
figures <- here(folder,"figures")
tables <- here(folder,"tables")

# ===================================================== MONTE CARLO EXPERIMENT  ===================================================


# DPG plus Baseyian estimation
source(here("code","1_montecarlo.R"))

# Empirical Case
source(here("code","2_empiricalcase.R"))

# Plot curves
source(here("code","3_plotcurves.R"))
