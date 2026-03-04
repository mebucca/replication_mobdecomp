# ==============================================================================
# SETTINGS & CONSTANTS
# ==============================================================================

c_up   <- "#1f77b4"
c_imm  <- "#ff7f0e"
c_down <- "#2ca02c"
delta  <- log(1.15)

# ==============================================================================
# FUNCTIONS
# ==============================================================================

# Calculate conditional normal distribution parameters
cond_dist <- function(x0, alpha, beta, sigma_func) {
  mu  <- alpha + beta * x0
  s   <- sigma_func(x0)
  ys  <- seq(mu - 3*s, mu + 3*s, length.out = 400)
  pdf <- dnorm(ys, mu, s)
  list(ys = ys, pdf = pdf, mu = mu, s = s)
}

# Assemble all dataframes for plotting
build_all_data <- function(params) {
  reg_list   <- list()
  diag_list  <- list()
  band_list  <- list()
  curve_list <- list()
  poly_list  <- list()
  vline_list <- list()
  
  scale <- 1  # horizontal stretching of conditional densities
  
  for (i in seq_len(nrow(params))) {
    g     <- params$group[i]
    alpha <- params$alpha[i]
    beta  <- params$beta[i]
    sigma <- params$sigma_fun[[i]]
    floor <- params$floor[[i]]
    
    # Regression line
    reg_list[[i]] <- data.frame(
      group = g,
      x     = x_vals,
      y     = alpha + beta * x_vals
    )
    
    # 45-degree line
    diag_list[[i]] <- data.frame(
      group = g,
      x     = x_vals,
      y     = y45
    )
    
    # Immobility band
    band_list[[i]] <- data.frame(
      group = g,
      x     = x_vals,
      ymin  = y45 - delta,
      ymax  = y45 + delta
    )
    
    # Vertical reference lines
    vline_list[[i]] <- data.frame(
      group = g,
      x     = x_points
    )
    
    # Conditional Density at each x0
    for (x0 in x_points) {
      cd <- cond_dist(x0, alpha, beta, sigma)
      
      # One-sided conditional distribution (normal curve)
      x_curve <- x0 + cd$pdf * scale
      
      curve_list[[length(curve_list) + 1]] <- data.frame(
        group = g,
        x     = x_curve,
        y     = cd$ys,
        x0    = x0
      )
      
      # Classification boundaries
      y45_at_x0 <- x0 + floor
      up        <- cd$ys >  y45_at_x0 + delta
      down      <- cd$ys <  y45_at_x0 - delta
      mid       <- cd$ys >= y45_at_x0 - delta & cd$ys <= y45_at_x0 + delta
      
      # Helper for constructing ribbons under the curve
      make_poly <- function(mask, region, col) {
        if (!any(mask)) return(NULL)
        y_seg <- cd$ys[mask]
        x_seg <- x_curve[mask]
        ord   <- order(y_seg)
        
        data.frame(
          group    = g,
          region   = region,
          x0       = x0,
          x        = c(x_seg[ord], rev(rep(x0, length(y_seg)))),
          y        = c(y_seg[ord], rev(y_seg[ord])),
          fill_col = col
        )
      }
      
      poly_list <- append(poly_list, list(make_poly(up,   "up",   c_up)))
      poly_list <- append(poly_list, list(make_poly(mid,  "mid",  c_imm)))
      poly_list <- append(poly_list, list(make_poly(down, "down", c_down)))
    }
  }
  
  list(
    reg    = dplyr::bind_rows(reg_list),
    diag   = dplyr::bind_rows(diag_list),
    band   = dplyr::bind_rows(band_list),
    curves = dplyr::bind_rows(curve_list),
    polys  = dplyr::bind_rows(purrr::compact(poly_list)),
    vlines = dplyr::bind_rows(vline_list)
  )
}

# Main Plotting Function
plot_facet <- function(all, params, df_xdens) {
  annot_df <- params |>
    dplyr::mutate(label = sprintf("α == %.2f *',' ~~ β == %.2f", alpha, beta)) |>
    dplyr::select(group, label)
  
  ggplot() +
    # 1) Polygons
    geom_polygon(
      data = all$polys,
      aes(x=x, y=y, group=interaction(group, region, x0), fill=fill_col),
      alpha = 0.85
    ) +
    scale_fill_identity(aesthetics="fill") +
    # 2) Side Density
    geom_xsidedensity(
      data = df_xdens,
      aes(x = x_plot, y = after_stat(scaled*0.8)),
      fill = "gray40", alpha = 0.6, trim = TRUE
    ) +
    # 3) Regression Line
    geom_line(
      data = all$reg,
      aes(x=x, y=y),
      color="black", linewidth=0.7
    ) +
    # 4) Immobility Band
    geom_ribbon(
      data = all$band,
      aes(x=x, ymin=ymin, ymax=ymax),
      fill = c_imm, alpha = 0.25
    ) +
    # 5) Grid Lines
    geom_vline(
      data = all$vlines,
      aes(xintercept = x),
      linetype = "dotted", color="gray50", linewidth=.3
    ) +
    # 6) 45-degree line
    geom_line(
      data = all$diag,
      aes(x=x, y=y),
      linetype="dashed", color="black", linewidth=.5
    ) +
    # 7) Conditional Normal Curves
    geom_path(
      data = all$curves,
      aes(x=x, y=y, group=interaction(group, x0)),
      color="black", linewidth=.4
    ) +
    # 8) Annotation
    geom_text(
      data = annot_df,
      aes(x=-Inf, y=Inf, label=label),
      hjust=-0.1, vjust=1.2, size=2.5, parse=TRUE
    ) +
    coord_cartesian(
      xlim = c(min(all$curves$x)-0.5, max(all$curves$x)+0.5),
      ylim = c(min(all$curves$y)-0.5, max(all$curves$y)+0.5),
      expand = FALSE
    ) +
    facet_wrap(~group, nrow=1) + 
    ggside::ggside(scale = "free_y") + 
    labs(x="Parents log permanent income ( x )", y="Children log income ( y )")
}

# ==============================================================================
# EXECUTION: MONTE CARLO SIMULATIONS
# ==============================================================================

set.seed(43252)
N        <- 5000
x_vals   <- seq(lb_mc, ub_mc, length.out = 400)
y45      <- x_vals + floor_mc
x_points <- quantile(x_vals, probs = c(0.1, 0.5, 0.9))

df_xdens_mc <- tibble(
  x      = exp(runif(N, 0, 12)),
  x_plot = log(x) - floor_mc
)

params <- data.frame(
  group  = c("Group A", "Group B"),
  alpha  = parameters_mc$Estimate[1:2],
  beta   = parameters_mc$Estimate[3:4],
  gamma  = parameters_mc$Estimate[5:6],
  lambda = parameters_mc$Estimate[7:8],
  floor  = rep(floor_mc, 2)
)

params$sigma_fun <- list(
  function(x) exp(params$gamma[1] + params$lambda[1]*x),
  function(x) exp(params$gamma[2] + params$lambda[2]*x)
)

all_data   <- build_all_data(params)
final_plot <- plot_facet(all_data, params, df_xdens_mc) + 
  scale_x_continuous(labels = function(x) round(x + floor_mc, 2))

print(final_plot)
ggsave(here("figures", "mobility_curve_montecarlo.png"), 
       plot = final_plot, width = 5, height = 3, dpi = 300, bg = "white")

# ==============================================================================
# EXECUTION: EMPIRICAL WOMEN
# ==============================================================================

x_vals   <- seq(lb_female, ub_female, length.out = 400)
y45      <- x_vals + floor_female
x_points <- percentiles - floor_female

df_xdens_women <- df_real %>%
  filter(male == 0) %>%
  mutate(
    group = if_else(group == 1, "White", "Black"),
    x_plot = log(x) - floor_female
  )

params <- data.frame(
  group  = c("Black", "White"),
  alpha  = parameters_female$Estimate[1:2],
  beta   = parameters_female$Estimate[3:4],
  gamma  = parameters_female$Estimate[5:6],
  lambda = parameters_female$Estimate[7:8],
  floor  = rep(floor_female, 2)
)

params$sigma_fun <- list(
  function(x) exp(params$gamma[1] + params$lambda[1]*x),
  function(x) exp(params$gamma[2] + params$lambda[2]*x)
)

all_data   <- build_all_data(params)
final_plot <- plot_facet(all_data, params, df_xdens_women) + 
  scale_x_continuous(labels = function(x) round(x + floor_female, 2))

print(final_plot)
ggsave(here("figures", "mobility_curve_women.png"), 
       plot = final_plot, width = 5, height = 3, dpi = 300, bg = "white")

# ==============================================================================
# EXECUTION: EMPIRICAL MEN
# ==============================================================================

x_vals   <- seq(lb_male, ub_male, length.out = 400)
y45      <- x_vals + floor_male
x_points <- percentiles - floor_male

df_xdens_male <- df_real %>%
  filter(male == 1) %>%
  mutate(
    group = if_else(group == 1, "White", "Black"),
    x_plot = log(x) - floor_male
  )

params <- data.frame(
  group  = c("Black", "White"),
  alpha  = parameters_male$Estimate[1:2],
  beta   = parameters_male$Estimate[3:4],
  gamma  = parameters_male$Estimate[5:6],
  lambda = parameters_male$Estimate[7:8],
  floor  = rep(floor_male, 2)
)

params$sigma_fun <- list(
  function(x) exp(params$gamma[1] + params$lambda[1]*x),
  function(x) exp(params$gamma[2] + params$lambda[2]*x)
)

all_data   <- build_all_data(params)
final_plot <- plot_facet(all_data, params, df_xdens_male) + 
  scale_x_continuous(labels = function(x) round(x + floor_male, 2))

print(final_plot)
ggsave(here("figures", "mobility_curve_men.png"), 
       plot = final_plot, width = 5, height = 3, dpi = 300, bg = "white")