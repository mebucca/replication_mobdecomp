# Full Pipeline: PSID Data Analysis of Intergenerational Mobility

# ------------------------------------------------------------------------------
# Helper function: Rolling window average
# ------------------------------------------------------------------------------

span <- 7  # Window size for smoothing income trajectories

# Centered rolling mean with NA padding at edges
rolling <- function(x, y) {
  roll_mean(x, n = y, align = "center", fill = NA, na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# Load raw PSID data
# ------------------------------------------------------------------------------

load(here("data", "myPSID.RData"))
data_full <- myPSID

# ==============================================================================
# Construct income trajectories for fathers, mothers, and children
# ==============================================================================
# We compute permanent income using rolling 7-year averages to smooth out
# transitory fluctuations. This approach requires at least 5 non-missing
# observations within each 7-year window.

# ------------------------------------------------------------------------------
# Father's income trajectory
# ------------------------------------------------------------------------------

father_income_at <- data_full %>%
  dplyr::select(F_ID, M_ID, F_Cohort_Individual, starts_with("F_Iadj_Y_")) %>%
  unite(Parents_ID, c("F_ID", "M_ID"), sep = "", remove = FALSE) %>%
  group_by(Parents_ID) %>%
  mutate(noparents = if_else(is.na(F_ID) & is.na(M_ID), 1L, 0L)) %>%
  filter(noparents == 0) %>%
  
  # Reshape from wide to long format (one row per person-year)
  pivot_longer(
    cols         = starts_with("F_Iadj_Y_"),
    names_prefix = "F_Iadj_Y_",
    names_to     = "Year",
    values_to    = "F_Iadj_Y"
  ) %>%
  mutate(
    Year      = as.numeric(Year),
    Age_at_F  = Year - F_Cohort_Individual - 1,  # Calculate father's age
    F_Iadj_Y  = if_else(F_Iadj_Y == 0, NA_real_, F_Iadj_Y)  # Treat zeros as missing
  ) %>%
  
  # Average within parent-year-age cells (handles duplicates)
  group_by(Parents_ID, Year, Age_at_F) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(Parents_ID, Year) %>%
  
  # Apply rolling window to create permanent income measure
  group_by(Parents_ID) %>%
  mutate(
    nonmissing_y  = as.integer(!is.na(F_Iadj_Y)),
    unique_y      = span * rolling(nonmissing_y, span),  # Count non-missing in window
    F_Iadj_Y_avg  = if_else(unique_y >= 5, rolling(F_Iadj_Y, span), NA_real_)  # Require ≥5 obs
  ) %>%
  ungroup() %>%
  dplyr::select(-nonmissing_y, -unique_y, -noparents)

# ------------------------------------------------------------------------------
# Mother's income trajectory
# ------------------------------------------------------------------------------

mother_income_at <- data_full %>%
  dplyr::select(M_ID, F_ID, M_Cohort_Individual, starts_with("M_Iadj_Y_")) %>%
  unite(Parents_ID, c("F_ID", "M_ID"), sep = "", remove = FALSE) %>%
  group_by(Parents_ID) %>%
  mutate(noparents = if_else(is.na(F_ID) & is.na(M_ID), 1L, 0L)) %>%
  filter(noparents == 0) %>%
  pivot_longer(
    cols         = starts_with("M_Iadj_Y_"),
    names_prefix = "M_Iadj_Y_",
    names_to     = "Year",
    values_to    = "M_Iadj_Y"
  ) %>%
  mutate(
    Year      = as.numeric(Year),
    Age_at_M  = Year - M_Cohort_Individual - 1,
    M_Iadj_Y  = if_else(M_Iadj_Y == 0, NA_real_, M_Iadj_Y)
  ) %>%
  group_by(Parents_ID, Year, Age_at_M) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(Parents_ID, Year) %>%
  group_by(Parents_ID) %>%
  mutate(
    nonmissing_y  = as.integer(!is.na(M_Iadj_Y)),
    unique_y      = span * rolling(nonmissing_y, span),
    M_Iadj_Y_avg  = if_else(unique_y >= 5, rolling(M_Iadj_Y, span), NA_real_)
  ) %>%
  ungroup() %>%
  dplyr::select(-nonmissing_y, -unique_y, -noparents)

# ------------------------------------------------------------------------------
# Combined parental income (father + mother)
# ------------------------------------------------------------------------------

parents_income_at <- father_income_at %>%
  dplyr::select(F_ID, Parents_ID, Year, Age_at_F, F_Iadj_Y) %>%
  full_join(
    mother_income_at %>%
      dplyr::select(M_ID, Parents_ID, Year, Age_at_M, M_Iadj_Y),
    by = c("Parents_ID", "Year")
  ) %>%
  
  # Sum father and mother income (household total)
  mutate(
    P_Iadj_Y = F_Iadj_Y + M_Iadj_Y,
    # Handle NA propagation: if sum is zero but one parent is missing, set to NA
    P_Iadj_Y = if_else(
      P_Iadj_Y == 0 & (is.na(F_Iadj_Y) | is.na(M_Iadj_Y)),
      NA_real_,
      P_Iadj_Y
    )
  ) %>%
  arrange(Parents_ID, Year) %>%
  
  # Apply rolling average to combined parental income
  group_by(Parents_ID) %>%
  mutate(
    nonmissing_y  = as.integer(!is.na(P_Iadj_Y)),
    unique_y      = span * rolling(nonmissing_y, span),
    P_Iadj_Y_avg  = if_else(unique_y >= 5, rolling(P_Iadj_Y, span), NA_real_)
  ) %>%
  fill(F_ID, M_ID, .direction = "downup") %>%  # Fill missing parent IDs
  ungroup() %>%
  dplyr::select(-nonmissing_y, -unique_y)

# ------------------------------------------------------------------------------
# Child's income trajectory
# ------------------------------------------------------------------------------

children_income_at <- data_full %>%
  dplyr::select(
    Child_ID, M_ID, F_ID,
    Child_Cohort_Individual,
    Child_IRace, Child_Sex_Individual,
    starts_with("Child_Iadj_Y_")
  ) %>%
  pivot_longer(
    cols         = starts_with("Child_Iadj_Y_"),
    names_prefix = "Child_Iadj_Y_",
    names_to     = "Year",
    values_to    = "Child_Iadj_Y"
  ) %>%
  mutate(
    Year          = as.numeric(Year),
    Age_at_Child  = Year - Child_Cohort_Individual - 1,
    Child_Iadj_Y  = if_else(Child_Iadj_Y == 0, NA_real_, Child_Iadj_Y)
  ) %>%
  
  # Restrict to prime working ages (25-60) to capture permanent income
  filter(Age_at_Child %in% 25:60) %>%
  
  group_by(Child_ID, Year, Age_at_Child) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  unite(Parents_ID, c("F_ID", "M_ID"), sep = "", remove = FALSE) %>%
  group_by(Child_ID) %>%
  mutate(noparents = if_else(is.na(F_ID) & is.na(M_ID), 1L, 0L)) %>%
  filter(noparents == 0) %>%
  
  # Apply rolling average to child's income
  mutate(
    nonmissing_y      = as.integer(!is.na(Child_Iadj_Y)),
    unique_y          = span * rolling(nonmissing_y, span),
    Child_Iadj_Y_avg  = if_else(unique_y >= 5, rolling(Child_Iadj_Y, span), NA_real_)
  ) %>%
  ungroup() %>%
  dplyr::select(-nonmissing_y, -unique_y, -noparents)

# ==============================================================================
# Fixed parental background: Age-42 anchoring
# ==============================================================================
# Anchor parental income at age 42 to provide a consistent reference point
# across families. For each child, we use the maximum of father's and mother's
# permanent income when each parent was 42 years old.

# Father's permanent income at age 42
father_42 <- father_income_at %>%
  filter(Age_at_F == 42) %>%
  select(
    Parents_ID,
    F_Iadj_Y_avg_42 = F_Iadj_Y_avg
  )

# Mother's permanent income at age 42
mother_42 <- mother_income_at %>%
  filter(Age_at_M == 42) %>%
  select(
    Parents_ID,
    M_Iadj_Y_avg_42 = M_Iadj_Y_avg
  )

# Take the maximum of mother's and father's age-42 income (dominant earner)
parent_income_42 <- full_join(father_42, mother_42, by = "Parents_ID") %>%
  mutate(
    P_Iadj_Y_avg_max_42 = pmax(F_Iadj_Y_avg_42, M_Iadj_Y_avg_42, na.rm = TRUE),
    # If both parents missing at age 42, set to NA
    P_Iadj_Y_avg_max_42 = if_else(
      is.na(F_Iadj_Y_avg_42) & is.na(M_Iadj_Y_avg_42),
      NA_real_,
      P_Iadj_Y_avg_max_42
    )
  )

# Merge fixed parental background into child data
children_income_at <- children_income_at %>%
  left_join(
    parent_income_42 %>%
      select(Parents_ID, P_Iadj_Y_avg_max_42),
    by = "Parents_ID"
  ) 

# Clean up zero vs NA handling for income variables
children_income_at <- children_income_at %>%
  mutate(across(contains("_Y"), ~ replace_na(.x, 0))) %>%  # Temporarily convert NA to 0
  mutate(across(contains("_Y"), ~ na_if(., 0)))            # Convert 0 back to NA

# ------------------------------------------------------------------------------
# Recode demographic variables
# ------------------------------------------------------------------------------

children_income_at <- children_income_at %>%
  mutate(
    Child_White = if_else(Child_IRace == 1, 1, 0),  # White = 1, other races = 0
    Child_Man = if_else(Child_Sex_Individual == 1, 1, 0),  # Male = 1, Female = 0
    Child_Age = Year - Child_Cohort_Individual
  ) %>%
  dplyr::select(-c(Child_IRace, Child_Sex_Individual))

# ==============================================================================
# Collapse to individual-level averages
# ==============================================================================
# For each child, compute their average income across all observed years,
# requiring at least 2 income observations for stability.

children_income_at <- children_income_at %>%
  ungroup() %>%
  group_by(Child_ID) %>%
  summarise(
    T_obs = sum(!is.na(Child_Iadj_Y)),  # Number of income observations
    logy_sd = sd(log(Child_Iadj_Y + 0.01), na.rm = TRUE),  # Within-person income volatility
    Child_Iadj_Y = mean(Child_Iadj_Y, na.rm = TRUE),  # Average child income
    P_Iadj_Y_avg_max_42 = first(P_Iadj_Y_avg_max_42),  # Parent's age-42 income
    Child_White = first(Child_White),
    Child_Man   = first(Child_Man),
    .groups = "drop"
  ) %>%
  filter(T_obs >= 2)  # Require at least 2 observations

# Save processed data
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}
write_csv(children_income_at, here("data", "children_income_at.csv"))

# Clean up intermediate objects to free memory
rm(data_full, mother_income_at, father_income_at, parents_income_at)
gc()

# ==============================================================================
# Prepare data for Stan estimation
# ==============================================================================

df_real <- children_income_at %>%
  filter(!is.na(P_Iadj_Y_avg_max_42), !is.na(Child_Iadj_Y)) %>%
  transmute(
    group = if_else(Child_White == 1, 1L, 0L),  # 1 = White, 0 = Black
    x = P_Iadj_Y_avg_max_42 + 0.01,  # Parent's income (small constant avoids log(0))
    y = Child_Iadj_Y + 0.01,  # Child's income
    male = Child_Man,
    T_obs = T_obs,  # Number of observations (for measurement error model)
    logy_sd = logy_sd  # Income volatility (for measurement error model)
  )

rm(children_income_at)

# ------------------------------------------------------------------------------
# Define mobility categories
# ------------------------------------------------------------------------------

# Mobility threshold: 15% income change defines upward/downward mobility
bw <- log(1.15)

df_real <- df_real %>%
  mutate(
    mobility_type = case_when(
      y / x > exp(bw) ~ "up",    # Child earns >15% more than parent
      x / y > exp(bw) ~ "down",  # Child earns >15% less than parent
      TRUE ~ "imm"               # Child's income within ±15% of parent
    )
  )

# ------------------------------------------------------------------------------
# Analysis configuration
# ------------------------------------------------------------------------------

params_names <- c("alpha", "beta", "gamma", "lambda")
mob_types    <- c("up", "down", "imm")
scenarios    <- c("obs", "alpha", "beta", "sigma")  # Observed + 3 counterfactuals

# Helper function to summarize posterior distributions
posterior_summary <- function(mat) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  tibble(
    mean = colMeans(mat),
    lwr  = apply(mat, 2, quantile, probs = 0.025),
    upr  = apply(mat, 2, quantile, probs = 0.975)
  )
}

# Storage for final results
combined_agg_comp_tables <- list()

# ==============================================================================
# Main estimation loop: Separate analysis by gender
# ==============================================================================
# Run the full pipeline (Stan estimation, plots, decomposition) separately
# for males and females to examine gender-specific patterns.

for (gender in c(0, 1)) { 
  
  gender_label <- ifelse(gender == 0, "female", "male")
  df_real_gender <- df_real %>% filter(male == gender)
  
  # ----------------------------------------------------------------------------
  # Construct evaluation grid for counterfactual predictions
  # ----------------------------------------------------------------------------
  
  # Reference point for log-centering (1st percentile of income distribution)
  x_ref_log <- quantile(log(df_real_gender$x), 0.01)
  
  # Create 8 evenly-spaced income levels spanning 1st to 99th percentile
  log_grid <- seq(
    quantile(log(df_real_gender$x) - x_ref_log, .01),
    quantile(log(df_real_gender$x) - x_ref_log, .99),
    length.out = 8
  )
  
  # Store grid bounds for reference
  assign(paste0("floor_", gender_label), x_ref_log) 
  assign(paste0("lb_", gender_label), min(log_grid)) 
  assign(paste0("ub_", gender_label), max(log_grid)) 
  assign(
    "percentiles",
    round(log(quantile(df_real$x, p = c(0.1, 0.5, 0.9))), 1)
  ) 
  
  # Back-transform to level scale and repeat for both racial groups
  x_new <- exp(log_grid + x_ref_log)
  x_new <- rep(x_new, times = 2) %>% round(0)
  
  group_new <- rep(0L:1L, each = length(log_grid))  # 0 = Black, 1 = White
  
  # Prepare Stan data list
  stan_data <- list(
    N = nrow(df_real_gender),
    x = df_real_gender$x,
    x_ref_log = x_ref_log,  # Log-centering reference
    y = df_real_gender$y,
    group = df_real_gender$group,
    epsilon = bw,  # Mobility threshold (15% on log scale)
    N_new = length(x_new),
    x_new = x_new,
    group_new = group_new,
    T_obs = df_real_gender$T_obs,  # For measurement error adjustment
    logy_sd = df_real_gender$logy_sd  # For measurement error adjustment
  )
  
  # ----------------------------------------------------------------------------
  # Compile and run Stan model
  # ----------------------------------------------------------------------------
  
  model_stan <- paste0("glm_mobility_model_it.stan")
  mod <- cmdstan_model(here("code", model_stan), force_recompile = TRUE)
  
  fit <- mod$sample(
    data = stan_data,
    chains = n_chains,  # Defined in master file
    iter_sampling = n_iter,
    iter_warmup = n_warmup,
    seed = 532,
    parallel_chains = n_chains,
    refresh = 100
  )
  
  # Variables to extract from posterior
  vars <- c("up_obs", "down_obs", "imm_obs",
            "up_alpha", "down_alpha", "imm_alpha",
            "up_beta", "down_beta", "imm_beta",
            "up_sigma", "down_sigma", "imm_sigma",
            "alpha", "beta", "gamma", "lambda",
            "y_pred", "residual", "log_lik")
  
  # Extract posterior draws in chunks to manage memory
  get_draws_chunked <- function(fit, vars, chunk = 10) {
    chunks <- split(vars, ceiling(seq_along(vars) / chunk))
    out <- list()
    for (i in seq_along(chunks)) {
      cat("Loading chunk", i, "of", length(chunks), "\n")
      dr <- fit$draws(variables = chunks[[i]]) |> posterior::as_draws_df()
      out[[i]] <- dr
    }
    # Combine chunks
    Reduce(posterior::bind_draws, out)
  }
  
  posterior <- get_draws_chunked(fit, vars, chunk = 8)
  
  # ----------------------------------------------------------------------------
  # Process posterior mobility probabilities
  # ----------------------------------------------------------------------------
  
  result <- tibble(x = x_new, group = group_new)
  
  # Extract and summarize mobility probabilities for each scenario
  for (s in scenarios) {
    for (m in mob_types) {
      var <- paste0(m, "_", s)
      if (any(grepl(paste0("^", var, "\\["), variables(posterior)))) {
        draws_mat <- as_draws_matrix(posterior, variables = NULL)
        draws_only <- draws_mat[, grepl(paste0("^", var, "\\["), colnames(draws_mat)), drop = FALSE]
        out <- posterior_summary(draws_only)
        names(out) <- paste0(m, "_", s, c("_mean", "_lwr", "_upr"))
        result <- bind_cols(result, out)
      } else {
        warning("Variable '", var, "' not found in posterior draws.")
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # Reshape for visualization
  # ----------------------------------------------------------------------------
  
  result_long_all <- result %>%
    pivot_longer(
      -c(x, group),
      names_to = c("type", "scenario", ".value"),
      names_pattern = "(.*)_(obs|alpha|beta|sigma)_(mean|lwr|upr)"
    ) %>%
    mutate(
      x_log   = round(log(x) - x_ref_log, 2),
      group   = factor(group, levels = 0:1, labels = c("Black*' | '*theta[w]", "White*' | '*theta[b]")),
      type    = factor(type, levels = c("up", "imm", "down"), labels = c("Upward", "Immobile", "Downward")),
      scenario = factor(scenario, levels = scenarios,
                        labels = c("Observed", "Swapped α", "Swapped β", "Swapped σ"))
    )
  
  # ----------------------------------------------------------------------------
  # Plot 1: Stacked area plots of mobility probabilities
  # ----------------------------------------------------------------------------
  
  # Compute marginal probabilities (averaged over income distribution)
  predicted_marginals <- result_long_all %>%
    group_by(group, scenario, type) %>%
    summarise(
      mean = mean(mean, na.rm = TRUE),
      lwr  = mean(lwr,  na.rm = TRUE),
      upr  = mean(upr,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(x = 1)
  
  # Small marginal plot
  p_marginal <- ggplot(predicted_marginals,
                       aes(x = x, y = mean, fill = type)) +
    geom_col(position = "stack", width = 0.5, alpha = 0.9) +
    facet_grid(group ~ "Pred. Marginal", switch = "y") +
    scale_x_continuous(expand = expansion(mult = c(0.4, 0.4))) +
    labs(x = NULL, y = "Mobility probability", fill = NULL) +
    scale_color_julia() + scale_fill_julia() + 
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          strip.placement = "outside", strip.text.y.left = element_blank())
  
  # Main stacked area plot by income level
  p1 <- ggplot(result_long_all, aes(x = round(x_log, 1), y = mean, fill = type, group = type)) +
    geom_area(position = "stack", alpha = 0.9) +
    facet_grid(group ~ scenario, labeller = labeller(group = label_parsed)) +
    labs(
      x = "Parent's log permanent income ( x )",
      y = NULL,
      fill = "Mobility type") +
    scale_color_julia() + scale_fill_julia() + 
    theme(strip.placement = "outside", legend.position = "bottom") +
    scale_x_continuous(labels = function(x) round(x + x_ref_log, 1))
  
  final_plot <- p_marginal + p1 + plot_layout(widths = c(1, 5))
  
  # Save plot
  if (!dir.exists(here("figures"))) {
    dir.create(here("figures"))
  }
  
  ggsave(here("figures", paste0("mobility_area_plot_real_", gender_label, ".png")),
         plot = final_plot, width = 6, height = 4, dpi = 300, bg = "white")
  
  print(final_plot)
  
  # ----------------------------------------------------------------------------
  # Plot 2: Counterfactual differences (CF - Observed)
  # ----------------------------------------------------------------------------
  
  diff_draws_list <- list()
  
  # Calculate difference between counterfactual and observed for each draw
  for (m in mob_types) {
    
    obs_cols <- grep(paste0("^", m, "_obs\\["), colnames(posterior), value = TRUE)
    obs_draws <- posterior %>% select(all_of(obs_cols)) %>% as.matrix()
    
    for (s in setdiff(scenarios, "obs")) {
      
      cf_cols <- grep(paste0("^", m, "_", s, "\\["), colnames(posterior), value = TRUE)
      if (length(cf_cols) == length(obs_cols) && length(cf_cols) > 0) {
        
        cf_draws <- posterior %>% select(all_of(cf_cols)) %>% as.matrix()
        diff_draws <- cf_draws - obs_draws
        diff_summary <- posterior_summary(diff_draws)
        names(diff_summary) <- paste0(m, "_", s, c("_diff_mean", "_diff_lwr", "_diff_upr"))
        diff_draws_list[[paste0(m, "_", s)]] <- diff_summary
      }
    }
  }
  
  # Combine into single table
  diff_result <- tibble(x = x_new, x_log = round(log(x_new) - x_ref_log, 1), group = group_new)
  
  for (key in names(diff_draws_list)) {
    diff_result <- bind_cols(diff_result, diff_draws_list[[key]])
  }
  
  # Reshape for plotting
  diff_long <- diff_result %>%
    pivot_longer(
      -c(x, x_log, group),
      names_to = c("type", "scenario", ".value"),
      names_pattern = "^(up|imm|down)_(alpha|beta|sigma)_diff_(mean|lwr|upr)$"
    ) %>%
    mutate(
      group = factor(group, levels = 0:1, labels = c("Black*' | '*theta[w]", "White*' | '*theta[b]")),
      type = factor(type, levels = c("up", "imm", "down"), labels = c("Upward", "Immobile", "Downward")),
      scenario = factor(scenario,
                        levels = c("alpha", "beta", "sigma"),
                        labels = c("Swapped α", "Swapped β", "Swapped σ"))
    )
  
  # Plot differences with confidence ribbons
  p2 <- ggplot(diff_long, aes(x = x_log, y = mean, color = type, fill = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    geom_line(size = 0.7) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
    geom_point(shape = 21, size = 2, stroke = 1) +
    geom_point(shape = 21, size = 1.9, stroke = 1, fill = "white") +
    facet_grid(group ~ scenario, labeller = labeller(group = label_parsed)) +
    scale_color_julia() + scale_fill_julia() + 
    coord_cartesian(ylim = c(-0.32, 0.32)) +
    labs(
      x = "Parent's log permanent income ( x )",
      y = "Δ Mobility probability\n(Counterfactual – Observed)",
      color = "Mobility type",
      fill = "Mobility type"
    ) +
    scale_x_continuous(labels = function(x) round(x + x_ref_log, 1))
  
  ggsave(here("figures", paste0("mobility_diff_plot_real_", gender_label, ".png")),
         plot = p2, width = 6, height = 4, dpi = 300, bg = "white")
  
  print(p2)
  
  # ----------------------------------------------------------------------------
  # Model fit diagnostics
  # ----------------------------------------------------------------------------
  
  loglik_cols <- grep("^log_lik\\[", colnames(posterior), value = TRUE)
  log_lik_matrix <- posterior %>% select(all_of(loglik_cols)) %>% as.matrix()
  loo_result  <- loo::loo(log_lik_matrix)
  waic_result <- loo::waic(log_lik_matrix)
  
  model_fit_tbl <- tibble(
    Parameter = c("LOOIC", "WAIC"),
    Estimate = round(c(loo_result$estimates["looic", "Estimate"],
                       waic_result$estimates["waic", "Estimate"]), 1),
    `95\\% CI` = c("", "")
  )
  
  # ----------------------------------------------------------------------------
  # Posterior parameter summaries
  # ----------------------------------------------------------------------------
  
  group_indices <- c(1, 2)  # Black and White
  
  param_summary <- map_dfr(params_names, function(p) {
    # Find all columns for this parameter (e.g. alpha[1], alpha[2])
    param_cols <- grep(paste0("^", p, "\\["), names(posterior), value = TRUE)
    
    if (length(param_cols) == 0) {
      warning("Missing parameter: ", p)
      return(NULL)
    }
    
    draws_mat <- posterior %>%
      select(all_of(param_cols)) %>%
      as.matrix()
    
    # Summarize for each group
    available_groups <- intersect(group_indices, seq_len(ncol(draws_mat)))
    
    map_dfr(available_groups, function(g) {
      draws <- draws_mat[, g]
      tibble(
        parameter = paste0(p, "[", g, "]"),
        mean = mean(draws),
        lwr = quantile(draws, 0.025),
        upr = quantile(draws, 0.975)
      )
    })
  })
  
  # Format for display
  param_summary_kbl <- param_summary %>%
    mutate(
      Estimate = round(mean, 3),
      `95\\% CI` = sprintf("[%0.3f, %0.3f]", lwr, upr)
    ) %>%
    select(Parameter = parameter, Estimate, `95\\% CI`)
  
  assign(paste0("parameters_", gender_label), param_summary_kbl)  
  
  # Combine with fit statistics
  param_summary_kbl_ext <- bind_rows(param_summary_kbl, model_fit_tbl)
  
  # Export LaTeX table
  if (!dir.exists(here("tables"))) {
    dir.create(here("tables"))
  }
  
  latex_table_params <- kable(param_summary_kbl_ext,
                              format = "latex", booktabs = TRUE,
                              caption = paste("Posterior Means and 95\\% Credible Intervals"),
                              align = c("l", "r", "c"), escape = FALSE) %>%
    kable_styling(latex_options = "hold_position")
  
  writeLines(latex_table_params, here("tables", paste0("posterior_params_summary_real_", gender_label, ".tex")))
  
  # ============================================================================
  # Aggregation and Composition Decomposition
  # ============================================================================
  # Decomposes racial mobility gaps into:
  # 1. Aggregation: difference due to parameters (holding x-distribution fixed)
  # 2. Composition: difference due to x-distribution (holding parameters fixed)
  
  # --------------------------------------------------------------------------
  # Setup prediction grid
  # --------------------------------------------------------------------------
  
  grid_df <- tibble(
    idx   = seq_along(x_new),
    group = group_new,  # 0 = Black, 1 = White
    x     = x_new
  )
  
  posterior_mat <- posterior %>% as_draws_matrix()
  
  mobility_levels <- c("up", "imm", "down")
  scenario_short  <- c("alpha", "beta", "sigma")
  
  # --------------------------------------------------------------------------
  # Construct density-based weights for aggregation
  # --------------------------------------------------------------------------
  # p_g(x) = the empirical density of income in each racial group
  
  # Helper to extract observed income by group
  empirical_x <- function(g) {
    df_real_gender %>%
      filter(group == g) %>%
      pull(x)
  }
  
  # Compute kernel density estimates for each group at grid points
  group_densities <- grid_df %>%
    select(group, x) %>%
    distinct() %>%
    arrange(group, x) %>%
    group_by(group) %>%
    group_modify(~ {
      g <- .y$group              
      x_emp <- empirical_x(g)
      
      if (length(x_emp) < 2) {
        dens_vals <- rep(1, nrow(.x))
      } else {
        dens <- density(x_emp, n = 512)
        dens_vals <- approx(dens$x, dens$y, xout = .x$x, rule = 2)$y
      }
      
      tibble(x = .x$x, density = dens_vals)
    }) %>%
    ungroup()
  
  # Normalize to get proper probability weights
  p_weights <- grid_df %>%
    left_join(group_densities, by = c("group", "x")) %>%
    replace_na(list(density = 0)) %>%
    group_by(group) %>%
    mutate(
      p = density / sum(density),
      p = ifelse(is.na(p) | !is.finite(p), 0, p)
    ) %>%
    ungroup() %>%
    arrange(idx) %>%
    select(idx, group, x, p)
  
  # --------------------------------------------------------------------------
  # 1. AGGREGATION EFFECT
  # --------------------------------------------------------------------------
  # For each group, compute weighted average of (CF - Obs) using own distribution
  
  agg_rows <- list()
  
  for (g in 0:1) {
    
    # Get grid indices and weights for this group
    idx_g <- grid_df %>% filter(group == g) %>% arrange(idx) %>% pull(idx)
    w_g <- p_weights %>% filter(group == g) %>% arrange(idx) %>% pull(p)
    w_g <- matrix(w_g, ncol = 1)
    
    # Loop over counterfactual scenarios and mobility types
    for (sc in scenario_short) {
      for (tp in mobility_levels) {
        
        # Extract posterior draws
        cf_cols <- paste0(tp, "_", sc, "[", idx_g, "]")
        ob_cols <- paste0(tp, "_obs[", idx_g, "]")
        
        cf_mat <- posterior_mat[, cf_cols, drop = FALSE]
        ob_mat <- posterior_mat[, ob_cols, drop = FALSE]
        
        # Compute weighted difference for each posterior draw
        diff_mat  <- cf_mat - ob_mat
        agg_draws <- as.numeric(diff_mat %*% w_g)
        
        agg_rows[[length(agg_rows) + 1]] <- tibble(
          Analysis = "Aggregation",
          Group    = ifelse(g == 0, "Black", "White"),
          Scenario = case_when(
            sc == "alpha" ~ "Swapped $\\alpha$",
            sc == "beta"  ~ "Swapped $\\beta$",
            sc == "sigma" ~ "Swapped $\\sigma$"
          ),
          Mobility = recode(tp, "up" = "Upward", "imm" = "Immobile", "down" = "Downward"),
          Mean     = mean(agg_draws),
          `Lower 95%` = quantile(agg_draws, 0.025),
          `Upper 95%` = quantile(agg_draws, 0.975)
        )
      }
    }
  }
  
  agg_table_df_real <- bind_rows(agg_rows)
  
  # --------------------------------------------------------------------------
  # 2. COMPOSITION EFFECT
  # --------------------------------------------------------------------------
  # For each group, compute weighted difference using OTHER group's distribution
  
  comp_rows <- list()
  
  for (g in 0:1) {
    
    g_other <- 1 - g
    
    # Get own group's weights
    idx_g <- grid_df %>% filter(group == g) %>% arrange(idx) %>% pull(idx)
    w_g <- p_weights %>% filter(group == g) %>% arrange(idx) %>% pull(p)
    
    # Compute OTHER group's density at OWN x-values
    x_emp_other <- empirical_x(g_other)
    
    if (length(x_emp_other) < 2) {
      w_other_raw <- rep(1, length(idx_g))
    } else {
      dens_o <- density(x_emp_other, n = 512)
      x_vals_g <- grid_df %>% filter(group == g) %>% arrange(idx) %>% pull(x)
      w_other_raw <- approx(dens_o$x, dens_o$y, xout = x_vals_g, rule = 2)$y
    }
    
    # Normalize and compute weight difference
    w_other <- w_other_raw / sum(w_other_raw)
    w_other[!is.finite(w_other)] <- 0
    
    delta_w <- w_other - w_g
    delta_w_mat <- matrix(delta_w, ncol = 1)
    
    # Apply weight difference to observed mobility
    for (tp in mobility_levels) {
      
      ob_cols <- paste0(tp, "_obs[", idx_g, "]")
      ob_mat  <- posterior_mat[, ob_cols, drop = FALSE]
      
      comp_draws <- as.numeric(ob_mat %*% delta_w_mat)
      
      comp_rows[[length(comp_rows) + 1]] <- tibble(
        Analysis = "Composition",
        Group    = ifelse(g == 0, "Black", "White"),
        Scenario = "'Composition'",
        Mobility = recode(tp, "up" = "Upward", "imm" = "Immobile", "down" = "Downward"),
        Mean     = mean(comp_draws),
        `Lower 95%` = quantile(comp_draws, 0.025),
        `Upper 95%` = quantile(comp_draws, 0.975)
      )
    }
  }
  
  comp_table_df_real <- bind_rows(comp_rows) %>% arrange(Group, Mobility)
  
  # --------------------------------------------------------------------------
  # Combined visualization: Aggregation vs Composition
  # --------------------------------------------------------------------------
  
  combined_agg_comp_table <- bind_rows(
    agg_table_df_real %>% mutate(Analysis = "Aggregation"),
    comp_table_df_real %>% mutate(Analysis = "Composition")
  ) %>%
    select(Analysis, Group, Scenario, Mobility, Mean, `Lower 95%`, `Upper 95%`) %>%
    arrange(Analysis, Group, Scenario, Mobility)
  
  # Prepare data for plotting
  plot_data <- combined_agg_comp_table %>%
    mutate(
      Analysis = factor(Analysis, levels = c("Aggregation", "Composition")),
      Group    = factor(Group, levels = c("Black", "White")),
      Mobility = factor(Mobility, levels = c("Upward", "Immobile", "Downward")),
      x_label = case_when(
        Analysis == "Aggregation" & str_detect(Scenario, "alpha") ~ "Swapped~alpha",
        Analysis == "Aggregation" & str_detect(Scenario, "beta")  ~ "Swapped~beta",
        Analysis == "Aggregation" & str_detect(Scenario, "sigma") ~ "Swapped~sigma",
        TRUE ~ "'Composition'"
      )
    )
  
  plot_data$x_label <- factor(plot_data$x_label,
                              levels = c("Swapped~alpha", "Swapped~beta", "Swapped~sigma", "'Composition'"))
  
  # Create combined bar plot
  p_combined <- ggplot(plot_data, aes(x = x_label, y = Mean, fill = Mobility)) +
    geom_col(position = position_dodge(width = .7), width = .65, alpha = .85) +
    geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`),
                  position = position_dodge(width = .7), width = .18) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_grid(Group ~ .) +
    scale_fill_julia() + scale_color_julia() +
    scale_x_discrete(labels = function(x) parse(text = x)) +
    labs(
      x = NULL,
      y = expression(Delta * " Mobility Probability"),
      fill = "Mobility type"
    ) +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 3.5, colour = "black", size = .5)  # Separate aggregation from composition
  
  print(p_combined)
  
  ggsave(here::here("figures", paste0("combined_agg_comp_effects_", gender_label, ".png")), 
         plot = p_combined, width = 6, height = 4, dpi = 300, bg = "white")
  
  # Clean up large objects to free memory before next iteration
  rm(posterior); gc() 
}
