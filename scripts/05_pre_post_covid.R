# =============================================================================
# NSCH Child Mental Health Analysis
# Script 05: Pre- vs. Post-COVID Comparisons
# =============================================================================
# Two definitions of "post-COVID":
#   Definition A: 2019 vs. 2022–2024 pooled (drop 2021 as transition)
#   Definition B: 2019 vs. 2023–2024 pooled (stricter; fully post-emergency)
#
# Analyses:
#   1. Unadjusted prevalence comparison (pre vs. post)
#   2. Adjusted logistic regression (OR) and modified Poisson (PR)
#   3. Subgroup-stratified models (by age, sex, race, poverty)
#   4. Sensitivity: show both definitions side by side
#   5. Forest plots
# =============================================================================

library(tidyverse)
library(survey)
library(srvyr)
library(broom)

options(survey.lonely.psu = "adjust")

proc_dir  <- here::here("data", "processed")
fig_dir   <- here::here("output", "figures")
table_dir <- here::here("output", "tables")

nsch_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    plot.caption     = element_text(color = "grey50", size = 9, hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

COND_COLORS <- c(
  "ADHD"              = "#2196F3",
  "Anxiety"           = "#FF9800",
  "Depression"        = "#9C27B0",
  "Behavior Problems" = "#F44336",
  "Any MH Condition"  = "#4CAF50"
)

# --- Load and prepare --------------------------------------------------------

nsch_full <- readRDS(file.path(proc_dir, "nsch_clean.rds")) |>
  filter(!is.na(race), !is.na(poverty_cat), !is.na(sex), !is.na(age_group))

# Create two analytic datasets
make_covid_dataset <- function(data, post_years, label) {
  data |>
    filter(survey_year %in% c(2019, post_years)) |>
    mutate(
      period       = factor(if_else(survey_year == 2019, "Pre-COVID", "Post-COVID"),
                            levels = c("Pre-COVID", "Post-COVID")),
      definition   = label
    )
}

dat_a <- make_covid_dataset(nsch_full, c(2022, 2023, 2024), "A: 2019 vs 2022–2024")
dat_b <- make_covid_dataset(nsch_full, c(2023, 2024),       "B: 2019 vs 2023–2024")

svy_a <- dat_a |> as_survey_design(strata = STRATUM, weights = FWC, ids = 1)
svy_b <- dat_b |> as_survey_design(strata = STRATUM, weights = FWC, ids = 1)

message("Definition A: ", nrow(dat_a), " children (2019 + 2022–2024)")
message("Definition B: ", nrow(dat_b), " children (2019 + 2023–2024)")

# --- Outcome definitions -----------------------------------------------------

outcomes <- c(
  "adhd_ever"   = "ADHD",
  "anx_ever"    = "Anxiety",
  "dep_ever"    = "Depression",
  "beh_ever"    = "Behavior Problems",
  "any_mh_ever" = "Any MH Condition"
)

# --- 1. Unadjusted weighted prevalence by period ----------------------------

message("\n--- Unadjusted prevalence ---")

calc_prevalence <- function(svy_obj, def_label) {
  svy_obj |>
    group_by(period) |>
    summarise(
      ADHD_pct              = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
      Anxiety_pct           = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
      Depression_pct        = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
      `Behavior Problems_pct` = survey_mean(beh_ever,   na.rm = TRUE, vartype = "ci") * 100,
      `Any MH Condition_pct`  = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
    ) |>
    mutate(definition = def_label) |>
    pivot_longer(
      cols          = -c(period, definition),
      names_to      = c("condition", ".value"),
      names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
    )
}

prev_a <- calc_prevalence(svy_a, "A: 2019 vs 2022–2024")
prev_b <- calc_prevalence(svy_b, "B: 2019 vs 2023–2024")
prev_all <- bind_rows(prev_a, prev_b)

cat("\n", strrep("=", 70), "\n")
cat("WEIGHTED PREVALENCE: PRE- vs POST-COVID\n")
cat(strrep("=", 70), "\n\n")

prev_all |>
  mutate(across(c(pct, pct_low, pct_upp), \(x) round(x, 1))) |>
  mutate(est = sprintf("%.1f%% [%.1f–%.1f]", pct, pct_low, pct_upp)) |>
  select(definition, condition, period, est) |>
  pivot_wider(names_from = period, values_from = est) |>
  arrange(definition, condition) |>
  print(n = Inf)

# --- 2. Adjusted regression: pre vs. post ------------------------------------

message("\n--- Adjusted regression models ---")

tidy_model <- function(model, model_type = "logistic") {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(model_type = model_type) |>
    select(term, estimate, conf.low, conf.high, p.value, model_type)
}

run_covid_models <- function(svy_obj, def_label) {
  imap_dfr(outcomes, \(label, var) {
    f_logit <- as.formula(paste0(var, " ~ period + age_group + sex + race + poverty_cat"))
    f_pois  <- as.formula(paste0("as.numeric(", var, ") ~ period + age_group + sex + race + poverty_cat"))

    m_logit <- svyglm(f_logit, design = svy_obj, family = quasibinomial())
    m_pois  <- svyglm(f_pois,  design = svy_obj, family = quasipoisson())

    bind_rows(
      tidy_model(m_logit, "logistic"),
      tidy_model(m_pois,  "poisson")
    ) |>
      mutate(outcome = label, definition = def_label)
  })
}

results_a <- run_covid_models(svy_a, "A: 2019 vs 2022–2024")
results_b <- run_covid_models(svy_b, "B: 2019 vs 2023–2024")
results_all <- bind_rows(results_a, results_b)

# Print post-COVID effect
cat("\n", strrep("=", 70), "\n")
cat("ADJUSTED POST-COVID EFFECT (OR and PR, ref = Pre-COVID/2019)\n")
cat("Controlling for age group, sex, race/ethnicity, poverty level\n")
cat(strrep("=", 70), "\n\n")

results_all |>
  filter(term == "periodPost-COVID") |>
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
    p.value   = format.pval(p.value, digits = 3),
    sig       = case_when(
      as.numeric(p.value) < 0.001 ~ "***",
      as.numeric(p.value) < 0.01  ~ "**",
      as.numeric(p.value) < 0.05  ~ "*",
      TRUE                        ~ ""
    ),
    est_label = if_else(model_type == "logistic", "OR", "PR")
  ) |>
  select(definition, outcome, est_label, estimate, conf.low, conf.high, p.value, sig) |>
  arrange(definition, outcome, est_label) |>
  print(n = Inf)

# --- 3. Subgroup-stratified: pre vs. post effect by demographic group --------

message("\n--- Subgroup-stratified models ---")

run_stratified <- function(svy_obj, strat_var, def_label) {
  strat_levels <- svy_obj$variables |> pull(!!sym(strat_var)) |> levels()

  map_dfr(strat_levels, \(lev) {
    svy_sub <- svy_obj |> filter(!!sym(strat_var) == lev)
    # Covariates minus the stratification variable
    covars <- c("age_group", "sex", "race", "poverty_cat")
    covars <- setdiff(covars, strat_var)

    imap_dfr(outcomes, \(label, var) {
      f <- as.formula(paste0(var, " ~ period + ", paste(covars, collapse = " + ")))
      m <- tryCatch(
        svyglm(f, design = svy_sub, family = quasibinomial()),
        error = function(e) NULL
      )
      if (is.null(m)) return(tibble())

      tidy(m, conf.int = TRUE, exponentiate = TRUE) |>
        filter(term == "periodPost-COVID") |>
        mutate(
          outcome    = label,
          stratum    = strat_var,
          level      = lev,
          definition = def_label
        ) |>
        select(definition, outcome, stratum, level, OR = estimate,
               ci_low = conf.low, ci_high = conf.high, p.value)
    })
  })
}

strat_a <- bind_rows(
  run_stratified(svy_a, "age_group",   "A: 2019 vs 2022–2024"),
  run_stratified(svy_a, "sex",         "A: 2019 vs 2022–2024"),
  run_stratified(svy_a, "race",        "A: 2019 vs 2022–2024"),
  run_stratified(svy_a, "poverty_cat", "A: 2019 vs 2022–2024")
)

strat_b <- bind_rows(
  run_stratified(svy_b, "age_group",   "B: 2019 vs 2023–2024"),
  run_stratified(svy_b, "sex",         "B: 2019 vs 2023–2024"),
  run_stratified(svy_b, "race",        "B: 2019 vs 2023–2024"),
  run_stratified(svy_b, "poverty_cat", "B: 2019 vs 2023–2024")
)

strat_all <- bind_rows(strat_a, strat_b)

cat("\n", strrep("=", 70), "\n")
cat("SUBGROUP-STRATIFIED POST-COVID ORs (adjusted, Any MH Condition)\n")
cat(strrep("=", 70), "\n\n")

strat_all |>
  filter(outcome == "Any MH Condition") |>
  mutate(
    across(c(OR, ci_low, ci_high), \(x) round(x, 3)),
    p.value = format.pval(p.value, digits = 3),
    sig     = case_when(
      as.numeric(p.value) < 0.001 ~ "***",
      as.numeric(p.value) < 0.01  ~ "**",
      as.numeric(p.value) < 0.05  ~ "*",
      TRUE                        ~ ""
    )
  ) |>
  select(definition, stratum, level, OR, ci_low, ci_high, p.value, sig) |>
  arrange(definition, stratum, level) |>
  print(n = Inf)

# --- 4. Sensitivity comparison: Definition A vs B side by side ---------------

cat("\n", strrep("=", 70), "\n")
cat("SENSITIVITY: DEFINITION A vs B SIDE BY SIDE\n")
cat("(Adjusted PRs for post-COVID, reference = 2019)\n")
cat(strrep("=", 70), "\n\n")

results_all |>
  filter(term == "periodPost-COVID", model_type == "poisson") |>
  mutate(est = sprintf("%.3f [%.3f–%.3f]", estimate, conf.low, conf.high)) |>
  select(definition, outcome, est, p.value) |>
  mutate(p.value = format.pval(p.value, digits = 3)) |>
  pivot_wider(names_from = definition, values_from = c(est, p.value)) |>
  print()

# --- 5. Forest plots ---------------------------------------------------------

message("\n--- Creating forest plots ---")

# Forest plot: Post-COVID PR by condition, both definitions
forest_data <- results_all |>
  filter(term == "periodPost-COVID", model_type == "poisson") |>
  mutate(
    outcome    = fct_rev(fct_reorder(outcome, estimate)),
    definition = fct_rev(factor(definition))
  )

p_covid_forest <- ggplot(forest_data,
    aes(x = estimate, y = outcome, color = definition, shape = definition)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.5), size = 0.7
  ) +
  scale_color_manual(values = c(
    "A: 2019 vs 2022–2024" = "#1565C0",
    "B: 2019 vs 2023–2024" = "#C62828"
  )) +
  labs(
    title    = "Adjusted Post-COVID Prevalence Ratios by Condition",
    subtitle = "Reference = 2019 (pre-COVID). Adjusted for age, sex, race, poverty.",
    x = "Prevalence Ratio (Post-COVID vs. Pre-COVID)", y = NULL,
    color = "Definition", shape = "Definition",
    caption = "Source: NSCH 2019–2024. Survey-weighted modified Poisson regression."
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "11_forest_pre_post_covid.png"), p_covid_forest,
       width = 10, height = 5, dpi = 150)
message("  Saved: 11_forest_pre_post_covid.png")

# Forest plot: Subgroup-stratified ORs for Any MH, Definition A
strat_forest <- strat_a |>
  filter(outcome == "Any MH Condition") |>
  mutate(
    label = paste0(stratum, ": ", level),
    label = fct_rev(fct_inorder(label))
  )

p_strat_forest <- ggplot(strat_forest, aes(x = OR, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high, color = stratum), size = 0.6) +
  geom_text(aes(label = sprintf("%.2f", OR)), hjust = -0.3, size = 3.2, color = "grey30") +
  facet_grid(stratum ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = c(
    "age_group" = "#2196F3", "sex" = "#AD1457",
    "race" = "#E65100", "poverty_cat" = "#1B5E20"
  ), guide = "none") +
  labs(
    title    = "Post-COVID Effect on Any MH Diagnosis, by Subgroup",
    subtitle = "Adjusted ORs (2019 vs 2022–2024) | Each stratum modeled separately",
    x = "Adjusted OR (Post-COVID vs. Pre-COVID)", y = NULL,
    caption  = "Source: NSCH 2019, 2022–2024. Survey-weighted logistic regression.\nEach subgroup adjusted for remaining demographics."
  ) +
  nsch_theme +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0))

ggsave(file.path(fig_dir, "12_forest_subgroup_covid.png"), p_strat_forest,
       width = 10, height = 8, dpi = 150)
message("  Saved: 12_forest_subgroup_covid.png")

# --- Save tables -------------------------------------------------------------

write_csv(prev_all,    file.path(table_dir, "prevalence_pre_post_covid.csv"))
write_csv(results_all, file.path(table_dir, "regression_pre_post_covid.csv"))
write_csv(strat_all,   file.path(table_dir, "regression_stratified_covid.csv"))

message("\nAll outputs saved.")
message("  Figures: output/figures/11_*.png, 12_*.png")
message("  Tables:  output/tables/prevalence_pre_post_covid.csv")
message("           output/tables/regression_pre_post_covid.csv")
message("           output/tables/regression_stratified_covid.csv")
