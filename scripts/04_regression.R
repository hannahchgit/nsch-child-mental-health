# =============================================================================
# NSCH Child Mental Health Analysis
# Script 04: Multivariate Regression — Odds Ratios & Prevalence Ratios
# =============================================================================
# Models:
#   A. Survey-weighted logistic regression (quasibinomial)   → Odds Ratios (OR)
#   B. Survey-weighted modified Poisson   (quasipoisson)    → Prevalence Ratios (PR)
#      Modified Poisson is preferred when prevalence >10%,
#      since ORs overestimate the true relative risk.
#
# Outcomes: ADHD, Anxiety, Depression, Behavior Problems, Any MH condition
# Predictors: survey year (linear trend + factor), age group, sex,
#             race/ethnicity, poverty level
# Interactions: sex × year, race × year
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
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# --- Load and prepare --------------------------------------------------------

nsch <- readRDS(file.path(proc_dir, "nsch_clean.rds"))

# Center year for interpretability: per-year change since 2019
nsch <- nsch |>
  filter(!is.na(race), !is.na(poverty_cat), !is.na(sex), !is.na(age_group)) |>
  mutate(
    year_centered = survey_year - 2019,
    year_factor   = factor(survey_year, levels = c(2019, 2021, 2022, 2023, 2024))
  )

svy <- nsch |>
  as_survey_design(strata = STRATUM, weights = FWC, ids = 1)

message("Analytic sample: ", nrow(nsch), " children across 5 survey years")

# --- Helper: tidy model results ----------------------------------------------

tidy_model <- function(model, model_type = "logistic") {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(
      model_type = model_type,
      estimate_label = case_when(
        model_type == "logistic" ~ "OR",
        model_type == "poisson"  ~ "PR"
      )
    ) |>
    select(term, estimate, conf.low, conf.high, p.value, model_type, estimate_label)
}

# --- A. Main models: each condition ------------------------------------------

outcomes <- c(
  "adhd_ever"    = "ADHD",
  "anx_ever"     = "Anxiety",
  "dep_ever"     = "Depression",
  "beh_ever"     = "Behavior Problems",
  "any_mh_ever"  = "Any MH Condition"
)

message("\n--- Running main regression models ---")

# 1. Year as continuous (linear trend per year)
run_trend_models <- function(label, outcome_var) {
  f <- as.formula(paste0(outcome_var, " ~ year_centered + age_group + sex + race + poverty_cat"))
  f_pois <- as.formula(paste0("as.numeric(", outcome_var, ") ~ year_centered + age_group + sex + race + poverty_cat"))

  m_logit  <- svyglm(f,      design = svy, family = quasibinomial())
  m_pois   <- svyglm(f_pois, design = svy, family = quasipoisson())

  bind_rows(
    tidy_model(m_logit, "logistic"),
    tidy_model(m_pois,  "poisson")
  ) |>
    mutate(outcome = label)
}

# 2. Year as factor (each year vs. 2019 reference)
run_factor_models <- function(label, outcome_var) {
  f <- as.formula(paste0(outcome_var, " ~ year_factor + age_group + sex + race + poverty_cat"))
  f_pois <- as.formula(paste0("as.numeric(", outcome_var, ") ~ year_factor + age_group + sex + race + poverty_cat"))

  m_logit  <- svyglm(f,      design = svy, family = quasibinomial())
  m_pois   <- svyglm(f_pois, design = svy, family = quasipoisson())

  bind_rows(
    tidy_model(m_logit, "logistic"),
    tidy_model(m_pois,  "poisson")
  ) |>
    mutate(outcome = label)
}

trend_results  <- imap_dfr(outcomes, run_trend_models)
factor_results <- imap_dfr(outcomes, run_factor_models)

# --- Print year-trend results ------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("ADJUSTED ANNUAL TREND (per 1-year increase)\n")
cat("Controlling for age group, sex, race/ethnicity, and poverty level\n")
cat(strrep("=", 70), "\n\n")

trend_results |>
  filter(term == "year_centered") |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = format.pval(p.value, digits = 3)) |>
  select(outcome, estimate_label, estimate, conf.low, conf.high, p.value) |>
  arrange(outcome, estimate_label) |>
  print(n = Inf)

# --- Print year-factor results -----------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("ADJUSTED YEAR-SPECIFIC ESTIMATES (reference: 2019)\n")
cat(strrep("=", 70), "\n\n")

factor_results |>
  filter(str_detect(term, "year_factor")) |>
  mutate(
    year = str_extract(term, "\\d{4}"),
    across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
    p.value = format.pval(p.value, digits = 3)
  ) |>
  select(outcome, year, estimate_label, estimate, conf.low, conf.high, p.value) |>
  arrange(outcome, estimate_label, year) |>
  print(n = Inf)

# --- Print demographic predictors (from factor model, logistic) ---------------

cat("\n", strrep("=", 70), "\n")
cat("DEMOGRAPHIC PREDICTORS — ADJUSTED ODDS RATIOS (Any MH Condition)\n")
cat(strrep("=", 70), "\n\n")

factor_results |>
  filter(outcome == "Any MH Condition", model_type == "logistic",
         !str_detect(term, "year_factor")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = format.pval(p.value, digits = 3),
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE            ~ "")) |>
  select(term, OR = estimate, ci_low = conf.low, ci_high = conf.high, p.value, sig) |>
  print(n = Inf)

# --- B. Interaction models: sex × year, race × year --------------------------

message("\n--- Running interaction models ---")

run_interaction <- function(label, outcome_var, interaction_term) {
  base <- "year_centered + age_group + sex + race + poverty_cat"
  f <- as.formula(paste0(outcome_var, " ~ ", base, " + ", interaction_term))

  m <- svyglm(f, design = svy, family = quasibinomial())

  tidy_model(m, "logistic") |>
    mutate(outcome = label)
}

# Sex × year interaction
sex_int <- imap_dfr(outcomes, \(label, var) {
  run_interaction(label, var, "sex:year_centered")
})

# Race × year interaction
race_int <- imap_dfr(outcomes, \(label, var) {
  run_interaction(label, var, "race:year_centered")
})

cat("\n", strrep("=", 70), "\n")
cat("SEX × YEAR INTERACTION (do trends differ by sex?)\n")
cat("Significant interaction = trend is steeper for one sex\n")
cat(strrep("=", 70), "\n\n")

sex_int |>
  filter(str_detect(term, ":")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = format.pval(p.value, digits = 3),
         sig = ifelse(as.numeric(p.value) < 0.05, "*", "")) |>
  select(outcome, term, OR = estimate, ci_low = conf.low, ci_high = conf.high, p.value, sig) |>
  print(n = Inf)

cat("\n", strrep("=", 70), "\n")
cat("RACE × YEAR INTERACTION (are racial disparities changing?)\n")
cat(strrep("=", 70), "\n\n")

race_int |>
  filter(str_detect(term, ":")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = format.pval(p.value, digits = 3),
         sig = ifelse(as.numeric(p.value) < 0.05, "*", "")) |>
  select(outcome, term, OR = estimate, ci_low = conf.low, ci_high = conf.high, p.value, sig) |>
  print(n = Inf)

# --- C. Forest plots ---------------------------------------------------------

message("\n--- Creating forest plots ---")

# Forest plot 1: Year trend (PR per year) across conditions
p_trend <- trend_results |>
  filter(term == "year_centered", model_type == "poisson") |>
  mutate(outcome = fct_reorder(outcome, estimate)) |>
  ggplot(aes(x = estimate, y = outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.7, color = "#1565C0") +
  geom_text(aes(label = sprintf("PR = %.2f [%.2f–%.2f]", estimate, conf.low, conf.high)),
            hjust = -0.1, size = 3.5, nudge_y = 0.2) +
  scale_x_continuous(limits = c(0.95, 1.12)) +
  labs(
    title    = "Adjusted Annual Trend in MH Diagnoses (Prevalence Ratios)",
    subtitle = "Per 1-year increase, 2019–2024; adjusted for age, sex, race, poverty",
    x = "Prevalence Ratio (per year)", y = NULL,
    caption  = "Source: NSCH 2019–2024. Survey-weighted modified Poisson regression."
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "08_forest_annual_trend.png"), p_trend, width = 9, height = 4.5, dpi = 150)
message("  Saved: 08_forest_annual_trend.png")

# Forest plot 2: Demographic ORs for Any MH Condition
demo_or <- factor_results |>
  filter(outcome == "Any MH Condition", model_type == "logistic",
         !str_detect(term, "year_factor")) |>
  mutate(
    category = case_when(
      str_detect(term, "age_group")   ~ "Age group",
      str_detect(term, "sex")         ~ "Sex",
      str_detect(term, "race")        ~ "Race/ethnicity",
      str_detect(term, "poverty_cat") ~ "Poverty level"
    ),
    # Clean up term labels
    label = term |>
      str_remove("age_group") |>
      str_remove("sex") |>
      str_remove("race") |>
      str_remove("poverty_cat") |>
      str_trim()
  )

p_demo <- demo_or |>
  mutate(label = fct_rev(fct_inorder(label))) |>
  ggplot(aes(x = estimate, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high, color = category), size = 0.6) +
  geom_text(aes(label = sprintf("%.2f", estimate)),
            hjust = -0.3, size = 3.2, color = "grey30") +
  facet_grid(category ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = c(
    "Age group" = "#2196F3", "Sex" = "#AD1457",
    "Race/ethnicity" = "#E65100", "Poverty level" = "#1B5E20"
  ), guide = "none") +
  labs(
    title    = "Adjusted Odds Ratios for Any MH Diagnosis",
    subtitle = "Reference: age 0–5, male, White NH, 400%+ FPL | Survey-weighted logistic regression",
    x = "Adjusted Odds Ratio", y = NULL,
    caption  = "Source: NSCH 2019–2024. All predictors mutually adjusted."
  ) +
  nsch_theme +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0))

ggsave(file.path(fig_dir, "09_forest_demographics.png"), p_demo, width = 9, height = 7, dpi = 150)
message("  Saved: 09_forest_demographics.png")

# Forest plot 3: Year-specific ORs (each year vs. 2019) by condition
p_year <- factor_results |>
  filter(str_detect(term, "year_factor"), model_type == "logistic") |>
  mutate(year = str_extract(term, "\\d{4}")) |>
  ggplot(aes(x = estimate, y = year, color = outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6), size = 0.5) +
  scale_color_manual(values = c(
    "ADHD" = "#2196F3", "Anxiety" = "#FF9800", "Depression" = "#9C27B0",
    "Behavior Problems" = "#F44336", "Any MH Condition" = "#4CAF50"
  )) +
  labs(
    title    = "Year-Specific Adjusted Odds Ratios vs. 2019",
    subtitle = "Adjusted for age, sex, race/ethnicity, and poverty",
    x = "Adjusted OR (ref = 2019)", y = NULL, color = NULL,
    caption  = "Source: NSCH 2019–2024. Survey-weighted logistic regression."
  ) +
  nsch_theme +
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "10_forest_year_vs_2019.png"), p_year, width = 9, height = 4.5, dpi = 150)
message("  Saved: 10_forest_year_vs_2019.png")

# --- D. Export all results ---------------------------------------------------

message("\nSaving regression tables...")

write_csv(trend_results,  file.path(table_dir, "regression_annual_trend.csv"))
write_csv(factor_results, file.path(table_dir, "regression_year_factor.csv"))
write_csv(sex_int,        file.path(table_dir, "regression_sex_interaction.csv"))
write_csv(race_int,       file.path(table_dir, "regression_race_interaction.csv"))

# --- E. Summary comparison table: OR vs PR side by side ----------------------

cat("\n", strrep("=", 70), "\n")
cat("COMPARISON: ODDS RATIOS vs PREVALENCE RATIOS (Annual trend)\n")
cat("Note: ORs overestimate RR when prevalence >10%.\n")
cat(strrep("=", 70), "\n\n")

trend_results |>
  filter(term == "year_centered") |>
  select(outcome, model_type, estimate, conf.low, conf.high) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3))) |>
  pivot_wider(
    names_from = model_type,
    values_from = c(estimate, conf.low, conf.high),
    names_glue = "{model_type}_{.value}"
  ) |>
  select(outcome,
         OR = logistic_estimate, OR_low = logistic_conf.low, OR_high = logistic_conf.high,
         PR = poisson_estimate,  PR_low = poisson_conf.low,  PR_high = poisson_conf.high) |>
  print()

message("\nAll regression outputs saved to output/tables/ and output/figures/")
