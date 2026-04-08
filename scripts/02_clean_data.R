# =============================================================================
# NSCH Child Mental Health Analysis
# Script 02: Data Cleaning and Preparation
# =============================================================================
# Loads topical questionnaire files (2019, 2021, 2022, 2023, 2024).
# Extracts and standardizes mental health diagnosis variables, demographics,
# and survey design variables. Outputs a single stacked, analysis-ready dataset.
#
# Mental health variables (consistent names across all 4 years):
#   K2Q31A / K2Q31B / K2Q31C / K2Q31D  ADD/ADHD: ever / current / severity / medication
#   K2Q32A / K2Q32B / K2Q32C           Depression: ever / current / severity
#   K2Q33A / K2Q33B / K2Q33C           Anxiety: ever / current / severity
#   K2Q34A / K2Q34B / K2Q34C           Behavior problems: ever / current / severity
#
# Value coding (1 = Yes, 2 = No; severity: 1 = Mild, 2 = Moderate, 3 = Severe)
#
# Survey design (topical files):
#   Weight  : FWC   (child-level topical weight)
#   Strata  : STRATUM
#   options(survey.lonely.psu = "adjust")
# =============================================================================

library(tidyverse)
library(haven)
library(survey)
library(srvyr)

options(survey.lonely.psu = "adjust")

raw_dir  <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load topical files ------------------------------------------------------

topical_files <- c(
  "2019" = file.path(raw_dir, "nsch_2019e_topical.sas7bdat"),
  "2021" = file.path(raw_dir, "nsch_2021e_topical.sas7bdat"),
  "2022" = file.path(raw_dir, "nsch_2022e_topical.sas7bdat"),
  "2023" = file.path(raw_dir, "nsch_2023e_topical.sas7bdat"),
  "2024" = file.path(raw_dir, "nsch_2024e_topical.sas7bdat")
)

# --- Select and rename variables ---------------------------------------------

# Variables present in all 4 years (verified in 02_clean_data.R exploration)
MH_VARS <- c(
  # ADHD
  "K2Q31A", "K2Q31B", "K2Q31C", "K2Q31D",
  # Depression
  "K2Q32A", "K2Q32B", "K2Q32C",
  # Anxiety
  "K2Q33A", "K2Q33B", "K2Q33C",
  # Behavior problems
  "K2Q34A", "K2Q34B", "K2Q34C"
)

DEMO_VARS <- c(
  "SC_AGE_YEARS",   # child age in years
  "SC_SEX",         # 1 = Male, 2 = Female
  "SC_RACE_R",      # race (detailed); see value labels in SAS script
  "SC_HISPANIC_R",  # 1 = Hispanic, 2 = Non-Hispanic
  "FPL_I4",         # poverty ratio (4th implicate); <1 = below poverty
  "SC_CSHCN",       # CSHCN composite: 1 = Yes, 2 = No
  "METRO_YN",       # metro area: 1 = Metro, 2 = Non-metro
  "FIPSST"          # state FIPS
)

DESIGN_VARS <- c("STRATUM", "FWC")

OPTIONAL_VARS <- c(
  "K4Q22_R",    # Mental health professional treatment (all years)
  "K4Q28X04",   # Needed MH care but didn't receive it (all years)
  "K4Q23",      # Currently taking medication for emotions/concentration/behavior
  "FORMTYPE",   # Age form type (1 = 0-5, 2 = 6-11, 3 = 12-17)
  "SC_K2Q22"    # Screener MH flag (in topical for 2019/2021/2024; not 2023)
)

select_vars <- function(df, yr) {
  keep <- c(MH_VARS, DEMO_VARS, DESIGN_VARS,
            intersect(OPTIONAL_VARS, names(df)))
  missing <- setdiff(c(MH_VARS, DEMO_VARS, DESIGN_VARS), names(df))
  if (length(missing) > 0) warning("Year ", yr, " missing: ", paste(missing, collapse = ", "))
  df |> select(all_of(keep))
}

# --- Recode to clean indicators ----------------------------------------------
# All A/B vars: 1 = Yes, 2 = No  → recode to logical (TRUE/FALSE)
# Severity (C vars): 1 = Mild, 2 = Moderate, 3 = Severe

recode_mh <- function(df) {
  df |>
    mutate(
      # --- ADHD ---
      adhd_ever      = K2Q31A == 1,
      adhd_current   = if_else(K2Q31A == 1, K2Q31B == 1, NA),
      adhd_severity  = if_else(K2Q31A == 1,
                         factor(K2Q31C, levels = 1:3,
                                labels = c("Mild", "Moderate", "Severe")),
                         factor(NA, levels = c("Mild", "Moderate", "Severe"))),
      adhd_medicated = if_else(K2Q31A == 1, K2Q31D == 1, NA),

      # --- Depression ---
      dep_ever     = K2Q32A == 1,
      dep_current  = if_else(K2Q32A == 1, K2Q32B == 1, NA),
      dep_severity = if_else(K2Q32A == 1,
                       factor(K2Q32C, levels = 1:3,
                              labels = c("Mild", "Moderate", "Severe")),
                       factor(NA, levels = c("Mild", "Moderate", "Severe"))),

      # --- Anxiety ---
      anx_ever     = K2Q33A == 1,
      anx_current  = if_else(K2Q33A == 1, K2Q33B == 1, NA),
      anx_severity = if_else(K2Q33A == 1,
                       factor(K2Q33C, levels = 1:3,
                              labels = c("Mild", "Moderate", "Severe")),
                       factor(NA, levels = c("Mild", "Moderate", "Severe"))),

      # --- Behavior problems ---
      beh_ever     = K2Q34A == 1,
      beh_current  = if_else(K2Q34A == 1, K2Q34B == 1, NA),
      beh_severity = if_else(K2Q34A == 1,
                       factor(K2Q34C, levels = 1:3,
                              labels = c("Mild", "Moderate", "Severe")),
                       factor(NA, levels = c("Mild", "Moderate", "Severe"))),

      # --- Any MH condition (ever diagnosed) ---
      any_mh_ever = adhd_ever | dep_ever | anx_ever | beh_ever,

      # --- Demographics ---
      age_group = cut(SC_AGE_YEARS,
                      breaks = c(-Inf, 5, 11, 17),
                      labels = c("0-5", "6-11", "12-17"),
                      right  = TRUE),
      sex       = factor(SC_SEX, levels = 1:2, labels = c("Male", "Female")),
      hispanic  = SC_HISPANIC_R == 1,

      # Race: SC_RACE_R codes: 1=White, 2=Black, 3=AIAN, 4=Asian, 5=NHPI, 7=Multiple
      race = factor(case_when(
        SC_HISPANIC_R == 1            ~ "Hispanic",
        SC_RACE_R == 1                ~ "White NH",
        SC_RACE_R == 2                ~ "Black NH",
        SC_RACE_R == 3                ~ "AIAN NH",
        SC_RACE_R == 4                ~ "Asian NH",
        SC_RACE_R %in% c(5, 7)       ~ "Other/Multi NH",
        TRUE                          ~ NA_character_
      ), levels = c("White NH", "Black NH", "Hispanic", "Asian NH",
                    "AIAN NH", "Other/Multi NH")),

      # Poverty: FPL_I4 is % of poverty line; <100% = below poverty
      poverty_cat = cut(FPL_I4,
                        breaks = c(0, 99, 199, 399, Inf),
                        labels = c("<100% FPL", "100-199% FPL",
                                   "200-399% FPL", "400%+ FPL"),
                        right  = TRUE),

      metro = factor(METRO_YN, levels = 1:2, labels = c("Metro", "Non-metro")),
      cshcn = SC_CSHCN == 1
    )
}

# --- Load, select, recode, and stack one year at a time ---------------------
# haven/ReadStat has a bug reading multiple large SAS7BDAT files consecutively
# in one session. Work around by caching each year to RDS via a subprocess,
# then reading the lightweight RDS files back in the main session.

cache_dir <- file.path(proc_dir, "year_cache")
dir.create(cache_dir, showWarnings = FALSE)

message("Processing each year (subprocess per file to avoid ReadStat memory bug)...")

iwalk(topical_files, \(path, yr) {
  cache_path <- file.path(cache_dir, paste0("nsch_", yr, ".rds"))
  if (file.exists(cache_path)) {
    message("  Skipping ", yr, " (cached)")
    return(invisible(NULL))
  }
  message("  Processing: ", yr)
  # Each year runs in a clean R subprocess — no accumulated state
  script <- sprintf('
    suppressPackageStartupMessages({ library(haven); library(dplyr) })
    select_vars <- function(df, yr) {
      MH_VARS     <- c("K2Q31A","K2Q31B","K2Q31C","K2Q31D",
                       "K2Q32A","K2Q32B","K2Q32C",
                       "K2Q33A","K2Q33B","K2Q33C",
                       "K2Q34A","K2Q34B","K2Q34C")
      DEMO_VARS   <- c("SC_AGE_YEARS","SC_SEX","SC_RACE_R","SC_HISPANIC_R",
                       "FPL_I4","SC_CSHCN","METRO_YN","FIPSST")
      DESIGN_VARS <- c("STRATUM","FWC")
      OPTIONAL    <- c("K4Q22_R","K4Q28X04","K4Q23","FORMTYPE","TREATNEED","SC_K2Q22")
      keep <- c(MH_VARS, DEMO_VARS, DESIGN_VARS, intersect(OPTIONAL, names(df)))
      df[, keep]
    }
    df <- read_sas("%s")
    df <- select_vars(df, "%s")
    saveRDS(df, "%s")
  ', path, yr, cache_path)
  tmp <- tempfile(fileext = ".R")
  writeLines(script, tmp)
  ret <- system2("/usr/local/bin/Rscript", tmp, stdout = TRUE, stderr = TRUE)
  unlink(tmp)
  if (!file.exists(cache_path)) {
    stop("Failed to cache year ", yr, ":\n", paste(ret, collapse = "\n"))
  }
})

message("Combining cached years and recoding...")

nsch_clean <- imap_dfr(topical_files, \(path, yr) {
  cache_path <- file.path(cache_dir, paste0("nsch_", yr, ".rds"))
  readRDS(cache_path) |>
    recode_mh() |>
    mutate(survey_year = as.integer(yr))
})

message("Combined dataset: ", nrow(nsch_clean), " rows x ", ncol(nsch_clean), " cols")

message("Combined dataset: ", nrow(nsch_clean), " rows x ", ncol(nsch_clean), " cols")

# --- Validation: prevalence check by year ------------------------------------

cat("\n", strrep("=", 65), "\n")
cat("UNWEIGHTED PREVALENCE CHECK BY YEAR\n")
cat(strrep("=", 65), "\n\n")

nsch_clean |>
  group_by(survey_year) |>
  summarise(
    n             = n(),
    pct_adhd      = mean(adhd_ever, na.rm = TRUE) * 100,
    pct_anxiety   = mean(anx_ever,  na.rm = TRUE) * 100,
    pct_dep       = mean(dep_ever,  na.rm = TRUE) * 100,
    pct_beh       = mean(beh_ever,  na.rm = TRUE) * 100,
    pct_any_mh    = mean(any_mh_ever, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(across(starts_with("pct"), \(x) round(x, 1))) |>
  print()

# --- Weighted prevalence (srvyr) ---------------------------------------------

cat("\n", strrep("=", 65), "\n")
cat("WEIGHTED PREVALENCE BY YEAR (srvyr, FWC weight)\n")
cat(strrep("=", 65), "\n\n")

svy_stacked <- nsch_clean |>
  as_survey_design(
    strata  = STRATUM,
    weights = FWC,
    ids     = 1
  )

svy_stacked |>
  group_by(survey_year) |>
  summarise(
    adhd_pct    = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
    anxiety_pct = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
    dep_pct     = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
    beh_pct     = survey_mean(beh_ever,     na.rm = TRUE, vartype = "ci") * 100,
    any_mh_pct  = survey_mean(any_mh_ever,  na.rm = TRUE, vartype = "ci") * 100
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  select(survey_year,
         adhd_pct, adhd_pct_low, adhd_pct_upp,
         anxiety_pct, anxiety_pct_low, anxiety_pct_upp,
         dep_pct, dep_pct_low, dep_pct_upp,
         any_mh_pct, any_mh_pct_low, any_mh_pct_upp) |>
  print()

# --- Save outputs ------------------------------------------------------------

message("\nSaving cleaned dataset -> data/processed/nsch_clean.rds")
saveRDS(nsch_clean, file.path(proc_dir, "nsch_clean.rds"))

message("Saving survey design object -> data/processed/nsch_svy.rds")
saveRDS(svy_stacked, file.path(proc_dir, "nsch_svy.rds"))

message("Saving flat CSV (no survey attributes) -> data/processed/nsch_clean.csv")
nsch_clean |>
  select(survey_year, STRATUM, FWC, FIPSST,
         starts_with("adhd_"), starts_with("dep_"), starts_with("anx_"),
         starts_with("beh_"), any_mh_ever,
         age_group, sex, race, hispanic, poverty_cat, metro, cshcn,
         SC_AGE_YEARS, SC_SEX, SC_RACE_R, SC_HISPANIC_R, FPL_I4) |>
  write_csv(file.path(proc_dir, "nsch_clean.csv"))

message("\nDone. Next step: scripts/03_analysis.R")
