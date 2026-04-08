# =============================================================================
# NSCH Child Mental Health Analysis
# Script 01: Data Import and Variable Identification
# =============================================================================
# Data: National Survey of Children's Health (NSCH) — Screener files
# Years: 2019, 2021, 2023, 2024
# Source: childhealthdata.org / data.census.gov
#
# NOTE ON FILE TYPES
# These are SCREENER files. They capture:
#   - CSHCN (Children with Special Health Care Needs) status
#   - Whether a child needs/receives treatment for emotional/behavioral conditions
#   - Basic demographics and household variables
#
# Condition-specific diagnoses (ADHD, anxiety, depression, conduct disorder)
# live in the TOPICAL questionnaire files (nsch_*_topical_SAS.zip).
# Download those from childhealthdata.org to extend this analysis.
#
# Key screener MH variables:
#   C_K2Q22  — Child currently needs treatment for emotional/developmental/behavioral condition
#   C_K2Q23  — Condition is chronic (12+ months); asked only if C_K2Q22 = 1 (Yes)
#   C_CSHCN  — Composite CSHCN screener status
#
# Survey design:
#   Weight : C_FWS  (child-level analytic weight)
#   Strata : STRATUM
#   NSCH uses a single-PSU-per-stratum design; set lonely.psu = "adjust"
# =============================================================================

library(tidyverse)
library(haven)
library(survey)
library(srvyr)

# --- File paths --------------------------------------------------------------

raw_dir  <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")

data_files <- c(
  "2019" = file.path(raw_dir, "nsch_2019e_screener.sas7bdat"),
  "2021" = file.path(raw_dir, "nsch_2021e_screener.sas7bdat"),
  "2023" = file.path(raw_dir, "nsch_2023e_screener.sas7bdat"),
  "2024" = file.path(raw_dir, "nsch_2024e_screener.sas7bdat")
)

missing_files <- data_files[!file.exists(data_files)]
if (length(missing_files) > 0) {
  stop("Missing data files:\n", paste(names(missing_files), unlist(missing_files), sep = " -> ", collapse = "\n"))
}

# --- Load data ---------------------------------------------------------------

message("Loading NSCH screener files...")

nsch_raw <- map(data_files, \(path) {
  message("  Reading: ", basename(path))
  read_sas(path)
})

# --- Structure check ---------------------------------------------------------

cat("\n", strrep("=", 65), "\n")
cat("DATASET STRUCTURE\n")
cat(strrep("=", 65), "\n\n")

iwalk(nsch_raw, \(df, yr) {
  wt_vars  <- grep("fwc|fwt|fwh|fws|weight|wgt", names(df), value = TRUE, ignore.case = TRUE)
  str_vars <- grep("^str|^psu|^stra", names(df), value = TRUE, ignore.case = TRUE)

  cat(sprintf(
    "Year %s | Rows: %6d | Cols: %2d | Weight vars: %-20s | Strata: %s\n",
    yr, nrow(df), ncol(df),
    paste(wt_vars, collapse = ", "),
    paste(str_vars, collapse = ", ")
  ))
})

# --- Variable label lookup helper --------------------------------------------

var_labels <- function(df) {
  map_chr(df, \(col) {
    lbl <- attr(col, "label")
    if (!is.null(lbl)) lbl else ""
  })
}

# --- Mental health variable identification -----------------------------------
# Strategy:
#   1. Search variable names AND labels for condition-specific keywords.
#   2. Exclude known false positives (e.g., "C_HISPANIC_R" matches "panic").
#
# Screener-level: expect C_K2Q22, C_K2Q23, C_CSHCN
# Topical-level keywords (for when you add those files):
#   ADHD       -> K2Q31A, K2Q31B, K2Q31C, K2Q31D
#   Anxiety    -> K2Q32A, K2Q32B, K2Q32C
#   Depression -> K2Q33A, K2Q33B, K2Q33C
#   Behavioral -> K2Q34A, K2Q35A, K2Q36A (conduct/oppositional/developmental delay)

mh_keyword_patterns <- list(
  anxiety    = "anxi|panic disorder|phob|generalized anxiety",
  depression = "depress|sad persistently|hopeless|mood disorder",
  behavioral = "behav|conduct|oppos|defiant|aggress|emotion.*develop.*behav|develop.*behav",
  adhd       = "adhd|attention deficit|hyperactiv|inattent"
)

# Variables to explicitly exclude (label keyword false-positives)
exclude_vars <- c("C_HISPANIC_R", "C_HISPANIC_R_IF")

find_mh_vars <- function(df, yr) {
  labels  <- var_labels(df)
  # Search both the variable name and its label, case-insensitive
  targets <- paste(tolower(names(df)), tolower(labels), sep = " ")

  imap_dfr(mh_keyword_patterns, \(pattern, domain) {
    hits <- str_detect(targets, pattern) & !names(df) %in% exclude_vars
    tibble(
      year     = yr,
      domain   = domain,
      variable = names(df)[hits],
      label    = labels[hits]
    )
  })
}

message("Identifying mental health variables across years...")
mh_vars <- imap_dfr(nsch_raw, find_mh_vars)

# --- Print MH variable results -----------------------------------------------

cat("\n", strrep("=", 65), "\n")
cat("MENTAL HEALTH VARIABLES FOUND IN SCREENER FILES\n")
cat(strrep("=", 65), "\n\n")

if (nrow(mh_vars) == 0) {
  cat("No condition-specific MH variables found in screener files.\n")
  cat("This is expected — diagnosis variables (ADHD, anxiety, depression,\n")
  cat("conduct disorder) are in the TOPICAL questionnaire files.\n\n")
} else {
  cat("Summary counts:\n")
  mh_vars |>
    count(year, domain) |>
    pivot_wider(names_from = year, values_from = n, values_fill = 0) |>
    print()

  cat("\nDetailed list:\n")
  mh_vars |>
    arrange(domain, variable, year) |>
    group_by(domain, variable) |>
    summarise(
      years = paste(sort(unique(year)), collapse = ", "),
      label = first(label),
      .groups = "drop"
    ) |>
    print(n = Inf)
}

# --- Screener MH variables: distributions ------------------------------------

cat("\n", strrep("=", 65), "\n")
cat("SCREENER MH VARIABLES: VALUE DISTRIBUTIONS BY YEAR\n")
cat(strrep("=", 65), "\n")
cat("(1 = Yes, 2 = No; NAs shown separately)\n\n")

# These three variables are present in all four screener years
screener_mh_vars <- c("C_K2Q22", "C_K2Q23", "C_CSHCN")

screener_var_labels <- c(
  C_K2Q22 = "Needs treatment for emotional/dev/behavioral condition",
  C_K2Q23 = "Condition is chronic (12+ months) [asked if C_K2Q22 = 1]",
  C_CSHCN = "CSHCN screener status (composite)"
)

iwalk(nsch_raw, \(df, yr) {
  cat("Year:", yr, "\n")
  present_vars <- intersect(screener_mh_vars, names(df))

  for (v in present_vars) {
    tbl <- table(df[[v]], useNA = "always")
    na_n <- sum(is.na(df[[v]]))
    pct  <- round(prop.table(tbl[!is.na(names(tbl)) | names(tbl) != "NA"]) * 100, 1)
    cat(sprintf("  %-15s %s\n", v, screener_var_labels[v]))
    cat(sprintf("    1 (Yes): %6d (%5.1f%%) | 2 (No): %6d (%5.1f%%) | NA: %d\n",
      sum(df[[v]] == 1, na.rm = TRUE),
      mean(df[[v]] == 1, na.rm = TRUE) * 100,
      sum(df[[v]] == 2, na.rm = TRUE),
      mean(df[[v]] == 2, na.rm = TRUE) * 100,
      na_n
    ))
  }
  cat("\n")
})

# --- Survey design setup (demonstration) ------------------------------------

cat(strrep("=", 65), "\n")
cat("SURVEY DESIGN SETUP (using 2021 as example)\n")
cat(strrep("=", 65), "\n\n")

# NSCH uses a single-PSU-per-stratum design — set lonely.psu to "adjust"
options(survey.lonely.psu = "adjust")

svy_2021 <- nsch_raw[["2021"]] |>
  as_survey_design(
    strata = STRATUM,
    weights = C_FWS,
    ids = 1           # no cluster variable in screener; treat each row as its own PSU
  )

cat("Survey design object (2021):\n")
print(svy_2021)

# Quick weighted estimate: % children needing behavioral/emotional treatment
cat("\nWeighted % needing treatment for emotional/dev/behavioral condition (C_K2Q22 = 1):\n")
svy_2021 |>
  filter(!is.na(C_K2Q22)) |>
  summarise(
    pct_needs_treatment = survey_mean(C_K2Q22 == 1, vartype = "ci", level = 0.95)
  ) |>
  mutate(across(where(is.numeric), \(x) round(x * 100, 2))) |>
  rename(pct = pct_needs_treatment, ci_low = pct_needs_treatment_low, ci_upp = pct_needs_treatment_upp) |>
  print()

# --- Save outputs ------------------------------------------------------------

dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

message("Saving raw data list -> data/processed/nsch_raw_list.rds")
saveRDS(nsch_raw, file.path(proc_dir, "nsch_raw_list.rds"))

if (nrow(mh_vars) > 0) {
  message("Saving MH variable index -> data/processed/mh_vars_index.csv")
  write_csv(mh_vars, file.path(proc_dir, "mh_vars_index.csv"))
}

# Summary of what's available
cat("\n", strrep("=", 65), "\n")
cat("NEXT STEPS\n")
cat(strrep("=", 65), "\n")
cat("Screener files loaded. Available MH variables per year:\n\n")
cat("  C_K2Q22  — Needs treatment for emotional/dev/behavioral condition (all years)\n")
cat("  C_K2Q23  — Chronic condition, 12+ months (all years, subset of C_K2Q22=1)\n")
cat("  C_CSHCN  — CSHCN composite status (all years)\n\n")
cat("For ADHD, anxiety, depression, and conduct disorder diagnoses:\n")
cat("  Download topical files from childhealthdata.org\n")
cat("  Key topical variables:\n")
cat("    K2Q31A/B/C/D  ADHD (ever diagnosed / current / severity / treatment)\n")
cat("    K2Q32A/B/C    Anxiety (ever / current / treatment)\n")
cat("    K2Q33A/B/C    Depression (ever / current / treatment)\n")
cat("    K2Q34A/35A    Behavioral/conduct problems\n\n")
cat("Run scripts/02_clean_data.R to prepare data for analysis.\n")
