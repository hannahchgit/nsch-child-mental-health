# =============================================================================
# NSCH Child Mental Health Analysis
# Script 03: Subgroup Analyses and Visualizations
# =============================================================================

library(tidyverse)
library(srvyr)

options(survey.lonely.psu = "adjust")

proc_dir  <- here::here("data", "processed")
fig_dir   <- here::here("output", "figures")
table_dir <- here::here("output", "tables")
dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load --------------------------------------------------------------------

nsch <- readRDS(file.path(proc_dir, "nsch_clean.rds"))

svy <- nsch |>
  as_survey_design(strata = STRATUM, weights = FWC, ids = 1)

# --- Shared theme / helpers --------------------------------------------------

nsch_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    plot.caption     = element_text(color = "grey50", size = 9, hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

CAPTION <- "Source: NSCH topical files, 2019–2024. Weighted estimates using FWC child weight. Ribbons/bars = 95% CI."

COND_COLORS <- c(
  "ADHD"              = "#2196F3",
  "Anxiety"           = "#FF9800",
  "Depression"        = "#9C27B0",
  "Behavior Problems" = "#F44336",
  "Any MH Condition"  = "#4CAF50"
)

# Tidy up a srvyr summarise result into long form for plotting
# expects cols named: <prefix>_pct, <prefix>_pct_low, <prefix>_pct_upp
tidy_svy <- function(df, ...) {
  grouping <- names(select(df, ...))
  df |>
    pivot_longer(
      cols          = -all_of(grouping),
      names_to      = c("condition", ".value"),
      names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
    )
}

# --- 1. Overall trend by condition -------------------------------------------

message("Fig 1: Overall trend")

trend <- svy |>
  group_by(survey_year) |>
  summarise(
    ADHD_pct              = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Anxiety_pct           = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
    Depression_pct        = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
    `Behavior Problems_pct` = survey_mean(beh_ever,   na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -survey_year,
    names_to      = c("condition", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

p1 <- ggplot(trend, aes(survey_year, pct, color = condition, fill = condition)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = COND_COLORS) +
  scale_fill_manual(values  = COND_COLORS) +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title   = "Diagnosed Mental Health Conditions Among U.S. Children, 2019–2024",
    subtitle = "Ever-diagnosed prevalence, weighted to U.S. child population",
    x = NULL, y = "Prevalence (%)", color = NULL, fill = NULL, caption = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "01_overall_trend.png"), p1, width = 9, height = 5.5, dpi = 150)
message("  Saved: 01_overall_trend.png")

# --- 2. By age group ---------------------------------------------------------

message("Fig 2: By age group")

age_long <- svy |>
  filter(!is.na(age_group)) |>
  group_by(survey_year, age_group) |>
  summarise(
    ADHD_pct             = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Anxiety_pct          = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
    Depression_pct       = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
    `Any MH Condition_pct` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -c(survey_year, age_group),
    names_to      = c("condition", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

p2 <- ggplot(age_long, aes(survey_year, pct, color = condition, fill = condition)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~age_group, nrow = 1, labeller = label_both) +
  scale_color_manual(values = COND_COLORS) +
  scale_fill_manual(values  = COND_COLORS) +
  scale_x_continuous(breaks = c(2019, 2022, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title    = "MH Diagnosis Trends by Child Age Group",
    subtitle = "Depression and anxiety concentrated in 12–17; ADHD spans 6–17",
    x = NULL, y = "Prevalence (%)", color = NULL, fill = NULL, caption = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "02_by_age_group.png"), p2, width = 11, height = 5.5, dpi = 150)
message("  Saved: 02_by_age_group.png")

# --- 3. By sex ---------------------------------------------------------------

message("Fig 3: By sex")

sex_long <- svy |>
  filter(!is.na(sex)) |>
  group_by(survey_year, sex) |>
  summarise(
    ADHD_pct             = survey_mean(adhd_ever, na.rm = TRUE, vartype = "ci") * 100,
    Anxiety_pct          = survey_mean(anx_ever,  na.rm = TRUE, vartype = "ci") * 100,
    Depression_pct       = survey_mean(dep_ever,  na.rm = TRUE, vartype = "ci") * 100,
    `Behavior Problems_pct` = survey_mean(beh_ever, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -c(survey_year, sex),
    names_to      = c("condition", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

p3 <- ggplot(sex_long, aes(survey_year, pct, color = sex, fill = sex)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~condition, nrow = 1) +
  scale_color_manual(values = c("Male" = "#1565C0", "Female" = "#AD1457")) +
  scale_fill_manual(values  = c("Male" = "#1565C0", "Female" = "#AD1457")) +
  scale_x_continuous(breaks = c(2019, 2022, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title    = "MH Diagnosis Trends by Sex",
    subtitle = "Boys: higher ADHD/behavior; Girls: higher anxiety/depression",
    x = NULL, y = "Prevalence (%)", color = NULL, fill = NULL, caption = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "03_by_sex.png"), p3, width = 11, height = 5, dpi = 150)
message("  Saved: 03_by_sex.png")

# --- 4. By race/ethnicity ----------------------------------------------------

message("Fig 4: By race/ethnicity")

race_long <- svy |>
  filter(!is.na(race)) |>
  group_by(survey_year, race) |>
  summarise(
    `Any MH Condition_pct` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100,
    ADHD_pct               = survey_mean(adhd_ever,   na.rm = TRUE, vartype = "ci") * 100,
    Anxiety_pct            = survey_mean(anx_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Depression_pct         = survey_mean(dep_ever,    na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -c(survey_year, race),
    names_to      = c("condition", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

RACE_COLORS <- c(
  "White NH"       = "#1A237E",
  "Black NH"       = "#B71C1C",
  "Hispanic"       = "#E65100",
  "Asian NH"       = "#1B5E20",
  "AIAN NH"        = "#4A148C",
  "Other/Multi NH" = "#00695C"
)

p4 <- race_long |>
  filter(condition == "Any MH Condition") |>
  ggplot(aes(survey_year, pct, color = race, fill = race)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = RACE_COLORS) +
  scale_fill_manual(values  = RACE_COLORS) +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title   = "Any MH Diagnosis by Race/Ethnicity, 2019–2024",
    subtitle = "White NH children show highest diagnosed prevalence; Asian NH lowest",
    x = NULL, y = "Prevalence (%)", color = NULL, fill = NULL, caption = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "04_by_race.png"), p4, width = 9, height = 5.5, dpi = 150)
message("  Saved: 04_by_race.png")

# --- 5. By poverty level -----------------------------------------------------

message("Fig 5: By poverty level")

pov_long <- svy |>
  filter(!is.na(poverty_cat)) |>
  group_by(survey_year, poverty_cat) |>
  summarise(
    `Any MH Condition_pct` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100,
    ADHD_pct               = survey_mean(adhd_ever,   na.rm = TRUE, vartype = "ci") * 100,
    Anxiety_pct            = survey_mean(anx_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Depression_pct         = survey_mean(dep_ever,    na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -c(survey_year, poverty_cat),
    names_to      = c("condition", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

POV_COLORS <- c(
  "<100% FPL"    = "#B71C1C",
  "100-199% FPL" = "#E65100",
  "200-399% FPL" = "#1565C0",
  "400%+ FPL"    = "#1B5E20"
)

p5 <- pov_long |>
  filter(condition == "Any MH Condition") |>
  ggplot(aes(survey_year, pct, color = poverty_cat, fill = poverty_cat)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = POV_COLORS, name = "Family income") +
  scale_fill_manual(values  = POV_COLORS, name = "Family income") +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title   = "Any MH Diagnosis by Family Poverty Level, 2019–2024",
    subtitle = "Higher diagnosed prevalence in lower-income families across all years",
    x = NULL, y = "Prevalence (%)",
    caption = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "05_by_poverty.png"), p5, width = 9, height = 5.5, dpi = 150)
message("  Saved: 05_by_poverty.png")

# --- 6. Severity trend (unweighted; only among diagnosed) --------------------

message("Fig 6: Severity among diagnosed")

severity_pct <- nsch |>
  pivot_longer(
    cols      = c(adhd_severity, anx_severity, dep_severity, beh_severity),
    names_to  = "condition",
    values_to = "severity"
  ) |>
  filter(!is.na(severity)) |>
  mutate(condition = recode(condition,
    adhd_severity = "ADHD",  anx_severity = "Anxiety",
    dep_severity  = "Depression", beh_severity = "Behavior Problems"
  )) |>
  count(survey_year, condition, severity) |>
  group_by(survey_year, condition) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

p6 <- ggplot(severity_pct,
       aes(survey_year, pct,
           fill = factor(severity, levels = c("Mild", "Moderate", "Severe")))) +
  geom_area(position = "stack", alpha = 0.85) +
  facet_wrap(~condition, nrow = 1) +
  scale_fill_manual(
    values = c("Mild" = "#81C784", "Moderate" = "#FFB74D", "Severe" = "#E57373"),
    name   = "Severity"
  ) +
  scale_x_continuous(breaks = c(2019, 2022, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title    = "Severity Distribution Among Diagnosed Children, 2019–2024",
    subtitle = "Among ever-diagnosed children only; unweighted",
    x = NULL, y = "% of diagnosed",
    caption  = "Source: NSCH topical files, 2019–2024. Severity not collected for non-diagnosed children."
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "06_severity_trend.png"), p6, width = 11, height = 5, dpi = 150)
message("  Saved: 06_severity_trend.png")

# --- 7. Treatment access -----------------------------------------------------

message("Fig 7: Treatment access")

# K4Q22_R:  received MH professional treatment  (1=Yes, 2=No)
# TREATNEED: needed but couldn't get MH care    (1=Yes, 2=No)

access_long <- svy |>
  filter(!is.na(K4Q22_R), !is.na(TREATNEED)) |>
  group_by(survey_year) |>
  summarise(
    `Received MH treatment_pct`         = survey_mean(K4Q22_R  == 1, na.rm = TRUE, vartype = "ci") * 100,
    `Needed but didn't receive care_pct` = survey_mean(TREATNEED == 1, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  pivot_longer(
    cols          = -survey_year,
    names_to      = c("measure", ".value"),
    names_pattern = "^(.+)_(pct|pct_low|pct_upp)$"
  )

p7 <- ggplot(access_long, aes(survey_year, pct, color = measure, fill = measure)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Received MH treatment"        = "#1565C0",
    "Needed but didn't receive care" = "#C62828"
  )) +
  scale_fill_manual(values = c(
    "Received MH treatment"        = "#1565C0",
    "Needed but didn't receive care" = "#C62828"
  )) +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(
    title    = "Mental Health Treatment Access, 2019–2024",
    subtitle = "% of all children who received treatment vs. had unmet need",
    x = NULL, y = "% of all children", color = NULL, fill = NULL,
    caption  = CAPTION
  ) +
  nsch_theme

ggsave(file.path(fig_dir, "07_treatment_access.png"), p7, width = 9, height = 5.5, dpi = 150)
message("  Saved: 07_treatment_access.png")

# --- Summary table -----------------------------------------------------------

message("Writing summary table...")

summary_table <- svy |>
  group_by(survey_year) |>
  summarise(
    n            = survey_total(1, vartype = NULL),
    adhd_pct     = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
    anx_pct      = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
    dep_pct      = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
    beh_pct      = survey_mean(beh_ever,     na.rm = TRUE, vartype = "ci") * 100,
    any_mh_pct   = survey_mean(any_mh_ever,  na.rm = TRUE, vartype = "ci") * 100
  ) |>
  mutate(across(where(is.double) & !n, \(x) round(x, 1)))

write_csv(summary_table, file.path(table_dir, "prevalence_by_year.csv"))

cat("\n", strrep("=", 65), "\n")
cat("WEIGHTED PREVALENCE SUMMARY (2019–2024)\n")
cat(strrep("=", 65), "\n\n")
print(summary_table)

message("\nAll figures: output/figures/")
message("Summary table: output/tables/prevalence_by_year.csv")
