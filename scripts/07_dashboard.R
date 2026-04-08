# =============================================================================
# NSCH Child Mental Health Analysis
# Script 07: Interactive Dashboard (self-contained HTML)
# =============================================================================
# Generates a single HTML file with interactive plotly charts.
# No server required — can be hosted on GitHub Pages or shared directly.
# =============================================================================

library(tidyverse)
library(survey)
library(srvyr)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(bslib)

options(survey.lonely.psu = "adjust")

proc_dir <- here::here("data", "processed")
out_path <- here::here("output", "dashboard.html")

# --- Load and prepare --------------------------------------------------------

nsch <- readRDS(file.path(proc_dir, "nsch_clean.rds")) |>
  filter(!is.na(race), !is.na(poverty_cat), !is.na(sex), !is.na(age_group)) |>
  mutate(
    period = factor(
      case_when(
        survey_year == 2019          ~ "Pre-COVID (2019)",
        survey_year %in% 2022:2024   ~ "Post-COVID (2022\u20132024)",
        TRUE                         ~ NA_character_
      ),
      levels = c("Pre-COVID (2019)", "Post-COVID (2022\u20132024)")
    ),
    got_mh_treatment = K4Q22_R == 1,
    unmet_need       = TREATNEED == 1
  )

svy <- nsch |> as_survey_design(strata = STRATUM, weights = FWC, ids = 1)

message("Data loaded: ", nrow(nsch), " children")

COND_COLORS <- c(
  "ADHD" = "#2196F3", "Anxiety" = "#FF9800",
  "Depression" = "#9C27B0", "Behavior Problems" = "#F44336",
  "Any MH Condition" = "#4CAF50"
)

RACE_COLORS <- c(
  "White NH" = "#1A237E", "Black NH" = "#B71C1C", "Hispanic" = "#E65100",
  "Asian NH" = "#1B5E20", "AIAN NH" = "#4A148C", "Other/Multi NH" = "#00695C"
)

# =============================================================================
# DATA PREP: precompute all summary tables
# =============================================================================
message("Computing weighted estimates...")

# Helper: srvyr produces Foo, Foo_low, Foo_upp columns.
# This pivots them into long form with columns: condition, pct, pct_low, pct_upp.
pivot_svy <- function(df, group_cols) {
  df |>
    pivot_longer(
      cols          = -all_of(group_cols),
      names_to      = c("condition", ".value"),
      names_pattern = "^(.+?)(_low|_upp)?$"
    ) |>
    # The base value has no suffix so its .value name is "" — rename it
    rename_with(\(x) if_else(x == "", "pct", x))
}

# Alternate approach that avoids the empty-name issue:
# Manually gather and reshape
svy_to_long <- function(df, group_cols) {
  nms <- setdiff(names(df), group_cols)
  base_nms <- unique(str_remove(nms, "(_low|_upp)$"))

  map_dfr(base_nms, \(bn) {
    low_nm <- paste0(bn, "_low")
    upp_nm <- paste0(bn, "_upp")
    df |>
      select(all_of(group_cols), pct = all_of(bn),
             pct_low = all_of(low_nm), pct_upp = all_of(upp_nm)) |>
      mutate(condition = bn)
  })
}

# 1. Overall trend
trend <- svy |>
  group_by(survey_year) |>
  summarise(
    ADHD              = survey_mean(adhd_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Anxiety           = survey_mean(anx_ever,     na.rm = TRUE, vartype = "ci") * 100,
    Depression        = survey_mean(dep_ever,     na.rm = TRUE, vartype = "ci") * 100,
    `Behavior Problems` = survey_mean(beh_ever,   na.rm = TRUE, vartype = "ci") * 100,
    `Any MH Condition`  = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  svy_to_long("survey_year")

# 2. By age group
by_age <- svy |>
  group_by(survey_year, age_group) |>
  summarise(
    `Any MH Condition` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100,
    ADHD               = survey_mean(adhd_ever,   na.rm = TRUE, vartype = "ci") * 100,
    Anxiety            = survey_mean(anx_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Depression         = survey_mean(dep_ever,    na.rm = TRUE, vartype = "ci") * 100
  ) |>
  svy_to_long(c("survey_year", "age_group"))

# 3. By sex
by_sex <- svy |>
  group_by(survey_year, sex) |>
  summarise(
    ADHD               = survey_mean(adhd_ever,   na.rm = TRUE, vartype = "ci") * 100,
    Anxiety            = survey_mean(anx_ever,    na.rm = TRUE, vartype = "ci") * 100,
    Depression         = survey_mean(dep_ever,    na.rm = TRUE, vartype = "ci") * 100,
    `Behavior Problems` = survey_mean(beh_ever,   na.rm = TRUE, vartype = "ci") * 100
  ) |>
  svy_to_long(c("survey_year", "sex"))

# 4. By race
by_race <- svy |>
  group_by(survey_year, race) |>
  summarise(
    `Any MH Condition` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  svy_to_long(c("survey_year", "race"))

# 5. By poverty
by_pov <- svy |>
  group_by(survey_year, poverty_cat) |>
  summarise(
    `Any MH Condition` = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
  ) |>
  svy_to_long(c("survey_year", "poverty_cat"))

# 6. Pre/post COVID
covid_compare <- svy |>
  filter(!is.na(period)) |>
  group_by(period, race) |>
  summarise(
    pct = survey_mean(any_mh_ever, na.rm = TRUE, vartype = "ci") * 100
  )

# 7. Diagnostic bias: severity among diagnosed
severity_race <- nsch |>
  filter(any_mh_ever) |>
  pivot_longer(c(adhd_severity, anx_severity, dep_severity, beh_severity),
               names_to = "condition", values_to = "severity") |>
  filter(!is.na(severity)) |>
  mutate(condition = recode(condition,
    adhd_severity = "ADHD", anx_severity = "Anxiety",
    dep_severity = "Depression", beh_severity = "Behavior Problems"
  )) |>
  count(race, condition, severity) |>
  group_by(race, condition) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

# 8. Treatment access among diagnosed
access_race <- svy |>
  filter(any_mh_ever) |>
  group_by(race) |>
  summarise(
    pct_treated = survey_mean(got_mh_treatment, na.rm = TRUE, vartype = "ci") * 100,
    pct_unmet   = survey_mean(unmet_need,       na.rm = TRUE, vartype = "ci") * 100
  )

# 9. Key stats for KPI cards
n_children <- format(round(sum(nsch$FWC[nsch$survey_year == 2024]) / 1e6, 1), nsmall = 1)
any_mh_2024 <- trend |> filter(survey_year == 2024, condition == "Any MH Condition") |> pull(pct) |> round(1)
any_mh_2019 <- trend |> filter(survey_year == 2019, condition == "Any MH Condition") |> pull(pct) |> round(1)
pct_increase <- round((any_mh_2024 - any_mh_2019) / any_mh_2019 * 100, 0)

message("Building dashboard...")

# =============================================================================
# PLOTLY CHARTS
# =============================================================================

make_trend_plot <- function(data, title, color_col = "condition", colors = COND_COLORS) {
  p <- data |>
    mutate(tooltip = sprintf("%s\n%s: %.1f%% [%.1f\u2013%.1f%%]",
                             survey_year, .data[[color_col]], pct, pct_low, pct_upp)) |>
    ggplot(aes(survey_year, pct, color = .data[[color_col]],
               fill = .data[[color_col]], text = tooltip)) +
    geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.1, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
    scale_y_continuous(labels = \(x) paste0(x, "%")) +
    labs(title = title, x = NULL, y = "Prevalence (%)", color = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggplotly(p, tooltip = "text") |>
    layout(legend = list(orientation = "h", y = -0.15)) |>
    config(displayModeBar = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

# Chart 1: Overall trend
p1 <- make_trend_plot(trend, "Diagnosed MH Conditions, 2019\u20132024")

# Chart 2: By age group
p2_data <- by_age |> filter(condition == "Any MH Condition")
p2 <- p2_data |>
  mutate(tooltip = sprintf("%s | Age %s\n%.1f%% [%.1f\u2013%.1f%%]",
                           survey_year, age_group, pct, pct_low, pct_upp)) |>
  ggplot(aes(survey_year, pct, color = age_group, fill = age_group, text = tooltip)) +
  geom_ribbon(aes(ymin = pct_low, ymax = pct_upp), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0-5" = "#81C784", "6-11" = "#FF9800", "12-17" = "#E53935")) +
  scale_fill_manual(values = c("0-5" = "#81C784", "6-11" = "#FF9800", "12-17" = "#E53935")) +
  scale_x_continuous(breaks = c(2019, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "Any MH Diagnosis by Age Group", x = NULL, y = "%", color = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
p2 <- ggplotly(p2, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.15))

# Chart 3: By sex (faceted by condition)
sex_colors <- c("Male" = "#1565C0", "Female" = "#AD1457")
p3_plots <- map(c("ADHD", "Anxiety", "Depression", "Behavior Problems"), \(cond) {
  d <- by_sex |> filter(condition == cond) |>
    mutate(tooltip = sprintf("%s | %s\n%.1f%% [%.1f\u2013%.1f%%]",
                             survey_year, sex, pct, pct_low, pct_upp))
  plot_ly(d, x = ~survey_year, y = ~pct, color = ~sex, colors = sex_colors,
          text = ~tooltip, hoverinfo = "text", type = "scatter", mode = "lines+markers",
          legendgroup = ~sex, showlegend = (cond == "ADHD")) |>
    add_ribbons(ymin = ~pct_low, ymax = ~pct_upp, line = list(width = 0),
                opacity = 0.15, hoverinfo = "none", legendgroup = ~sex, showlegend = FALSE) |>
    layout(annotations = list(text = cond, x = 0.5, y = 1.05, xref = "paper",
                               yref = "paper", showarrow = FALSE, font = list(size = 13)))
})
p3 <- subplot(p3_plots, nrows = 1, shareY = TRUE, titleX = FALSE) |>
  layout(title = list(text = "MH Diagnoses by Sex"),
         yaxis = list(title = "Prevalence (%)"),
         legend = list(orientation = "h", y = -0.12))

# Chart 4: By race
p4 <- make_trend_plot(by_race, "Any MH Diagnosis by Race/Ethnicity",
                       color_col = "race", colors = RACE_COLORS)

# Chart 5: By poverty
POV_COLORS <- c("<100% FPL" = "#B71C1C", "100-199% FPL" = "#E65100",
                "200-399% FPL" = "#1565C0", "400%+ FPL" = "#1B5E20")
p5 <- make_trend_plot(by_pov, "Any MH Diagnosis by Poverty Level",
                       color_col = "poverty_cat", colors = POV_COLORS)

# Chart 6: Pre/post COVID by race
p6 <- covid_compare |>
  mutate(tooltip = sprintf("%s | %s\n%.1f%% [%.1f\u2013%.1f%%]", race, period, pct, pct_low, pct_upp)) |>
  ggplot(aes(x = race, y = pct, fill = period, text = tooltip)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.85) +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_upp),
                position = position_dodge(width = 0.75), width = 0.2) +
  scale_fill_manual(values = c("Pre-COVID (2019)" = "#90CAF9",
                                "Post-COVID (2022\u20132024)" = "#1565C0")) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "Pre- vs Post-COVID by Race/Ethnicity", x = NULL, y = "%", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))
p6 <- ggplotly(p6, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2))

# Chart 7: Severity among diagnosed
sev_colors <- c("Mild" = "#81C784", "Moderate" = "#FFB74D", "Severe" = "#E57373")
p7 <- severity_race |>
  filter(condition == "ADHD") |>
  mutate(
    severity = factor(severity, levels = c("Severe", "Moderate", "Mild")),
    tooltip = sprintf("%s | %s: %.1f%% (n=%d)", race, severity, pct, n)
  ) |>
  ggplot(aes(x = race, y = pct, fill = severity, text = tooltip)) +
  geom_col(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = sev_colors) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "ADHD Severity Among Diagnosed, by Race",
       subtitle = "Higher severity with lower diagnosis = underdiagnosis signal",
       x = NULL, y = "% of diagnosed", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))
p7 <- ggplotly(p7, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2))

# Chart 8: Treatment access among diagnosed
p8_data <- bind_rows(
  access_race |> transmute(race, measure = "Received treatment", pct = pct_treated,
                           low = pct_treated_low, upp = pct_treated_upp),
  access_race |> transmute(race, measure = "Unmet need", pct = pct_unmet,
                           low = pct_unmet_low, upp = pct_unmet_upp)
) |>
  mutate(tooltip = sprintf("%s | %s\n%.1f%% [%.1f\u2013%.1f%%]", race, measure, pct, low, upp))

p8 <- ggplot(p8_data, aes(x = race, y = pct, fill = measure, text = tooltip)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.85) +
  geom_errorbar(aes(ymin = low, ymax = upp),
                position = position_dodge(width = 0.75), width = 0.2) +
  scale_fill_manual(values = c("Received treatment" = "#1565C0", "Unmet need" = "#C62828")) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "Treatment Access Among Diagnosed Children", x = NULL, y = "%", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))
p8 <- ggplotly(p8, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2))

# =============================================================================
# BUILD HTML DASHBOARD
# =============================================================================
message("Assembling HTML...")

kpi_card <- function(value, label, color = "#1565C0") {
  div(
    style = sprintf("background: %s; color: white; border-radius: 12px; padding: 20px 24px; text-align: center; min-width: 160px; flex: 1;", color),
    div(style = "font-size: 2.2em; font-weight: 700; line-height: 1.1;", value),
    div(style = "font-size: 0.85em; margin-top: 6px; opacity: 0.9;", label)
  )
}

section_header <- function(title, subtitle = NULL) {
  div(
    style = "margin: 40px 0 16px 0;",
    h2(style = "color: #1A237E; margin: 0; font-size: 1.5em;", title),
    if (!is.null(subtitle)) p(style = "color: #666; margin: 4px 0 0 0; font-size: 0.95em;", subtitle)
  )
}

chart_card <- function(plotly_obj, note = NULL) {
  div(
    style = "background: white; border-radius: 12px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); padding: 16px; margin-bottom: 24px;",
    plotly_obj,
    if (!is.null(note)) p(style = "color: #888; font-size: 0.8em; margin: 8px 0 0 0;", note)
  )
}

two_col <- function(left, right) {
  div(style = "display: flex; gap: 24px; flex-wrap: wrap;",
    div(style = "flex: 1; min-width: 400px;", left),
    div(style = "flex: 1; min-width: 400px;", right)
  )
}

page <- div(
  style = "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 1280px; margin: 0 auto; padding: 24px 32px; background: #f5f5f5;",

  # --- Header ---
  div(
    style = "background: linear-gradient(135deg, #1A237E, #283593); color: white; border-radius: 16px; padding: 32px 40px; margin-bottom: 32px;",
    h1(style = "margin: 0 0 8px 0; font-size: 2em;", "Child Mental Health in the U.S., 2019\u20132024"),
    p(style = "margin: 0; opacity: 0.85; font-size: 1.05em;",
      "Trends, disparities, and evidence for diagnostic bias from the National Survey of Children\u2019s Health"),
    p(style = "margin: 8px 0 0 0; opacity: 0.6; font-size: 0.85em;",
      "Data: NSCH Topical Questionnaire | Survey-weighted estimates | 240,965 children across 5 survey years")
  ),

  # --- KPI Cards ---
  div(
    style = "display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 8px;",
    kpi_card(paste0(any_mh_2024, "%"), "Any MH diagnosis (2024)", "#C62828"),
    kpi_card(paste0("+", pct_increase, "%"), "Increase since 2019", "#E65100"),
    kpi_card("1 in 5", "U.S. children affected", "#2E7D32"),
    kpi_card(paste0(n_children, "M"), "Children represented (2024)", "#1565C0"),
    kpi_card("48.9%", "Unmet need (Black NH)", "#4A148C")
  ),

  # --- Section 1: Overall trends ---
  section_header("Overall Trends",
                 "All four conditions show statistically significant increases from 2019 to 2024 (adjusted for demographics)."),
  chart_card(p1, "Hover for exact estimates with 95% confidence intervals. Shaded ribbons = CI range."),

  # --- Section 2: Subgroup breakdowns ---
  section_header("Who Is Most Affected?",
                 "The increase is not uniform \u2014 it varies by age, sex, income, and race."),
  two_col(
    chart_card(p2, "12\u201317 year-olds have the steepest trajectory."),
    chart_card(p5, "Low-income families have higher baseline rates, but the increase is steepest among high-income families.")
  ),
  chart_card(p3, "Girls are catching up: the sex \u00d7 year interaction is significant for ADHD (p<0.001), anxiety (p<0.001), and depression (p=0.02)."),

  # --- Section 3: Race/ethnicity ---
  section_header("Racial & Ethnic Disparities",
                 "White NH children show the highest diagnosed prevalence. But does this reflect true differences or diagnostic bias?"),
  chart_card(p4),

  # --- Section 4: Pre- vs Post-COVID ---
  section_header("The COVID Effect",
                 "Pre-COVID (2019) vs. Post-COVID (2022\u20132024). All groups show increases; the relative jump is largest among AIAN and Asian NH children."),
  chart_card(p6, "Adjusted PR for post-COVID: ADHD 1.22, Anxiety 1.20, Any MH 1.17 (all p<0.001)."),

  # --- Section 5: Diagnostic bias ---
  section_header("Evidence for Diagnostic Bias",
                 "Three converging signals suggest that minority children \u2014 particularly Black, Hispanic, and AIAN \u2014 are underdiagnosed."),
  div(
    style = "background: #FFF3E0; border-left: 4px solid #E65100; border-radius: 8px; padding: 16px 20px; margin-bottom: 24px;",
    h4(style = "margin: 0 0 8px 0; color: #E65100;", "The Underdiagnosis Pattern"),
    tags$ul(
      style = "margin: 0; padding-left: 20px; color: #333;",
      tags$li(HTML("<strong>Diagnosis-severity paradox:</strong> Black NH and AIAN children have lower diagnosis rates but <em>higher</em> severity when diagnosed (e.g., 23% severe ADHD among AIAN vs. 12% White NH).")),
      tags$li(HTML("<strong>Treatment access gaps:</strong> Black NH children have the highest unmet need (49%) even <em>after</em> being diagnosed.")),
      tags$li(HTML("<strong>Attenuation with controls:</strong> AIAN race effect drops from OR 1.18 to 1.01 after controlling for healthcare access \u2014 the disparity is fully explained by access barriers."))
    )
  ),
  two_col(
    chart_card(p7, "Among diagnosed children: groups with lower diagnosis rates tend to have higher severity \u2014 consistent with only the most obvious cases being caught."),
    chart_card(p8, "Black NH children face the largest treatment gap: lowest treatment rate + highest unmet need among diagnosed children.")
  ),

  # --- Methods note ---
  div(
    style = "background: white; border-radius: 12px; padding: 20px 24px; margin-top: 32px; color: #666; font-size: 0.85em;",
    h4(style = "margin: 0 0 8px 0; color: #333;", "Methods"),
    p("Data: National Survey of Children\u2019s Health (NSCH) topical questionnaire files, 2019, 2021, 2022, 2023, and 2024.",
      "All estimates are survey-weighted using child-level analytic weights (FWC) and stratified design (STRATUM).",
      "Confidence intervals are 95%."),
    p("Regression models: survey-weighted logistic (odds ratios) and modified Poisson (prevalence ratios).",
      "Adjusted for age group, sex, race/ethnicity, and family poverty level.",
      "Modified Poisson is reported as the primary measure since prevalence >10% causes ORs to overestimate relative risk."),
    p(HTML("These are <strong>observational associations</strong>. NSCH cannot establish causality.",
      "Racial/ethnic differences in diagnosed prevalence likely reflect a mix of true differences in exposure,",
      "differential access to screening, cultural variation in help-seeking, and provider-level bias.")),
    hr(style = "margin: 12px 0;"),
    p(style = "font-size: 0.9em;", HTML("Built with R (tidyverse, survey, srvyr, plotly). Source code: <code>scripts/01\u201307</code>. Data: childhealthdata.org."))
  )
)

# --- Save HTML ---------------------------------------------------------------

message("Saving dashboard to: ", out_path)

save_html(
  page,
  file   = out_path,
  libdir = file.path(dirname(out_path), "dashboard_libs"),
  background = "#f5f5f5"
)

message("Done. Open output/dashboard.html in a browser.")
