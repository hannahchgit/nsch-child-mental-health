# Rising Tides: Child Mental Health Diagnoses in the U.S., 2019–2024

A survey-weighted analysis of child mental health trends using the **National Survey of Children's Health (NSCH)** topical questionnaire files for survey years 2019, 2021, 2022, 2023, and 2024 (N = 240,965 children aged 0–17).

## Research questions

1. How have diagnosed mental health conditions trended among U.S. children from 2019 to 2024?
2. Which demographic subgroups are driving the increase?
3. Do patterns in diagnosis rates, severity, and treatment access suggest diagnostic bias by race/ethnicity?

## Headline findings

- Age-standardized prevalence of **any current MH condition** rose from **15.7% (2019) to 19.4% (2024)**.
- **Current ADHD** and **current anxiety** each increased at **PR ≈ 1.04–1.05 per year** (FDR q < 0.001).
- The sex × time interaction is significant — the post-COVID increase is substantially larger for **girls** than boys.
- Convergent exploratory evidence for **diagnostic bias**: lower diagnosis rates but higher severity among Black and AIAN children, and the highest unmet treatment need among Black NH children.

## Methods (summary)

- Survey design: `FWC` child weights, `STRATUM` strata, `survey.lonely.psu = "adjust"`.
- **Primary outcome:** *current* diagnosis (K2Q31A+B, K2Q32A+B, K2Q33A+B, K2Q34A+B) — not "ever diagnosed" (cumulative lifetime).
- **Age standardization:** direct method, 2019 weighted age distribution as the reference.
- **Models:** survey-weighted logistic (OR) and modified Poisson (PR) regression, adjusted for age, sex, race/ethnicity, poverty.
- **Pre-specified primary hypotheses (4):** per-year trend in any-MH, ADHD, anxiety; sex × time interaction. All other tests are exploratory with **Benjamini–Hochberg FDR** correction within each family.
- Pre/post-COVID framings: 2019 vs. pooled 2022–2024 (primary), and 2019 vs. 2023–2024 (sensitivity).

## Repo structure

```
scripts/
  01_import_data.R        # screener import + MH variable discovery
  02_clean_data.R         # topical file load, recode to *_ever / *_current
  03_analysis.R           # descriptive trends, subgroup figures
  04_regression.R         # logistic + modified Poisson, sex×year, race×year
  05_pre_post_covid.R     # pre/post-COVID contrasts, stratified
  06_race_diagnostic_bias.R  # severity paradox, access, attenuation
  07_dashboard.R          # interactive plotly HTML dashboard
paper/
  manuscript.Rmd          # full manuscript with tables + figures
output/                   # rendered HTML (gitignored)
data/                     # raw + processed NSCH files (gitignored)
```

## Reproducing

1. Download NSCH **topical** SAS files (2019, 2021, 2022, 2023, 2024) from [childhealthdata.org](https://www.childhealthdata.org/) and place them in `data/raw/`.
2. On macOS, clear extended attributes: `xattr -c data/raw/*.sas7bdat`.
3. Run scripts in order: `Rscript scripts/02_clean_data.R`, then `03`–`07`.
4. Render the manuscript:
   ```r
   Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc::pandoc_bin()))
   rmarkdown::render("paper/manuscript.Rmd", output_dir = "output")
   ```

### Dependencies

R ≥ 4.3 with: `tidyverse`, `haven`, `survey`, `srvyr`, `broom`, `gt`, `kableExtra`, `ggplot2`, `gridExtra`, `plotly`, `here`, `rmarkdown`, `pandoc`.

## Data

NSCH public-use data files are available from the [Data Resource Center for Child & Adolescent Health](https://www.childhealthdata.org/). Raw and processed data are **not** included in this repo.

## Limitations

Parent-reported diagnoses reflect both true symptom presence and prior contact with the diagnostic system. Cross-sectional design precludes causal inference. Exploratory findings (depression, race/poverty subgroups, attenuation, severity paradox) are hypothesis-generating and warrant replication.

## Author

Hannah Choi 
