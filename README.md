# Returns to Education Among U.S. Veterans (R) — UCLA Econ 104 (Data Science for Economists)

*Econometrics project analyzing the relationship between education and labor-market earnings among U.S. military veterans using 2022 ACS microdata.*

**Author:** Cole Kazu Yanagisawa  
**Goal:** Estimate the “returns to education” for working-age veterans and identify how age, gender, disability, and race contribute to wage variation.  
**Stack:** R, tidyverse, broom, lmtest, sandwich, stargazer, janitor, glue, scales

---

## Project Summary

Using nationally representative 2022 ACS data, this project quantifies how education affects veterans’ annual wage income.  
Findings show that veterans with a college degree earn **~$36,000 more annually** on average than those with only a high school diploma — even though higher education is typically tuition-free for veterans through the **GI Bill**.

---

## Overview

This analysis examines how educational attainment influences earnings among **U.S. veterans aged 18–64**, combining descriptive statistics with stepwise regression models that add demographic and occupational controls.

**Final model (chosen):**
```math
\log(\text{Wage}) = \beta_0 + \beta_1\text{Education} +
\beta_2\text{Age} + \beta_3\text{Age}^2 +
\beta_4\text{Female} + \beta_5\text{Disability} +
\beta_6\text{Class of Worker} +
\beta_7\text{Race} + \beta_8\text{Hispanic} + \epsilon
```

**Why: ** Progressively adds variables to isolate education’s partial effect on earnings, ensuring interpretability and robustness.

---

## Repo Structure

```
├── .RData              # R workspace image
├── .RHistory          # R command history
├── data/ 
│   └── acs_2022_vet.csv # Cleaned ACS 2022 veteran microdata
├── LICENSE
├── outputs/
│   ├── acs_veterans_clean.rds
│   ├── education_distribution.csv
│   ├── fig_earnings_by_agegroup.png
│   ├── fig_earnings_by_education.png
│   ├── fig_education_distribution.png
│   ├── fig_earnings_distribution.png
│   ├── fig_education_distribution.png
│   ├── fig_gender_edu_boxplot.png
│   ├── regressions/
│   │   ├── returns_to_education.html
│   │   ├── m1_treatment_summary.html
│   │   ├── model_comparison_summary.csv
│   │   └── fig_predicted_earnings.png
│   ├── summary_age_split.csv
│   ├── summary_by_education.csv
│   └──summary_gender_disability.csv 
|── Project_Log.Rmd
├── Project-Log.pdf
├── README.md
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_descriptive_analysis.R
│   └── 03_regression_analysis.R
└── veteran_returns_education.Rproj
```

- Scripts numbered sequentially for workflow clarity.
- All generated outputs (tables, HTML, figures) saved in /outputs.

---

## Key Findings (Short)

- Education Distribution: 2.3 % of veterans lack a HS diploma; 35 % hold a college degree or higher.
- Earnings: Mean ≈ $75 k; Median ≈ $60 k; strong upward gradient with education.
- Returns: Each additional education level adds roughly 8–12 % to earnings.
- Disability Gap: Veterans with disabilities earn about −30 % less than peers.
- Gender Gap: Female veterans earn about −40 % less than males, controlling for other factors.
- Race/Ethnicity: Black and AIAN veterans earn about −22–23 % less than White counterparts; Mexican-origin veterans about −4 %.

---

## Model Fit Summary

| Model | Description | Adj. R² |
|:------|:-------------|:-------:|
| (1) | Education only | **0.074** |
| (2) | + Age & Gender | **0.131** |
| (3) | + Disability & Class of Worker | **0.143** |
| (4) | + Race & Ethnicity | **0.149** |

Each stage adds explanatory power, confirming that demographic and employment factors meaningfully shape earnings.  
However, education remains the single strongest predictor throughout all specifications.

---

## Methods & Diagnostics

- **Descriptive Analysis:** Histograms and boxplots summarize education and earnings distributions.  
- **Regression Modeling:** OLS on log(wage income) using both **polynomial** and **treatment** contrasts for education categories.  
- **Sequential Controls:** Introduced in four stages to isolate education’s independent effect:  
  1. Education only  
  2. + Age & Gender  
  3. + Disability & Class of Worker  
  4. + Race & Ethnicity  
- **Heteroskedasticity:** Addressed via robust (White/HC1) standard errors using `sandwich`.  
- **Robustness Checks:** Separate regressions by gender and disability groups confirm consistent returns to education across subpopulations.  
- **Visualization:** Predicted earnings charts labeled by education level and grouped by demographics for interpretability.

---

## Reproduce Locally

1. **Install required packages:**
   ```r
   install.packages(c(
     "tidyverse","broom","janitor","glue","scales",
     "lmtest","sandwich","stargazer"
   ))
   ```
2. **Run scripts in order: **
   - `scripts/01_data_cleaning.R`  
   - `scripts/02_descriptive_analysis.R`  
   - `scripts/03_regression_analysis.R`
3. **View outputs in `/outputs/` folder.**
  All cleaned data, regression summaries, and figures will appear under the /outputs/ directory.
  
  
## Deliverables 
- Final Memo (PDF) 
- Regression Tables (HTML) 
- Education Summary (CSV) 
- Predicted Earnings Plot

## Citation (Data)

>Ruggles, Steven et al. (2023). IPUMS USA: Version 12.0 [dataset]. Minneapolis, MN: IPUMS.
>https://usa.ipums.org/usa/

--- 

## Notes 

- Focused exclusively on U.S. veterans aged 18–64, a population eligible for GI Bill and Yellow Ribbon education benefits, providing a natural test case for the value of tuition-free college.
- Interpretation: Cross-sectional analysis identifies correlations, not causal effects.
- Future Work: Extend with service-era, occupation, or VA disability rating controls; explore causal identification strategies.



