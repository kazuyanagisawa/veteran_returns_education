# Regression Analysis: Returns to Education (Veterans, ACS 2022)

library(tidyverse)
library(readr)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(glue)

# 1. Load cleaned dataset
acs_vet <- read_rds("outputs/acs_veterans_clean.rds")
glue("Loaded {nrow(acs_vet)} rows for regression analysis.")

# 2. Prepare variables
edu_levels <- c("<HS", "HS", "Some/AA", "BA", "MA", "GD")

acs_vet <- acs_vet %>%
  mutate(
    educ_grp = fct_drop(factor(educ_grp, levels = edu_levels, ordered = TRUE)),
    log_wage = log(incwage),
    sex_female = if_else(sex == 2, 1, 0),
    age_sq = age^2
  ) %>%
  drop_na(log_wage, educ_grp, age, sex_female)

glue("Data ready: {nrow(acs_vet)} complete observations after filtering.")

# 3. Baseline regression: log(earnings) ~ education
m1 <- lm(log_wage ~ educ_grp, data = acs_vet)
summary(m1)

# 3a. Supplementary baseline: treatment contrasts (group-to-group)
# Provides direct comparisons (e.g., HS vs <HS, BA vs <HS, etc.)
options(contrasts = c("contr.treatment", "contr.poly"))

m1_treat <- lm(log_wage ~ educ_grp, data = acs_vet)
summary(m1_treat)

# Export side-by-side comparison
stargazer(m1, m1_treat,
          type = "text",
          title = "Comparison of Polynomial vs. Treatment Contrasts (Education Only)",
          dep.var.labels = "Log(Wage Income)",
          out = "outputs/regressions/m1_contrast_comparison.txt")

# Reset contrasts
options(contrasts = c("contr.treatment", "contr.poly"))

# 3b. Unordered factor version (explicit dummies)
acs_vet <- acs_vet %>%
  mutate(educ_grp_unordered = factor(educ_grp, ordered = FALSE))

m1_treat_corrected <- lm(log_wage ~ educ_grp_unordered, data = acs_vet)
summary(m1_treat_corrected)

stargazer(m1_treat_corrected,
          type = "text",
          title = "Baseline Model with Treatment Contrasts (Education Dummies)",
          dep.var.labels = "Log(Wage Income)",
          out = "outputs/regressions/m1_treatment_summary.txt")

stargazer(m1_treat_corrected,
          type = "html",
          title = "Baseline Model with Treatment Contrasts (Education Dummies)",
          dep.var.labels = "Log(Wage Income)",
          out = "outputs/regressions/m1_treatment_summary.html")

glue("✅ Treatment contrast model saved to outputs/regressions/")

# 4. Add age and gender controls
m2 <- lm(log_wage ~ educ_grp + age + age_sq + sex_female, data = acs_vet)
summary(m2)

# 5. Add disability and class of worker
m3 <- lm(log_wage ~ educ_grp + age + age_sq + sex_female + any_disability + classwkr, data = acs_vet)
summary(m3)

# 6. Robust standard errors (HC1)
robust_se_m1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
robust_se_m2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
robust_se_m3 <- coeftest(m3, vcov = vcovHC(m3, type = "HC1"))

# 7. Export regression tables
dir.create("outputs/regressions", showWarnings = FALSE)

stargazer(m1, m2, m3,
          type = "text",
          se = list(
            sqrt(diag(vcovHC(m1, type = "HC1"))),
            sqrt(diag(vcovHC(m2, type = "HC1"))),
            sqrt(diag(vcovHC(m3, type = "HC1")))
          ),
          title = "Returns to Education Among U.S. Veterans (ACS 2022)",
          dep.var.labels = "Log(Wage Income)",
          covariate.labels = c(
            "Education: High School", "Education: Some College / AA",
            "Education: Bachelor's", "Education: Master's", "Education: Graduate+",
            "Age", "Age Squared", "Female", "Any Disability", "Class of Worker (Public/Self)"
          ),
          omit.stat = c("f", "ser"),
          out = "outputs/regressions/returns_to_education.txt"
)

stargazer(m1, m2, m3,
          type = "html",
          se = list(
            sqrt(diag(vcovHC(m1, type = "HC1"))),
            sqrt(diag(vcovHC(m2, type = "HC1"))),
            sqrt(diag(vcovHC(m3, type = "HC1")))
          ),
          title = "Returns to Education Among U.S. Veterans (ACS 2022)",
          dep.var.labels = "Log(Wage Income)",
          covariate.labels = c(
            "Education: High School", "Education: Some College / AA",
            "Education: Bachelor's", "Education: Master's", "Education: Graduate+",
            "Age", "Age Squared", "Female", "Any Disability", "Class of Worker (Public/Self)"
          ),
          omit.stat = c("f", "ser"),
          out = "outputs/regressions/returns_to_education.html"
)

glue("✅ Regression tables exported to outputs/regressions/")

# 8. Visualize predicted earnings by education
pred_means <- acs_vet %>%
  group_by(educ_grp) %>%
  summarise(mean_log_wage = mean(log_wage, na.rm = TRUE)) %>%
  mutate(pred_wage = exp(mean_log_wage))

ggplot(pred_means, aes(x = educ_grp, y = pred_wage, fill = educ_grp)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::dollar(round(pred_wage, 0))),
    vjust = -0.5, size = 3.5, fontface = "bold", color = "black"
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Predicted Average Earnings by Education (Veterans 18–64)",
    x = "Education Group",
    y = "Predicted Annual Earnings ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(margin = margin(b = 10)),
    axis.text.x = element_text(face = "bold")
  )

ggsave("outputs/regressions/fig_predicted_earnings.png", width = 8, height = 5)

# 9. Comparison table: key coefficients and model fit
model_summary <- tibble(
  Model = c("Model 1: Education only",
            "Model 2: + Age & Gender",
            "Model 3: + Disability & Class of Worker"),
  Education_Linear = c(coef(m1)["educ_grp.L"], coef(m2)["educ_grp.L"], coef(m3)["educ_grp.L"]),
  Female = c(NA, coef(m2)["sex_female"], coef(m3)["sex_female"]),
  Disability = c(NA, NA, coef(m3)["any_disability"]),
  Class_Worker = c(NA, NA, coef(m3)["classwkr"]),
  R2 = c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared)
) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(model_summary)
write_csv(model_summary, "outputs/regressions/model_comparison_summary.csv")