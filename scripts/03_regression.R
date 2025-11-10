# Regression Analysis: Returns to Education (Veterans, ACS 2022)

library(tidyverse)
library(readr)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(glue)
library(dplyr)
library(ggplot2)
library(scales)

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

glue("Treatment contrast model saved to outputs/regressions/")

# 4. Add age and gender controls
m2 <- lm(log_wage ~ educ_grp + age + age_sq + sex_female, data = acs_vet)
summary(m2)

# 5. Add disability and class of worker
m3 <- lm(log_wage ~ educ_grp + age + age_sq + sex_female + any_disability + classwkr, data = acs_vet)
summary(m3)

# 5b. Add race and ethnicity controls (using lowercase variable names from clean_names)

acs_vet <- acs_vet %>%
  mutate(
    race_factor = factor(race,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                         labels = c("White", "Black", "AIAN", "Chinese", "Japanese",
                                    "Other Asian/Pacific", "Other", "Two Races", "Three+ Races")),
    hispanic_factor = factor(hispan,
                             levels = c(0, 1, 2, 3, 4, 9),
                             labels = c("Not Hispanic", "Mexican", "Puerto Rican",
                                        "Cuban", "Other Hispanic", "Not Reported"))
  )

# Drop "Not Reported" category to avoid NA contrasts
acs_vet <- acs_vet %>%
  filter(hispanic_factor != "Not Reported")

# Model 4: Include race and ethnicity controls
m4 <- lm(log_wage ~ educ_grp + age + age_sq + sex_female +
           any_disability + classwkr + race_factor + hispanic_factor,
         data = acs_vet)

summary(m4)

# 📊 Predicted (adjusted) earnings by disability status from Model 4

newdata <- acs_vet %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    age_sq = mean(age_sq, na.rm = TRUE),
    sex_female = mean(sex_female, na.rm = TRUE),
    classwkr = mean(classwkr, na.rm = TRUE)
  ) %>%
  slice(rep(1, 2)) %>%
  mutate(any_disability = c(0, 1))

newdata$educ_grp <- factor("BA", levels = levels(acs_vet$educ_grp))
newdata$race_factor <- factor("White", levels = levels(acs_vet$race_factor))
newdata$hispanic_factor <- factor("Not Hispanic", levels = levels(acs_vet$hispanic_factor))

newdata$pred_log_wage <- predict(m4, newdata = newdata)
newdata$pred_wage <- exp(newdata$pred_log_wage)
newdata$label <- c("No Disability", "With Disability")

ggplot(newdata, aes(x = label, y = pred_wage, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = dollar(round(pred_wage, 0))),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Adjusted Predicted Earnings by Disability Status (Model 4)",
    x = NULL, y = "Predicted Annual Earnings ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(face = "bold"))

ggsave("outputs/regressions/fig_predicted_disability_gap.png", width = 7, height = 6)

# --- Export regression results (Models 1–4) with aligned labels ---

# Compute robust SEs
robust_m1 <- sqrt(diag(vcovHC(m1, type = "HC1")))
robust_m2 <- sqrt(diag(vcovHC(m2, type = "HC1")))
robust_m3 <- sqrt(diag(vcovHC(m3, type = "HC1")))
robust_m4 <- sqrt(diag(vcovHC(m4, type = "HC1")))

# --- Export memo-aligned regression table (Models 1–4) ---
stargazer(m1, m2, m3, m4,
          type = "html",
          se = list(
            sqrt(diag(vcovHC(m1, type = "HC1"))),
            sqrt(diag(vcovHC(m2, type = "HC1"))),
            sqrt(diag(vcovHC(m3, type = "HC1"))),
            sqrt(diag(vcovHC(m4, type = "HC1")))
          ),
          title = "Returns to Education Among U.S. Veterans (ACS 2022)",
          dep.var.labels = "Log(Wage Income)",
          column.labels = c("Edu Only",
                            "+ Age & Gender",
                            "+ Disability & Class",
                            "+ Race & Ethnicity"),
          keep = c("educ_grp", "sex_female", "any_disability", "classwkr",
                   "race_factorBlack", "race_factorAIAN", "hispanic_factorMexican"),
          covariate.labels = c(
            "Education: High School", "Education: Some College / AA",
            "Education: Bachelor's", "Education: Master's", "Education: Graduate+",
            "Female", "Any Disability", "Class of Worker (Public/Self)",
            "Race: Black", "Race: AIAN", "Hispanic: Mexican"
          ),
          omit.stat = c("f", "ser"),
          digits = 3,
          column.sep.width = "-5pt",
          out = "outputs/regressions/returns_to_education_memo.html")

# PNG for memo
library(webshot2)
webshot("outputs/regressions/returns_to_education_memo.html",
        file = "outputs/regressions/returns_to_education_memo.png",
        vwidth = 1600, vheight = 1000, zoom = 1.5)

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

# 10. Education Milestones: No HS vs. College+

edu_fractions <- acs_vet %>%
  summarise(
    total = n(),
    no_hs = sum(educ_grp == "<HS", na.rm = TRUE),
    college_plus = sum(educ_grp %in% c("BA", "MA", "GD"), na.rm = TRUE)
  ) %>%
  mutate(
    frac_no_hs = round(no_hs / total * 100, 1),
    frac_college_plus = round(college_plus / total * 100, 1)
  )

print(edu_fractions)

write_csv(edu_fractions, "outputs/regressions/education_fractions.csv")

glue("🎓 {edu_fractions$frac_no_hs}% of veterans have less than HS; {edu_fractions$frac_college_plus}% have college+.")

# 11. Robustness Checks — Separate Regressions by Gender and Disability

# a) Split by gender
m4_male <- lm(log_wage ~ educ_grp + age + age_sq + any_disability + classwkr +
                race_factor + hispanic_factor,
              data = acs_vet %>% filter(sex_female == 0))

m4_female <- lm(log_wage ~ educ_grp + age + age_sq + any_disability + classwkr +
                  race_factor + hispanic_factor,
                data = acs_vet %>% filter(sex_female == 1))

# b) Split by disability status
m4_nondis <- lm(log_wage ~ educ_grp + age + age_sq + sex_female + classwkr +
                  race_factor + hispanic_factor,
                data = acs_vet %>% filter(any_disability == 0))

m4_disab <- lm(log_wage ~ educ_grp + age + age_sq + sex_female + classwkr +
                 race_factor + hispanic_factor,
               data = acs_vet %>% filter(any_disability == 1))

# Export all robustness regressions together
stargazer(m4_male, m4_female, m4_nondis, m4_disab,
          type = "text",
          title = "Robustness: Returns to Education by Gender and Disability",
          dep.var.labels = "Log(Wage Income)",
          column.labels = c("Male", "Female", "No Disability", "With Disability"),
          omit.stat = c("f", "ser"),
          out = "outputs/regressions/robustness_gender_disability.txt")