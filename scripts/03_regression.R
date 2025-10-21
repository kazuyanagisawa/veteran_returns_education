
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

acs_vet <- acs_vet %>%
  mutate(
    log_wage = log(incwage),
    sex_female = if_else(sex == 2, 1, 0),
    age_sq = age^2
  ) %>%
  drop_na(log_wage, educ_grp, age, sex_female)

glue("Data ready: {nrow(acs_vet)} complete observations after filtering.")


# 3. Baseline regression: log(earnings) ~ education

m1 <- lm(log_wage ~ educ_grp, data = acs_vet)
summary(m1)


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
            "Education: High School", "Education: Some College / AA", "Education: Bachelor's", "Education: Graduate+",
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
            "Education: High School", "Education: Some College / AA", "Education: Bachelor's", "Education: Graduate+",
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
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Predicted Average Earnings by Education (Veterans 18–64)",
    x = "Education Group",
    y = "Predicted Annual Earnings ($)"
  ) +
  theme_minimal()
ggsave("outputs/regressions/fig_predicted_earnings.png", width = 7, height = 4)