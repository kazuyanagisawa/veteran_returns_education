# Descriptive Statistics: Veterans' Earnings and Education

library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(glue)
library(scales)

# 1. Load cleaned dataset
acs_vet <- read_rds("outputs/acs_veterans_clean.rds")
glue("Loaded {nrow(acs_vet)} veteran records")

# 2. Set ordered factor levels for education group
edu_levels <- c("<HS", "HS", "Some/AA", "BA", "MA", "GD")

acs_vet <- acs_vet %>%
  mutate(
    educ_grp = fct_drop(factor(educ_grp, levels = edu_levels, ordered = TRUE))
  )

# 3. Basic earnings summary
summary_stats <- acs_vet %>%
  summarise(
    mean_earnings   = mean(incwage, na.rm = TRUE),
    median_earnings = median(incwage, na.rm = TRUE),
    p10             = quantile(incwage, 0.10, na.rm = TRUE),
    p90             = quantile(incwage, 0.90, na.rm = TRUE)
  )

print(summary_stats)

# 4. Earnings by education group
edu_summary <- acs_vet %>%
  group_by(educ_grp, .drop = FALSE) %>%
  summarise(
    n               = n(),
    mean_earnings   = mean(incwage, na.rm = TRUE),
    median_earnings = median(incwage, na.rm = TRUE),
    p10             = quantile(incwage, 0.10, na.rm = TRUE),
    p90             = quantile(incwage, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 0)))

write_csv(edu_summary, "outputs/summary_by_education.csv")

# 4a. Education distribution 
edu_dist <- acs_vet %>%
  count(educ_grp, .drop = FALSE) %>%
  mutate(share = n / sum(n))

print(edu_dist)
write_csv(edu_dist, "outputs/education_distribution.csv")

ggplot(edu_dist, aes(x = educ_grp, y = share, fill = educ_grp)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", share * 100)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Education Distribution Among U.S. Veterans (ACS 2022)",
    x = "Education Level",
    y = "Share of Veterans"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(margin = margin(b = 10)))
ggsave("outputs/fig_education_distribution.png", width = 8, height = 5)

# 5. Compare by gender and disability
gender_disab <- acs_vet %>%
  mutate(sex = if_else(sex == 1, "Male", "Female")) %>%
  group_by(sex, any_disability, educ_grp, .drop = FALSE) %>%
  summarise(
    mean_earnings   = mean(incwage, na.rm = TRUE),
    median_earnings = median(incwage, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(gender_disab, "outputs/summary_gender_disability.csv")

# 5a. Split results by age group
age_split <- acs_vet %>%
  mutate(age_group = if_else(age < 40, "<40", "≥40")) %>%
  group_by(age_group, educ_grp, .drop = FALSE) %>%
  summarise(mean_earnings = mean(incwage, na.rm = TRUE), .groups = "drop")

write_csv(age_split, "outputs/summary_age_split.csv")

ggplot(age_split, aes(x = educ_grp, y = mean_earnings, fill = age_group)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Average Earnings by Education and Age Group\n(Veterans 18–64, ACS 2022)",
    x = "Education Level",
    y = "Mean Annual Earnings ($)",
    fill = "Age Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(margin = margin(b = 10)))
ggsave("outputs/fig_earnings_by_agegroup.png", width = 8, height = 5)

# 6. Visualizations

## a) Distribution of earnings
ggplot(acs_vet, aes(x = incwage)) +
  geom_histogram(binwidth = 10000, fill = "#457b9d", color = "white") +
  scale_x_continuous(labels = label_dollar()) +
  coord_cartesian(xlim = c(0, 250000)) +
  labs(
    title = "Distribution of Veteran Earnings (2022 ACS)",
    x = "Annual Wage Income ($)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)
ggsave("outputs/fig_earnings_distribution.png", width = 8, height = 5)

## b) Average earnings by education (ordered & labeled)
ggplot(edu_summary, aes(x = educ_grp, y = mean_earnings, fill = educ_grp)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = dollar(mean_earnings)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Average Earnings by Education Level\n(Veterans 18–64, ACS 2022)",
    x = "Education Group",
    y = "Mean Annual Earnings ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(margin = margin(b = 10)))
ggsave("outputs/fig_earnings_by_education.png", width = 8, height = 5)

## c) Gender differences by education
ggplot(acs_vet %>% mutate(sex = if_else(sex == 1, "Male", "Female")),
       aes(x = educ_grp, y = incwage, fill = sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  coord_cartesian(ylim = c(0, 200000)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Earnings by Education and Gender (Veterans 18–64, ACS 2022)",
    x = "Education Group",
    y = "Earnings ($)",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(margin = margin(b = 10)))
ggsave("outputs/fig_gender_edu_boxplot.png", width = 8, height = 5)

# 7. Completion message
glue("✅ Summary tables and figures saved to /outputs/")
