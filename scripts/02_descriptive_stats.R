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
acs_vet <- acs_vet %>%
  mutate(
    educ_grp = factor(
      educ_grp,
      levels = c("<HS", "HS", "Some/AA", "BA", "Some GS*", "MA", "GD"),
      ordered = TRUE
    )
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
  group_by(educ_grp) %>%
  summarise(
    n               = n(),
    mean_earnings   = mean(incwage, na.rm = TRUE),
    median_earnings = median(incwage, na.rm = TRUE),
    p10             = quantile(incwage, 0.10, na.rm = TRUE),
    p90             = quantile(incwage, 0.90, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 0)))

print(edu_summary)
write_csv(edu_summary, "outputs/summary_by_education.csv")

# 5. Compare by gender and disability
gender_disab <- acs_vet %>%
  mutate(sex = if_else(sex == 1, "Male", "Female")) %>%
  group_by(sex, any_disability, educ_grp) %>%
  summarise(
    mean_earnings   = mean(incwage, na.rm = TRUE),
    median_earnings = median(incwage, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(gender_disab, "outputs/summary_gender_disability.csv")

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
ggsave("outputs/fig_earnings_distribution.png", width = 7, height = 4)

## b) Average earnings by education (ordered & labeled)
ggplot(edu_summary, aes(x = educ_grp, y = mean_earnings, fill = educ_grp)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = dollar(mean_earnings)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Average Earnings by Education Level (Veterans 18–64)",
    x = "Education Group",
    y = "Mean Annual Earnings ($)"
  ) +
  theme_minimal(base_size = 13)
ggsave("outputs/fig_earnings_by_education.png", width = 7, height = 4)

## c) Gender differences by education
ggplot(acs_vet %>% mutate(sex = if_else(sex == 1, "Male", "Female")),
       aes(x = educ_grp, y = incwage, fill = sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  coord_cartesian(ylim = c(0, 200000)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Earnings by Education and Gender (Veterans 18–64)",
    x = "Education Group",
    y = "Earnings ($)",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13)
ggsave("outputs/fig_gender_edu_boxplot.png", width = 7, height = 4)

# 7. Completion message
glue("✅ Summary tables and figures saved to /outputs/")