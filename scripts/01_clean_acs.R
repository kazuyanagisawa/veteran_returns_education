
library(tidyverse)
library(janitor)
library(glue)


# 1. Load & inspect

acs <- read_csv("data/acs_2022_vet.csv") %>%
  clean_names()

glue("Loaded {nrow(acs)} rows and {ncol(acs)} columns.")


# 2. Filter: working-age veterans with valid wages

acs_vet <- acs %>%
  filter(
    age >= 18, age <= 64,
    !is.na(incwage),
    incwage > 0, 
    educd <117,  # valid education codes
  )

glue("Subset to {nrow(acs_vet)} veterans aged 18–64 with valid earnings.")


# 3. Create education categories (using EDUCD detailed codes)

acs_vet <- acs_vet %>%
  mutate(
    educ_grp = case_when(
      educd < 62 ~ "<HS",
      educd >= 62 & educd < 65 ~ "HS",
      educd >= 65 & educd < 101 ~ "Some/AA",
      educd == 101 ~ "BA",
      educd == 114 ~ "MA",
      educd %in% c(115, 116) ~ "GD",
      TRUE ~ NA_character_
    ),
    educ_grp = factor(
      educ_grp,
      levels = c("<HS", "HS", "Some/AA", "BA", "MA", "GD"),
      ordered = TRUE
    )
  )

# Order education factor levels
acs_vet <- acs_vet %>%
  mutate(
    educ_grp = factor(
      educ_grp,
      levels = c("<HS", "HS", "Some/AA", "BA", "Some GS*", "MA", "GD"),
      ordered = TRUE
    )
  )


# 4. Disability indicator

acs_vet <- acs_vet %>%
  mutate(
    any_disability = if_else(
      (diffrem == 2 | diffphys == 2 | diffhear == 2 | diffcare == 2 |
         diffsens == 2 | diffeye == 2 | diffmob == 2),
      1, 0
    )
  )


# 5. Save cleaned dataset

dir.create("outputs", showWarnings = FALSE)
write_rds(acs_vet, "outputs/acs_veterans_clean.rds")

glue("Saved cleaned dataset with {nrow(acs_vet)} rows to outputs/acs_veterans_clean.rds")

# Preview a few columns
acs_vet %>%
  select(age, sex, incwage, educd, educ_grp, any_disability) %>%
  slice_head(n = 10)

acs_vet <- read_rds("outputs/acs_veterans_clean.rds")
table(acs_vet$educ_grp, useNA = "ifany")
