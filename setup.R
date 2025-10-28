# setup.R — Auto-install required packages from requirements.txt
# --------------------------------------------------------------
# Run this once before executing project scripts.

message("📦 Checking and installing required packages...")

# 1. Read package list from requirements.txt
req_file <- "requirements.txt"
if (!file.exists(req_file)) {
  stop("❌ requirements.txt not found in the project root.")
}

packages <- scan(req_file, what = character(), quiet = TRUE)

# 2. Identify missing packages
missing_pkgs <- packages[!packages %in% rownames(installed.packages())]

# 3. Install missing packages (if any)
if (length(missing_pkgs) > 0) {
  message(glue::glue("Installing missing packages: {paste(missing_pkgs, collapse = ', ')}"))
  install.packages(missing_pkgs, dependencies = TRUE)
} else {
  message("✅ All required packages are already installed.")
}

# 4. Load all packages
lapply(packages, require, character.only = TRUE)

message("✨ Environment ready. You can now run your analysis scripts:")
message("   - scripts/01_data_cleaning.R")
message("   - scripts/02_descriptive_analysis.R")
message("   - scripts/03_regression_analysis.R")