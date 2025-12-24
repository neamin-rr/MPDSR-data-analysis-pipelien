
# Load required library
library(dplyr)

# Round the expected values in the dataset
Population_data_2024_2025 <- Population_data_2024_2025 %>%
  mutate(
    `Expected MD` = round(`Expected MD`),
    `Expected extended perinatal death` = round(`Expected extended perinatal death`)
  )

# Display first few rows to verify transformations
head(Population_data_2024_2025)

# Rename columns to standardized snake_case format and round values again
Population_data_2024_2025 <- Population_data_2024_2025 %>%
  rename(
    Expected_MD = `Expected MD`,
    Expected_perinatal_deaths = `Expected extended perinatal death`
  ) %>%
  mutate(
    Expected_MD = round(Expected_MD),
    Expected_perinatal_deaths = round(Expected_perinatal_deaths)
  )

# Save the cleaned dataset to an RDS file
saveRDS(Population_data_2024_2025, "Population_data_2024_2025_cleaned.rds")