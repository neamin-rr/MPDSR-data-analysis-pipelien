# ----------------------------------------------------
# Quarter Lookup Table — Up to 2020EFY
# ----------------------------------------------------
# Generates an Ethiopian fiscal quarter table
# with proper dates, a combined Year_Qtr identifier,
# and no duplicates.
# Saves the output to CSV and RDS formats.
# ----------------------------------------------------
rm(list=ls())
# Load libraries
library(dplyr)
library(lubridate)

# ---- Function: Generate Quarter Lookup ----
generate_quarter_table <- function(start_year, end_year) {
  
  # Validate input
  stopifnot(start_year <= end_year)
  
  # Build table
  quarter_table <- tibble()
  
  for (yr in start_year:end_year) {
    # Quarter definitions for each fiscal year
    q <- tibble(
      Year_01 = yr,
      Qtr     = c("Q1", "Q2", "Q3", "Q4"),
      Qtr_start = c(
        paste0("11/1/", yr - 1),
        paste0("2/1/", yr),
        paste0("5/1/", yr),
        paste0("8/1/", yr)
      ),
      Qtr_end = c(
        paste0("1/30/", yr),
        paste0("4/30/", yr),
        paste0("7/30/", yr),
        paste0("10/30/", yr)
      ),
      Year = paste0(yr, "EFY")
    )
    
    quarter_table <- bind_rows(quarter_table, q)
  }
  
  # Convert to Date and create Year_Qtr
  quarter_table %>%
    mutate(
      Qtr_start = mdy(Qtr_start),
      Qtr_end   = mdy(Qtr_end),
      Year_Qtr  = paste0(Year_01, "_", Qtr)
    ) %>%
    distinct()  # ensure no duplicates
}

# ---- Generate the table ----
quarter_lookup_2020 <- generate_quarter_table(2006, 2020)

# ---- View result ----
print(head(quarter_lookup_2020))
print(tail(quarter_lookup_2020))

# ---- Save outputs ----

# Save as CSV
write.csv(
  quarter_lookup_2020,
  file = "quarter_lookup_up_to_2020.csv",
  row.names = FALSE
)

# Save as RDS for R use
saveRDS(
  quarter_lookup_2020,
  file = "quarter_lookup_up_to_2020.rds"
)

message("✔ Quarter lookup table created & saved successfully.")
