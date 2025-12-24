# Clear environment
rm(list = ls())

# Load necessary libraries
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)

#-------------------------------
# 1. Load Excel files
#-------------------------------

# MPDSR weekly notifications 2014-2021
MPDSR_weekly_2014_2021 <- read_excel(
  "C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/WEEkly_Notfication/MPDSR weekly notfication_2014_2021.xlsx",
  sheet = "Phem weekly Report "
)

# PHEM weekly data 2020-2023
PHEM_weekly_2020_2023 <- read_excel(
  "C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/WEEkly_Notfication/PHEM weekly data as of 2020-2023.xlsx",
  sheet = "Weekly data"
)

# PHEM weekly data 2024-2025
Copy_PHEM_2017_full <- read_excel(
  "C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/WEEkly_Notfication/Copy of PHEM Weekly Surveillance Data EFY2017_Full(1).xlsx",
  sheet = "PHEM Weekly Data "
)

#-------------------------------
# 2. Standardize column names
#-------------------------------
MPDSR_weekly_2014_2021 <- clean_names(MPDSR_weekly_2014_2021)
PHEM_weekly_2020_2023 <- clean_names(PHEM_weekly_2020_2023)
Copy_PHEM_2017_full <- clean_names(Copy_PHEM_2017_full)

#-------------------------------
# 3. Clean and select relevant columns
#-------------------------------

# 3a. MPDSR 2014-2021
weekly_MPDSR_2014_2021_clean <- MPDSR_weekly_2014_2021 %>%
  filter(!year %in% c(2021, 2022)) %>%   # remove 2021 & 2022
  select(1:8)                             # keep columns 1-8

colnames(weekly_MPDSR_2014_2021_clean) <- c(
  "RegionName", "ZoneName", "WoredaName", "Year", 
  "Epidemic_Week", "Month", "Maternal_Death", "Perinatal_Death"
)

# 3b. PHEM 2020-2023
weekly_MPDSR_2020_2023_clean <- PHEM_weekly_2020_2023 %>%
  filter(year != 2020) %>%
  select(region_name, zone_name, woreda_name, year, epidemic_week, month, maternal_death, prinatal_death) %>%
  rename(
    RegionName = region_name,
    ZoneName = zone_name,
    WoredaName = woreda_name,
    Year = year,
    Epidemic_Week = epidemic_week,
    Month = month,
    Maternal_Death = maternal_death,
    Perinatal_Death = prinatal_death
  )

# 3c. PHEM 2024-2025
weekly_MPDSR_2024_2025_clean <- Copy_PHEM_2017_full %>%
  select(
    RegionName = region,
    ZoneName = zone,
    WoredaName = woreda,
    Year = year,
    Epidemic_Week = epi_week,
    Month = month,
    Maternal_Death = maternal_death,
    Perinatal_Death = perinatal_death
  )

#-------------------------------
# 4. Merge all datasets
#-------------------------------
weekly_MPDSR_all <- bind_rows(
  weekly_MPDSR_2014_2021_clean,
  weekly_MPDSR_2020_2023_clean,
  weekly_MPDSR_2024_2025_clean
)

#-------------------------------
# 5. Aggregate weekly data
#-------------------------------
weekly_MPDSR_summary <- weekly_MPDSR_all %>%
  group_by(Year, Epidemic_Week, Month) %>%
  summarise(
    Total_Maternal_Deaths = sum(Maternal_Death, na.rm = TRUE),
    Total_Perinatal_Deaths = sum(Perinatal_Death, na.rm = TRUE),
    .groups = "drop"
  )

#-------------------------------
# 6. Standardize month names
#-------------------------------
weekly_MPDSR_summary <- weekly_MPDSR_summary %>%
  mutate(
    Month = str_to_title(str_trim(Month)),
    Month = recode(Month,
                   "Jan" = "January", "Jannuary" = "January",
                   "Feruary" = "February", "Feburary" = "February",
                   "Feb" = "February", "Mar" = "March",
                   "march" = "March", "Apr" = "April", "april" = "April",
                   "April?" = "April", "May" = "May", "Megabit" = "March",
                   "Jun" = "June", "june" = "June", "Jul" = "July",
                   "JulY" = "July", "Aug" = "August", "aug" = "August",
                   "Augest" = "August", "Auguset" = "August", "Sep" = "September",
                   "september" = "September", "Sptember" = "September",
                   "Sene" = "June", "Ocober" = "October", "Octover" = "October",
                   "october" = "October", "Nov" = "November", "november" = "November",
                   "Novembet" = "November", "Novemder" = "November",
                   "December" = "December", "Deceberm" = "December",
                   .default = Month)
  )

#-------------------------------
# 7. Convert to Ethiopian year and quarter
#-------------------------------
get_ethiopian_year_quarter <- function(date) {
  g_month <- month(date)
  g_year <- year(date)
  
  eth_year <- ifelse(g_month >= 9, g_year - 7, g_year - 8)
  
  eth_quarter <- case_when(
    g_month %in% c(7, 8, 9)    ~ "Q1",
    g_month %in% c(10, 11, 12) ~ "Q2",
    g_month %in% c(1, 2, 3)    ~ "Q3",
    g_month %in% c(4, 5, 6)    ~ "Q4"
  )
  
  return(list(Year_EC = eth_year, Quarter_EC = eth_quarter))
}

# Create dummy date for calculation
weekly_MPDSR_summary <- weekly_MPDSR_summary %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d"))

# Apply Ethiopian year/quarter function
eth_info <- lapply(weekly_MPDSR_summary$Date, get_ethiopian_year_quarter)
eth_info_df <- do.call(rbind, lapply(eth_info, as.data.frame))

weekly_MPDSR_summary <- bind_cols(weekly_MPDSR_summary, eth_info_df) %>%
  mutate(Year_QR = paste0(Year_EC, "_", Quarter_EC)) %>%
  select(-Date)

#-------------------------------
# 8. Save final dataset
#-------------------------------
Ethio_FY_MPDSR_2006_2017_EC <- weekly_MPDSR_summary

saveRDS(Ethio_FY_MPDSR_2006_2017_EC, file = "Ethio_FY_MPDSR_2006_2017_EC.rds")
