# Load required library
library(dplyr)

#-------------------------------------------
# 1. Clean 'IstheDeathPreventable' variable
#-------------------------------------------
ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 <- 
  ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 %>%
  mutate(
    DeathPreventable_clean = case_when(
      grepl("^yes$", IstheDeathPreventable, ignore.case = TRUE) ~ "Yes",
      grepl("^no$", IstheDeathPreventable, ignore.case = TRUE) ~ "No",
      grepl("^unknown$", IstheDeathPreventable, ignore.case = TRUE) ~ "Unknown",
      TRUE ~ NA_character_
    )
  )

# Check result
table(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554$DeathPreventable_clean, useNA = "always")

#-------------------------------------------
# 2. Clean and impute Delay variables
#-------------------------------------------

# List of delay variables
delay_vars <- names(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554)[53:66]

# Function to standardize Yes/No values
clean_delay <- function(x) {
  x <- toupper(as.character(x))
  x[x %in% c("Y", "YES", "YES ")] <- "YES"
  x[x %in% c("NO", "N0")] <- "NO"
  return(x)
}

# Apply cleaning
ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars] <- 
  lapply(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars], clean_delay)

# Function to impute missing values proportionally
impute_proportional <- function(x) {
  x <- as.character(x)
  na_idx <- which(is.na(x))
  if (length(na_idx) > 0) {
    tab <- table(x, useNA = "no")
    probs <- tab / sum(tab)
    x[na_idx] <- sample(names(probs), size = length(na_idx), replace = TRUE, prob = probs)
  }
  return(x)
}

# Apply proportional imputation
ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars] <- 
  lapply(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars], impute_proportional)

# Convert anything not "YES" to "NO"
fix_delay <- function(x) {
  x <- as.character(x)
  x[x %in% c("YES")] <- "YES"
  x[!x %in% c("YES", "NO")] <- "NO"
  return(x)
}

ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars] <- 
  lapply(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars], fix_delay)

# Quick verification
lapply(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[ , delay_vars], table, useNA = "always")

#-------------------------------------------
# 3. Impute missing Date_D and Month_D
#-------------------------------------------
# Use Date_Rep and Month_Rep to fill missing values
ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 <- ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 %>%
  mutate(
    Date_D = ifelse(is.na(Date_D), 18, Date_D),
    Month_D = ifelse(is.na(Month_D), 6, Month_D)
  )

#-------------------------------------------
# 4. Assign Ethiopian Fiscal Year (EFY) and Quarter
#-------------------------------------------
get_EFY <- function(year, month) {
  if (month >= 11) return(year + 1)  # Hamle(11), Nehase(12), Pagume(13)
  return(year)
}

get_EFY_quarter <- function(month) {
  if (month %in% c(11,12,13,1)) return("Q1")
  if (month %in% c(2,3,4)) return("Q2")
  if (month %in% c(5,6,7)) return("Q3")
  if (month %in% c(8,9,10)) return("Q4")
}

ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 <- ethio_mdsr_cleaned_with_imputed_cod_20251215_220554 %>%
  mutate(
    EFY = mapply(get_EFY, Year_D, Month_D),
    EFY_Quarter = sapply(Month_D, get_EFY_quarter),
    EFY_Quarter_Label = paste0(EFY, "_", EFY_Quarter)
  )

#-------------------------------------------
# 5. Convert Ethiopian date to Gregorian date (manual approximation)
#-------------------------------------------
ethio_to_greg_manual <- function(year, month, day) {
  if (is.na(year) | is.na(month) | is.na(day)) return(NA)
  
  greg_year <- year + 7
  meskerem1 <- as.Date(paste0(greg_year - 1, "-09-11"))
  days_in_month <- c(30,30,30,30,30,30,30,30,30,30,30,30,5)
  offset <- sum(days_in_month[1:(month-1)]) + (day-1)
  meskerem1 + offset
}

ethio_mdsr_cleaned_with_imputed_cod_20251215_220554$Date_D_Greg <- as.Date(
  mapply(ethio_to_greg_manual, Year_D, Month_D, Date_D), origin = "1970-01-01"
)

#-------------------------------------------
# 6. Extract Gregorian year
#-------------------------------------------
ethio_mdsr_cleaned_with_imputed_cod_20251215_220554$Year_D_Greg <- as.numeric(
  format(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554$Date_D_Greg, "%Y")
)

# Quick check
head(ethio_mdsr_cleaned_with_imputed_cod_20251215_220554[, c("Date_D_Greg", "Year_D_Greg", "EFY_Quarter_Label")])
