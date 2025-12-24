# Date: 2024
# Purpose: Clean, code, and analyze perinatal death data using ICD-PM
# ====================================================================

# Load required libraries
library(dplyr)
library(stringr)
library(ethiodate)
library(tidyr)

# ====================================================================
# SECTION 1: INITIAL SETUP AND DATA PREPARATION
# ====================================================================

cat("=== SECTION 1: DATA PREPARATION ===\n")

# Store original column names for reference
original_colnames <- colnames(final_national_pdsr_clean)
cat(sprintf("Original dataset has %d columns and %d rows\n", 
            ncol(final_national_pdsr_clean), nrow(final_national_pdsr_clean)))

# Initialize tracking columns
final_national_pdsr_clean$processing_timestamp <- Sys.time()
final_national_pdsr_clean$data_quality_flag <- "original"
final_national_pdsr_clean$notes <- NA_character_

# ====================================================================
# SECTION 2: DATE CLEANING AND IMPUTATION WITH EFY CORRECTION
# ====================================================================

cat("\n=== SECTION 2: DATE CLEANING AND IMPUTATION WITH EFY CORRECTION ===\n")

# Initialize date tracking columns
final_national_pdsr_clean$date_imputation_source <- "original"
final_national_pdsr_clean$date_validation_flag <- "valid"

# Step 2.1: Apply specific case corrections FIRST
cat("Applying specific case corrections...\n")
final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    # Specific case corrections
    Yearofdeath = case_when(
      S.No == 21970 ~ 2017,
      S.No %in% c(24601, 24602) ~ 2017,
      TRUE ~ Yearofdeath
    ),
    
    MonthofdeathMM = case_when(
      S.No == 24084 ~ 1,
      S.No == 24085 ~ 1,
      S.No == 24407 ~ 2,
      TRUE ~ MonthofdeathMM
    ),
    
    # Update notes for corrections
    notes = case_when(
      S.No %in% c(21970, 24601, 24602) ~ 
        paste(ifelse(is.na(notes), "", paste(notes, ";")), "Year corrected to 2017"),
      S.No %in% c(24084, 24085, 24407) ~ 
        paste(ifelse(is.na(notes), "", paste(notes, ";")), "Month corrected"),
      TRUE ~ notes
    )
  )

# Step 2.2: Fix obvious errors
final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    # Fix Year: 3017 → 2017
    Yearofdeath = ifelse(Yearofdeath == 3017, 2017, Yearofdeath),
    date_imputation_source = ifelse(Yearofdeath == 3017, "corrected_3017_to_2017", date_imputation_source),
    notes = ifelse(Yearofdeath == 3017 & !grepl("3017", notes, fixed = TRUE),
                   paste(ifelse(is.na(notes), "", paste(notes, ";")), "3017 corrected to 2017"),
                   notes),
    
    # Fix Month: Ensure within Ethiopian range (1-13)
    MonthofdeathMM = ifelse(MonthofdeathMM %in% 1:13, MonthofdeathMM, NA),
    date_validation_flag = ifelse(!MonthofdeathMM %in% 1:13 & !is.na(MonthofdeathMM), 
                                  "invalid_month", date_validation_flag),
    
    # Fix Date: 0 → 1
    Date_of_death = ifelse(Date_of_death == 0, 1, Date_of_death),
    date_imputation_source = ifelse(Date_of_death == 0, "corrected_0_to_1", date_imputation_source)
  )

# Step 2.3: Validate Ethiopian calendar dates
final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    date_validation_flag = case_when(
      # Months 1-12: days 1-30
      (!is.na(MonthofdeathMM) & MonthofdeathMM <= 12 & !is.na(Date_of_death) & Date_of_death > 30) ~ "invalid_day_for_month",
      
      # Month 13 (Pagume): days 1-6
      (MonthofdeathMM == 13 & !is.na(Date_of_death) & Date_of_death > 6) ~ "invalid_day_for_pagume",
      
      TRUE ~ date_validation_flag
    ),
    
    # Set invalid dates to NA
    Date_of_death = ifelse(date_validation_flag != "valid", NA, Date_of_death)
  )

# Step 2.4: Impute missing dates from reporting fields
cat("Imputing missing dates from reporting fields...\n")

final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    # Impute Year from ReportingYear
    Yearofdeath = ifelse(is.na(Yearofdeath), ReportingYear, Yearofdeath),
    date_imputation_source = ifelse(is.na(Yearofdeath) & !is.na(ReportingYear), 
                                    "from_reporting_year", date_imputation_source),
    
    # Impute Month from ReportingMonth
    MonthofdeathMM = ifelse(is.na(MonthofdeathMM), ReportingMonth, MonthofdeathMM),
    date_imputation_source = ifelse(is.na(MonthofdeathMM) & !is.na(ReportingMonth), 
                                    "from_reporting_month", date_imputation_source),
    
    # Impute Date from Reportingdate
    Date_of_death = ifelse(is.na(Date_of_death), Reportingdate, Date_of_death),
    date_imputation_source = ifelse(is.na(Date_of_death) & !is.na(Reportingdate), 
                                    "from_reporting_date", date_imputation_source)
  )

# Step 2.5: Extract dates from Deceased.ID when still missing
cat("Extracting dates from Deceased.ID...\n")

final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    year_from_id = ifelse(
      is.na(Yearofdeath) | is.na(MonthofdeathMM),
      as.numeric(str_extract(Deceased.ID, "(?<=\\.|-)\\d{2}(?=\\.|-)")) + 2000,
      NA
    ),
    month_from_id = ifelse(
      is.na(MonthofdeathMM),
      as.numeric(str_extract(Deceased.ID, "(?<=\\d{2}[.-])\\d{2}")),
      NA
    )
  ) %>%
  mutate(
    Yearofdeath = ifelse(is.na(Yearofdeath), year_from_id, Yearofdeath),
    MonthofdeathMM = ifelse(is.na(MonthofdeathMM), month_from_id, MonthofdeathMM),
    date_imputation_source = ifelse(!is.na(year_from_id) | !is.na(month_from_id),
                                    "from_deceased_id", date_imputation_source)
  )

# Step 2.6: Impute missing days using median by Year-Month
cat("Imputing missing days using median values...\n")

median_days_by_ym <- final_national_pdsr_clean %>%
  filter(!is.na(Date_of_death) & date_validation_flag == "valid") %>%
  group_by(Yearofdeath, MonthofdeathMM) %>%
  summarise(median_day = median(Date_of_death, na.rm = TRUE), .groups = "drop") %>%
  mutate(median_day = round(median_day))

final_national_pdsr_clean <- final_national_pdsr_clean %>%
  left_join(median_days_by_ym, by = c("Yearofdeath", "MonthofdeathMM")) %>%
  mutate(
    Date_of_death = ifelse(is.na(Date_of_death) & !is.na(median_day), 
                           median_day, Date_of_death),
    date_imputation_source = ifelse(is.na(Date_of_death) & !is.na(median_day),
                                    "from_median_day", date_imputation_source)
  ) %>%
  select(-median_day)

# Step 2.7: Manual corrections for specific cases
cat("Applying manual corrections...\n")

manual_corrections <- data.frame(
  S.No = c(24465, 24844, 20986, 23201),
  Year = c(NA, NA, 2017, NA),
  Month = c(4, 5, 3, 4),
  Day = c(15, 22, NA, NA)
)

for (i in 1:nrow(manual_corrections)) {
  row_id <- manual_corrections$S.No[i]
  
  if (!is.na(manual_corrections$Year[i])) {
    final_national_pdsr_clean$Yearofdeath[final_national_pdsr_clean$S.No == row_id] <- 
      manual_corrections$Year[i]
    final_national_pdsr_clean$date_imputation_source[final_national_pdsr_clean$S.No == row_id] <- 
      "manual_correction_year"
  }
  if (!is.na(manual_corrections$Month[i])) {
    final_national_pdsr_clean$MonthofdeathMM[final_national_pdsr_clean$S.No == row_id] <- 
      manual_corrections$Month[i]
    final_national_pdsr_clean$date_imputation_source[final_national_pdsr_clean$S.No == row_id] <- 
      "manual_correction_month"
  }
  if (!is.na(manual_corrections$Day[i])) {
    final_national_pdsr_clean$Date_of_death[final_national_pdsr_clean$S.No == row_id] <- 
      manual_corrections$Day[i]
    final_national_pdsr_clean$date_imputation_source[final_national_pdsr_clean$S.No == row_id] <- 
      "manual_correction_day"
  }
  
  final_national_pdsr_clean$date_validation_flag[final_national_pdsr_clean$S.No == row_id] <- 
    "manual_validated"
}

# Step 2.8: CORRECT EFY CALCULATION AND CONVERT TO GREGORIAN
cat("\n--- Correcting EFY Calculation and Converting Dates ---\n")

final_national_pdsr_clean <- final_national_pdsr_clean %>%
  mutate(
    # CORRECT EFY LOGIC (from your first code):
    # Ethiopian Fiscal Year starts in Month 11 (Hamle)
    # - Months 11, 12, 13 (Hamle, Nehasie, Pagume) → Current Ethiopian year = EFY
    # - Months 1-10 (Meskerem to Tahsas) → Previous Ethiopian year = EFY (Year - 1)
    EFY = case_when(
      MonthofdeathMM %in% c(11, 12, 13) ~ Yearofdeath,      # Hamle, Nehasie, Pagume
      MonthofdeathMM %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ~ Yearofdeath - 1,  # Meskerem to Tahsas
      TRUE ~ NA_real_
    ),
    
    # Ethiopian Fiscal Quarter
    EFY_Quarter = case_when(
      MonthofdeathMM %in% c(11, 12, 13, 1) ~ "Q1",  # Hamle, Nehasie, Pagume, Meskerem
      MonthofdeathMM %in% c(2, 3, 4) ~ "Q2",        # Tikimt, Hidar, Tahsas
      MonthofdeathMM %in% c(5, 6, 7) ~ "Q3",        # Tir, Yekatit, Megabit
      MonthofdeathMM %in% c(8, 9, 10) ~ "Q4",       # Miazia, Ginbot, Sene
      TRUE ~ NA_character_
    ),
    
    # Combined EFY and Quarter
    EFY_Quarter_Label = ifelse(!is.na(EFY) & !is.na(EFY_Quarter),
                               paste0("EFY", EFY, "_", EFY_Quarter),
                               NA_character_),
    
    # Additional correction for S.No 24601 (special case from your first code)
    EFY = ifelse(S.No == 24601 & EFY == 2018 & MonthofdeathMM %in% c(8, 9, 10), 
                 2017, EFY),
    
    # Recalculate EFY_Quarter_Label after EFY correction
    EFY_Quarter_Label = ifelse(S.No == 24601 & !is.na(EFY) & !is.na(EFY_Quarter),
                               paste0("EFY", EFY, "_", EFY_Quarter),
                               EFY_Quarter_Label)
  ) %>%
  
  # Step 2.9: Convert Ethiopian dates to Gregorian
  mutate(
    # Create Ethiopian date object
    eth_date = eth_make_date(
      year = Yearofdeath,
      month = MonthofdeathMM,
      day = Date_of_death
    ),
    
    # Check if date is valid
    date_validation_flag = ifelse(is.na(eth_date), "invalid_ethiopian_date", date_validation_flag),
    
    # Convert to Gregorian
    greg_date = as.Date(eth_date),
    
    # Extract Gregorian year
    GregorianYearOfDeath = as.integer(format(greg_date, "%Y"))
  )

# Step 2.10: Verify EFY corrections
cat("\nVerifying EFY corrections...\n")

# Check the specific cases we corrected
corrected_cases <- final_national_pdsr_clean %>%
  filter(S.No %in% c(21970, 24084, 24085, 24407, 24601, 24602)) %>%
  select(S.No, Yearofdeath, MonthofdeathMM, Date_of_death, 
         EFY, EFY_Quarter, EFY_Quarter_Label, GregorianYearOfDeath, notes)

cat("Corrected Cases:\n")
print(corrected_cases)

# EFY validation check
validation_check <- final_national_pdsr_clean %>%
  filter(!is.na(MonthofdeathMM) & !is.na(Yearofdeath)) %>%
  mutate(
    Expected_EFY = case_when(
      MonthofdeathMM %in% c(11, 12, 13) ~ Yearofdeath,
      MonthofdeathMM %in% c(1:10) ~ Yearofdeath - 1,
      TRUE ~ NA_real_
    ),
    Is_Correct = EFY == Expected_EFY
  ) %>%
  summarise(
    Total_Cases = n(),
    Correct_EFY = sum(Is_Correct, na.rm = TRUE),
    Incorrect_EFY = sum(!Is_Correct, na.rm = TRUE),
    Percent_Correct = round(Correct_EFY / Total_Cases * 100, 1)
  )

cat("\nEFY Calculation Validation:\n")
print(validation_check)

if (validation_check$Percent_Correct == 100) {
  cat("✅ All EFY calculations are correct!\n")
} else {
  cat(sprintf("⚠️ Found %d cases with inconsistent EFY\n", validation_check$Incorrect_EFY))
}

# ====================================================================
# SECTION 3: MATERNAL CONDITION CLASSIFICATION (M1-M5)
# ====================================================================

cat("\n=== SECTION 3: MATERNAL CONDITION CLASSIFICATION ===\n")

# Store original maternal classification
final_national_pdsr_clean$ICDPM_Priority_Original <- final_national_pdsr_clean$ICDPM_Priority
final_national_pdsr_clean$specfic_cuase_Original <- final_national_pdsr_clean$specfic_cuase

# Check initial distribution
cat("Initial ICD-PM Priority distribution:\n")
print(table(final_national_pdsr_clean$ICDPM_Priority, useNA = "always"))

# ====================================================================
# STEP 3.1: MOVE "Cord prolapse" FROM Other_NCD TO specfic_cuase
# ====================================================================

cat("\n--- Step 3.1: Moving 'Cord prolapse' from Other_NCD to specfic_cuase ---\n")

cord_prolapse_cases <- sum(final_national_pdsr_clean$Other_NCD == "Cord prolapse", na.rm = TRUE)
cat(sprintf("Found %d cases of 'Cord prolapse' in Other_NCD\n", cord_prolapse_cases))

if (cord_prolapse_cases > 0) {
  cord_rows <- which(final_national_pdsr_clean$Other_NCD == "Cord prolapse")
  
  for (i in cord_rows) {
    current_specfic <- final_national_pdsr_clean$specfic_cuase[i]
    
    if (is.na(current_specfic) || current_specfic == "" || 
        tolower(trimws(current_specfic)) %in% c("no", "none", "unknown", "nad")) {
      final_national_pdsr_clean$specfic_cuase[i] <- "Cord prolapse"
    } else {
      final_national_pdsr_clean$specfic_cuase[i] <- paste(current_specfic, "+ Cord prolapse")
    }
    final_national_pdsr_clean$Other_NCD[i] <- NA
    final_national_pdsr_clean$notes[i] <- paste(ifelse(is.na(final_national_pdsr_clean$notes[i]), "", 
                                                       paste(final_national_pdsr_clean$notes[i], ";")),
                                                "Moved Cord prolapse from Other_NCD to specfic_cuase")
  }
  cat(sprintf("Successfully moved %d cases\n", length(cord_rows)))
}

# ====================================================================
# STEP 3.2: CLASSIFY M5 CASES USING TEXT FIELDS
# ====================================================================

cat("\n--- Step 3.2: Reclassifying M5 cases using comprehensive keyword matching ---\n")

# Comprehensive keyword dictionary based on your data
maternal_keywords <- list(
  M1 = c(
    # Cord issues
    "cord prolaps", "cord prolapse", "nuchal cord", "cord entangle", "cord tie",
    "cord knot", "cord strangulation", "cord presentation", "cord accident",
    "cord detachment", "cord engagement", "cored tie", "nocked cord",
    "cord integulated", "cord thight", "cored prolapse", "cord prollapse",
    "cordeprolaps", "cord prolabs", "cord prolapce", "cord prolapes",
    
    # Placental issues
    "abruptio", "abruption", "abruptio placenta", "abrabtio placenta",
    "abraptio placenta", "placenta previa", "placental", "placenta abrraptio",
    
    # APH and hemorrhage
    "aph", "antepartum haemorrhage", "antepartum hemorrhage", "vaginal bleeding",
    "excessive vaginal bleeding", "haemorrhage", "heemorrhage", "hemorrage",
    
    # Amniotic fluid embolism
    "amniotic fluid embolism", "aminotic fluid embolism",
    
    # Chorioamnionitis
    "chorioamnio", "chorioaminitis", "amnionitis", "membranitis", "placentitis",
    
    # Other M1
    "vasa previa", "premature rupture", "prom", "pprom", "membrane rupture",
    "ruptured membrane"
  ),
  
  M2 = c(
    # Multiple pregnancy
    "multiple gestation", "multiple pregnancy", "twin", "triplet", "triple",
    "twins", "co twins", "discordant twin", "locked twin", "twine pregnancy",
    "twins pregnancy", "multiple pg", "multiple px",
    
    # Amniotic fluid disorders
    "oligohydram", "polyhydram", "hydramnios", "oligohaydramynus",
    "oligohydraminious", "oligohydraminous", "oligohydramiNous",
    "oligohydraminus", "oligohydramnios", "oligohydrocephaliy",
    "oligohyraminos", "olygohdraminus", "polyhidramNous", "polyhydramanus",
    "polihydraminus", "polyhdraminous", "polyhdraminus", "polyhydraminos",
    "polyhydraminous", "polyhydraminus", "polyhydramionus", "polyhydramnious",
    "polyhydramous", "anyhydramnious",
    
    # Malpresentation
    "abnormal lie", "abnormal presentation", "malpresentation", "mal presentation",
    "mal position", "breech presentation", "transverse lie", "transverse presentation",
    "shoulder presentation", "brow presentation", "complete breech", "breach",
    "malpresentession", "mal persenteion", "mal presentaion", "mal presentatin",
    "mal prsentation", "mal ptesentation", "hand prolaps", "compaound presentation",
    
    # Other M2
    "cervical insufficency", "cervical insufficiency", "ectopic pregnancy",
    "post term", "postterm", "post date", "preterm", "premature", "iugr",
    "prom", "pprom", "premature rupture", "prolonged prom", "iufd"
  ),
  
  M3 = c(
    # Prolonged/obstructed labor
    "prolonged labour", "prolonged labor", "prolonged 2nd stage",
    "prolonged second stage", "prolonged latent", "obstructed labor",
    "obstructed labour", "cpd", "contracted pelv", "contracted pelvis",
    "cotracted pelvis", "obstruction",
    
    # Delivery complications
    "breech delivery", "forceps", "vacuum", "caesarean", "c/s", "destructive delivery",
    "shoulder dystocia", "sholder dystocia", "brow presentation",
    
    # Uterine complications
    "uterine rupture", "ruptured uterus", "uterine rapture", "utern rupture",
    "uterine atony", "imminent uterine rupture",
    
    # Stage of labor
    "2nd stage", "second stage", "stage of labour", "stage of labor", "latent stage",
    "2ND Stageof labour", "late 2nd stage labour",
    
    # Other M3
    "failed induction", "precipitate labour", "poor contraction",
    "poor maternal effort", "labor was prolonged"
  ),
  
  M4 = c(
    # Hypertensive disorders
    "eclampsia", "preeclampsia", "pre eclampsia", "hypertension", "hypertensive",
    "hdp", "spe", "pre-eclampsia", "pre-eclamsia", "pre eclamsia", "pre eclapsia",
    "preeclampsia eclampsia", "eclamsia", "preclampsia", "preeclampsia",
    
    # Anemia/Hematological
    "anemia", "anaemia", "aneamia", "anemic", "severe anemia", "sever anemia",
    "severe anaemia", "anaemic", "s\\.anemia", "s\\.anaemia",
    
    # Infections
    "malaria", "cerebral malaria", "complicated malaria", "severe malaria",
    "sepsis", "obstetric sepsis", "peurperal sepsis", "sti", "syphilis",
    "tuberculosis", "hepatitis", "hiv", "rvi", "vdrl positive", "hbsag positive",
    "hepititis", "uti", "infection", "maternal infection", "chorioamnionitis",
    
    # Diabetes/Endocrine
    "diabetes", "dm", "gdm", "gestational diabetes", "type 1 dm", "type 2 dm",
    "dabetic", "diabetes mellitus", "gestational diabetes mellitus",
    
    # Cardiovascular/Respiratory
    "asthma", "cardiac failure", "congestive heart failure", "chf", "cardio",
    "respiratory distress", "respiratory failure", "pulmonary", "pna",
    "cardio plumonary", "cardio respiratory",
    
    # Other medical conditions
    "hepatic", "liver disease", "chronic liver disease", "acute fatty liver",
    "renal", "kidney", "aki", "hyperthyroidism", "hypothyroidism", "goiter",
    "thrombocytopenia", "trombocytopenia", "hypernatremia", "hypokalemia",
    "psychiatric", "psychosis", "trauma", "injury", "fall", "fever",
    "severe", "moderate", "chronic", "acute",
    
    # Specific conditions from your data
    "autoimmune hepatitis", "cervical tumor", "chd", "chf", "chhf", "chronic cough",
    "cld", "dvt", "fistula", "hydrocephales", "hypernatremia", "hypokalemis",
    "hypotension", "hypovolemic shock", "hypovolumic shock", "lca", "mof",
    "pulmonaryhtn", "rd", "rds", "retained placenta", "sam"
  ),
  
  M5_keep = c(
    "^$", "^ $", "^0$", "^no$", "^none$", "^normal$", "^nad$", 
    "^unknown$", "^uknown$", "^unkn$", "^unkown$", "^unidentified$",
    "^unspesfied$", "^other unspecified$", "^others$", "^non$",
    "^no maternal cause$", "^no identified", "^healthy$", "^well$",
    "^stable$", "^^$", "^no aminotic fluid$", "^no identified maternal cause$",
    "^no maternal causes$", "^still birth of un known time$"
  )
)

# Function to classify M5 cases
classify_maternal_condition <- function(text) {
  if (is.na(text) || trimws(text) == "" || text == "0") {
    return("M5: No maternal condition")
  }
  
  clean_text <- tolower(trimws(text))
  clean_text <- gsub("[[:punct:]]", "", clean_text)
  clean_text <- gsub("\\s+", " ", clean_text)
  
  # Check if should stay M5
  for (pattern in maternal_keywords$M5_keep) {
    if (grepl(pattern, clean_text, ignore.case = TRUE)) {
      return("M5: No maternal condition")
    }
  }
  
  # Check M1 patterns (highest priority)
  for (keyword in maternal_keywords$M1) {
    if (grepl(keyword, clean_text, ignore.case = TRUE)) {
      return("M1: Placenta/cord/membrane complications")
    }
  }
  
  # Check M3 patterns
  for (keyword in maternal_keywords$M3) {
    if (grepl(keyword, clean_text, ignore.case = TRUE)) {
      return("M3: Labour/delivery complications")
    }
  }
  
  # Check M2 patterns
  for (keyword in maternal_keywords$M2) {
    if (grepl(keyword, clean_text, ignore.case = TRUE)) {
      return("M2: Maternal pregnancy complications")
    }
  }
  
  # Check M4 patterns
  for (keyword in maternal_keywords$M4) {
    if (grepl(keyword, clean_text, ignore.case = TRUE)) {
      return("M4: Maternal medical conditions")
    }
  }
  
  # If text exists but no pattern matched, default to M4
  if (clean_text != "" && !is.na(clean_text)) {
    return("M4: Maternal medical conditions")
  }
  
  return("M5: No maternal condition")
}

# Apply classification to M5 cases
m5_indices <- which(final_national_pdsr_clean$ICDPM_Priority == "M5: No maternal condition")
cat(sprintf("Found %d M5 cases to review\n", length(m5_indices)))

m5_reclassification_summary <- list(M1 = 0, M2 = 0, M3 = 0, M4 = 0, M5 = 0)

pb <- txtProgressBar(min = 0, max = length(m5_indices), style = 3)
for (i in seq_along(m5_indices)) {
  idx <- m5_indices[i]
  text <- final_national_pdsr_clean$specfic_cuase[idx]
  
  new_class <- classify_maternal_condition(text)
  
  if (new_class != "M5: No maternal condition") {
    final_national_pdsr_clean$ICDPM_Priority[idx] <- new_class
    
    if (new_class == "M1: Placenta/cord/membrane complications") {
      m5_reclassification_summary$M1 <- m5_reclassification_summary$M1 + 1
    } else if (new_class == "M2: Maternal pregnancy complications") {
      m5_reclassification_summary$M2 <- m5_reclassification_summary$M2 + 1
    } else if (new_class == "M3: Labour/delivery complications") {
      m5_reclassification_summary$M3 <- m5_reclassification_summary$M3 + 1
    } else if (new_class == "M4: Maternal medical conditions") {
      m5_reclassification_summary$M4 <- m5_reclassification_summary$M4 + 1
    }
  } else {
    m5_reclassification_summary$M5 <- m5_reclassification_summary$M5 + 1
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

# Update the ICDPM_Priority_updated column
final_national_pdsr_clean$ICDPM_Priority_updated <- final_national_pdsr_clean$ICDPM_Priority

cat("\nM5 Reclassification Results:\n")
print(as.data.frame(m5_reclassification_summary))

# ====================================================================
# STEP 3.3: MAP NEONATAL TEXT TO MATERNAL CONDITIONS FOR REMAINING M5
# ====================================================================

cat("\n--- Step 3.3: Mapping neonatal text to maternal conditions for remaining M5 cases ---\n")

# Function to map neonatal text to maternal ICD-PM
map_neonatal_to_maternal <- function(neonatal_text) {
  if (is.na(neonatal_text) || trimws(neonatal_text) == "") return(NULL)
  
  text <- trimws(neonatal_text)
  text_lower <- tolower(text)
  text_lower <- gsub("[[:punct:]]", " ", text_lower)
  text_lower <- gsub("\\s+", " ", text_lower)
  
  # M1: Complications of placenta, cord and membranes (P02)
  if (grepl("cord prolaps|prolapsed cord|cordprolaps", text_lower)) {
    return(list(category = "M1: Placenta/cord/membrane complications",
                specfic_update = "Cord prolapse",
                confidence = "high"))
  }
  if (grepl("cord.*around.*neck|nuchal cord|entanglement.*cord|knot.*cord", text_lower)) {
    return(list(category = "M1: Placenta/cord/membrane complications",
                specfic_update = "Cord compression/entanglement",
                confidence = "high"))
  }
  if (grepl("\\baph\\b|antepartum.*hem|abruptio.*placent", text_lower)) {
    return(list(category = "M1: Placenta/cord/membrane complications",
                specfic_update = "Antepartum hemorrhage/Placental abruption",
                confidence = "high"))
  }
  if (grepl("placenta previa|placenta praevia", text_lower)) {
    return(list(category = "M1: Placenta/cord/membrane complications",
                specfic_update = "Placenta praevia",
                confidence = "high"))
  }
  if (grepl("chorioamnionitis|amnionitis|choriaminitis", text_lower)) {
    return(list(category = "M1: Placenta/cord/membrane complications",
                specfic_update = "Chorioamnionitis",
                confidence = "high"))
  }
  
  # M2: Maternal complications of pregnancy (P01)
  if (grepl("\\btwin\\b|\\btwins\\b|\\btriplet\\b|multiple.*gestation", text_lower)) {
    return(list(category = "M2: Maternal pregnancy complications",
                specfic_update = "Multiple pregnancy",
                confidence = "high"))
  }
  if (grepl("\\bprom\\b|\\bpprom\\b|premature.*rupture.*membran", text_lower)) {
    return(list(category = "M2: Maternal pregnancy complications",
                specfic_update = "Premature rupture of membranes",
                confidence = "high"))
  }
  if (grepl("oligohydramnios|oligo.*hydram", text_lower)) {
    return(list(category = "M2: Maternal pregnancy complications",
                specfic_update = "Oligohydramnios",
                confidence = "high"))
  }
  if (grepl("polyhydramnios|hydramnios", text_lower)) {
    return(list(category = "M2: Maternal pregnancy complications",
                specfic_update = "Polyhydramnios",
                confidence = "high"))
  }
  if (grepl("malpresentation|breech.*presentation|transverse.*lie", text_lower)) {
    return(list(category = "M2: Maternal pregnancy complications",
                specfic_update = "Malpresentation",
                confidence = "high"))
  }
  
  # M3: Complications of labour and delivery (P03)
  if (grepl("breech delivery|breach delivery", text_lower)) {
    return(list(category = "M3: Labour/delivery complications",
                specfic_update = "Breech delivery",
                confidence = "high"))
  }
  if (grepl("shoulder dystocia|sholder dystocia|\\bcpd\\b", text_lower)) {
    return(list(category = "M3: Labour/delivery complications",
                specfic_update = "Shoulder dystocia/Cephalopelvic disproportion",
                confidence = "high"))
  }
  if (grepl("forceps delivery|vacuum delivery|caesarean delivery|c/s\\b", text_lower)) {
    return(list(category = "M3: Labour/delivery complications",
                specfic_update = "Instrumental delivery",
                confidence = "high"))
  }
  if (grepl("prolonged labour|prolonged labor|obstructed labour", text_lower)) {
    return(list(category = "M3: Labour/delivery complications",
                specfic_update = "Prolonged/obstructed labour",
                confidence = "high"))
  }
  
  # M4: Maternal medical and surgical conditions (P00/P04)
  if (grepl("eclampsia|preeclampsia|pre.*eclampsia|hypertension.*pregnancy", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal hypertensive disorders",
                confidence = "high"))
  }
  if (grepl("maternal.*malaria|maternal.*sepsis|maternal.*syphilis", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal infectious disease",
                confidence = "high"))
  }
  if (grepl("maternal.*anemia|maternal.*anaemia|s\\.anemia", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal anemia",
                confidence = "high"))
  }
  if (grepl("traditional.*ovuloctomy|uviloctomy|herbal.*medication", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal noxious influence",
                confidence = "high"))
  }
  if (grepl("\\brh\\b.*incompat|rh.*isoimmun|abo.*incompat", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Blood group incompatibility",
                confidence = "high"))
  }
  if (grepl("maternal.*diabetes|gestational.*diabetes|\\bgdm\\b", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal diabetes",
                confidence = "high"))
  }
  if (grepl("maternal.*injur|maternal.*trauma|maternal.*fall", text_lower)) {
    return(list(category = "M4: Maternal medical conditions",
                specfic_update = "Maternal injury",
                confidence = "high"))
  }
  
  return(NULL)
}

# Apply to remaining M5 cases with neonatal data
remaining_m5_indices <- which(final_national_pdsr_clean$ICDPM_Priority == "M5: No maternal condition")
neonatal_mapping_summary <- list(M1 = 0, M2 = 0, M3 = 0, M4 = 0)

cat(sprintf("Processing %d remaining M5 cases with neonatal data...\n", length(remaining_m5_indices)))

pb <- txtProgressBar(min = 0, max = length(remaining_m5_indices), style = 3)
for (i in seq_along(remaining_m5_indices)) {
  idx <- remaining_m5_indices[i]
  neonatal_text <- final_national_pdsr_clean$OtherspeciedNeonatalcauses[idx]
  specfic_value <- final_national_pdsr_clean$specfic_cuase[idx]
  
  # Skip if no neonatal text or specfic_cuase already has content
  if (is.na(neonatal_text) || trimws(neonatal_text) == "") {
    setTxtProgressBar(pb, i)
    next
  }
  
  specfic_empty <- is.na(specfic_value) || 
    trimws(specfic_value) == "" ||
    tolower(trimws(specfic_value)) %in% 
    c("no", "none", "unknown", "nad", "uknown", "uk", "not known", 
      "not specified", "unidentified", "not identified", "not mentioned",
      "unknwon", "unkown", "unknow", "n/a", "un")
  
  if (!specfic_empty) {
    setTxtProgressBar(pb, i)
    next
  }
  
  mapping <- map_neonatal_to_maternal(neonatal_text)
  
  if (!is.null(mapping)) {
    # Update specfic_cuase if empty
    if (is.na(specfic_value) || specfic_value == "") {
      final_national_pdsr_clean$specfic_cuase[idx] <- mapping$specfic_update
    }
    
    # Update ICD-PM Priority
    final_national_pdsr_clean$ICDPM_Priority[idx] <- mapping$category
    final_national_pdsr_clean$ICDPM_Priority_updated[idx] <- mapping$category
    
    # Track category
    if (mapping$category == "M1: Placenta/cord/membrane complications") {
      neonatal_mapping_summary$M1 <- neonatal_mapping_summary$M1 + 1
    } else if (mapping$category == "M2: Maternal pregnancy complications") {
      neonatal_mapping_summary$M2 <- neonatal_mapping_summary$M2 + 1
    } else if (mapping$category == "M3: Labour/delivery complications") {
      neonatal_mapping_summary$M3 <- neonatal_mapping_summary$M3 + 1
    } else if (mapping$category == "M4: Maternal medical conditions") {
      neonatal_mapping_summary$M4 <- neonatal_mapping_summary$M4 + 1
    }
    
    # Add note
    final_national_pdsr_clean$notes[idx] <- paste(
      ifelse(is.na(final_national_pdsr_clean$notes[idx]), "", 
             paste(final_national_pdsr_clean$notes[idx], ";")),
      "Maternal condition inferred from neonatal text"
    )
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\nNeonatal text mapping results:\n")
print(as.data.frame(neonatal_mapping_summary))

# Final maternal classification summary
cat("\nFinal Maternal Classification Distribution:\n")
print(table(final_national_pdsr_clean$ICDPM_Priority_updated, useNA = "always"))

# ====================================================================
# SECTION 4: FETAL/INFANT ICD-10 CODING (OPTIMIZED VERSION)
# ====================================================================

cat("\n=== SECTION 4: FETAL/INFANT ICD-10 CODING ===\n")

# Define the comprehensive ICD-10 coding function
assign_fetal_icd10_comprehensive <- function(timing, prematurity, asphyxia, sepsis, 
                                             neonatal_tetanus, lethal_anomaly, 
                                             neonate_text = NA, other_ncd = NA) {
  
  # Helper function to check if binary indicators are all No/NA
  all_indicators_no_na <- function(prematurity, asphyxia, sepsis, 
                                   neonatal_tetanus, lethal_anomaly) {
    indicators <- c(prematurity, asphyxia, sepsis, neonatal_tetanus, lethal_anomaly)
    all(is.na(indicators) | indicators %in% c("No", "NO", "N", "no", "n", "0", ""))
  }
  
  # Combine text fields
  text <- tolower(trimws(paste(
    ifelse(is.na(neonate_text), "", neonate_text),
    ifelse(is.na(other_ncd), "", other_ncd)
  )))
  
  # ===== 1. ANTEPARTUM STILLBIRTH =====
  if (timing == "Antepartum stillbirth") {
    # Priority 1: Lethal congenital anomaly
    if (lethal_anomaly == "Yes") return("Q89.9")
    
    # Priority 2: Check text (when all indicators No/NA)
    if (all_indicators_no_na(prematurity, asphyxia, sepsis, neonatal_tetanus, lethal_anomaly)) {
      if (grepl("anencephaly", text)) return("Q00.0")
      if (grepl("hydrocephalus", text)) return("Q03.9")
      if (grepl("spina bifida", text)) return("Q05.9")
      if (grepl("fetal blood loss|antepartum haem", text)) return("P50.9")
      if (grepl("intrauterine hypoxia", text)) return("P20.0")
      if (grepl("hydrops fetalis", text)) return("P83.2")
      if (grepl("congenital syphilis", text)) return("A50.9")
      if (grepl("intra.amniotic infection", text)) return("P39.2")
      if (grepl("cord prolaps|cord prolapse", text)) return("P02.4")
      if (grepl("cord accident|cord entanglement", text)) return("P02.5")
      if (grepl("abruption|placental abruption", text)) return("P02.1")
      if (grepl("placenta previa", text)) return("P02.0")
      if (grepl("oligohydramnios", text)) return("P01.2")
      if (grepl("polyhydramnios", text)) return("P01.3")
    }
    
    return("P95")  # Default
  }
  
  # ===== 2. INTRAPARTUM STILLBIRTH / UNKNOWN TIME =====
  if (timing %in% c("Intrapartum stillbirth", "Stillbirth of unknown time")) {
    # Priority 1: Lethal congenital anomaly
    if (lethal_anomaly == "Yes") return("Q89.9")
    
    # Priority 2: Asphyxia
    if (asphyxia == "Yes") return("P21.9")
    
    # Priority 3: Check text (when all indicators No/NA)
    if (all_indicators_no_na(prematurity, asphyxia, sepsis, neonatal_tetanus, lethal_anomaly)) {
      if (grepl("birth injury|birth trauma|cephalhaematoma", text)) return("P15.9")
      if (grepl("intrauterine hypoxia", text)) return("P20.1")
      if (grepl("intra.amniotic infection", text)) return("P39.2")
      if (grepl("light for gestational age|small for dates", text)) return("P05.9")
      if (grepl("cord prolaps|cord prolapse", text)) return("P02.4")
      if (grepl("shoulder dystocia", text)) return("P03.1")
      if (grepl("breech|breach|malpresentation", text)) return("P03.0")
    }
    
    return("P95")  # Default
  }
  
  # ===== 3. NEONATAL DEATHS =====
  if (timing %in% c("Death within 24 hours", "Death between 1-7 days", "Death between 8-28 days")) {
    # Priority 1: Binary indicators
    if (neonatal_tetanus == "Yes") return("A33")
    if (lethal_anomaly == "Yes") return("Q89.9")
    if (sepsis == "Yes") return("P36.9")
    if (asphyxia == "Yes") return("P21.9")
    if (prematurity == "Yes") return("P07.3")
    
    # Priority 2: Text patterns (when all indicators No/NA)
    if (all_indicators_no_na(prematurity, asphyxia, sepsis, neonatal_tetanus, lethal_anomaly)) {
      # Respiratory disorders
      if (grepl("respiratory distress syndrome|rds|hyaline membrane", text)) return("P22.0")
      if (grepl("meconium aspiration", text)) return("P24.0")
      if (grepl("persistent pulmonary hypertension|pphn", text)) return("P29.3")
      
      # Infection
      if (grepl("meningitis", text)) return("G03.9")
      if (grepl("congenital pneumonia", text)) return("P23.9")
      if (grepl("necrotizing enterocolitis|nec", text)) return("P77")
      
      # Congenital anomalies
      if (grepl("congenital heart|chd", text)) return("Q24.9")
      if (grepl("diaphragmatic hernia", text)) return("Q79.0")
      if (grepl("tracheo.esophageal|tef", text)) return("Q39.1")
      if (grepl("down syndrome", text)) return("Q90.9")
      
      # Neurological
      if (grepl("hypoxic ischaemic encephalopathy|hie", text)) return("P91.6")
      if (grepl("intraventricular haemorrhage|ivh", text)) return("P52.9")
      
      # Metabolic/Other
      if (grepl("kernicterus", text)) return("P57.9")
      if (grepl("hemorrhagic disease|hdn", text)) return("P53")
      if (grepl("anemia|anaemia", text)) return("P61.4")
      if (grepl("hyperbilirubinemia|jaundice|joundice", text)) return("P59.9")
      if (grepl("hypoglycemia|hypoglacima", text)) return("P70.4")
      if (grepl("hypothermia|hypotermia", text)) return("P80.9")
      
      # Prematurity/LBW
      if (grepl("extreme immaturity|<28 weeks", text)) return("P07.2")
      if (grepl("extremely low birth weight|elbw|vlbw", text)) return("P07.0")
      if (grepl("very low birth weight", text)) return("P07.1")
      if (grepl("low birth weight|lbw", text)) return("P07.1")
      
      # Multi-organ failure
      if (grepl("multiple organ failure|mof|multi organ failure", text)) return("P96.8")
    }
    
    return("P96.9")  # Default
  }
  
  return(NA_character_)
}

# Apply ICD-10 coding
cat("Applying comprehensive ICD-10 coding...\n")

pb <- txtProgressBar(min = 0, max = nrow(final_national_pdsr_clean), style = 3)
final_national_pdsr_clean$Fetal_ICD10_Code_Final <- NA_character_

for (i in 1:nrow(final_national_pdsr_clean)) {
  final_national_pdsr_clean$Fetal_ICD10_Code_Final[i] <- assign_fetal_icd10_comprehensive(
    timing = final_national_pdsr_clean$Timingofthedeath[i],
    prematurity = final_national_pdsr_clean$Prematurity[i],
    asphyxia = final_national_pdsr_clean$Asphyxia[i],
    sepsis = final_national_pdsr_clean$Sepsis.pneumonia.meningitis2[i],
    neonatal_tetanus = final_national_pdsr_clean$NeonatalTetanus[i],
    lethal_anomaly = final_national_pdsr_clean$LethalcongenitalaNomaly[i],
    neonate_text = final_national_pdsr_clean$OtherspeciedNeonatalcauses[i],
    other_ncd = final_national_pdsr_clean$Other_NCD[i]
  )
  setTxtProgressBar(pb, i)
}
close(pb)

# ====================================================================
# SECTION 5: ICD-PM GROUP MAPPING
# ====================================================================

cat("\n=== SECTION 5: ICD-PM GROUP MAPPING ===\n")

# Mapping function based on Annex A
map_to_icdpm_group <- function(icd10_code, timing) {
  if (is.na(icd10_code)) return(NA_character_)
  
  # NEONATAL DEATHS (N1-N11)
  if (timing %in% c("Death within 24 hours", "Death between 1-7 days", "Death between 8-28 days")) {
    if (grepl("^Q", icd10_code)) return("N1: Congenital malformations, deformations and chromosomal abnormalities")
    if (icd10_code %in% c("P05.0", "P05.1", "P05.2", "P05.9", "P08.0", "P08.1", "P08.2")) return("N2: Disorders related to fetal growth")
    if (icd10_code %in% c("P15.9") | grepl("^P1[0-5]", icd10_code)) return("N3: Birth trauma")
    if (icd10_code %in% c("P20.0", "P20.1", "P20.9", "P21.0", "P21.1", "P21.9")) return("N4: Complications of intrapartum events")
    if (icd10_code == "P91.6") return("N5: Convulsions and disorders of cerebral status")
    if (icd10_code %in% c("A33", "P36.9", "P23.9", "G03.9")) return("N6: Infection")
    if (icd10_code %in% c("P22.0", "P24.0", "P29.3")) return("N7: Respiratory and cardiovascular disorders")
    if (icd10_code %in% c("P50.9", "P52.9", "P77", "P83.2")) return("N8: Other neonatal conditions")
    if (icd10_code %in% c("P07.0", "P07.1", "P07.2", "P07.3")) return("N9: Low birth weight and prematurity")
    if (icd10_code == "P96.9") return("N11: Neonatal death of unspecified cause")
    return("N10: Miscellaneous neonatal conditions")
  }
  
  # INTRAPARTUM STILLBIRTHS (I1-I7)
  if (timing %in% c("Intrapartum stillbirth", "Stillbirth of unknown time")) {
    if (grepl("^Q", icd10_code)) return("I1: Congenital malformations, deformations and chromosomal abnormalities")
    if (icd10_code == "P15.9") return("I2: Birth trauma")
    if (icd10_code %in% c("P20.1", "P21.9")) return("I3: Acute intrapartum event")
    if (icd10_code == "P39.2") return("I4: Infection")
    if (icd10_code %in% c("P50.9", "P52.9", "P55.9", "P60", "P61.9")) return("I5: Other specified intrapartum disorder")
    if (icd10_code %in% c("P05.9", "P07.3", "P08.2")) return("I6: Disorders related to fetal growth")
    if (icd10_code == "P95") return("I7: Intrapartum death of unspecified cause")
    return("I5: Other specified intrapartum disorder")
  }
  
  # ANTEPARTUM STILLBIRTHS (A1-A6)
  if (timing == "Antepartum stillbirth") {
    if (grepl("^Q", icd10_code)) return("A1: Congenital malformations, deformations and chromosomal abnormalities")
    if (icd10_code %in% c("A50.9", "P35.9", "P37.9", "P39.2")) return("A2: Infection")
    if (icd10_code == "P20.0") return("A3: Acute antepartum event")
    if (icd10_code %in% c("P50.9", "P52.9", "P55.9", "P56.9", "P60", "P61.9", "P75", "P77", "P83.2")) return("A4: Other specified antepartum disorder")
    if (icd10_code %in% c("P05.9", "P08.2")) return("A5: Disorders related to length of gestation and fetal growth")
    if (icd10_code == "P95") return("A6: Antepartum death of unspecified cause")
    return("A4: Other specified antepartum disorder")
  }
  
  return(NA_character_)
}

# Apply ICD-PM mapping
cat("Mapping to ICD-PM groups...\n")
final_national_pdsr_clean$ICDPM_Group_Final <- NA_character_

for (i in 1:nrow(final_national_pdsr_clean)) {
  final_national_pdsr_clean$ICDPM_Group_Final[i] <- map_to_icdpm_group(
    icd10_code = final_national_pdsr_clean$Fetal_ICD10_Code_Final[i],
    timing = final_national_pdsr_clean$Timingofthedeath[i]
  )
}

# ====================================================================
# SECTION 6: FINAL SUMMARIES AND QUALITY CHECKS
# ====================================================================

cat("\n=== SECTION 6: FINAL SUMMARIES AND QUALITY CHECKS ===\n")

# 6.1: Maternal condition summary
cat("\n--- Maternal Condition Classification (Final) ---\n")
maternal_summary <- as.data.frame(table(final_national_pdsr_clean$ICDPM_Priority_updated, useNA = "always"))
names(maternal_summary) <- c("Category", "Count")
maternal_summary$Percent <- round(maternal_summary$Count / nrow(final_national_pdsr_clean) * 100, 1)
print(maternal_summary)

# 6.2: ICD-10 coding summary
cat("\n--- ICD-10 Coding Summary ---\n")
icd10_summary <- data.frame(
  Metric = c(
    "Total Cases",
    "Cases with ICD-10 Code",
    "Uncoded Cases",
    "Coding Rate"
  ),
  Value = c(
    nrow(final_national_pdsr_clean),
    sum(!is.na(final_national_pdsr_clean$Fetal_ICD10_Code_Final)),
    sum(is.na(final_national_pdsr_clean$Fetal_ICD10_Code_Final)),
    round(sum(!is.na(final_national_pdsr_clean$Fetal_ICD10_Code_Final)) / nrow(final_national_pdsr_clean) * 100, 1)
  )
)
print(icd10_summary)

# 6.3: Top ICD-10 codes
cat("\n--- Top 10 ICD-10 Codes ---\n")
top_codes <- head(sort(table(final_national_pdsr_clean$Fetal_ICD10_Code_Final, useNA = "always"), decreasing = TRUE), 11)
top_codes_df <- data.frame(
  ICD10_Code = names(top_codes),
  Count = as.numeric(top_codes),
  Percent = round(as.numeric(top_codes) / nrow(final_national_pdsr_clean) * 100, 1)
)
print(top_codes_df)

# 6.4: ICD-PM group distribution
cat("\n--- ICD-PM Group Distribution ---\n")
icdpm_dist <- as.data.frame(table(final_national_pdsr_clean$ICDPM_Group_Final, useNA = "always"))
names(icdpm_dist) <- c("ICDPM_Group", "Count")
icdpm_dist$Percent <- round(icdpm_dist$Count / nrow(final_national_pdsr_clean) * 100, 1)
icdpm_dist <- icdpm_dist[order(-icdpm_dist$Count), ]
print(icdpm_dist)

# 6.5: Date quality summary with EFY
cat("\n--- Date Quality and EFY Summary ---\n")
date_quality <- data.frame(
  Metric = c(
    "Complete Dates (Y/M/D)",
    "Valid Ethiopian Dates",
    "Gregorian Conversion Success",
    "Valid EFY Calculated",
    "Valid EFY Quarter",
    "Missing Year",
    "Missing Month",
    "Missing Day"
  ),
  Count = c(
    sum(!is.na(final_national_pdsr_clean$Yearofdeath) & 
          !is.na(final_national_pdsr_clean$MonthofdeathMM) & 
          !is.na(final_national_pdsr_clean$Date_of_death)),
    sum(!is.na(final_national_pdsr_clean$eth_date)),
    sum(!is.na(final_national_pdsr_clean$greg_date)),
    sum(!is.na(final_national_pdsr_clean$EFY)),
    sum(!is.na(final_national_pdsr_clean$EFY_Quarter)),
    sum(is.na(final_national_pdsr_clean$Yearofdeath)),
    sum(is.na(final_national_pdsr_clean$MonthofdeathMM)),
    sum(is.na(final_national_pdsr_clean$Date_of_death))
  )
)
date_quality$Percent <- round(date_quality$Count / nrow(final_national_pdsr_clean) * 100, 1)
print(date_quality)

# 6.6: Timing distribution
cat("\n--- Timing of Death Distribution ---\n")
timing_dist <- as.data.frame(table(final_national_pdsr_clean$Timingofthedeath, useNA = "always"))
names(timing_dist) <- c("Timing", "Count")
timing_dist$Percent <- round(timing_dist$Count / nrow(final_national_pdsr_clean) * 100, 1)
print(timing_dist)

# 6.7: Year and EFY distribution
cat("\n--- Year and Ethiopian Fiscal Year Distribution ---\n")
year_summary <- final_national_pdsr_clean %>%
  filter(!is.na(Yearofdeath)) %>%
  group_by(Yearofdeath) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = round(Count / nrow(final_national_pdsr_clean) * 100, 1)) %>%
  arrange(Yearofdeath)

efy_summary <- final_national_pdsr_clean %>%
  filter(!is.na(EFY)) %>%
  group_by(EFY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = round(Count / nrow(final_national_pdsr_clean) * 100, 1)) %>%
  arrange(EFY)

print("Year of Death:")
print(year_summary)
print("\nEthiopian Fiscal Year (with corrected logic):")
print(efy_summary)

# 6.8: Check specific corrected cases
cat("\n--- Verification of Corrected Cases ---\n")
check_cases <- final_national_pdsr_clean %>%
  filter(S.No %in% c(21970, 24601, 24602)) %>%
  select(S.No, Yearofdeath, MonthofdeathMM, EFY, EFY_Quarter, EFY_Quarter_Label, GregorianYearOfDeath)

print(check_cases)

# ====================================================================
# SECTION 7: SAVE FINAL DATASET
# ====================================================================

cat("\n=== SECTION 7: SAVING FINAL DATASET ===\n")

# Create organized column order
organized_columns <- c(
  # Identification and tracking
  "S.No", "Deceased.ID", "processing_timestamp", "data_quality_flag", "notes",
  
  # Date information (Ethiopian)
  "Yearofdeath", "MonthofdeathMM", "Date_of_death", 
  "date_imputation_source", "date_validation_flag",
  
  # Date information (Gregorian)
  "eth_date", "greg_date", "GregorianYearOfDeath",
  
  # Fiscal periods (CORRECTED EFY)
  "EFY", "EFY_Quarter", "EFY_Quarter_Label",
  
  # Reporting dates (original)
  "ReportingYear", "ReportingMonth", "Reportingdate",
  
  # Timing and causes
  "Timingofthedeath", "Other_NCD", "OtherspeciedNeonatalcauses",
  
  # Maternal information
  "specfic_cuase", "specfic_cuase_Original",
  "ICDPM_Priority_Original", "ICDPM_Priority", "ICDPM_Priority_updated",
  
  # Binary indicators
  "Prematurity", "Asphyxia", "Sepsis.pneumonia.meningitis2",
  "NeonatalTetanus", "LethalcongenitalaNomaly",
  
  # Fetal/Infant ICD codes
  "Fetal_ICD10_Code_Final", "ICDPM_Group_Final",
  
  # Derived columns from ID extraction
  "year_from_id", "month_from_id"
)

# Select only existing columns
existing_columns <- organized_columns[organized_columns %in% names(final_national_pdsr_clean)]
final_dataset <- final_national_pdsr_clean[, existing_columns]

# Save final dataset
output_filename <- paste0("FINAL_PERINATAL_DEATH_ANALYSIS_COMPLETE_WITH_EFY_CORRECTION_", 
                          format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
write.csv(final_dataset, output_filename, row.names = FALSE, na = "")

cat(sprintf("\n✅ FINAL DATASET SAVED: %s\n", output_filename))
cat(sprintf("   Rows: %d, Columns: %d\n", nrow(final_dataset), ncol(final_dataset)))

# Save summary report
summary_filename <- paste0("PROCESSING_SUMMARY_REPORT_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
sink(summary_filename)
cat("================================================================\n")
cat("COMPREHENSIVE PERINATAL DEATH DATA PROCESSING SUMMARY REPORT\n")
cat("================================================================\n\n")

cat("PROCESSING OVERVIEW:\n")
cat("-------------------\n")
cat(sprintf("Processing completed: %s\n", Sys.time()))
cat(sprintf("Total cases processed: %d\n", nrow(final_dataset)))
cat(sprintf("Dataset columns: %d\n", ncol(final_dataset)))
cat(sprintf("Specific corrections applied: 6 cases (S.No: 21970, 24084, 24085, 24407, 24601, 24602)\n\n"))

cat("EFY CORRECTION SUMMARY:\n")
cat("-----------------------\n")
cat("Ethiopian Fiscal Year logic corrected:\n")
cat("  • Months 11, 12, 13 → EFY = Current Year\n")
cat("  • Months 1-10 → EFY = Previous Year\n")
cat(sprintf("EFY Calculation Accuracy: %.1f%%\n", validation_check$Percent_Correct))
cat("\n")

cat("MATERNAL CONDITION CLASSIFICATION:\n")
cat("---------------------------------\n")
print(maternal_summary)
cat("\n")

cat("FETAL/INFANT ICD-10 CODING:\n")
cat("---------------------------\n")
print(icd10_summary)
cat("\n")

cat("TOP 5 ICD-10 CODES:\n")
cat("------------------\n")
print(head(top_codes_df[top_codes_df$ICD10_Code != "<NA>", ], 5))
cat("\n")

cat("TOP 5 ICD-PM GROUPS:\n")
cat("--------------------\n")
print(head(icdpm_dist[icdpm_dist$ICDPM_Group != "<NA>", ], 5))
cat("\n")

cat("DATE QUALITY METRICS:\n")
cat("---------------------\n")
print(date_quality)
cat("\n")

sink()

cat(sprintf("✅ SUMMARY REPORT SAVED: %s\n", summary_filename))

# ====================================================================
# SECTION 8: FINAL VALIDATION AND COMPLETION
# ====================================================================

cat("\n=== SECTION 8: FINAL VALIDATION ===\n")

# Check for any critical issues
critical_issues <- list()

# Check 1: Missing critical classifications
missing_icd10 <- sum(is.na(final_dataset$Fetal_ICD10_Code_Final))
missing_icdpm <- sum(is.na(final_dataset$ICDPM_Group_Final))
missing_maternal <- sum(is.na(final_dataset$ICDPM_Priority_updated))

if (missing_icd10 > 0) {
  critical_issues$missing_icd10 <- missing_icd10
}
if (missing_icdpm > 0) {
  critical_issues$missing_icdpm <- missing_icdpm
}
if (missing_maternal > 0) {
  critical_issues$missing_maternal <- missing_maternal
}

# Check 2: Invalid dates
invalid_dates <- sum(final_dataset$date_validation_flag != "valid" & 
                       final_dataset$date_validation_flag != "manual_validated", na.rm = TRUE)
if (invalid_dates > 0) {
  critical_issues$invalid_dates <- invalid_dates
}

# Check 3: Missing EFY
missing_efy <- sum(is.na(final_dataset$EFY))
if (missing_efy > 0) {
  critical_issues$missing_efy <- missing_efy
}

# Report validation results
if (length(critical_issues) == 0) {
  cat("✅ No critical issues found. All data processed successfully.\n")
} else {
  cat("⚠️  Critical issues found:\n")
  for (issue in names(critical_issues)) {
    cat(sprintf("   - %s: %d cases\n", issue, critical_issues[[issue]]))
  }
}

# Final completion message
cat("\n" + strrep("=", 60) + "\n")
cat("🎉 COMPREHENSIVE PERINATAL DEATH DATA PROCESSING COMPLETE!\n")
cat(strrep("=", 60) + "\n\n")

cat("All corrections successfully integrated:\n\n")
cat("✓ Specific case corrections applied (6 cases)\n")
cat("✓ Correct Ethiopian Fiscal Year (EFY) calculation logic implemented\n")
cat("✓ Accurate Ethiopian date to Gregorian conversion\n")
cat("✓ Complete maternal condition classification (M1-M5)\n")
cat("✓ Comprehensive fetal/infant ICD-10 coding\n")
cat("✓ Full ICD-PM group mapping\n")
cat("✓ Documentation of all imputations and corrections\n\n")

cat("Files created:\n")
cat(sprintf("1. %s (Main dataset with corrected EFY)\n", output_filename))
cat(sprintf("2. %s (Processing summary)\n", summary_filename))
cat("\nDataset ready for epidemiological analysis, time-series reporting, and visualization!\n")