# ==============================================================================
# MATERNAL DEATH CAUSE CLASSIFICATION & STANDARDIZATION
# Data: Ethiopia MDSR Dataset
# Purpose: Clean, standardize, and classify causes of maternal death into 9 groups
# ==============================================================================

# LIBRARIES --------------------------------------------------------------------
library(dplyr)
library(stringr)

# 1. TEXT CLEANING & STANDARDIZATION -------------------------------------------
# ------------------------------------------------------------------------------
cat("Step 1: Cleaning and standardizing indirect cause text data...\n")

# Clean and standardize the IndirectCausesotherSpecified variable
ethio_mdsr_cleaned_with_imputed <- ethio_mdsr_cleaned_with_imputed %>%
  mutate(
    # Convert to lowercase and trim whitespace
    IndirectCauses_clean = str_trim(tolower(IndirectCausesotherSpecified))
  )

cat("  ✓ Text cleaning completed\n")

# 2. ICD-10 BASED MAPPING TO 9 GROUPS ------------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 2: Mapping to ICD-10 categories and 9-group classification...\n")

# First map to detailed ICD-10 categories
ethio_mdsr_cleaned_with_imputed <- ethio_mdsr_cleaned_with_imputed %>%
  mutate(
    # Create ICD-10 category mapping
    ICD10_Indirect = case_when(
      # Pregnancy-related infections (Group 4)
      str_detect(IndirectCauses_clean, "sepsis|infection|pneumonia|meningitis|tb|hiv|malaria|hepatitis|covid|anthrax|measles|typhoid|dengue|rabies") ~ 
        "O23/O85/O86 - Pregnancy-related infection",
      
      # Amniotic fluid embolism (Group 5)
      str_detect(IndirectCauses_clean, "amniotic fluid|af embolism|a\\.f embolism|afe|af|a\\.f|amnniotic|amnotic") ~ 
        "O88.1 - Amniotic fluid embolism",
      
      # Pulmonary embolism (Group 5)
      str_detect(IndirectCauses_clean, "pulmonary embolism|pte|thromboembolism|plumonary embolism|dvt|deep vein|thrombo") ~ 
        "I26 - Pulmonary embolism",
      
      # Other embolism (Group 5)
      str_detect(IndirectCauses_clean, "embolism|emboliz|embolish|imbolism") ~ 
        "O88 - Other obstetric embolism",
      
      # Shock (Group 5)
      str_detect(IndirectCauses_clean, "shock|hypovolemic|cardiogenic|septic shock|anaphylactic") ~ 
        "R57 - Shock",
      
      # Multi-organ failure (Group 5)
      str_detect(IndirectCauses_clean, "multi organ failure|mof|organ failure|multiple organ") ~ 
        "R68.8 - Multi-organ failure",
      
      # Coagulation disorders (Group 5)
      str_detect(IndirectCauses_clean, "dic|coagulation|thrombocytopenia|thrombocytopnea|pancytopenia") ~ 
        "O99.1 - Coagulation disorders",
      
      # Complications of anesthesia/management (Group 6)
      str_detect(IndirectCauses_clean, "anesthesia|anesthetic|spinal|intubation|surgical|operation|post\\.anesthesia|post\\.operative|failed intubation") ~ 
        "O74/O75 - Complications of anesthesia/management",
      
      # Cardiovascular diseases (Group 7)
      str_detect(IndirectCauses_clean, "cardiac|heart|chf|cardiomyopathy|hypertension|htn|arrhythmia|cardiovascular|myocardial|endocarditis|pericarditis") ~ 
        "O99.4/I00-I99 - Cardiovascular diseases",
      
      # Respiratory diseases (Group 7)
      str_detect(IndirectCauses_clean, "respiratory|ards|asthma|pneumonia|pneumonitis|bronchitis|tuberculosis|tb|resp distress|copd") ~ 
        "O99.5/J00-J99 - Respiratory diseases",
      
      # Neurological/Psychiatric diseases (Group 7)
      str_detect(IndirectCauses_clean, "epilepsy|seizure|psychosis|mental|meningitis|encephalitis|stroke|cerebral|brain|neuro|convulsion|coma") ~ 
        "O99.3/G00-G99 - Neuro/psychiatric diseases",
      
      # Gastrointestinal/Hepatic diseases (Group 7)
      str_detect(IndirectCauses_clean, "hepatitis|liver|pancreatitis|gastrointestinal|git|abdominal|appendicitis|peritonitis|intestinal|bowel") ~ 
        "O99.6/K00-K93 - Digestive diseases",
      
      # Renal diseases (Group 7)
      str_detect(IndirectCauses_clean, "renal|kidney|aki|arf|renal failure|nephritis|pyelonephritis|uti|urinary") ~ 
        "O99.8/N00-N99 - Renal diseases",
      
      # Endocrine/Metabolic diseases (Group 7)
      str_detect(IndirectCauses_clean, "diabetes|dm|dka|thyroid|thyrotoxic|hypoglycemia|hypocalcemia|metabolic|goiter") ~ 
        "O99.2/E00-E90 - Endocrine/metabolic diseases",
      
      # Hematological diseases (Group 7)
      str_detect(IndirectCauses_clean, "anemia|leukemia|lymphoma|hematologic|blood disorder|sickle cell") ~ 
        "O99.0/D50-D89 - Hematological diseases",
      
      # Neoplasms (Group 7)
      str_detect(IndirectCauses_clean, "cancer|malignancy|tumor|carcinoma|lymphoma|leukemia|sarcoma") ~ 
        "C00-D48 - Neoplasms",
      
      # Infectious diseases (Group 7)
      str_detect(IndirectCauses_clean, "infection|sepsis|bacteremia|viremia|fungemia") ~ 
        "A00-B99 - Infectious diseases",
      
      # External causes (Group 9)
      str_detect(IndirectCauses_clean, "accident|trauma|injury|snake bite|poisoning|suicide|suicidal|self\\.harm|violence|assault|homicide") ~ 
        "V01-Y98 - External causes",
      
      # Unknown/Missing values
      str_detect(IndirectCauses_clean, "^no$|^no |unknown|not specified|not assigned|unexplained|missing|not known|unspecified") ~ 
        "Unknown",
      
      is.na(IndirectCausesotherSpecified) | IndirectCausesotherSpecified == "" ~ "Missing",
      
      # Default for unspecified cases
      TRUE ~ "Other specified"
    ),
    
    # Map ICD-10 categories to 9-group classification
    Group_9_from_Indirect = case_when(
      str_detect(ICD10_Indirect, "O23/O85/O86") ~ "Group 4: Pregnancy-related infection",
      str_detect(ICD10_Indirect, "O88|I26|R57|R68.8|O99.1") ~ "Group 5: Other obstetric complications",
      str_detect(ICD10_Indirect, "O74/O75") ~ "Group 6: Unanticipated complications of management",
      str_detect(ICD10_Indirect, "O99.0|O99.2|O99.3|O99.4|O99.5|O99.6|O99.8|C00-D48|A00-B99|D50-D89|E00-E90|G00-G99|I00-I99|J00-J99|K00-K93|N00-N99") ~ 
        "Group 7: Non-obstetric complications",
      str_detect(ICD10_Indirect, "V01-Y98") ~ "Group 9: Coincidental causes",
      ICD10_Indirect %in% c("Unknown", "Missing") ~ "Group 8: Unknown/undetermined",
      TRUE ~ "Group 8: Unknown/undetermined"
    )
  )

cat("  ✓ ICD-10 mapping completed\n")

# 3. VALIDATE MAPPING RESULTS --------------------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 3: Validating mapping results...\n")

# Check distribution of 9-group classification
group_distribution <- table(ethio_mdsr_cleaned_with_imputed$Group_9_from_Indirect, useNA = "always")
print(group_distribution)

# 4. BINARY VARIABLE DISTRIBUTION ANALYSIS -------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 4: Analyzing binary variable distributions...\n")

binary_variables <- c(
  "DO1hemorrhage", 
  "DO2Obstructedlabor", 
  "DO3HDP",
  "DO4Abortion", 
  "DO5sepsis",
  "INDC1Anemia", 
  "IND2Malaria", 
  "IND3HIV", 
  "IND4TB"
)

cat("Binary variable analysis:\n")
cat("-------------------------\n")

binary_summary <- data.frame(
  Variable = character(),
  Yes_Count = integer(),
  Yes_Percentage = numeric(),
  Total_Observations = integer(),
  stringsAsFactors = FALSE
)

for (var in binary_variables) {
  # Clean and standardize variable
  var_values <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed[[var]])))
  
  # Count "Yes" responses (including variations)
  yes_count <- sum(str_detect(var_values, "^yes|^y$|^1$"), na.rm = TRUE)
  total_count <- nrow(ethio_mdsr_cleaned_with_imputed)
  yes_percentage <- round(yes_count / total_count * 100, 1)
  
  # Store summary
  binary_summary <- rbind(binary_summary, data.frame(
    Variable = var,
    Yes_Count = yes_count,
    Yes_Percentage = yes_percentage,
    Total_Observations = total_count
  ))
  
  # Print summary
  cat(sprintf("%-20s: %4d (%.1f%%)\n", var, yes_count, yes_percentage))
}

# 5. FINAL 9-GROUP CLASSIFICATION ----------------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 5: Creating final 9-group classification...\n")

ethio_mdsr_cleaned_with_imputed <- ethio_mdsr_cleaned_with_imputed %>%
  mutate(
    Nine_Group_Final = NA_character_  # Initialize with missing values
  )

# Apply classification logic
for(i in seq_len(nrow(ethio_mdsr_cleaned_with_imputed))) {
  
  # Extract and clean binary variables
  hemorrhage <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$DO1hemorrhage[i])))
  obstructed_labor <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$DO2Obstructedlabor[i])))
  hypertensive <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$DO3HDP[i])))
  abortion <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$DO4Abortion[i])))
  sepsis <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$DO5sepsis[i])))
  malaria <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$IND2Malaria[i])))
  hiv <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$IND3HIV[i])))
  tuberculosis <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$IND4TB[i])))
  anemia <- str_trim(tolower(as.character(ethio_mdsr_cleaned_with_imputed$INDC1Anemia[i])))
  
  # Priority-based classification
  if (!is.na(hemorrhage) && str_detect(hemorrhage, "^yes|^y$|^1$")) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 1: Hemorrhage"
  } 
  else if (!is.na(hypertensive) && str_detect(hypertensive, "^yes|^y$|^1$")) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 3: Hypertensive disorders"
  } 
  else if (!is.na(sepsis) && str_detect(sepsis, "^yes|^y$|^1$")) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 4: Pregnancy-related infection"
  } 
  else if (!is.na(abortion) && str_detect(abortion, "^yes|^y$|^1$")) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 2: Pregnancy with abortive outcome"
  } 
  else if (!is.na(obstructed_labor) && str_detect(obstructed_labor, "^yes|^y$|^1$")) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 5: Other obstetric complications"
  } 
  else if ((!is.na(malaria) && str_detect(malaria, "^yes|^y$|^1$")) ||
           (!is.na(hiv) && str_detect(hiv, "^yes|^y$|^1$")) ||
           (!is.na(tuberculosis) && str_detect(tuberculosis, "^yes|^y$|^1$"))) {
    ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 7: Non-obstetric complications"
  } 
  else {
    # Use indirect cause classification as fallback
    indirect_group <- as.character(ethio_mdsr_cleaned_with_imputed$Group_9_from_Indirect[i])
    
    if (!is.na(indirect_group) && indirect_group != "") {
      ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- indirect_group
    } else {
      # Default classification
      ethio_mdsr_cleaned_with_imputed$Nine_Group_Final[i] <- "Group 1: Hemorrhage"
    }
  }
}

# 6. CAUSE TYPE CLASSIFICATION -------------------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 6: Classifying cause types (Direct/Indirect/Unspecified)...\n")

ethio_mdsr_cleaned_with_imputed <- ethio_mdsr_cleaned_with_imputed %>%
  mutate(
    Cause_Type = case_when(
      Nine_Group_Final %in% c(
        "Group 1: Hemorrhage",
        "Group 2: Pregnancy with abortive outcome",
        "Group 3: Hypertensive disorders",
        "Group 4: Pregnancy-related infection",
        "Group 5: Other obstetric complications",
        "Group 6: Unanticipated complications of management"
      ) ~ "Direct",
      
      Nine_Group_Final %in% c(
        "Group 7: Non-obstetric complications"
      ) ~ "Indirect",
      
      Nine_Group_Final %in% c(
        "Group 8: Unknown/undetermined",
        "Group 9: Coincidental causes"
      ) ~ "Unspecified",
      
      TRUE ~ NA_character_
    )
  )

# 7. FINAL RESULTS & SUMMARY ---------------------------------------------------
# ------------------------------------------------------------------------------
cat("\nStep 7: Generating final summary statistics...\n")

# Final 9-group distribution
cat("\nFinal 9-Group Classification Distribution:\n")
cat("------------------------------------------\n")
final_group_table <- table(ethio_mdsr_cleaned_with_imputed$Nine_Group_Final, useNA = "always")
print(final_group_table)

# Cause type distribution
cat("\nCause Type Distribution:\n")
cat("-------------------------\n")
cause_type_table <- table(ethio_mdsr_cleaned_with_imputed$Cause_Type, useNA = "always")
print(cause_type_table)

# Proportions with percentages
cat("\nCause Type Proportions (%):\n")
cat("---------------------------\n")
cause_type_proportions <- prop.table(table(ethio_mdsr_cleaned_with_imputed$Cause_Type)) * 100
cause_type_proportions <- round(cause_type_proportions, 1)
print(cause_type_proportions)

# Create comprehensive summary data frame
summary_df <- data.frame(
  Cause_Type = names(cause_type_table),
  Count = as.numeric(cause_type_table),
  Percentage = round(as.numeric(cause_type_table) / nrow(ethio_mdsr_cleaned_with_imputed) * 100, 1)
)

cat("\n=== CLASSIFICATION COMPLETED SUCCESSFULLY ===\n")
cat(sprintf("Total observations processed: %d\n", nrow(ethio_mdsr_cleaned_with_imputed)))