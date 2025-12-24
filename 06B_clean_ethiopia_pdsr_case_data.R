# ====================================================
# PERINATAL MORTALITY DATA CLEANING - COMPLETE WORKFLOW
# ====================================================

library(dplyr)
library(stringr)

# Load and prepare data
national_pdsr <- `01_ethio_PDSR_cleaned`

cat("=== INITIAL DATA CHECK ===\n")
cat("Total cases:", nrow(national_pdsr), "\n")
cat("Variables:", ncol(national_pdsr), "\n\n")

# ========== PART 1: CLEANING BASIC VARIABLES ==========

cat("1. Cleaning APGAR scores...\n")
national_pdsr <- national_pdsr %>%
  mutate(
    apgar5_clean = case_when(
      is.na(ifaliveAPGARscoreat5thminute) ~ NA_real_,
      ifaliveAPGARscoreat5thminute < 0 ~ NA_real_,
      ifaliveAPGARscoreat5thminute >= 10 ~ 
        as.numeric(substr(as.character(ifaliveAPGARscoreat5thminute), 1, 1)),
      TRUE ~ floor(ifaliveAPGARscoreat5thminute)
    )
  )

cat("2. Cleaning baby birth status...\n")
national_pdsr <- national_pdsr %>%
  mutate(
    Statusofthebabyatbirth = case_when(
      str_to_lower(Statusofthebabyatbirth) %in% c("alive birth", "alivebirth", "alive") ~ "Alive birth",
      str_to_lower(Statusofthebabyatbirth) %in% c("stillbirth", "still birth") ~ "Stillbirth",
      is.na(Statusofthebabyatbirth) & ifaliveAPGARscoreat5thminute >= 1 ~ "Alive birth",
      is.na(Statusofthebabyatbirth) & (ifaliveAPGARscoreat5thminute < 1 | is.na(ifaliveAPGARscoreat5thminute)) ~ "Stillbirth",
      TRUE ~ NA_character_
    )
  )

cat("3. Cleaning place of birth...\n")
national_pdsr <- national_pdsr %>%
  mutate(
    Wherewasthedeceasedbabyborn = case_when(
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("hospital", "hospiatl") ~ "Hospital",
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("health center", "health_center", "health_ center",
                                                               "hc", "h/center", "helth center") ~ "Health Center",
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("health post", "health_post") ~ "Health Post",
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("clinic") ~ "Clinic",
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("home") ~ "Home",
      str_to_lower(trimws(Wherewasthedeceasedbabyborn)) %in% c("on transit", "ontransit") ~ "On transit",
      is.na(trimws(Wherewasthedeceasedbabyborn)) | str_to_lower(trimws(Wherewasthedeceasedbabyborn)) == "unknown" ~ 
        case_when(
          str_detect(str_to_lower(Reportinghealthfacilitytype), "hospital") ~ "Hospital",
          str_detect(str_to_lower(Reportinghealthfacilitytype), "health center") ~ "Health Center",
          TRUE ~ "NGO Facility"
        ),
      TRUE ~ "Unknown"
    )
  )

# ========== PART 2: CLEANING CAUSE OF DEATH VARIABLES ==========

cat("4. Cleaning binary cause variables...\n")
cause_vars <- c("Prematurity", "Asphyxia", "Sepsis.pneumonia.meningitis2", 
                "NeonatalTetanus", "LethalcongenitalaNomaly")

national_pdsr <- national_pdsr %>%
  mutate(across(all_of(cause_vars), ~ {
    var_clean <- str_to_upper(trimws(.))
    case_when(
      var_clean %in% c("YES", "Y", "LCA") ~ "Yes",
      var_clean %in% c("NO", "N0", "N") ~ "No",
      var_clean %in% c("NOT", "UNKNOWN", "UNK", NA_character_) ~ "No",
      TRUE ~ var_clean
    )
  }))

cat("5. Cleaning maternal condition variables...\n")
maternal_vars <- c("Obstructedlabor", "RupturedUterus", "PreeclampsiaEclampsia", 
                   "APHPlacentaPreviaorabruption", "ObstetricSepsis")

# Clean Obstructedlabor and move unusual entries to OtherspeciedNeonatalcauses
national_pdsr <- national_pdsr %>%
  mutate(
    Obstructedlabor = case_when(
      str_to_lower(Obstructedlabor) %in% c("yes", "y") ~ "Yes",
      str_to_lower(Obstructedlabor) %in% c("no", "n") ~ "No",
      TRUE ~ NA_character_
    ),
    OtherspeciedNeonatalcauses = case_when(
      str_to_lower(Obstructedlabor) %in% c("chd") ~ "Congenital Heart Disease",
      str_to_lower(Obstructedlabor) %in% c("uremic +dhn") ~ "Hypoxic Ischaemic Encephalopathy",
      str_to_lower(Obstructedlabor) %in% c("hsd") ~ "P50 Fetal blood loss",
      str_to_lower(Obstructedlabor) %in% c("dic down syndrom") ~ "Down Syndrome with DIC",
      str_to_lower(Obstructedlabor) %in% c("sever anemia") ~ "Severe Anemia",
      str_to_lower(Obstructedlabor) %in% c("tef") ~ "Tracheoesophageal Fistula",
      str_to_lower(Obstructedlabor) %in% c("rds") ~ "Respiratory Distress Syndrome",
      str_to_lower(Obstructedlabor) %in% c("missed", "other") ~ "Other",
      TRUE ~ OtherspeciedNeonatalcauses
    )
  )

# Clean other maternal variables
other_maternal_vars <- c("RupturedUterus", "PreeclampsiaEclampsia", 
                         "APHPlacentaPreviaorabruption", "ObstetricSepsis")

national_pdsr <- national_pdsr %>%
  mutate(across(all_of(other_maternal_vars), ~ {
    x <- str_to_lower(trimws(.))
    case_when(
      x %in% c("yes") ~ "Yes",
      x %in% c("no", "missed", "on", "unknwon", "kes ephrem", "n", "nn") ~ "No",
      TRUE ~ NA_character_
    )
  }))

# ========== PART 3: CLEANING OUTCOME & TIMING VARIABLES ==========

cat("6. Cleaning preventability and timing variables...\n")
national_pdsr <- national_pdsr %>%
  mutate(
    Isthedeathpreventable = case_when(
      str_to_lower(trimws(Isthedeathpreventable)) %in% c("yes", "y", "ye") ~ "Yes",
      str_to_lower(trimws(Isthedeathpreventable)) %in% c("no", "n", "0") ~ "No",
      str_to_lower(trimws(Isthedeathpreventable)) %in% c("unknown", "un known", "uknown", "missed", "missing") ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    Timingofthedeath = case_when(
      str_detect(str_to_lower(trimws(Timingofthedeath)), "antepartum") ~ "Antepartum stillbirth",
      str_detect(str_to_lower(trimws(Timingofthedeath)), "intrapartum") ~ "Intrapartum stillbirth",
      str_detect(str_to_lower(trimws(Timingofthedeath)), "first 24") ~ "Death within 24 hours",
      str_detect(str_to_lower(trimws(Timingofthedeath)), "1st.*7") ~ "Death between 1-7 days",
      str_detect(str_to_lower(trimws(Timingofthedeath)), "8.*28") ~ "Death between 8-28 days",
      str_detect(str_to_lower(trimws(Timingofthedeath)), "stillbirth.*unknown|unknown time") ~ "Stillbirth of unknown time",
      str_to_lower(trimws(Timingofthedeath)) %in% c("missed", "missing") ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

cat("7. Imputing missing timing data...\n")
# Calculate proportions for imputation
stillbirth_props <- national_pdsr %>%
  filter(Statusofthebabyatbirth == "Stillbirth", !is.na(Timingofthedeath)) %>%
  count(Timingofthedeath) %>%
  mutate(prop = n / sum(n))

alive_props <- national_pdsr %>%
  filter(Statusofthebabyatbirth == "Alive birth", !is.na(Timingofthedeath)) %>%
  count(Timingofthedeath) %>%
  mutate(prop = n / sum(n))

# Impute missing values
set.seed(123)
idx_na <- which(is.na(national_pdsr$Timingofthedeath))

national_pdsr$Timingofthedeath[idx_na] <- sapply(idx_na, function(i) {
  status <- national_pdsr$Statusofthebabyatbirth[i]
  
  if(status == "Stillbirth") {
    sample(stillbirth_props$Timingofthedeath, size = 1, prob = stillbirth_props$prop)
  } else if(status == "Alive birth") {
    sample(alive_props$Timingofthedeath, size = 1, prob = alive_props$prop)
  } else {
    NA_character_
  }
})

# ========== PART 4: CLEANING DELAY VARIABLES ==========

cat("8. Cleaning Delay 1 variables (household level)...\n")
D1_vars <- c("Familypoverty.D1", "DidNotrecognizethedangersignsofnewborninfants.D1",
             "Unawareofthewarningsignsofproblemsduringpregnancy.D1", "DidNotkNowwheretogo.D1",
             "HadNoonetotakecareofotherchildren..D1", "Reliantontraditionalpracticemedicine.d1",
             "Lackofdecisiontogotothehealthfacility.D1")

national_pdsr <- national_pdsr %>%
  mutate(across(all_of(D1_vars), ~ case_when(
    str_to_lower(.) %in% c("yes", "y", "yes") ~ "Yes",
    str_to_lower(.) %in% c("no", "n", "n0", "nn") ~ "No",
    is.na(.) | str_detect(str_to_lower(.), "unk") ~ "No",
    TRUE ~ as.character(.)
  )))

cat("9. Cleaning Delay 2 variables (transport/access)...\n")
D2_vars <- c("TransportwasNotavailable.D2", "Transportwastooexpensive.d2",
             "Nofacilitywithinreasonabledistance.D2", "Lackofroadaccess.D2")

national_pdsr <- national_pdsr %>%
  mutate(across(all_of(D2_vars), ~ case_when(
    str_to_lower(.) %in% c("yes") ~ "Yes",
    str_to_lower(.) %in% c("no") ~ "No",
    str_detect(str_to_lower(.), "6-8 km|security|she tried") ~ "Yes",
    is.na(.) ~ "No",
    TRUE ~ "No"
  )))

cat("10. Cleaning Delay 3 variables (facility level)...\n")
D3_vars <- c("Familylackedmoneyforhealthcare.D3", "DelayedarrivaltonextfacilityfromaNotherreferringfacility.D3",
             "Delayedmanagementafteradmission.D3", "Feartobescoldedorshoutedatbythestaff.D3",
             "Humanerrorormismanagement.D3", "Delayinfirstevaluationbycaregiverafteradmission.D3",
             "Lackofsuppliesorequipment.D3")

# Extract detailed responses from Familylackedmoneyforhealthcare.D3
national_pdsr <- national_pdsr %>%
  mutate(
    DelayedarrivaltonextfacilityfromaNotherreferringfacility.D3 = ifelse(
      str_detect(Familylackedmoneyforhealthcare.D3, regex("Delayed arrival", ignore_case = TRUE)), "Yes",
      DelayedarrivaltonextfacilityfromaNotherreferringfacility.D3
    ),
    Delayedmanagementafteradmission.D3 = ifelse(
      str_detect(Familylackedmoneyforhealthcare.D3, regex("Delayed management", ignore_case = TRUE)), "Yes",
      Delayedmanagementafteradmission.D3
    ),
    Humanerrorormismanagement.D3 = ifelse(
      str_detect(Familylackedmoneyforhealthcare.D3, regex("Human error", ignore_case = TRUE)), "Yes",
      Humanerrorormismanagement.D3
    ),
    Lackofsuppliesorequipment.D3 = ifelse(
      str_detect(Familylackedmoneyforhealthcare.D3, regex("Lack of supplies", ignore_case = TRUE)), "Yes",
      Lackofsuppliesorequipment.D3
    )
  ) %>%
  mutate(across(all_of(D3_vars), ~ case_when(
    str_to_lower(.) %in% c("yes", "y") ~ "Yes",
    str_to_lower(.) %in% c("no") ~ "No",
    is.na(.) ~ "No",
    TRUE ~ "No"
  )))

# ========== PART 5: ICD-PM CLASSIFICATION ==========

cat("11. Applying ICD-PM classification with priority logic...\n")

# Clean text-based ICD-PM first
national_pdsr$MaternalCondition_clean <- tolower(trimws(national_pdsr$Maternaldiseaseorconditionidentified))
national_pdsr$MaternalCondition_clean <- gsub("[[:punct:]]", "", national_pdsr$MaternalCondition_clean)
national_pdsr$MaternalCondition_clean <- gsub("\\s+", " ", national_pdsr$MaternalCondition_clean)

national_pdsr$ICDPM_MaternalCondition <- "M5: No maternal condition"

# Define patterns for each category
m1_patterns <- c("placenta previa", "abruptio", "abruption", "cord prolaps", "cord prolapse", 
                 "chorioamnionitis", "amnionitis", "vasa previa")
m2_patterns <- c("multiple pregnancy", "twin", "triplet", "prom ", "pprom", "oligohydram", 
                 "polyhydram", "ectopic pregnancy")
m3_patterns <- c("prolonged labour", "obstructed labour", "uterine rupture", "breech delivery", 
                 "forceps", "caesarean", "c/s", "malpresentation during labour")
m4_patterns <- c("eclampsia", "preeclampsia", "hypertension", "htn", "pe ", "hdp", "anemia", 
                 "anaemia", "malaria", "hiv", "rvi", "sepsis", "diabetes", "dm")
no_condition_patterns <- c("^no$", "^no ", "no disease", "no condition", "nad", "normal", 
                           "healthy", "well", "stable", "none")

# Apply text-based classification
national_pdsr$ICDPM_MaternalCondition[grepl(paste(m1_patterns, collapse = "|"), 
                                            national_pdsr$MaternalCondition_clean)] <- "M1: Placenta/cord/membrane complications"
national_pdsr$ICDPM_MaternalCondition[grepl(paste(m2_patterns, collapse = "|"), 
                                            national_pdsr$MaternalCondition_clean)] <- "M2: Maternal pregnancy complications"
national_pdsr$ICDPM_MaternalCondition[grepl(paste(m3_patterns, collapse = "|"), 
                                            national_pdsr$MaternalCondition_clean)] <- "M3: Labour/delivery complications"
national_pdsr$ICDPM_MaternalCondition[grepl(paste(m4_patterns, collapse = "|"), 
                                            national_pdsr$MaternalCondition_clean)] <- "M4: Maternal medical conditions"
national_pdsr$ICDPM_MaternalCondition[grepl(paste(no_condition_patterns, collapse = "|"), 
                                            national_pdsr$MaternalCondition_clean)] <- "M5: No maternal condition"

# Now apply priority-based classification using binary variables
available_vars <- maternal_vars[maternal_vars %in% names(national_pdsr)]

# Create logical versions
for(var in available_vars) {
  logical_var <- paste0(var, "_logical")
  national_pdsr[[logical_var]] <- grepl("yes|YES|Yes", national_pdsr[[var]], ignore.case = TRUE)
}

# Priority-based classification
national_pdsr$ICDPM_Priority <- "M5: No maternal condition"

if("PreeclampsiaEclampsia_logical" %in% names(national_pdsr)) {
  national_pdsr$ICDPM_Priority[national_pdsr$PreeclampsiaEclampsia_logical] <- "M4: Maternal medical conditions"
}

if("APHPlacentaPreviaorabruption_logical" %in% names(national_pdsr)) {
  needs_update <- national_pdsr$ICDPM_Priority == "M5: No maternal condition" & 
    national_pdsr$APHPlacentaPreviaorabruption_logical
  national_pdsr$ICDPM_Priority[needs_update] <- "M1: Placenta/cord/membrane complications"
}

if("ObstetricSepsis_logical" %in% names(national_pdsr)) {
  needs_update <- national_pdsr$ICDPM_Priority == "M5: No maternal condition" & 
    national_pdsr$ObstetricSepsis_logical
  national_pdsr$ICDPM_Priority[needs_update] <- "M4: Maternal medical conditions"
}

if("RupturedUterus_logical" %in% names(national_pdsr)) {
  needs_update <- national_pdsr$ICDPM_Priority == "M5: No maternal condition" & 
    national_pdsr$RupturedUterus_logical
  national_pdsr$ICDPM_Priority[needs_update] <- "M3: Labour/delivery complications"
}

if("Obstructedlabor_logical" %in% names(national_pdsr)) {
  needs_update <- national_pdsr$ICDPM_Priority == "M5: No maternal condition" & 
    national_pdsr$Obstructedlabor_logical
  national_pdsr$ICDPM_Priority[needs_update] <- "M3: Labour/delivery complications"
}

# Update main variable with priority-based classification
national_pdsr$ICDPM_MaternalCondition <- national_pdsr$ICDPM_Priority

# Count conditions
national_pdsr$ConditionCount <- rowSums(
  sapply(paste0(available_vars, "_logical"), function(x) {
    if(x %in% names(national_pdsr)) as.numeric(national_pdsr[[x]]) else 0
  }), na.rm = TRUE
)

# ========== PART 6: GEOGRAPHIC VARIABLES ==========

cat("12. Cleaning geographic variables...\n")
national_pdsr <- national_pdsr %>%
  mutate(
    # Standardize ZoneName for Addis Ababa
    ZoneName = case_when(
      Region_clean == "Addis Ababa" & str_detect(ZoneName, regex("Region 14", ignore_case = TRUE)) ~ "Kolfe Keraniyo",
      Region_clean == "Addis Ababa" & str_detect(ZoneName, regex("Kolfe Keranyo", ignore_case = TRUE)) ~ "Kolfe Keraniyo",
      Region_clean == "Addis Ababa" & str_detect(ZoneName, regex("Nifas Silk", ignore_case = TRUE)) ~ "Nefas Silk Lafto",
      TRUE ~ ZoneName
    ),
    
    # Update zone_gis for GIS mapping
    zone_gis = if_else(Region_clean == "Addis Ababa", "Region 14", zone_gis),
    
    # Standardize zone_final for Addis Ababa
    zone_final = case_when(
      Region_clean == "Addis Ababa" & zone_final == "Region 14" ~ "Kolfe Keraniyo",
      TRUE ~ zone_final
    ),
    zone_final = str_to_title(zone_final),
    zone_final = str_replace_all(zone_final, "Akaki Kaliti|Akaki kality", "Akaki Kality"),
    zone_final = str_replace_all(zone_final, "Bole|bole", "Bole"),
    zone_final = str_replace_all(zone_final, "Kolfe Keraniyo|Kolfe Keranyo", "Kolfe Keraniyo"),
    zone_final = str_replace_all(zone_final, "Nefas Silk Lafto|Nifas Silk", "Nefas Silk Lafto"),
    
    # Create analysis zone
    zone_for_analysis = if_else(
      Region_clean == "Addis Ababa",
      zone_final,      # use standardized zone_final for Addis Ababa
      zone_gis         # use zone_gis for other regions
    )
  )

# ========== PART 7: CLEAN UP AND SAVE ==========

cat("13. Removing unnecessary variables...\n")
national_pdsr <- national_pdsr %>%
  select(-Occupationsotherspecified, -which(names(.) == "37"))

# Remove intermediate logical variables
logical_vars <- paste0(available_vars, "_logical")
national_pdsr <- national_pdsr %>%
  select(-any_of(logical_vars))

cat("14. Saving cleaned data...\n")
saveRDS(national_pdsr, "national_pdsr_cleaned.rds")
write.csv(national_pdsr, "national_pdsr_cleaned.csv", row.names = FALSE)

# ========== PART 8: FINAL REPORT ==========

cat("\n=== DATA CLEANING COMPLETE ===\n")
cat("Total cases:", nrow(national_pdsr), "\n")
cat("Saved as: national_pdsr_cleaned.rds and .csv\n\n")

cat("=== KEY VARIABLE DISTRIBUTIONS ===\n")

# Display key tables
key_vars <- c("Statusofthebabyatbirth", "Timingofthedeath", 
              "ICDPM_MaternalCondition", "Isthedeathpreventable",
              "Wherewasthedeceasedbabyborn")

for(var in key_vars) {
  cat("\n---", var, "---\n")
  print(table(national_pdsr[[var]], useNA = "always"))
}

cat("\n=== ICD-PM CLASSIFICATION RESULTS ===\n")
icdpm_table <- table(national_pdsr$ICDPM_MaternalCondition, useNA = "always")
total_cases <- nrow(national_pdsr)
for(i in 1:length(icdpm_table)) {
  if(!is.na(names(icdpm_table)[i])) {
    pct <- round(icdpm_table[i] / total_cases * 100, 1)
    cat(sprintf("%-40s: %6d (%5.1f%%)\n", 
                names(icdpm_table)[i], icdpm_table[i], pct))
  }
}

cat("\n=== CONDITION COUNTS ===\n")
count_table <- table(national_pdsr$ConditionCount, useNA = "always")
for(count in names(count_table)) {
  if(!is.na(count)) {
    pct <- round(count_table[count] / total_cases * 100, 1)
    label <- ifelse(count == "0", "No conditions", 
                    ifelse(count == "1", "1 condition", paste(count, "conditions")))
    cat(sprintf("%-20s: %6d (%5.1f%%)\n", label, count_table[count], pct))
  }
}

cat("\n=== REGIONAL ANALYSIS ===\n")
# Loop over all regions and print zone distribution
unique_regions <- unique(national_pdsr$Region_clean)
for(r in unique_regions) {
  cat("\nRegion:", r, "\n")
  region_zones <- table(national_pdsr$zone_for_analysis[national_pdsr$Region_clean == r])
  if(length(region_zones) > 0) {
    for(z in names(region_zones)) {
      cat(sprintf("  %-30s: %5d\n", z, region_zones[z]))
    }
  } else {
    cat("  No zone data available\n")
  }
}