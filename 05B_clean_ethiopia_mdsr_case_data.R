# Clear environment
rm(list = ls())

# Load libraries
library(sf)
library(dplyr)
library(stringr)
library(mice)

# Read shapefile
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path)

# Load MDSR dataset
eth_MDSR_data_clean <- `eth_MDSR_data_clean_2025-12-14`

# ----------------------------
# Helper functions
# ----------------------------
clean_yes_no <- function(x) {
  x <- tolower(trimws(x))
  dplyr::case_when(
    x == "yes" ~ "Yes",
    x == "no"  ~ "No",
    TRUE       ~ NA_character_
  )
}

# ----------------------------
# Clean categorical variables
# ----------------------------
eth_MDSR_data_clean <- eth_MDSR_data_clean %>%
  # Remove unnecessary columns
  select(-woreda) %>%
  
  # Type of Health Facility
  mutate(TypeofHFs = str_trim(TypeofHFs),
         TypeofHFs = case_when(
           TypeofHFs %in% c("Gov_Hos","Gov_Hos ") ~ "Government Hospital",
           TypeofHFs %in% c("Gov_HCs","Gov_HCs ","Health Post") ~ "Government Health Center",
           TypeofHFs == "NGO_HFs" ~ "NGO Facility",
           TypeofHFs %in% c("Private_clinic","Private_clinic ","Private_HF","Private_HFs") ~ "Private Facility",
           TRUE ~ "Other"
         ),
         
         # MDRF source
         MDRFextractedfrom = str_trim(MDRFextractedfrom),
         MDRFextractedfrom = case_when(
           MDRFextractedfrom == "FBAF" ~ "Facility-Based Abstraction Form",
           MDRFextractedfrom == "VA"   ~ "Verbal Autopsy",
           TypeofHFs == "Government Hospital" & MDRFextractedfrom == "Other" ~ "Facility-Based Abstraction Form",
           TypeofHFs == "Government Health Center" & MDRFextractedfrom == "Other" ~ "Verbal Autopsy",
           TRUE ~ MDRFextractedfrom
         ),
         
         # Residence
         Residence.Cat = str_trim(Residence.Cat),
         Residence.Cat = case_when(
           tolower(Residence.Cat) %in% c("rural","sidama") ~ "Rural",
           tolower(Residence.Cat) %in% c("urban") ~ "Urban",
           Region1 %in% c("Addis Ababa","Dire Dawa") ~ "Urban",
           TRUE ~ "Rural"
         ),
         
         # Marital Status
         MaritalStatus_clean = case_when(
           tolower(MaritalStatus) %in% c("married","marreid") ~ "Married",
           tolower(MaritalStatus) %in% c("single","siingle","missed") ~ "Single",
           tolower(MaritalStatus) == "divorced" ~ "Divorced",
           tolower(MaritalStatus) == "widowed"  ~ "Widowed",
           TRUE ~ NA_character_
         ),
         
         # Religion
         Religion_standardized = case_when(
           tolower(ReligionDeceased) %in% c("muslim","muslim/islam") ~ "Muslim",
           tolower(ReligionDeceased) %in% c("orthodox","orthodocs","ortodox") ~ "Orthodox",
           tolower(ReligionDeceased) == "protestant" ~ "Protestant",
           tolower(ReligionDeceased) == "catholic" ~ "Catholic",
           tolower(ReligionDeceased) %in% c("traditional","wakefata","wakefeta","waqefata","pagan") ~ "Traditional",
           tolower(ReligionDeceased) == "other" ~ "Other",
           tolower(ReligionDeceased) == "unknown" ~ "Unknown",
           TRUE ~ str_to_title(ReligionDeceased)
         ),
         
         # Education
         Education_clean = tolower(trimws(LevelofEducation)),
         Education_categorized = case_when(
           grepl("no formal|illiterate|unable read", Education_clean) ~ "No formal education",
           grepl("can read and write", Education_clean) ~ "No formal, but literate",
           grepl("elementary|primary", Education_clean) ~ "Elementary",
           grepl("high school", Education_clean) ~ "High school",
           grepl("college", Education_clean) ~ "College+",
           TRUE ~ "Unknown"
         ),
         
         # Gravidity and Parity imputation
         gravity_impute = ifelse(is.na(Gravidity), median(Gravidity, na.rm = TRUE), Gravidity),
         parity_impute = ifelse(is.na(Parity), median(Parity, na.rm = TRUE), Parity),
         
         # ANC attendance
         AttendedANC_clean = case_when(
           tolower(AttendedANC) %in% c("yes","both","health centre","health post","hospital","private") ~ "Yes",
           tolower(AttendedANC) == "no" ~ "No",
           TRUE ~ "Unknown"
         ),
         
         IfYeswhereistheANC_clean = ifelse(
           AttendedANC_clean == "Yes" & (is.na(IfYeswhereistheANC) | IfYeswhereistheANC == ""), 
           AttendedANC, 
           IfYeswhereistheANC
         ),
         
         ANC_attendance_final = ifelse(AttendedANC_clean %in% c("Unknown","Missing") & !is.na(IfYeswhereistheANC_clean), "Yes", AttendedANC_clean),
         
         ANC_facility_final = ifelse(ANC_attendance_final=="Yes", IfYeswhereistheANC_clean, NA_character_),
         ANC_facility_level = case_when(
           grepl("hospital", ANC_facility_final, ignore.case=TRUE) ~ "Hospital",
           grepl("health|clinic|private", ANC_facility_final, ignore.case=TRUE) ~ "Health Center/Clinic",
           grepl("health post", ANC_facility_final, ignore.case=TRUE) ~ "Health Post",
           TRUE ~ NA_character_
         )
  )

# ----------------------------
# Numeric imputations
# ----------------------------
ethio_mdsr_cleaned_with_imputed <- eth_MDSR_data_clean %>%
  mutate(
    AgeAtDeathinyears = ifelse(is.na(AgeAtDeathinyears), median(AgeAtDeathinyears, na.rm = TRUE), AgeAtDeathinyears),
    ANC_visits_clean = ifelse(ANC_attendance_final=="No", 0,
                              ifelse(is.na(as.numeric(as.character(IfYesNumberofANCvisits))),
                                     median(as.numeric(as.character(IfYesNumberofANCvisits))[ANC_attendance_final=="Yes"], na.rm = TRUE),
                                     as.numeric(as.character(IfYesNumberofANCvisits))))
  )

# ----------------------------
# Yes/No standardization for causes of death & indicators
# ----------------------------
yes_no_vars <- c("DO2Obstructedlabor","DO3HDP","DO4Abortion","DO5sepsis",
                 "INDC1Anemia","IND2Malaria","IND3HIV","IND4TB","IND5others")

ethio_mdsr_cleaned_with_imputed[yes_no_vars] <- lapply(ethio_mdsr_cleaned_with_imputed[yes_no_vars], clean_yes_no)

# ----------------------------
# Save cleaned dataset
# ----------------------------
saveRDS(ethio_mdsr_cleaned_with_imputed, "ethio_mdsr_cleaned_with_imputed.rds")
