
# Initialize environment
rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

# Load required libraries
library(sf)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# ==============================================================================
# 1. DATA IMPORT
# ==============================================================================

# Load administrative boundaries
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path, quiet = TRUE)

# Load MDSR data
mdsr_file_path <- "C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/National_MPDSR_data/National_MDSR_2013_2024_V01_8_28_2025.xlsx"
eth_MDSR_data <- read_excel(mdsr_file_path, sheet = "MDSR_Data") %>%
  as.data.frame()

cat("Data loaded successfully.\n")
cat("Original dimensions:", dim(eth_MDSR_data), "\n")

# ==============================================================================
# 2. INITIAL DATA CLEANING
# ==============================================================================

# Remove unnecessary columns
cols_to_remove <- c(
  # Date columns
  7:10, 20:27, 59:61,
  # Demographic categories
  29:30, 37, 40, 42, 46, 48:49, 55,
  # Classification and delay columns
  71, 95:103
)

eth_MDSR_data_clean <- eth_MDSR_data[, -cols_to_remove]

cat("Columns removed. New dimensions:", dim(eth_MDSR_data_clean), "\n")

# Filter out 2018 records and fix data entry error
eth_MDSR_data_clean <- eth_MDSR_data_clean %>%
  filter(Year_D != 2018) %>%
  mutate(Year_D = ifelse(Year_D == 20116, 2016, Year_D))

# Remove completely empty rows
eth_MDSR_data_clean <- eth_MDSR_data_clean %>%
  filter(!if_all(everything(), ~ is.na(.) | . == ""))

cat("Rows filtered. Remaining records:", nrow(eth_MDSR_data_clean), "\n")

# ==============================================================================
# 3. REGION NAME STANDARDIZATION
# ==============================================================================

region_mapping <- c(
  "Addis Ababa" = "Addis Ababa",
  "Afar" = "Afar",
  "Amhara" = "Amhara",
  "Benishangul Gumuz" = "Benishangul Gumz",
  "Benishangul-Gumuz" = "Benishangul Gumz",
  "CER" = "Central Ethiopia",
  "Dire Dawa" = "Dire Dawa",
  "Gambella" = "Gambella",
  "Harari" = "Harari",
  "Oromia" = "Oromia",
  "Sidama" = "Sidama",
  "somali" = "Somali",
  "Somali" = "Somali",
  "South Ethiopia" = "Southern Ethiopia",
  "SWEPRS" = "South West Ethiopia",
  "SWEPR" = "South West Ethiopia",
  "Tigray" = "Tigray"
)

eth_MDSR_data_clean <- eth_MDSR_data_clean %>%
  mutate(
    Region_new_1 = recode(Region_new_1, !!!region_mapping),
    Region_new_1 = as.character(Region_new_1)
  )

cat("Region names standardized.\n")

# ==============================================================================
# 4. ZONE NAME STANDARDIZATION BY REGION
# ==============================================================================

standardize_zones <- function(data) {
  data %>%
    # Addis Ababa
    mutate(
      ZonName = ifelse(Region_new_1 == "Addis Ababa", "Region 14", ZonName)
    ) %>%
    # Afar
    mutate(
      ZonName = case_when(
        Region_new_1 == "Afar" & ZonName == "Zone 01" ~ "Awsi /Zone 1",
        Region_new_1 == "Afar" & ZonName == "Zone 02" ~ "Kilbati /Zone2",
        Region_new_1 == "Afar" & ZonName == "Zone 03" ~ "Gabi /Zone 3",
        Region_new_1 == "Afar" & ZonName == "Zone 04" ~ "Fanti /Zone 4",
        Region_new_1 == "Afar" & ZonName == "Zone 05" ~ "Hari /Zone 5",
        TRUE ~ ZonName
      )
    ) %>%
    # Amhara
    mutate(
      ZonName = case_when(
        Region_new_1 == "Amhara" & ZonName %in% c("Awi") ~ "Awi",
        Region_new_1 == "Amhara" & ZonName %in% c("Central Gondar", "Gondar town") ~ "Central Gondar",
        Region_new_1 == "Amhara" & ZonName %in% c("East Gojjam", "Debre Markos city") ~ "East Gojam",
        Region_new_1 == "Amhara" & ZonName %in% c("North Gondar") ~ "North Gondar",
        Region_new_1 == "Amhara" & ZonName %in% c("North Shewa", "North  Shewa", "D/Berhan city", "Debre birhan") ~ "North Shewa (AM)",
        Region_new_1 == "Amhara" & ZonName %in% c("North Wollo", "Woldia city") ~ "North Wello",
        Region_new_1 == "Amhara" & ZonName %in% c("Oromia spe", "Oromia", "Oromo special Zone") ~ "Oromia",
        Region_new_1 == "Amhara" & ZonName %in% c("South Gonder", "Debertabor", "Debertabor City") ~ "South Gondar",
        Region_new_1 == "Amhara" & ZonName %in% c("South Wolo", "Dessie", "Kombolcha City") ~ "South Wello",
        Region_new_1 == "Amhara" & ZonName %in% c("Wag Himra") ~ "Wag Hamra",
        Region_new_1 == "Amhara" & ZonName %in% c("West Gojjam", "West gojjam", "Bahir Dar", "Bahir Dar City") ~ "West Gojam",
        Region_new_1 == "Amhara" & ZonName %in% c("West Gondar") ~ "West Gondar",
        TRUE ~ ZonName
      )
    ) %>%
    # Benishangul Gumz
    mutate(
      ZonName = case_when(
        Region_new_1 == "Benishangul Gumz" & ZonName %in% c("Assosa") ~ "Assosa",
        Region_new_1 == "Benishangul Gumz" & ZonName %in% c("Kamashi", "Kamash", "kamash") ~ "Kamashi",
        Region_new_1 == "Benishangul Gumz" & ZonName %in% c("Metekel", "Metkel") ~ "Metekel",
        Region_new_1 == "Benishangul Gumz" & ZonName %in% c("Maokomo") ~ "Mao Komo Special",
        TRUE ~ ZonName
      )
    ) %>%
    # Central Ethiopia
    mutate(
      ZonName = case_when(
        Region_new_1 == "Central Ethiopia" & ZonName %in% c("Gurage", "Misrak gurage", "Misrak Gurage", "East Gurage") ~ "Guraghe",
        Region_new_1 == "Central Ethiopia" & ZonName == "Hadiya" ~ "Hadiya",
        Region_new_1 == "Central Ethiopia" & ZonName == "Halaba" ~ "Halaba",
        Region_new_1 == "Central Ethiopia" & ZonName %in% c("Kembata Timbaro", "Kembata Tembaro", "kembata tembaro", 
                                                            "Kembata tembaro", "Kenbata", "Kembata", "Tenbaro SP.Woreda") ~ "Kembata Tembaro",
        Region_new_1 == "Central Ethiopia" & ZonName %in% c("Siliti", "Silte", "silte", "Silite") ~ "Siltie",
        Region_new_1 == "Central Ethiopia" & ZonName %in% c("Yem", "yem") ~ "Yem Special",
        TRUE ~ ZonName
      )
    ) %>%
    # Dire Dawa (urban/rural classification)
    mutate(
      Region_new_1 = ifelse(
        grepl("Hiwot Fana", Reporinghealthfacility, ignore.case = TRUE),
        "Harari",
        Region_new_1
      )
    )
}

# Apply zone standardization
eth_MDSR_data_clean <- standardize_zones(eth_MDSR_data_clean)

# ==============================================================================
# 5. SPECIAL HANDLING FOR DIRE DAWA
# ==============================================================================

classify_dire_dawa_zones <- function(data) {
  urban_facilities <- c(
    "Sabian", "Melka Jebdu Health Center", "Legehare Health Center",
    "Addis Ketema Health Center", "Gende Kore Health Center",
    "Dil Chora Hospital", "Goro Health Center", "Art Hospital"
  )
  
  rural_facilities <- c(
    "Aseliso", "Jeldesa Health Center", "Wahil", "Biyo Awale Health Center",
    "Kalcha Health Center", "Legoda Gununfeta Health Center", "Melkakero Health Center"
  )
  
  data %>%
    mutate(
      Reporinghealthfacility = case_when(
        grepl("sabian", Reporinghealthfacility, ignore.case = TRUE) ~ "Sabian",
        grepl("melka.*jebdu", Reporinghealthfacility, ignore.case = TRUE) ~ "Melka Jebdu Health Center",
        grepl("legehare", Reporinghealthfacility, ignore.case = TRUE) ~ "Legehare Health Center",
        grepl("addis.*ketema", Reporinghealthfacility, ignore.case = TRUE) ~ "Addis Ketema Health Center",
        grepl("gende.*kore", Reporinghealthfacility, ignore.case = TRUE) ~ "Gende Kore Health Center",
        grepl("jeld", Reporinghealthfacility, ignore.case = TRUE) ~ "Jeldesa Health Center",
        grepl("biyo.*awale", Reporinghealthfacility, ignore.case = TRUE) ~ "Biyo Awale Health Center",
        grepl("dil.*chora", Reporinghealthfacility, ignore.case = TRUE) ~ "Dil Chora Hospital",
        grepl("kal[i]?cha", Reporinghealthfacility, ignore.case = TRUE) ~ "Kalcha Health Center",
        grepl("legoda.*gununfeta", Reporinghealthfacility, ignore.case = TRUE) ~ "Legoda Gununfeta Health Center",
        grepl("melkakero", Reporinghealthfacility, ignore.case = TRUE) ~ "Melkakero Health Center",
        TRUE ~ Reporinghealthfacility
      ),
      ZonName = case_when(
        Reporinghealthfacility %in% urban_facilities ~ "Dire Dawa urban",
        Reporinghealthfacility %in% rural_facilities ~ "Dire Dawa rural",
        Region_new_1 == "Dire Dawa" ~ "Dire Dawa rural",
        TRUE ~ ZonName
      )
    )
}

# Apply Dire Dawa classification
eth_MDSR_data_clean <- classify_dire_dawa_zones(eth_MDSR_data_clean)

# ==============================================================================
# 6. STANDARDIZE REMAINING REGIONS
# ==============================================================================

standardize_remaining_regions <- function(data) {
  data %>%
    # Gambella
    mutate(
      ZonName = case_when(
        Region_new_1 == "Gambella" & ZonName %in% c("Aguawak", "Agnwa", "Anywaa", "Gambella town", "Gambella Town") ~ "Agnewak",
        Region_new_1 == "Gambella" & ZonName %in% c("Megenger", "Mejenger") ~ "Majang",
        Region_new_1 == "Gambella" & ZonName %in% c("Nuer") ~ "Nuwer",
        Region_new_1 == "Gambella" & ZonName %in% c("Itang special woreda", "Itang Special woreda") ~ "Itang Special woreda",
        TRUE ~ ZonName
      )
    ) %>%
    # Harari
    mutate(
      ZonName = case_when(
        Region_new_1 == "Harari" & ZonName %in% c("Hareri", "Dire Dawa", "Harari") ~ "Harari",
        TRUE ~ ZonName
      )
    ) %>%
    # Oromia
    mutate(
      ZonName = case_when(
        Region_new_1 == "Oromia" & ZonName %in% c("Qeleme Wellega", "Kelam Welega", "Kelem Wollega", 
                                                  "Kelem Walaga", "K/Wollega") ~ "Kelem Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("Horro Guduru Wollega", "Horro Guduru Wallaga Zone", "HGW") ~ "Horo Gudru Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("East Wellega", "East Wollega") ~ "East Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("West Wellega", "West Wollega") ~ "West Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("Ilu Aba Bora", "Illuababoor", "Ilu Abba Boor", "IAB") ~ "Ilu Aba Bora",
        Region_new_1 == "Oromia" & ZonName %in% c("Buno Bedele", "BuNo Bedele", "BuNo Beddele", "Buno Bedelle") ~ "Buno Bedele",
        Region_new_1 == "Oromia" & ZonName %in% c("West Shewa", "west shewa") ~ "West Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("East Shewa", "East Shawa", "East Shoa") ~ "East Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("North Shewa", "North shewa", "North shoa") ~ "North Shewa (OR)",
        Region_new_1 == "Oromia" & ZonName %in% c("South West Shewa", "Southwest Shewa", "SWS") ~ "South West Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("West Hararge", "West Harerghe", "West Harerge", "West Hararghe") ~ "West Hararge",
        Region_new_1 == "Oromia" & ZonName %in% c("West Arsi", "west arsi") ~ "West Arsi",
        Region_new_1 == "Oromia" & ZonName %in% c("Borena", "Borana") ~ "Borena",
        Region_new_1 == "Oromia" & ZonName %in% c("East Hararge") ~ "East Hararge",
        Region_new_1 == "Oromia" & ZonName %in% c("Bale") ~ "Bale",
        Region_new_1 == "Oromia" & ZonName %in% c("Jimma", "jimma", "Jimma Sp.", "jimma town", "Jimma Town") ~ "Jimma",
        Region_new_1 == "Oromia" & ZonName == "Guji" ~ "Guji",
        Region_new_1 == "Oromia" & ZonName == "West Guji" ~ "West Guji",
        Region_new_1 == "Oromia" & ZonName %in% c("Arsi") ~ "Arsi",
        Region_new_1 == "Oromia" & ZonName %in% c("Finfine Special Zone", "FSZ") ~ "Finfine Special",
        Region_new_1 == "Oromia" & ZonName %in% c("East Bale", "E/bale") ~ "East Bale",
        Region_new_1 == "Oromia" & ZonName %in% c("East Borena") ~ "Borena",
        Region_new_1 == "Oromia" & ZonName %in% c("Sheger City", "Shager City", "Shager city") ~ "Finfine Special",
        Region_new_1 == "Oromia" & ZonName %in% c("Adama", "Adama City", "Adama city") ~ "East Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("Shashemene Town", "ShashemeneTown", "Shashamane Town", 
                                                  "Shashemene", "Shashemene City") ~ "West Arsi",
        Region_new_1 == "Oromia" & ZonName %in% c("Gelan Town", "Batu Town", "Batu", "Bishoftu Town", "Bishoftu", "Dukem Town") ~ "East Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("Robe Town", "Robe town", "Robe") ~ "Bale",
        Region_new_1 == "Oromia" & ZonName %in% c("Nekemte Town", "Nekemte") ~ "East Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("Holeta Town", "Holeta") ~ "West Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("Asella Town", "Asella") ~ "Arsi",
        Region_new_1 == "Oromia" & ZonName %in% c("Dodola Town", "Dodola T") ~ "West Arsi",
        Region_new_1 == "Oromia" & ZonName %in% c("Nedjo Town", "Nedjo") ~ "West Wellega",
        Region_new_1 == "Oromia" & ZonName %in% c("Mettu Town") ~ "Ilu Aba Bora",
        Region_new_1 == "Oromia" & ZonName %in% c("Woliso Town") ~ "South West Shewa",
        Region_new_1 == "Oromia" & ZonName %in% c("Agaro", "Agaro Town") ~ "Jimma",
        TRUE ~ ZonName
      ),
      # Correct region for Awassa entries
      Region_new_1 = ifelse(ZonName == "Awassa", "Sidama", Region_new_1)
    ) %>%
    # Sidama
    mutate(
      ZonName = case_when(
        Region_new_1 == "Sidama" & ZonName %in% c("Awassa", "Hawassa", "sidama", "North Sidama", 
                                                  "South Sidama", "Easten Sidama", "Eastern Sidama",
                                                  "Central Sidama", "Central sidama", "East Sidama") ~ "Sidama",
        TRUE ~ ZonName
      )
    ) %>%
    # Somali
    mutate(
      ZonName = case_when(
        Region_new_1 == "Somali" & ZonName %in% c("Fafan", "Liban", "Erer", "Nogob", "Shabelle") ~ ZonName,
        Region_new_1 == "Somali" & ZonName %in% c("fafan") ~ "Fafan",
        Region_new_1 == "Somali" & ZonName %in% c("Liban", "Liben") ~ "Liban",
        Region_new_1 == "Somali" & ZonName %in% c("Erer", "Erar") ~ "Erer",
        Region_new_1 == "Somali" & ZonName %in% c("Korahye", "Korahay", "Korahe") ~ "Korahe",
        Region_new_1 == "Somali" & ZonName %in% c("Afder", "Afdar", "Afdher") ~ "Afder",
        Region_new_1 == "Somali" & ZonName %in% c("Jarar", "Jarer") ~ "Jarar",
        Region_new_1 == "Somali" & ZonName %in% c("Shabele", "Shable", "Shabelle") ~ "Shabelle",
        Region_new_1 == "Somali" & ZonName %in% c("Dollo", "dollol", "Doolo") ~ "Doolo",
        Region_new_1 == "Somali" & ZonName == "Jijiga" ~ "Fafan",
        TRUE ~ ZonName
      )
    ) %>%
    # Southern Ethiopia
    mutate(
      ZonName = case_when(
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Wolayita", "Wolayta", "Woliyta", "Wolita", "wolita") ~ "Wolayita",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Gamo Gofa", "Gamo") ~ "Gamo",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Gofa", "Goffa") ~ "Gofa",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Gedeo", "Gedio") ~ "Gedeo",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("South Omo", "South omo", "Ari") ~ "South Omo",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Derashe", "Gardula") ~ "Derashe",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Segen", "Alle") ~ "Alle",
        Region_new_1 == "Southern Ethiopia" & ZonName == "Segen People" ~ "Burji",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Alaba", "Halaba") ~ "Halaba",
        Region_new_1 == "Southern Ethiopia" & ZonName %in% c("Konso", "konso") ~ "Konso",
        Region_new_1 == "Southern Ethiopia" & ZonName == "Kore" ~ "Amaro",
        Region_new_1 == "Southern Ethiopia" & ZonName == "Basketo" ~ "Basketo",
        TRUE ~ ZonName
      ),
      # Reassign Halaba/Alaba to Central Ethiopia
      Region_new_1 = case_when(
        ZonName %in% c("Alaba", "Halaba") ~ "Central Ethiopia",
        TRUE ~ Region_new_1
      ),
      ZonName = ifelse(Region_new_1 == "Central Ethiopia" & ZonName %in% c("Alaba", "Halaba"), "Halaba", ZonName)
    ) %>%
    # South West Ethiopia
    mutate(
      Region_new_1 = ifelse(ZonName == "Basketo" & Region_new_1 == "South West Ethiopia", "Southern Ethiopia", Region_new_1),
      ZonName = case_when(
        Region_new_1 == "South West Ethiopia" & ZonName %in% c("Bench Maji", "Bench Sheko", "Bench-Sheko", "Mizan Aman") ~ "Bench Sheko",
        Region_new_1 == "South West Ethiopia" & ZonName %in% c("Dawro", "Tarcha TA") ~ "Dawuro",
        Region_new_1 == "South West Ethiopia" & ZonName %in% c("Kaffa", "Kafa", "Bonga TA") ~ "Kefa",
        Region_new_1 == "South West Ethiopia" & ZonName %in% c("SHEKA", "sheka", "Tepi TA") ~ "Sheka",
        Region_new_1 == "South West Ethiopia" & ZonName == "West Omo" ~ "Mirab Omo",
        TRUE ~ ZonName
      )
    ) %>%
    # Tigray
    mutate(
      ZonName = case_when(
        Region_new_1 == "Tigray" & ZonName %in% c("North Western Tigray") ~ "North Western",
        Region_new_1 == "Tigray" & ZonName %in% c("South Tigray") ~ "Southern",
        Region_new_1 == "Tigray" & ZonName %in% c("Western Tigray") ~ "Western",
        Region_new_1 == "Tigray" & ZonName %in% c("South East Tigray") ~ "South Eastern",
        Region_new_1 == "Tigray" & ZonName %in% c("Eastern Tigray") ~ "Eastern",
        Region_new_1 == "Tigray" & ZonName %in% c("Central Tigray") ~ "Central",
        Region_new_1 == "Tigray" & ZonName %in% c("Mekele Especial Zone", "Mekele Especial") ~ "Mekelle",
        TRUE ~ ZonName
      )
    ) %>%
    # Fix Arada (Addis Ababa sub-city)
    mutate(
      Region_new_1 = ifelse(ZonName == "Arada", "Addis Ababa", Region_new_1),
      ZonName = ifelse(ZonName == "Arada", "Region 14", ZonName)
    ) %>%
    # Fix North Shewa typo
    mutate(
      ZonName = ifelse(ZonName == "North Shewa (OR", "North Shewa (OR)", ZonName)
    )
}

# Apply remaining region standardization
eth_MDSR_data_clean <- standardize_remaining_regions(eth_MDSR_data_clean)

# ==============================================================================
# 7. DATA QUALITY CHECKS
# ==============================================================================

# Check for missing values
na_summary <- eth_MDSR_data_clean %>%
  summarise(
    Total_Records = n(),
    NA_Region = sum(is.na(Region_new_1)),
    NA_Zone = sum(is.na(ZonName)),
    Percent_NA_Region = round(NA_Region / Total_Records * 100, 2),
    Percent_NA_Zone = round(NA_Zone / Total_Records * 100, 2)
  )

cat("\n=== DATA QUALITY SUMMARY ===\n")
print(na_summary)

# Check region distribution
region_dist <- eth_MDSR_data_clean %>%
  count(Region_new_1) %>%
  arrange(desc(n))

cat("\n=== REGION DISTRIBUTION ===\n")
print(region_dist)

# ==============================================================================
# 8. SAVE CLEANED DATA
# ==============================================================================

# Generate timestamp for filename
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_filename <- paste0("eth_MDSR_data_clean_", timestamp, ".rds")

# Save the cleaned data
saveRDS(eth_MDSR_data_clean, file = output_filename)

cat("\n=== CLEANING COMPLETE ===\n")
cat("Cleaned data saved to:", output_filename, "\n")
cat("Final dimensions:", dim(eth_MDSR_data_clean), "\n")
cat("Unique regions:", length(unique(eth_MDSR_data_clean$Region_new_1)), "\n")
cat("Unique zones:", length(unique(eth_MDSR_data_clean$ZonName)), "\n")

