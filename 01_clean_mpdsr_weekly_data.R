# ==============================================================================
# MPDSR DATA CLEANING AND GEOGRAPHIC STANDARDIZATION SCRIPT
# ==============================================================================

# Clear workspace and load required packages
# ------------------------------------------------------------------------------
rm(list = ls())

# Load required libraries
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(sf)          # For spatial data handling

# ==============================================================================
# 1. DATA IMPORT
# ==============================================================================

# Import MPDSR weekly surveillance data
# ------------------------------------------------------------------------------
phem_weekly_2017 <- read_excel(
  "C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/WEEkly_Notfication/Copy of PHEM Weekly Surveillance Data EFY2017_Full(1).xlsx",
  sheet = "PHEM Weekly Data "
)

# Inspect data structure
cat("Data imported successfully.\n")
cat("Dimensions:", dim(phem_weekly_2017), "\n")
cat("Columns:", names(phem_weekly_2017), "\n\n")

# ==============================================================================
# 2. DATA AGGREGATION
# ==============================================================================

# Aggregate maternal and perinatal deaths by region and zone
# ------------------------------------------------------------------------------
summary_yearly_mpdsr <- phem_weekly_2017 %>%
  group_by(Region, Zone) %>%
  summarise(
    total_maternal_deaths = sum(`Maternal death`, na.rm = TRUE),
    total_perinatal_deaths = sum(`Perinatal death`, na.rm = TRUE),
    n_weeks_reported = n(),  # Count number of weekly reports
    .groups = "drop"
  )

cat("Data aggregated by region and zone.\n")

# ==============================================================================
# 3. SPATIAL DATA PREPARATION
# ==============================================================================

# Load Ethiopia administrative boundary shapefile
# ------------------------------------------------------------------------------
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path, quiet = TRUE)

# Standardize region names in spatial data for consistency
eth_adm2 <- eth_adm2 %>%
  mutate(ADM1_EN = case_when(
    ADM1_EN == "Benishangul Gumz" ~ "B-Gumuz",
    ADM1_EN == "Gambela" ~ "Gambella",
    ADM1_EN == "Southern Ethiopia" ~ "South Ethiopia",
    ADM1_EN == "South West Ethiopia" ~ "SWEPRS",
    TRUE ~ ADM1_EN
  ))

cat("Spatial data loaded and standardized.\n")

# ==============================================================================
# 4. ZONE NAME STANDARDIZATION
# ==============================================================================

# Initialize corrected zone column
summary_yearly_mpdsr <- summary_yearly_mpdsr %>%
  mutate(Zone_corrected = Zone)

# Define zone standardization functions for each region
# ------------------------------------------------------------------------------

# Function to standardize Addis Ababa zones
standardize_addis_ababa <- function(df) {
  df %>%
    mutate(Zone_corrected = ifelse(Region == "Addis Ababa", "Region 14", Zone_corrected))
}

# Function to standardize Afar zones
standardize_afar <- function(df) {
  df %>%
    mutate(Zone_corrected = case_when(
      Region == "Afar" & Zone_corrected %in% c("Zone 1") ~ "Awsi /Zone 1",
      Region == "Afar" & Zone_corrected %in% c("Zone 2", "Zone2") ~ "Kilbati /Zone2",
      Region == "Afar" & Zone_corrected %in% c("Zone 3") ~ "Gabi /Zone 3",
      Region == "Afar" & Zone_corrected %in% c("Zone 4") ~ "Fanti /Zone 4",
      Region == "Afar" & Zone_corrected %in% c("Zone 5") ~ "Hari /Zone 5",
      TRUE ~ Zone_corrected
    ))
}

# Function to standardize Amhara zones
standardize_amhara <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Amhara" & Zone_corrected %in% c("Awi") ~ "Awi",
        Region == "Amhara" & Zone_corrected %in% c("Bahir Dar City", "Bahir_Dar_City") ~ "West Gojam",
        Region == "Amhara" & Zone_corrected %in% c("Central Gondar", "Central_Gondar", "Gondar City", "Gondar_City") ~ "Central Gondar",
        Region == "Amhara" & Zone_corrected %in% c("Debre Birhan City", "Debre_Birhan_City") ~ "North Shewa (AM)",
        Region == "Amhara" & Zone_corrected %in% c("Debre Markos City", "Debre_Markos_City") ~ "East Gojam",
        Region == "Amhara" & Zone_corrected %in% c("Debre Tabor City", "Debre_Tabor_City") ~ "South Gondar",
        Region == "Amhara" & Zone_corrected %in% c("Dessie City", "Dessie_City", "Kombolcha City", "Kombolcha_City") ~ "South Wello",
        Region == "Amhara" & Zone_corrected %in% c("East Gojjam", "East_Gojjam") ~ "East Gojam",
        Region == "Amhara" & Zone_corrected %in% c("North Gojjam", "North_Gojjam") ~ "West Gojam",
        Region == "Amhara" & Zone_corrected %in% c("North Gondar", "North_Gondar") ~ "North Gondar",
        Region == "Amhara" & Zone_corrected %in% c("North Shewa", "North_Shewa") ~ "North Shewa (AM)",
        Region == "Amhara" & Zone_corrected %in% c("North Wollo", "North_Wollo", "N/wollo", "N0rth_W0ll0", "Woldia City", "Woldia_City") ~ "North Wello",
        Region == "Amhara" & Zone_corrected %in% c("Oromo Special Zone", "Oromo_Special_Zone") ~ "Oromia",
        Region == "Amhara" & Zone_corrected %in% c("South Gondar", "South_Gondar") ~ "South Gondar",
        Region == "Amhara" & Zone_corrected %in% c("South Wollo", "South_Wollo") ~ "South Wello",
        Region == "Amhara" & Zone_corrected %in% c("Wag Hemra", "Wag_Hemra") ~ "Wag Hamra",
        Region == "Amhara" & Zone_corrected %in% c("West Gojjam", "West_Gojjam") ~ "West Gojam",
        Region == "Amhara" & Zone_corrected %in% c("West Gondar", "West_Gondar") ~ "West Gondar",
        Region == "Amhara" & Zone_corrected %in% c("Wolkait Tegedie Setit Humera", "Wolkait_Tegedie_Setit_Humera") ~ "Western",
        TRUE ~ Zone_corrected
      ),
      Region = case_when(
        Zone_corrected == "Western" ~ "Tigray",
        TRUE ~ Region
      )
    )
}

# Function to standardize B-Gumuz zones
standardize_bgumuz <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "B-Gumuz" & Zone_corrected %in% c("Maokomo Special Woreda") ~ "Mao Komo Special",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Central Ethiopia zones
standardize_central_ethiopia <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Central Ethiopia" & Zone_corrected %in% c("Gurage", "Misrak Gurage", "Kebena Sp.Woreda", "Kebena Sp.woreda",
                                                             "Mareko Sp.Woreda", "Mareko Sp.woreda", "Tenbaro Sp.Woreda", "Tenbaro Sp.woreda") ~ "Guraghe",
        Region == "Central Ethiopia" & Zone_corrected %in% c("Hadiyya") ~ "Hadiya",
        Region == "Central Ethiopia" & Zone_corrected %in% c("Kembata", "Tenbaro Sp.Woreda") ~ "Kembata Tembaro",
        Region == "Central Ethiopia" & Zone_corrected %in% c("Silte") ~ "Siltie",
        Region == "Central Ethiopia" & Zone_corrected %in% c("Yem") ~ "Yem Special",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Gambella zones
standardize_gambella <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Gambella" & Zone_corrected %in% c("Anywa", "Gambella") ~ "Agnewak",
        Region == "Gambella" & Zone_corrected == "Majang" ~ "Majang",
        Region == "Gambella" & Zone_corrected == "Nuer" ~ "Nuwer",
        Region == "Gambella" & Zone_corrected == "Itang" ~ "Itang Special woreda",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Harari zones
standardize_harari <- function(df) {
  df %>%
    mutate(
      Region = if_else(toupper(Region) == "HARARI", "Harari", Region),
      Zone_corrected = if_else(toupper(Zone_corrected) == "HARARI", "Harari", Zone_corrected)
    )
}

# Function to standardize Oromia zones
standardize_oromia <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Oromia" & Zone_corrected == "Adama Town" ~ "East Shewa",
        Region == "Oromia" & Zone_corrected == "Agaro Town" ~ "Jimma",
        Region == "Oromia" & Zone_corrected == "Ambo Town" ~ "West Shewa",
        Region == "Oromia" & Zone_corrected == "Asella Town" ~ "Arsi",
        Region == "Oromia" & Zone_corrected == "Batu Town" ~ "East Shewa",
        Region == "Oromia" & Zone_corrected == "Bishoftu Town" ~ "East Shewa",
        Region == "Oromia" & Zone_corrected == "Bule Hora Town" ~ "West Guji",
        Region == "Oromia" & Zone_corrected == "Dodola Town" ~ "West Arsi",
        Region == "Oromia" & Zone_corrected == "Holeta Town" ~ "Finfine Special",
        Region == "Oromia" & Zone_corrected == "Jimma Town" ~ "Jimma",
        Region == "Oromia" & Zone_corrected == "Maya Town" ~ "East Hararge",
        Region == "Oromia" & Zone_corrected == "Metu Town" ~ "Ilu Aba Bora",
        Region == "Oromia" & Zone_corrected == "Modjo town" ~ "East Shewa",
        Region == "Oromia" & Zone_corrected == "Moyale Town" ~ "Borena",
        Region == "Oromia" & Zone_corrected == "Nejo Town" ~ "West Wellega",
        Region == "Oromia" & Zone_corrected == "Nekemte Town" ~ "East Wellega",
        Region == "Oromia" & Zone_corrected == "North Shoa" ~ "North Shewa (OR)",
        Region == "Oromia" & Zone_corrected == "Robe Town" ~ "Bale",
        Region == "Oromia" & Zone_corrected == "Sendafa Town" ~ "Finfine Special",
        Region == "Oromia" & Zone_corrected == "Shager-City" ~ "Finfine Special",
        Region == "Oromia" & Zone_corrected == "Shakiso Town" ~ "Guji",
        Region == "Oromia" & Zone_corrected == "Shashemene  Town" ~ "West Arsi",
        Region == "Oromia" & Zone_corrected == "Sheno Town" ~ "North Shewa (OR)",
        Region == "Oromia" & Zone_corrected == "West Shoa" ~ "West Shewa",
        Region == "Oromia" & Zone_corrected == "Woliso Town" ~ "South West Shewa",
        Region == "Oromia" & Zone_corrected == "East Wollega" ~ "East Wellega",
        Region == "Oromia" & Zone_corrected == "West Wollega" ~ "West Wellega",
        Region == "Oromia" & Zone_corrected == "Horro Guduru Wollega" ~ "Horo Gudru Wellega",
        Region == "Oromia" & Zone_corrected == "Kellem Wollega" ~ "Kelem Wellega",
        Region == "Oromia" & Zone_corrected == "Illuababora" ~ "Ilu Aba Bora",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Sidama zones
standardize_sidama <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Sidama" ~ "Sidama",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Somali zones
standardize_somali <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Somali" & Zone_corrected %in% c("Afder", "Afder ") ~ "Afder",
        Region == "Somali" & Zone_corrected %in% c("Daawa", "dawa", "DAWA") ~ "Daawa",
        Region == "Somali" & Zone_corrected %in% c("Dollo", "Doolo") ~ "Doolo",
        Region == "Somali" & Zone_corrected %in% c("Erer", "erer") ~ "Erer",
        Region == "Somali" & Zone_corrected %in% c("Fafan", "fafan") ~ "Fafan",
        Region == "Somali" & Zone_corrected %in% c("Jarar", "jarar") ~ "Jarar",
        Region == "Somali" & Zone_corrected %in% c("Korahay", "Korahe", "korahe") ~ "Korahe",
        Region == "Somali" & Zone_corrected %in% c("Liban", "Liban ", "Liben") ~ "Liban",
        Region == "Somali" & Zone_corrected %in% c("Nogob", "nogob") ~ "Nogob",
        Region == "Somali" & Zone_corrected %in% c("Shabelle", "Shabeelle", "shebelle") ~ "Shabelle",
        Region == "Somali" & Zone_corrected %in% c("Siti", "sitti") ~ "Siti",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize South Ethiopia zones
standardize_south_ethiopia <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "South Ethiopia" & Zone_corrected == "Ale" ~ "Alle",
        Region == "South Ethiopia" & Zone_corrected == "Gedio" ~ "Gedeo",
        Region == "South Ethiopia" & Zone_corrected == "Koore" ~ "Amaro",
        Region == "South Ethiopia" & Zone_corrected == "Ari" ~ "South Omo",
        Region == "South Ethiopia" & Zone_corrected == "Gardula" ~ "Derashe",
        Region == "South Ethiopia" & Zone_corrected == "Wolaiyta" ~ "Wolayita",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize SWEPRS zones
standardize_sweeps <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "SWEPRS" & Zone_corrected == "Kaffa" ~ "Kefa",
        Region == "SWEPRS" & Zone_corrected == "West Omo" ~ "Mirab Omo",
        Region == "SWEPRS" & Zone_corrected == "Konta" ~ "Konta Special",
        TRUE ~ Zone_corrected
      )
    )
}

# Function to standardize Tigray zones
standardize_tigray <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Region == "Tigray" & Zone_corrected == "East" ~ "Eastern",
        Region == "Tigray" & Zone_corrected == "North West" ~ "North Western",
        Region == "Tigray" & Zone_corrected == "South" ~ "Southern",
        Region == "Tigray" & Zone_corrected == "South East" ~ "South Eastern",
        Region == "Tigray" & Zone_corrected == "West" ~ "Western",
        TRUE ~ Zone_corrected
      )
    )
}

# Function for final cross-region corrections
final_corrections <- function(df) {
  df %>%
    mutate(
      Zone_corrected = case_when(
        Zone_corrected == "Sitti"        ~ "Siti",
        Zone_corrected == "East Borena"  ~ "Borena",
        Zone_corrected == "West Harerge" ~ "West Hararghe",
        TRUE ~ Zone_corrected
      )
    )
}

# Apply all standardization functions in sequence
# ------------------------------------------------------------------------------
cat("Starting zone name standardization...\n")

summary_yearly_mpdsr <- summary_yearly_mpdsr %>%
  standardize_addis_ababa() %>%
  standardize_afar() %>%
  standardize_amhara() %>%
  standardize_bgumuz() %>%
  standardize_central_ethiopia() %>%
  standardize_gambella() %>%
  standardize_harari() %>%
  standardize_oromia() %>%
  standardize_sidama() %>%
  standardize_somali() %>%
  standardize_south_ethiopia() %>%
  standardize_sweeps() %>%
  standardize_tigray() %>%
  final_corrections()

# Aggregate data after all corrections
summary_yearly_mpdsr <- summary_yearly_mpdsr %>%
  group_by(Region, Zone_corrected) %>%
  summarise(
    count = n(),
    total_maternal_deaths = sum(total_maternal_deaths, na.rm = TRUE),
    total_perinatal_deaths = sum(total_perinatal_deaths, na.rm = TRUE),
    n_weeks_reported = sum(n_weeks_reported, na.rm = TRUE),
    .groups = "drop"
  )

cat("Zone name standardization completed.\n")

# ==============================================================================
# 5. DATA VALIDATION
# ==============================================================================

# Check for remaining mismatches between MPDSR data and shapefile
# ------------------------------------------------------------------------------
mpdsr_zones <- unique(summary_yearly_mpdsr$Zone_corrected)
shapefile_zones <- unique(eth_adm2$ADM2_EN)

# Identify zones in MPDSR data not found in shapefile
mismatched_zones <- setdiff(mpdsr_zones, shapefile_zones)

if (length(mismatched_zones) > 0) {
  cat("Warning: The following zones are not found in the shapefile:\n")
  print(mismatched_zones)
} else {
  cat("All zones matched successfully with shapefile.\n")
}

# Check region distribution
cat("\nRegion distribution in cleaned data:\n")
print(table(summary_yearly_mpdsr$Region))

# ==============================================================================
# 6. DATA EXPORT
# ==============================================================================

# Save cleaned data for future use
# ------------------------------------------------------------------------------
output_file <- "summary_yearly_MPDSR_zone_cleaned.rds"
saveRDS(summary_yearly_mpdsr, file = output_file)

cat("\n=================================================================\n")
cat("PROCESSING COMPLETE\n")
cat("=================================================================\n")
cat("Input data dimensions:", dim(phem_weekly_2017), "\n")
cat("Output data dimensions:", dim(summary_yearly_mpdsr), "\n")
cat("Unique regions in output:", length(unique(summary_yearly_mpdsr$Region)), "\n")
cat("Unique zones in output:", length(unique(summary_yearly_mpdsr$Zone_corrected)), "\n")
cat("Output saved to:", output_file, "\n")
cat("=================================================================\n")

# Clean up intermediate objects (optional)

rm(phem_weekly_2017, eth_adm2, mismatched_zones, mpdszones, shapefile_zones)
