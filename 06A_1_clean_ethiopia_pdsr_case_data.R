# ====================================================
# INITIAL DATA CLEANUP
# ====================================================

# Remove unnecessary zone-related columns
columns_to_remove <- c("Zone_std", "zone_for_analysis", "zone_final", "Zone_Res", "zone_gis")
national_pdsr_clean[columns_to_remove] <- NULL

# Initial exploration of Region_clean
cat("=== REGION DISTRIBUTION ===\n")
table(national_pdsr_clean$Region_clean)

# ====================================================
# STANDARDIZE ZONENAME FOR ADDIS ABABA
# ====================================================

cat("\n=== Standardizing ZoneName for Addis Ababa ===\n")

# Check current ZoneName values for Addis Ababa
addis_current <- table(national_pdsr_clean$ZoneName[national_pdsr_clean$Region_clean == "Addis Ababa"], 
                       useNA = "always")
cat("Current ZoneName distribution for Addis Ababa:\n")
print(addis_current)

# Apply standardization for Addis Ababa zones
national_pdsr_clean$ZoneName <- with(
  national_pdsr_clean,
  case_when(
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "akaki.*kaliti|akaki.*kality") ~ "Akaki Kaliti",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "addis.*ketema") ~ "Addis Ketema",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "arada") ~ "Arada",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "bole") ~ "Bole",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "gulele|gulelle") ~ "Gulele",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "kirkos") ~ "Kirkos",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "kolfe.*keraniyo|kolfe.*keranyo") ~ "Kolfe Keraniyo",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "lemi.*kura") ~ "Lemi Kura",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "lideta") ~ "Lideta",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "nefas.*silk.*lafto|nifas.*silk") ~ "Nefas Silk Lafto",
    
    Region_clean == "Addis Ababa" & 
      str_detect(tolower(ZoneName), "yeka") ~ "Yeka",
    
    # Keep all other cases unchanged
    TRUE ~ ZoneName
  )
)

# ====================================================
# CREATE AND POPULATE ZONE_GIS VARIABLE
# ====================================================

cat("\n=== Creating zone_gis variable ===\n")

# Initialize zone_gis with ZoneName
national_pdsr_clean$zone_gis <- national_pdsr_clean$ZoneName

# Define region-specific mapping functions
map_zone_gis <- function(data) {
  # Addis Ababa - Direct mapping to "Region 14"
  data$zone_gis[data$Region_clean == "Addis Ababa"] <- "Region 14"
  
  # Amhara Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Amhara",
    case_when(
      ZoneName == "North Gojjam" ~ "West Gojam",
      ZoneName %in% c("oromo special zone", "Oromo Special Zone") ~ "Oromia",
      ZoneName == "S/Gongar" ~ "South Gondar",
      
      # Pattern matches for Amhara
      str_detect(tolower(ZoneName), "awi") ~ "Awi",
      str_detect(tolower(ZoneName), "central.*gondar|c/gondar") ~ "Central Gondar",
      str_detect(tolower(ZoneName), "east.*gojjam") ~ "East Gojam",
      str_detect(tolower(ZoneName), "north.*gondar") ~ "North Gondar",
      str_detect(tolower(ZoneName), "north.*shewa|n/shewa") ~ "North Shewa (AM)",
      str_detect(tolower(ZoneName), "north.*wello|north.*wollo") ~ "North Wello",
      str_detect(tolower(ZoneName), "oromi.*special|oromia.*special") ~ "Oromia",
      str_detect(tolower(ZoneName), "south.*gonder|south.*gondar") ~ "South Gondar",
      str_detect(tolower(ZoneName), "south.*wello|south.*wollo|s/wollo") ~ "South Wello",
      str_detect(tolower(ZoneName), "wag.*himra|wag.*hamra") ~ "Wag Hamra",
      str_detect(tolower(ZoneName), "west.*gojjam|west.*gojam|w\\.gojam") ~ "West Gojam",
      str_detect(tolower(ZoneName), "west.*gondar|w/gondar") ~ "West Gondar",
      str_detect(tolower(ZoneName), "north.*gojjam|n/gojam|n\\.gojam|n/goja|N/GOJAM|N\n/Goja|N\n/Goja") ~ "West Gojam",
      
      # City mappings for Amhara
      str_detect(tolower(ZoneName), "bahir") ~ "West Gojam",
      str_detect(tolower(ZoneName), "dessie|dese|kombolcha") ~ "South Wello",
      str_detect(tolower(ZoneName), "gondar") ~ "Central Gondar",
      str_detect(tolower(ZoneName), "markos") ~ "East Gojam",
      str_detect(tolower(ZoneName), "birhan|brehan") ~ "North Shewa (AM)",
      str_detect(tolower(ZoneName), "tabor|detabor") ~ "South Gondar",
      str_detect(tolower(ZoneName), "woldia") ~ "North Wello",
      
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Fix specific issue in Amhara
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Amhara" & zone_gis == "N\n/Goja",
    "West Gojam",
    zone_gis
  ))
  
  # Benishangul-Gumuz Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Benishangul-Gumuz",
    case_when(
      str_detect(tolower(ZoneName), "assosa|assosal|addis ketema") ~ "Assosa",
      str_detect(tolower(ZoneName), "kamashi") ~ "Kamashi",
      str_detect(tolower(ZoneName), "maokomo|maoko|mao komo") ~ "Mao Komo Special",
      str_detect(tolower(ZoneName), "metekel|pawi") ~ "Metekel",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Central Ethiopia Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Central Ethiopia",
    case_when(
      ZoneName == "Mareko SP.W" ~ "Guraghe",
      str_detect(tolower(ZoneName), "gurage|mareko") ~ "Guraghe",
      str_detect(tolower(ZoneName), "hadiya") ~ "Hadiya",
      str_detect(tolower(ZoneName), "halaba") ~ "Halaba",
      str_detect(tolower(ZoneName), "kembata|kenbata|tembaro|tenbaro") ~ "Kembata Tembaro",
      str_detect(tolower(ZoneName), "silit|silte") ~ "Siltie",
      str_detect(tolower(ZoneName), "yem|yew") ~ "Yem Special",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Dire Dawa Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Dire Dawa",
    case_when(
      ZoneName == "Dire Dawa" ~ "Dire Dawa urban",
      is.na(ZoneName) ~ NA_character_,
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Gambella Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Gambella",
    case_when(
      str_detect(tolower(ZoneName), "gambella.*special|anuak") ~ "Agnewak",
      str_detect(tolower(ZoneName), "itang.*special") ~ "Itang Special woreda",
      str_detect(tolower(ZoneName), "mejeng|majang") ~ "Majang",
      str_detect(tolower(ZoneName), "nuer") ~ "Nuwer",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Oromia Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Oromia",
    case_when(
      str_detect(tolower(ZoneName), "finfine|ofsz|burayu|sebeta|gelan|shager") ~ "Finfine Special",
      str_detect(tolower(ZoneName), "arsi|asella") ~ "Arsi",
      str_detect(tolower(ZoneName), "east.*shewa|dukem|bishoftu|holeta|sendafa|adama|batu|fentale") ~ "East Shewa",
      str_detect(tolower(ZoneName), "borena|borana") ~ "Borena",
      str_detect(tolower(ZoneName), "kellem.*wollega|kelem.*wellega|kelem.*walaga|kelem.*wallaga") ~ "Kelem Wellega",
      str_detect(tolower(ZoneName), "south.*west.*shewa|southwest.*shewa|sws|woliso") ~ "South West Shewa",
      str_detect(tolower(ZoneName), "^bale$|goba|robe|dodola") ~ "Bale",
      str_detect(tolower(ZoneName), "horro.*guduru|horro.*gudru|horo.*gudru|horro.*wallaga") ~ "Horo Gudru Wellega",
      str_detect(tolower(ZoneName), "ilu.*aba.*bora|illuababor") ~ "Ilu Aba Bora",
      str_detect(tolower(ZoneName), "east.*wollega|east.*wellega|east.*wolega|east.*wallaga|neke") ~ "East Wellega",
      str_detect(tolower(ZoneName), "west.*shewa|ambo") ~ "West Shewa",
      str_detect(tolower(ZoneName), "east.*hararge") ~ "East Hararge",
      str_detect(tolower(ZoneName), "buno.*bedelle|buno.*bedele") ~ "Buno Bedele",
      str_detect(tolower(ZoneName), "^guji$") ~ "Guji",
      str_detect(tolower(ZoneName), "jimma|agaro") ~ "Jimma",
      str_detect(tolower(ZoneName), "west.*guji") ~ "West Guji",
      str_detect(tolower(ZoneName), "north.*shewa|north.*shoa|north.*showa") ~ "North Shewa (OR)",
      str_detect(tolower(ZoneName), "west.*hararge|west.*hararghe|west.*harerge|west.*harerghe") ~ "West Hararge",
      str_detect(tolower(ZoneName), "west.*arsi|shashemene") ~ "West Arsi",
      str_detect(tolower(ZoneName), "west.*wollega|west.*wellega|west.*wallaga|west.*wolega|west.*wollaga|nejo") ~ "West Wellega",
      str_detect(tolower(ZoneName), "east.*bale|e\\.bale") ~ "East Bale",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Fix specific case in Oromia
  data$zone_gis <- ifelse(
    data$Region_clean == "Oromia" & data$zone_gis == "E/bale", 
    "East Bale", 
    data$zone_gis
  )
  
  # Sidama Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Sidama",
    case_when(
      str_detect(tolower(ZoneName), "sidama|hawasa") ~ "Sidama",
      is.na(ZoneName) ~ NA_character_,
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Somali Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Somali",
    case_when(
      str_detect(tolower(ZoneName), "fafan") ~ "Fafan",
      str_detect(tolower(ZoneName), "shabele|shabeele|shabelle") ~ "Shabelle",
      str_detect(tolower(ZoneName), "jarar") ~ "Jarar",
      str_detect(tolower(ZoneName), "dollo") ~ "Doolo",
      str_detect(tolower(ZoneName), "korehay|korahay|korahe") ~ "Korahe",
      str_detect(tolower(ZoneName), "erar|erer") ~ "Erer",
      str_detect(tolower(ZoneName), "afdher|afder") ~ "Afder",
      str_detect(tolower(ZoneName), "nogob") ~ "Nogob",
      str_detect(tolower(ZoneName), "siti") ~ "Siti",
      str_detect(tolower(ZoneName), "liban") ~ "Liban",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # South Ethiopia Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "South Ethiopia",
    case_when(
      str_detect(tolower(ZoneName), "south.*omo|ari") ~ "South Omo",
      str_detect(tolower(ZoneName), "gamo") ~ "Gamo",
      str_detect(tolower(ZoneName), "segnen|alle|ale") ~ "Alle",
      str_detect(tolower(ZoneName), "wolita|wolayita") ~ "Wolayita",
      str_detect(tolower(ZoneName), "basketo") ~ "Basketo",
      str_detect(tolower(ZoneName), "kore|koree") ~ "Amaro",
      str_detect(tolower(ZoneName), "konso") ~ "Konso",
      str_detect(tolower(ZoneName), "gedio|gedeo") ~ "Gedeo",
      str_detect(tolower(ZoneName), "gardula") ~ "Derashe",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # South West Ethiopia Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "South West Ethiopia",
    case_when(
      str_detect(tolower(ZoneName), "keffa|kefa") ~ "Kefa",
      str_detect(tolower(ZoneName), "sheka") ~ "Sheka",
      str_detect(tolower(ZoneName), "konta") ~ "Konta Special",
      str_detect(tolower(ZoneName), "dawro|dawuro") ~ "Dawuro",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  # Tigray Region
  data$zone_gis <- with(data, ifelse(
    Region_clean == "Tigray",
    case_when(
      str_detect(tolower(ZoneName), "central.*tigray") ~ "Central",
      str_detect(tolower(ZoneName), "eastern.*tigray") ~ "Eastern",
      str_detect(tolower(ZoneName), "mekele") ~ "Mekelle",
      str_detect(tolower(ZoneName), "north.*western.*tigray") ~ "North Western",
      str_detect(tolower(ZoneName), "south.*east.*tigray|south.*east$") ~ "South Eastern",
      str_detect(tolower(ZoneName), "south.*tigray") ~ "Southern",
      str_detect(tolower(ZoneName), "^western$") ~ "Western",
      TRUE ~ ZoneName
    ),
    zone_gis
  ))
  
  return(data)
}

# Apply the mapping function
national_pdsr_clean <- map_zone_gis(national_pdsr_clean)

# ====================================================
# CREATE ZONE_FOR_ANALYSIS VARIABLE
# ====================================================

cat("\n=== Creating zone_for_analysis variable ===\n")

# Create analysis zone (use ZoneName for Addis Ababa, zone_gis for others)
national_pdsr_clean$zone_for_analysis <- with(
  national_pdsr_clean,
  ifelse(Region_clean == "Addis Ababa", ZoneName, zone_gis)
)

# ====================================================
# VERIFICATION AND SUMMARY
# ====================================================

cat("\n=== VERIFICATION ===\n")

# Check Addis Ababa specifically
cat("Addis Ababa zone_for_analysis distribution:\n")
print(table(national_pdsr_clean$zone_for_analysis[national_pdsr_clean$Region_clean == "Addis Ababa"]))

