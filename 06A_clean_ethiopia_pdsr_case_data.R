# ====================================================
# DATA PREPARATION AND CLEANING FOR ETHIOPIAN PDSR DATA
# ====================================================

# Clear environment
rm(list = ls())

# Load libraries
library(sf)
library(dplyr)
library(stringr)

# ====================================================
# 1. LOAD DATA
# ====================================================

# Load cleaned PDSR data
ethio_PDSR_cleaned <- read.csv("C:/Users/neami/OneDrive/Desktop/data/mpdsr_one_corrected/MPDSR_annual_report/data_report/ethio_PDSR_cleaned.csv")

# Load Ethiopia administrative shapefile (ADM2 level)
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path, quiet = TRUE)

# ====================================================
# 2. REGION AND ZONE NAME STANDARDIZATION
# ====================================================

# 2.1 Initial inspection
colnames(ethio_PDSR_cleaned)

# 2.2 Addis Ababa standardization
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = if_else(Region_clean == "Addis Ababa", "Region 14", ZoneName),
    zone_final = case_when(
      Region_clean == "Addis Ababa" & str_detect(tolower(ZoneName), "akaki.*kality") ~ "Akaki Kaliti",
      Region_clean == "Addis Ababa" & str_detect(tolower(ZoneName), "kolfe.*keranyo") ~ "Kolfe Keraniyo",
      Region_clean == "Addis Ababa" & str_detect(tolower(ZoneName), "nifas.*silk") ~ "Nefas Silk Lafto",
      Region_clean == "Addis Ababa" & str_detect(tolower(ZoneName), "bole") & ZoneName != "Bole" ~ "Bole",
      TRUE ~ ZoneName
    )
  )

# 2.3 Tigray region corrections
sno_tigray <- c(13113, 15034, 15031, 15032, 15029, 15033, 15030)
ethio_PDSR_cleaned$Region_clean[ethio_PDSR_cleaned$S.No %in% sno_tigray] <- "Tigray"

# Additional Tigray location corrections
tigray_locations <- c("Humera", "Humera general hospital", "Humera General Hospita", 
                      "Kafita Humera", "Setetu Humera", "Dansha", "Dansha Town")
ethio_PDSR_cleaned$Region_clean <- ifelse(
  ethio_PDSR_cleaned$ZoneName %in% tigray_locations |
    ethio_PDSR_cleaned$Facility %in% tigray_locations,
  "Tigray",
  ethio_PDSR_cleaned$Region_clean
)

# ====================================================
# 3. REGION-SPECIFIC ZONE MAPPING
# ====================================================

# 3.1 Amhara Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = if_else(
      Region_clean == "Amhara",
      case_when(
        # Standard zones
        ZoneName == "North Gojjam" ~ "West Gojam",
        ZoneName %in% c("oromo special zone", "Oromo Special Zone") ~ "Oromia",
        ZoneName == "S/Gongar" ~ "South Gondar",
        
        # Pattern matches
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
        str_detect(tolower(ZoneName), "north.*gojjam|n/gojam|n\\.gojam|n/goja|N/GOJAM|N\n/Goja") ~ "West Gojam",
        
        # City mappings
        str_detect(tolower(ZoneName), "bahir") ~ "West Gojam",
        str_detect(tolower(ZoneName), "dessie|dese|kombolcha") ~ "South Wello",
        str_detect(tolower(ZoneName), "gondar") ~ "Central Gondar",
        str_detect(tolower(ZoneName), "markos") ~ "East Gojam",
        str_detect(tolower(ZoneName), "birhan|brehan") ~ "North Shewa (AM)",
        str_detect(tolower(ZoneName), "tabor|detabor") ~ "South Gondar",
        str_detect(tolower(ZoneName), "woldia") ~ "North Wello",
        
        # Default
        TRUE ~ ZoneName
      ),
      ZoneName
    )
  )

# 3.2 Benishangul-Gumuz Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Benishangul-Gumuz" & str_detect(tolower(ZoneName), "assosa|assosal|addis ketema") ~ "Assosa",
      Region_clean == "Benishangul-Gumuz" & str_detect(tolower(ZoneName), "kamashi") ~ "Kamashi",
      Region_clean == "Benishangul-Gumuz" & str_detect(tolower(ZoneName), "maokomo|maoko|mao komo") ~ "Mao Komo Special",
      Region_clean == "Benishangul-Gumuz" & str_detect(tolower(ZoneName), "metekel|pawi") ~ "Metekel",
      TRUE ~ zone_gis
    )
  )

# 3.3 Central Ethiopia Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    Region_clean = case_when(
      Region_clean == "Central Ethiopia Region" ~ "Central Ethiopia",
      TRUE ~ Region_clean
    ),
    zone_gis = if_else(
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
    )
  )

# 3.4 Dire Dawa Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Dire Dawa" & ZoneName == "Dire Dawa" ~ "Dire Dawa urban",
      Region_clean == "Dire Dawa" & is.na(ZoneName) ~ NA_character_,
      TRUE ~ zone_gis
    )
  )

# 3.5 Gambella Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Gambella" & str_detect(tolower(ZoneName), "gambella.*special|anuak") ~ "Agnewak",
      Region_clean == "Gambella" & str_detect(tolower(ZoneName), "itang.*special") ~ "Itang Special woreda",
      Region_clean == "Gambella" & str_detect(tolower(ZoneName), "mejeng|majang") ~ "Majang",
      Region_clean == "Gambella" & str_detect(tolower(ZoneName), "nuer") ~ "Nuwer",
      TRUE ~ zone_gis
    )
  )

# 3.6 Oromia Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = if_else(
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
    ),
    zone_gis = ifelse(Region_clean == "Oromia" & zone_gis == "E/bale", "East Bale", zone_gis)
  )

# 3.7 Sidama Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Sidama" & str_detect(tolower(ZoneName), "sidama|hawasa") ~ "Sidama",
      Region_clean == "Sidama" & is.na(ZoneName) ~ NA_character_,
      TRUE ~ zone_gis
    )
  )

# 3.8 Somali Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "fafan") ~ "Fafan",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "shabele|shabeele|shabelle") ~ "Shabelle",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "jarar") ~ "Jarar",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "dollo") ~ "Doolo",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "korehay|korahay|korahe") ~ "Korahe",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "erar|erer") ~ "Erer",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "afdher|afder") ~ "Afder",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "nogob") ~ "Nogob",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "siti") ~ "Siti",
      Region_clean == "Somali" & str_detect(tolower(ZoneName), "liban") ~ "Liban",
      TRUE ~ zone_gis
    )
  )

# 3.9 South Ethiopia Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "south.*omo|ari") ~ "South Omo",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "gamo") ~ "Gamo",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "segnen|alle|ale") ~ "Alle",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "wolita|wolayita") ~ "Wolayita",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "basketo") ~ "Basketo",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "kore|koree") ~ "Amaro",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "konso") ~ "Konso",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "gedio|gedeo") ~ "Gedeo",
      Region_clean == "South Ethiopia" & str_detect(tolower(ZoneName), "gardula") ~ "Derashe",
      TRUE ~ zone_gis
    )
  )

# 3.10 South West Ethiopia Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "South West Ethiopia" & str_detect(tolower(ZoneName), "keffa|kefa") ~ "Kefa",
      Region_clean == "South West Ethiopia" & str_detect(tolower(ZoneName), "sheka") ~ "Sheka",
      Region_clean == "South West Ethiopia" & str_detect(tolower(ZoneName), "konta") ~ "Konta Special",
      Region_clean == "South West Ethiopia" & str_detect(tolower(ZoneName), "dawro|dawuro") ~ "Dawuro",
      TRUE ~ zone_gis
    )
  )

# 3.11 Tigray Region
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    zone_gis = case_when(
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "central.*tigray") ~ "Central",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "eastern.*tigray") ~ "Eastern",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "mekele") ~ "Mekelle",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "north.*western.*tigray") ~ "North Western",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "south.*east.*tigray|south.*east$") ~ "South Eastern",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "south.*tigray") ~ "Southern",
      Region_clean == "Tigray" & str_detect(tolower(ZoneName), "^western$") ~ "Western",
      TRUE ~ zone_gis
    )
  )

# ====================================================
# 4. VARIABLE CLEANING AND IMPUTATION
# ====================================================

# 4.1 Residence of deceased parents
ethio_PDSR_cleaned <- ethio_PDSR_cleaned %>%
  mutate(
    Residenceofdeceasedparents_clean = case_when(
      tolower(Residenceofdeceasedparents) %in% c("urban", "urbun") ~ "Urban",
      tolower(Residenceofdeceasedparents) %in% c("rural", "rular") ~ "Rural",
      TRUE ~ Residenceofdeceasedparents
    ),
    # Impute missing values based on region
    Residenceofdeceasedparents_clean = ifelse(
      is.na(Residenceofdeceasedparents_clean),
      ifelse(Region_Res %in% c("Addis Ababa", "Dire Dawa"), "Urban", "Rural"),
      Residenceofdeceasedparents_clean
    )
  )

# 4.2 PDRF extraction source
ethio_PDSR_cleaned$ThisPDRFisextractedfrom[ethio_PDSR_cleaned$ThisPDRFisextractedfrom == "FBPD"] <- "FBAF"
ethio_PDSR_cleaned$ThisPDRFisextractedfrom <- ifelse(
  ethio_PDSR_cleaned$ThisPDRFisextractedfrom %in% c("Missed", NA),
  ifelse(ethio_PDSR_cleaned$Reportinghealthfacilitytype == "Government Hospital", "FBAF", "VA"),
  ethio_PDSR_cleaned$ThisPDRFisextractedfrom
)

# 4.3 Gestational age
ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks <- as.numeric(ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks)
ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks <- ifelse(
  ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks < 28,
  NA,
  ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks
)
median_gest_age <- median(ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks, na.rm = TRUE)
ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks[is.na(ethio_PDSR_cleaned$Estimatedgestationalageatdeliveryinweeks)] <- median_gest_age

# 4.4 Sex of deceased
ethio_PDSR_cleaned$Sexofthedeceased <- ifelse(
  ethio_PDSR_cleaned$Sexofthedeceased %in% c("female", "Female"), "Female",
  ifelse(ethio_PDSR_cleaned$Sexofthedeceased %in% c("male", "Male"), "Male", NA)
)
sex_counts <- table(ethio_PDSR_cleaned$Sexofthedeceased)
sex_props <- prop.table(sex_counts)
missing_idx <- which(is.na(ethio_PDSR_cleaned$Sexofthedeceased))
set.seed(123)
ethio_PDSR_cleaned$Sexofthedeceased[missing_idx] <- sample(
  names(sex_props), 
  size = length(missing_idx), 
  replace = TRUE, 
  prob = sex_props
)

# 4.5 Place of death
ethio_PDSR_cleaned$Placeofdeath <- tolower(ethio_PDSR_cleaned$Placeofdeath)
ethio_PDSR_cleaned$Placeofdeath <- ifelse(
  grepl("health center|health centre|health_center", ethio_PDSR_cleaned$Placeofdeath), "Health Center",
  ifelse(
    grepl("hospital", ethio_PDSR_cleaned$Placeofdeath), "Hospital",
    ifelse(
      grepl("home", ethio_PDSR_cleaned$Placeofdeath), "Home",
      ifelse(
        grepl("on transit|during referal|on trasit", ethio_PDSR_cleaned$Placeofdeath), "On Transit",
        ethio_PDSR_cleaned$Placeofdeath
      )
    )
  )
)
ethio_PDSR_cleaned$Placeofdeath <- ifelse(
  is.na(ethio_PDSR_cleaned$Placeofdeath) | ethio_PDSR_cleaned$Placeofdeath == "missed",
  ifelse(ethio_PDSR_cleaned$Reportinghealthfacilitytype == "Government Hospital", "Hospital", "Health Center"),
  ethio_PDSR_cleaned$Placeofdeath
)
ethio_PDSR_cleaned$Placeofdeath <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ethio_PDSR_cleaned$Placeofdeath, perl = TRUE)

# 4.6 Maternal outcome
ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive <- ifelse(
  ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive %in% c("yes", "Yes"), "Yes",
  ifelse(ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive %in% c("no", "No"), "No", NA)
)
alive_counts <- table(ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive)
alive_props <- prop.table(alive_counts)
missing_idx <- which(is.na(ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive))
set.seed(123)
ethio_PDSR_cleaned$Isthemotherofthedeceasedperinatealive[missing_idx] <- sample(
  names(alive_props),
  size = length(missing_idx),
  replace = TRUE,
  prob = alive_props
)

# 4.7 Mother's age
ethio_PDSR_cleaned$AgeofthemotherYrs <- as.numeric(ethio_PDSR_cleaned$AgeofthemotherYrs)
ethio_PDSR_cleaned$AgeofthemotherYrs <- ifelse(ethio_PDSR_cleaned$AgeofthemotherYrs < 13, NA, ethio_PDSR_cleaned$AgeofthemotherYrs)
median_age <- median(ethio_PDSR_cleaned$AgeofthemotherYrs, na.rm = TRUE)
ethio_PDSR_cleaned$AgeofthemotherYrs[is.na(ethio_PDSR_cleaned$AgeofthemotherYrs)] <- median_age

# 4.8 Parity
ethio_PDSR_cleaned$Parity <- as.numeric(ethio_PDSR_cleaned$Parity)
median_parity <- median(ethio_PDSR_cleaned$Parity, na.rm = TRUE)
ethio_PDSR_cleaned$Parity[is.na(ethio_PDSR_cleaned$Parity)] <- median_parity

# 4.9 Mother's religion
ethio_PDSR_cleaned$ReligioNofthemother <- tolower(ethio_PDSR_cleaned$ReligioNofthemother)
ethio_PDSR_cleaned$ReligioNofthemother <- ifelse(
  ethio_PDSR_cleaned$ReligioNofthemother %in% c("orthodox", "ortodox", "christian"), "Orthodox",
  ifelse(
    ethio_PDSR_cleaned$ReligioNofthemother %in% c("muslim", "musilim"), "Muslim",
    ifelse(
      ethio_PDSR_cleaned$ReligioNofthemother %in% c("protestant", "adventist"), "Protestant",
      ifelse(
        ethio_PDSR_cleaned$ReligioNofthemother == "catholic", "Catholic",
        ifelse(
          ethio_PDSR_cleaned$ReligioNofthemother %in% c("pagan", "traditional", "waqefata"), "Traditional",
          ifelse(ethio_PDSR_cleaned$ReligioNofthemother == "other", "Other", NA)
        )
      )
    )
  )
)
ethio_PDSR_cleaned$ReligioNofthemother[ethio_PDSR_cleaned$Region_clean == "Somali"] <- "Muslim"
religion_counts <- table(ethio_PDSR_cleaned$ReligioNofthemother)
religion_props <- prop.table(religion_counts)
missing_idx <- which(is.na(ethio_PDSR_cleaned$ReligioNofthemother))
set.seed(123)
ethio_PDSR_cleaned$ReligioNofthemother[missing_idx] <- sample(
  names(religion_props),
  size = length(missing_idx),
  replace = TRUE,
  prob = religion_props
)
# Remove unused column
ethio_PDSR_cleaned$ReligioNofotherspecified <- NULL

# 4.10 Educational status
ethio_PDSR_cleaned$Educationalstatusofthemother <- tolower(ethio_PDSR_cleaned$Educationalstatusofthemother)
ethio_PDSR_cleaned$Educationalstatusofthemother <- ifelse(
  grepl("college", ethio_PDSR_cleaned$Educationalstatusofthemother), "College and above",
  ifelse(
    grepl("high", ethio_PDSR_cleaned$Educationalstatusofthemother), "High school",
    ifelse(
      grepl("elementary", ethio_PDSR_cleaned$Educationalstatusofthemother), "Elementary",
      ifelse(
        grepl("no formal education, but can read|can read and write", ethio_PDSR_cleaned$Educationalstatusofthemother), 
        "No formal, but can read & write",
        ifelse(grepl("^no formal", ethio_PDSR_cleaned$Educationalstatusofthemother), "No formal education", NA)
      )
    )
  )
)
ethio_PDSR_cleaned$Educationalstatusofthemother[
  is.na(ethio_PDSR_cleaned$Educationalstatusofthemother) & ethio_PDSR_cleaned$Region_clean == "Addis Ababa"
] <- "High school"
edu_counts <- table(ethio_PDSR_cleaned$Educationalstatusofthemother)
edu_props <- prop.table(edu_counts)
missing_idx <- which(is.na(ethio_PDSR_cleaned$Educationalstatusofthemother))
set.seed(123)
ethio_PDSR_cleaned$Educationalstatusofthemother[missing_idx] <- sample(
  names(edu_props),
  size = length(missing_idx),
  replace = TRUE,
  prob = edu_props
)

# 4.11 ANC visits
ethio_PDSR_cleaned$ANC_visit <- ifelse(
  ethio_PDSR_cleaned$ANC_visit %in% c("Yes"), "Yes",
  ifelse(ethio_PDSR_cleaned$ANC_visit %in% c("No", "no"), "No", NA)
)
missing_idx <- which(is.na(ethio_PDSR_cleaned$ANC_visit))
addis_idx <- missing_idx[ethio_PDSR_cleaned$Region_clean[missing_idx] == "Addis Ababa"]
ethio_PDSR_cleaned$ANC_visit[addis_idx] <- "Yes"
remaining_idx <- setdiff(missing_idx, addis_idx)
anc_counts <- table(ethio_PDSR_cleaned$ANC_visit)
anc_props <- prop.table(anc_counts)
set.seed(123)
ethio_PDSR_cleaned$ANC_visit[remaining_idx] <- sample(
  names(anc_props),
  size = length(remaining_idx),
  replace = TRUE,
  prob = anc_props
)

# 4.12 Number of ANC visits
anc_num <- "NumberofANCvisitsinrelationtothedeceasedcasereport0ifNoANCvisits"
ethio_PDSR_cleaned[[anc_num]] <- as.numeric(ethio_PDSR_cleaned[[anc_num]])
ethio_PDSR_cleaned[[anc_num]][ethio_PDSR_cleaned[[anc_num]] > 8] <- 8
ethio_PDSR_cleaned[[anc_num]][ethio_PDSR_cleaned$ANC_visit == "No"] <- 0
median_anc <- median(
  ethio_PDSR_cleaned[[anc_num]][ethio_PDSR_cleaned[[anc_num]] >= 1 & ethio_PDSR_cleaned[[anc_num]] <= 8],
  na.rm = TRUE
)
ethio_PDSR_cleaned[[anc_num]][
  ethio_PDSR_cleaned$ANC_visit == "Yes" & (is.na(ethio_PDSR_cleaned[[anc_num]]) | ethio_PDSR_cleaned[[anc_num]] == 0)
] <- median_anc

# 4.13 TT vaccination
tt_var <- "NumberofTTvaccineduringthepregnancyofthedeceasedcase"
ethio_PDSR_cleaned[[tt_var]] <- tolower(ethio_PDSR_cleaned[[tt_var]])
ethio_PDSR_cleaned[[tt_var]] <- ifelse(
  ethio_PDSR_cleaned[[tt_var]] %in% c("no", "no tt", "nott"), "No TT",
  ifelse(
    ethio_PDSR_cleaned[[tt_var]] %in% c("one", "one tt", "onett"), "One TT",
    ifelse(ethio_PDSR_cleaned[[tt_var]] %in% c("two & more", "two and above", "two and above tt"), "Two and above TT", NA)
  )
)
missing_idx <- which(is.na(ethio_PDSR_cleaned[[tt_var]]))
no_anc_idx <- missing_idx[ethio_PDSR_cleaned$ANC_visit[missing_idx] == "No"]
ethio_PDSR_cleaned[[tt_var]][no_anc_idx] <- "No TT"
yes_anc_idx <- setdiff(missing_idx, no_anc_idx)
tt_counts <- table(ethio_PDSR_cleaned[[tt_var]][ethio_PDSR_cleaned$ANC_visit == "Yes" & !is.na(ethio_PDSR_cleaned[[tt_var]])])
tt_props <- prop.table(tt_counts)
set.seed(123)
ethio_PDSR_cleaned[[tt_var]][yes_anc_idx] <- sample(
  names(tt_props),
  size = length(yes_anc_idx),
  replace = TRUE,
  prob = tt_props
)

# 4.14 Mode of delivery
mode_var <- "Modeofdeliveryofthedeceasedbaby"
ethio_PDSR_cleaned[[mode_var]] <- tolower(ethio_PDSR_cleaned[[mode_var]])
ethio_PDSR_cleaned[[mode_var]] <- ifelse(
  grepl("c/s|c\\\\s|c/s|cessarian|cs", ethio_PDSR_cleaned[[mode_var]]), "C/S",
  ifelse(
    grepl("svd|vaginal", ethio_PDSR_cleaned[[mode_var]]), "SVD",
    ifelse(
      grepl("forceps|instrumental|operative", ethio_PDSR_cleaned[[mode_var]]), "Operative vaginal delivery",
      ifelse(grepl("vaccum|vacume|vaccume|vacuum", ethio_PDSR_cleaned[[mode_var]]), "Vacuum", NA)
    )
  )
)
missing_idx <- which(is.na(ethio_PDSR_cleaned[[mode_var]]))
mode_counts <- table(ethio_PDSR_cleaned[[mode_var]])
mode_props <- prop.table(mode_counts)
set.seed(123)
ethio_PDSR_cleaned[[mode_var]][missing_idx] <- sample(
  names(mode_props),
  size = length(missing_idx),
  replace = TRUE,
  prob = mode_props
)

# ====================================================
# 5. FINAL DATA CHECK
# ====================================================

# Display summary of key variables
cat("Final data check:\n")
cat("=================\n")
cat("Region distribution:\n")
print(table(ethio_PDSR_cleaned$Region_clean))

cat("\nZone standardization check:\n")
print(table(ethio_PDSR_cleaned$zone_gis))

cat("\nResidence type:\n")
print(table(ethio_PDSR_cleaned$Residenceofdeceasedparents_clean, useNA = "always"))

cat("\nData cleaning complete. Dataset ready for analysis.\n")