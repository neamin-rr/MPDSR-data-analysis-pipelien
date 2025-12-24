# ==============================================================================
# Maternal and PERINATAL DEATH SURVEILLANCE AND RESPONSE (MPDSR) DATA ANALYSIS
# Ethiopia, 2006-2017 Ethiopian Fiscal Years
# ==============================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(knitr)
library(kableExtra)
library(sf)
library(patchwork)
library(ggspatial)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(gt)
library(stringr)

# ==============================================================================
# SECTION 1: MDRF DATA EXPLORATION AND TREND ANALYSIS
# ==============================================================================

cat("SECTION 1: MDRF DATA TREND ANALYSIS\n")
cat("===================================\n\n")

# Initial data exploration
cat("1.1 Data Structure Overview:\n")
head(Final_national_MDSR_data_20251216_cleaned)
str(Final_national_MDSR_data_20251216_cleaned[
  c("MDRFextractedfrom", "EFY", "EFY_Quarter", "EFY_Quarter_Label")
])

cat("\n1.2 Distribution by Ethiopian Fiscal Year:\n")
table(Final_national_MDSR_data_20251216_cleaned$EFY)

cat("\n1.3 Distribution by EFY Quarter:\n")
table(Final_national_MDSR_data_20251216_cleaned$EFY_Quarter_Label)

cat("\n1.4 Data Source by EFY Quarter:\n")
table(Final_national_MDSR_data_20251216_cleaned$MDRFextractedfrom,
      Final_national_MDSR_data_20251216_cleaned$EFY_Quarter_Label)

# Clean up unnecessary objects
rm(quarter_source_counts, quarter_source_props, quarter_source_table, quarter_totals)

# Calculate proportions by quarter
quarter_label_props <- Final_national_MDSR_data_20251216_cleaned %>%
  group_by(EFY_Quarter_Label, MDRFextractedfrom) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(
    Final_national_MDSR_data_20251216_cleaned %>%
      group_by(EFY_Quarter_Label) %>%
      summarise(total = n(), .groups = "drop"),
    by = "EFY_Quarter_Label"
  ) %>%
  mutate(
    proportion = round(count / total * 100, 1),
    EFY_Quarter_Label = factor(EFY_Quarter_Label, 
                               levels = unique(EFY_Quarter_Label),
                               ordered = TRUE)
  ) %>%
  arrange(EFY_Quarter_Label, MDRFextractedfrom) %>%
  select(EFY_Quarter_Label, MDRFextractedfrom, count, total, proportion)

# Display Table 1
cat("\nTable 1: Maternal Deaths by Quarter and Data Source\n")
kable(quarter_label_props, 
      caption = "Table 1: Maternal Deaths by Quarter and Data Source",
      digits = 1,
      align = c("l", "l", "r", "r", "r")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  )

# Create trend plot
quarter_label_props <- quarter_label_props %>%
  mutate(EFY_Quarter_Label = factor(EFY_Quarter_Label, 
                                    levels = unique(EFY_Quarter_Label),
                                    ordered = TRUE))

cat("\nGenerating Figure 1: MDRF Source Trends...\n")

ggplot(quarter_label_props, aes(x = EFY_Quarter_Label, y = proportion, group = MDRFextractedfrom)) +
  geom_point(aes(color = MDRFextractedfrom), size = 3) +
  geom_line(aes(color = MDRFextractedfrom), size = 1.2) +
  geom_smooth(aes(color = MDRFextractedfrom), method = "loess", se = FALSE, 
              linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Facility-Based Abstraction Form" = "#1f77b4",
                                "Verbal Autopsy" = "#ff7f0e")) +
  labs(title = "Figure 1: Proportion of MDRF Sources Over Quarters (2006-2017 EFY)",
       x = "EFY Quarter",
       y = "Proportion (%)",
       color = "Source") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# ==============================================================================
# SECTION 2: 2017 PDSR DATA PREPARATION AND ZERO NOTIFICATION ZONES
# ==============================================================================

cat("\n\nSECTION 2: 2017 PDSR DATA ANALYSIS\n")
cat("===================================\n\n")

# Check EFY distribution in PDSR data
cat("2.1 EFY Distribution in PDSR Data:\n")
table(corrected_PDSR_data_2010_2017$EFY)

# Prepare 2017 data
PDSR_2017 <- corrected_PDSR_data_2010_2017 %>%
  filter(EFY == 2017) %>%
  mutate(Region_clean = ifelse(is.na(Region_clean), "Amhara", Region_clean))

cat("\n2.2 2017 PDSR Data Summary:\n")
cat("Total records in 2017:", nrow(PDSR_2017), "\n")
cat("Regions represented:", length(unique(PDSR_2017$Region_clean)), "\n\n")

# Identify zero notification zones
MPDSR_2017_zero_perinatal <- MPDSR_updated_2017_notfication %>%
  filter(sum_perinatal_deaths == 0)

cat("2.3 Zones with Zero Perinatal Death Notifications:\n")
cat("Number of zones:", nrow(MPDSR_2017_zero_perinatal), "\n\n")

# Table 2: Silent zones summary
silent_zones_summary <- MPDSR_2017_zero_perinatal %>%
  group_by(Region, Zone_corrected) %>%
  summarise(`Perinatal Deaths` = sum(count, na.rm = TRUE)) %>%
  ungroup()

cat("Table 2: Zones with Zero Perinatal Death Notifications (2017 EFY)\n")
silent_zones_summary %>%
  kable(
    caption = "Table 2: Zones with Zero Perinatal Death Notifications, 2017 EFY",
    booktabs = TRUE,
    align = c("l", "l", "c")
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 12,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Create sunburst visualization for silent zones
cat("\nGenerating Sunburst Visualization for Silent Zones...\n")

sunburst_data <- bind_rows(
  # Parent regions
  MPDSR_2017_zero_perinatal %>%
    group_by(Region) %>%
    summarise(value = sum(count, na.rm = TRUE)) %>%
    rename(label = Region) %>%
    mutate(parent = ""),
  # Child zones
  MPDSR_2017_zero_perinatal %>%
    select(label = Zone_corrected, parent = Region, value = count)
)

# Generate colors for regions
region_colors <- RColorBrewer::brewer.pal(n = max(3, length(unique(sunburst_data$label[sunburst_data$parent == ""]))), "Set2")
names(region_colors) <- sunburst_data$label[sunburst_data$parent == ""]

# Assign colors
sunburst_data <- sunburst_data %>%
  rowwise() %>%
  mutate(
    color = if(parent == "") {
      region_colors[label]
    } else {
      lighten(region_colors[parent], amount = 0.3)
    }
  )

# Create sunburst plot
fig <- plot_ly(
  data = sunburst_data,
  ids = ~label,
  labels = ~label,
  parents = ~parent,
  values = ~value,
  type = 'sunburst',
  branchvalues = 'total',
  marker = list(
    colors = ~color,
    line = list(width = 1, color = "white")
  ),
  insidetextfont = list(size = 14, color = 'black')
) %>%
  layout(
    title = list(
      text = "Figure 2: Silent Zones by Region for Perinatal Deaths in 2017 EFY",
      x = 0.5,
      xanchor = "center",
      yanchor = "top",
      font = list(size = 20)
    ),
    uniformtext = list(minsize = 12, mode = 'hide'),
    margin = list(t = 45, l = 22, r = 22, b = 22)
  )

fig

# ==============================================================================
# SECTION 3: TOP ZONES ANALYSIS (HIGH BURDEN)
# ==============================================================================

cat("\n\nSECTION 3: HIGH BURDEN ZONES ANALYSIS\n")
cat("======================================\n\n")

# Check column names
cat("3.1 Available columns in MPDSR data:\n")
print(colnames(MPDSR_updated_2017_notfication)[1:10])

# Top 20 zones by perinatal deaths
perinatal_summary <- MPDSR_updated_2017_notfication %>%
  group_by(Region, Zone_corrected) %>%
  summarise(`Perinatal Deaths` = sum(sum_perinatal_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(`Perinatal Deaths`))

top_20_zones <- perinatal_summary %>%
  slice_max(order_by = `Perinatal Deaths`, n = 20)

# Table 3: Top 20 zones
total_row <- data.frame(
  Region = "Total",
  Zone_corrected = "",
  `Perinatal Deaths` = sum(top_20_zones$`Perinatal Deaths`)
)

top_20_zones_total <- bind_rows(top_20_zones, total_row)

cat("\nTable 3: Top 20 Zones by Perinatal Deaths in 2017 EFY\n")
top_20_zones_total %>%
  kable(
    caption = "Table 3: Top 20 Zones by Perinatal Deaths in 2017 EFY",
    booktabs = TRUE,
    align = c("l", "l", "c")
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 12,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Create bar plot for top 20 zones
cat("\nGenerating Figure 3: Top 20 Zones Bar Plot...\n")
top_20_zones <- top_20_zones %>%
  arrange(`Perinatal Deaths`) %>%
  mutate(Zone_corrected = factor(Zone_corrected, levels = Zone_corrected))

ggplot(top_20_zones, aes(x = Zone_corrected, y = `Perinatal Deaths`, fill = Region)) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(aes(label = `Perinatal Deaths`), 
            hjust = 1.1, color = "black", size = 3.8, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Figure 3: Top 20 Zones by Perinatal Deaths Notification in 2017 EFY",
    x = "Zone",
    y = "Perinatal Deaths",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12)
  )

# ==============================================================================
# SECTION 4: REGIONAL PERFORMANCE ANALYSIS
# ==============================================================================

cat("\n\nSECTION 4: REGIONAL PERFORMANCE METRICS\n")
cat("========================================\n\n")

# Check region distribution
cat("4.1 Region Distribution in 2017 PDSR Data:\n")
table(PDSR_2017$Region_clean, useNA = "always")

# Prepare region counts
region_counts <- PDSR_2017 %>%
  group_by(Region_clean) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count))

# Standardize region names
region_counts_fixed <- region_counts %>%
  mutate(
    region_std = toupper(Region_clean),
    region_std = case_when(
      region_std == "SOUTH ETHIOPIA" ~ "SOUTHERN ETHIOPIA",
      region_std == "BENISHANGUL-GUMUZ" ~ "BENISHANGUL GUMZ",
      TRUE ~ region_std
    ),
    perinatal_deaths_reviewed = Count
  ) %>%
  select(region_std, perinatal_deaths_reviewed)

# Check for mismatches
cat("\n4.2 Checking for region name mismatches:\n")
setdiff(region_counts_fixed$region_std, merged_region_2017_performace$region_std)

# Merge with performance data
merged_region_2017_performance_final <- merged_region_2017_performace %>%
  left_join(region_counts_fixed, by = "region_std") %>%
  mutate(perinatal_deaths_reviewed = replace_na(perinatal_deaths_reviewed, 0))

# Calculate national total (excluding national row)
national_perinatal_reviewed <- merged_region_2017_performance_final %>%
  filter(region_std != "ETHIOPIA") %>%
  summarise(total = sum(perinatal_deaths_reviewed, na.rm = TRUE)) %>%
  pull(total)

# Update national row
merged_region_2017_performance_final <- merged_region_2017_performance_final %>%
  mutate(
    perinatal_deaths_reviewed = if_else(
      region_std == "ETHIOPIA",
      national_perinatal_reviewed,
      perinatal_deaths_reviewed
    )
  )

# Display national summary
cat("\n4.3 National Summary:\n")
merged_region_2017_performance_final %>%
  filter(region_std == "ETHIOPIA") %>%
  select(
    region_std,
    perinatal_deaths_reviewed,
    perinatal_deaths_notified,
    expected_perinatal_deaths
  ) %>%
  print()

# Create performance metrics table
table_perinatal_2017_fmt <- merged_region_2017_performance_final %>%
  mutate(
    prop_notified_est = perinatal_deaths_notified / expected_perinatal_deaths * 100,
    prop_reviewed_notified = if_else(
      perinatal_deaths_notified > 0,
      perinatal_deaths_reviewed / perinatal_deaths_notified * 100,
      0
    ),
    prop_reviewed_est = perinatal_deaths_reviewed / expected_perinatal_deaths * 100,
    Region = case_when(
      region_std == "ETHIOPIA" ~ "National",
      region_std == "SOUTH WEST ETHIOPIA" ~ "SWEPRS",
      TRUE ~ str_to_title(region_std)
    )
  ) %>%
  transmute(
    Region,
    `Estimated births` = round(estimated_live_births),
    `Estimated extended perinatal deaths` = round(expected_perinatal_deaths),
    `Proportion of Notified P.D. Vs. Estimated` = 
      paste0(perinatal_deaths_notified, " (", round(prop_notified_est, 1), "%)"),
    `Proportion of reported PDRF vs. Notified P.D.` = 
      paste0(perinatal_deaths_reviewed, " (", round(prop_reviewed_notified, 1), "%)"),
    `% of reported PDRF estimated` = paste0(round(prop_reviewed_est, 1), "%")
  ) %>%
  arrange(Region != "National", Region)

# Table 4: Regional Performance (using gt for better formatting)
cat("\nTable 4: Regional Performance Metrics (2017 EFY)\n")
gt_table <- table_perinatal_2017_fmt %>%
  gt() %>%
  tab_header(
    title = md("**Table 4: Perinatal Death Surveillance and Response Performance, 2017**"),
    subtitle = md("Regional and national summary")
  ) %>%
  cols_align(
    align = "left",
    columns = "Region"
  ) %>%
  cols_align(
    align = "right",
    columns = -"Region"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Region == "National")
  ) %>%
  tab_source_note(
    md("**Note:** Percentages are calculated using estimated extended perinatal deaths as denominators.")
  ) %>%
  opt_table_outline()

gt_table

# Save performance data
cat("\n4.4 Saving processed performance data...\n")
saveRDS(merged_region_2017_performance_final, "merged_region_2017_performance_final.rds")
cat("✓ Saved to: merged_region_2017_performance_final.rds\n")

# ==============================================================================
# SECTION 5: ZONE-LEVEL REVIEW ANALYSIS
# ==============================================================================

cat("\n\nSECTION 5: ZONE-LEVEL REVIEW ANALYSIS\n")
cat("=====================================\n\n")

# Check zone distribution
cat("5.1 Zone Distribution in 2017 PDSR Data:\n")
cat("Number of unique zones:", length(unique(PDSR_2017$zone_for_analysis)), "\n")
table(PDSR_2017$Region_clean, useNA = "always")

# Zone-level review counts
region_zone_counts <- PDSR_2017 %>%
  group_by(Region_clean, zone_for_analysis) %>%
  summarise(count = n(), .groups = "drop")

# Top 20 zones by reviews
top_20_review_zones <- region_zone_counts %>%
  arrange(desc(count)) %>%
  slice(1:20)

# Table 5: Top 20 zones by reviews
total_review_row <- top_20_review_zones %>%
  summarise(
    Region_clean = "Total",
    zone_for_analysis = "",
    count = sum(count, na.rm = TRUE)
  )

top_20_review_table <- bind_rows(top_20_review_zones, total_review_row)

cat("\nTable 5: Top 20 Zones by Perinatal Deaths Reviewed in 2017 EFY\n")
top_20_review_table %>%
  kable(
    col.names = c("Region", "Zone", "Perinatal Deaths Reviewed"),
    align = c("l", "l", "r"),
    caption = "Table 5: Top 20 Zones by Perinatal Deaths Reviewed in 2017 EFY"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

# Create bar plot for top 20 review zones
cat("\nGenerating Figure 4: Top 20 Review Zones Bar Plot...\n")
top_20_review_zones <- top_20_review_zones %>%
  arrange(count) %>%
  mutate(zone_for_analysis = factor(zone_for_analysis, levels = zone_for_analysis))

ggplot(top_20_review_zones, aes(x = zone_for_analysis, y = count, fill = Region_clean)) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(aes(label = count), 
            hjust = 1.1, color = "black", size = 3.8, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Figure 4: Top 20 Zones by Perinatal Deaths Reviewed in 2017 EFY",
    x = "Zone",
    y = "Perinatal Deaths Reviewed",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12)
  )

# ==============================================================================
# SECTION 6: SPATIAL MAPPING PREPARATION
# ==============================================================================

cat("\n\nSECTION 6: SPATIAL MAPPING PREPARATION\n")
cat("=======================================\n\n")

# Check zone_gis column
cat("6.1 Checking zone_gis column:\n")
cat("Number of NA values in zone_gis:", sum(is.na(PDSR_2017$zone_gis)), "\n")

# Prepare GIS zone counts
region_zone_gis_counts <- PDSR_2017 %>%
  group_by(Region_clean, zone_gis) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    zone_gis = replace_na(zone_gis, "Awi"),  # replace NA with "Awi"
    zone_gis = ifelse(Region_clean == "Addis Ababa", "Region 14", zone_gis)
  ) %>%
  group_by(Region_clean, zone_gis) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

# Prepare review data for merging
review_data <- region_zone_gis_counts %>%
  rename(Zone_corrected = zone_gis,
         Region = Region_clean,
         perinatal_deaths_reviewed = count)

# Merge with notification data
merged_MPDSR_2017 <- summary_yearly_MPDSR_zone_cleaned_2017_map %>%
  left_join(review_data, by = c("Region", "Zone_corrected")) %>%
  mutate(
    perinatal_deaths_reviewed = replace_na(perinatal_deaths_reviewed, 0)
  )

# Check zone matching with shapefile
cat("\n6.2 Loading shapefile and checking zone matching...\n")
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path, quiet = TRUE)

# Check for mismatches
zones_shp <- eth_adm2$ADM2_EN %>% unique() %>% sort()
zones_MPDSR <- merged_MPDSR_2017$Zone_corrected %>% unique() %>% sort()

cat("Zones in shapefile:", length(zones_shp), "\n")
cat("Zones in MPDSR data:", length(zones_MPDSR), "\n")

# Fix known mismatches
merged_MPDSR_2017 <- merged_MPDSR_2017 %>%
  mutate(Zone_corrected = ifelse(Zone_corrected == "West Hararghe", "West Hararge", Zone_corrected))

# Save merged data
saveRDS(merged_MPDSR_2017, "merged_NR_PDSR_2017.rds")
cat("\n✓ Saved merged data to: merged_NR_PDSR_2017.rds\n")

# ==============================================================================
# SECTION 7: QUARTILE ANALYSIS AND SPATIAL MAPPING
# ==============================================================================

cat("\n\nSECTION 7: QUARTILE ANALYSIS AND SPATIAL MAPPING\n")
cat("=================================================\n\n")

# Prepare spatial data
MPDSR_map_clean <- merged_MPDSR_2017 %>%
  mutate(ADM2_EN = ifelse(Region == "Addis Ababa", "Region 14", Zone_corrected)) %>%
  left_join(eth_adm2 %>% select(ADM2_EN, geometry), by = "ADM2_EN") %>%
  select(
    zone_name = Zone_corrected,
    perinatal_deaths_notified = sum_perinatal_deaths,
    perinatal_deaths_reviewed,
    geometry
  ) %>%
  mutate(
    perinatal_deaths_notified = replace_na(perinatal_deaths_notified, 0),
    perinatal_deaths_reviewed = replace_na(perinatal_deaths_reviewed, 0)
  )

# QUARTILE ANALYSIS FOR CLASSIFICATION
cat("\n7.1 Quartile Analysis for Classification\n")
cat("----------------------------------------\n")

# Calculate quartiles for perinatal deaths notification
notif_quartiles <- quantile(MPDSR_map_clean$perinatal_deaths_notified, 
                            probs = c(0, 0.25, 0.5, 0.75, 1))
cat("\nNotification Quartiles:\n")
print(notif_quartiles)

# Calculate quartiles for perinatal deaths review
review_quartiles <- quantile(MPDSR_map_clean$perinatal_deaths_reviewed, 
                             probs = c(0, 0.25, 0.5, 0.75, 1))
cat("\nReview Quartiles:\n")
print(review_quartiles)

# CREATE CATEGORIES BASED ON QUARTILES
cat("\n7.2 Creating Classification Categories Based on Quartiles\n")
cat("---------------------------------------------------------\n")

# Create categories based on quartile values
MPDSR_map_clean <- MPDSR_map_clean %>%
  mutate(
    # Notification categories based on quartiles
    notified_category = case_when(
      perinatal_deaths_notified == 0 ~ "Zero reporting (0)",
      perinatal_deaths_notified >= 1 & perinatal_deaths_notified <= 16 ~ "Low (1-16)",
      perinatal_deaths_notified >= 17 & perinatal_deaths_notified <= 103 ~ "Moderate (17-103)",
      perinatal_deaths_notified >= 104 & perinatal_deaths_notified <= 207 ~ "High (104-207)",
      perinatal_deaths_notified >= 208 ~ "Very High (>=208)"
    ),
    
    # Review categories based on quartiles
    reviewed_category = case_when(
      perinatal_deaths_reviewed == 0 ~ "Zero reporting (0)",
      perinatal_deaths_reviewed >= 1 & perinatal_deaths_reviewed <= 2 ~ "Low (1-2)",
      perinatal_deaths_reviewed >= 3 & perinatal_deaths_reviewed <= 66 ~ "Moderate (3-66)",
      perinatal_deaths_reviewed >= 67 ~ "High (>=67)"
    ),
    
    # Set as factor with levels for correct legend order
    notified_category = factor(notified_category,
                               levels = c("Zero reporting (0)", "Low (1-16)", 
                                          "Moderate (17-103)", "High (104-207)", 
                                          "Very High (>=208)")),
    reviewed_category = factor(reviewed_category,
                               levels = c("Zero reporting (0)", "Low (1-2)", 
                                          "Moderate (3-66)", "High (>=67)"))
  ) %>%
  st_as_sf()

# Display classification summary
cat("\n7.3 Classification Distribution Summary\n")
cat("---------------------------------------\n")

cat("\nNotification Categories Distribution:\n")
print(table(MPDSR_map_clean$notified_category))

cat("\nReview Categories Distribution:\n")
print(table(MPDSR_map_clean$reviewed_category))

# CREATE SPATIAL MAPS
cat("\n\nGenerating Figure 5: Spatial Distribution Maps (Quartile-Based Classification)...\n")

# Define colors
ev_colors <- c(
  "Zero reporting (0)" = "red",
  "Low (1-16)" = "#edf8fb",
  "Moderate (17-103)" = "#b2e2e2",
  "High (104-207)" = "#66c2a4",
  "Very High (>=208)" = "#238b45",
  "Low (1-2)" = "#edf8fb",
  "Moderate (3-66)" = "#b2e2e2",
  "High (>=67)" = "#66c2a4"
)

# Notification Map
notif_map <- ggplot(MPDSR_map_clean) +
  geom_sf(aes(fill = notified_category), color = "black", linewidth = 0.1, alpha = 0.95) +
  scale_fill_manual(values = ev_colors, name = "Notified Deaths") +
  coord_sf(datum = st_crs(MPDSR_map_clean)) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(title = "A) Perinatal Death Notification", 
       subtitle = "Ethiopia, 2017 EFY (Classification based on quartiles)") +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.75),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40")
  )

# Review Map
review_map <- ggplot(MPDSR_map_clean) +
  geom_sf(aes(fill = reviewed_category), color = "black", linewidth = 0.1, alpha = 0.95) +
  scale_fill_manual(values = ev_colors, name = "Reviewed Deaths") +
  coord_sf(datum = st_crs(MPDSR_map_clean)) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(title = "B) Perinatal Death Review", 
       subtitle = "Ethiopia, 2017 EFY (Classification based on quartiles)") +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.75),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40")
  )

# Combine Maps
combined_map <- notif_map + review_map +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Figure 5: Perinatal Death Surveillance and Response in Ethiopia",
    subtitle = "Zone-Level Analysis with Quartile-Based Classification, 2017 EFY",
    caption = paste("Data Source: Ethiopian Ministry of Health | Zones:", nrow(MPDSR_map_clean),
                    "\nClassification based on quartile analysis of notification and review data"),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50")
    )
  )

print(combined_map)

# ==============================================================================
# SECTION 8: TEMPORAL TREND ANALYSIS (2010-2017)
# ==============================================================================

cat("\n\nSECTION 8: TEMPORAL TRENDS (2010-2017)\n")
cat("======================================\n\n")

# National trend data
national_trends <- data.frame(
  EFY = factor(
    c("2010EFY","2011EFY","2012EFY","2013EFY",
      "2014EFY","2015EFY","2016EFY","2017EFY"),
    levels = c("2010EFY","2011EFY","2012EFY","2013EFY",
               "2014EFY","2015EFY","2016EFY","2017EFY")
  ),
  Notified = c(NA, NA, NA, 3.8, 9.1, 11.1, 12.2, 12.8),
  Reviewed = c(0.1, 0.5, 0.6, 1.4, 2.9, 5.4, 4.9, 4.6)
)

trend_long <- national_trends %>%
  pivot_longer(
    cols = c(Notified, Reviewed),
    names_to = "Type",
    values_to = "Rate"
  )

cat("\nGenerating Figure 6: Temporal Trends...\n")

p <- ggplot(trend_long, aes(x = EFY, y = Rate, group = Type, color = Type)) +
  
  annotate(
    "rect",
    xmin = 0.5, xmax = 3.5,
    ymin = -Inf, ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  
  scale_color_manual(
    values = c(
      "Notified" = "#1b9e77",
      "Reviewed" = "#d95f02"
    ),
    labels = c(
      "Notified" = "Notified perinatal deaths",
      "Reviewed" = "Reviewed perinatal deaths"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 14),
    breaks = seq(0, 14, 2),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Figure 6: Perinatal Death Notification and Review Rates\nAgainst Estimated Perinatal Deaths",
    subtitle = "Ethiopia, 2010–2017 Ethiopian Fiscal Years",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Rate (%)",
    color = NULL,
    caption = "Shaded area indicates years when notification was not integrated into the national database"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      face = "bold",
      size = 11
    ),
    axis.text.y = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

p <- p +
  geom_label(
    data = data.frame(
      EFY = "2011EFY",
      Rate = 11.8
    ),
    aes(
      x = EFY,
      y = Rate,
      label = "Notification was not integrated\ninto the national database\n(2010–2012 EFY)"
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 4,
    linewidth = 0.3
  ) +
  geom_curve(
    data = data.frame(
      x = "2011EFY",
      y = 11,
      xend = "2012EFY",
      yend = 6
    ),
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    curvature = -0.3,
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.4
  )

print(p)

# ==============================================================================
# SECTION 9: PDSR FORM SOURCE ANALYSIS
# ==============================================================================

cat("\n\nSECTION 9: PDSR FORM SOURCE ANALYSIS\n")
cat("=====================================\n\n")

# Check available columns
cat("9.1 Checking PDSR data columns:\n")
print(colnames(corrected_PDSR_data_2010_2017)[1:10])

# Calculate source proportions
quarter_source_summary_final <- corrected_PDSR_data_2010_2017 %>%
  group_by(EFY_Quarter, ThisPDRFisextractedfrom) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY_Quarter) %>%
  mutate(
    quarter_total = sum(count, na.rm = TRUE),
    proportion = count / quarter_total,
    proportion_percent = round(proportion * 100, 1)
  ) %>%
  ungroup()

# Table 6: PDSR Form Sources
cat("\nTable 6: PDSR Form Sources by Quarter\n")
kable(quarter_source_summary_final,
      caption = "Table 6: Quarter-wise Proportion of PDSR Forms by Source",
      digits = 1,
      align = c("l", "l", "r", "r", "r", "r")) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

# Source trend plot
cat("\nGenerating Figure 7: PDSR Form Source Trends...\n")
ggplot(quarter_source_summary_final, 
       aes(x = EFY_Quarter, 
           y = proportion_percent, 
           color = ThisPDRFisextractedfrom, 
           group = ThisPDRFisextractedfrom)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Figure 7: Proportion of PDSR Forms by Source Across Quarters",
    x = "Ethiopian Fiscal Year Quarter",
    y = "Proportion (%)",
    color = "PDSR Form Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

# ==============================================================================
# SECTION 10: SUMMARY AND EXPORT
# ==============================================================================

cat("\n\nSECTION 10: SUMMARY AND DATA EXPORT\n")
cat("====================================\n\n")

# Create summary statistics
summary_stats <- list(
  Analysis_Date = format(Sys.Date(), "%Y-%m-%d"),
  Total_Zones_Analyzed = nrow(MPDSR_map_clean),
  Zones_Zero_Notifications = sum(MPDSR_map_clean$perinatal_deaths_notified == 0),
  Zones_Zero_Reviews = sum(MPDSR_map_clean$perinatal_deaths_reviewed == 0),
  Total_PDSR_Records_2017 = nrow(PDSR_2017),
  National_Notification_Rate_2017 = "12.8%",
  National_Review_Rate_2017 = "4.6%",
  Tables_Generated = 6,
  Figures_Generated = 7
)

cat("10.1 Analysis Summary:\n")
print(as.data.frame(summary_stats))

cat("\n10.2 Files Created:\n")
cat("✓ merged_region_2017_performance_final.rds - Regional performance data\n")
cat("✓ merged_NR_PDSR_2017.rds - Zone-level notification and review data\n")

cat("\n10.3 Tables Generated:\n")
cat("1. Table 1: Maternal Deaths by Quarter and Data Source\n")
cat("2. Table 2: Zones with Zero Perinatal Death Notifications\n")
cat("3. Table 3: Top 20 Zones by Perinatal Deaths\n")
cat("4. Table 4: Regional Performance Metrics\n")
cat("5. Table 5: Top 20 Zones by Reviews\n")
cat("6. Table 6: PDSR Form Sources by Quarter\n")

cat("\n10.4 Figures Generated:\n")
cat("1. Figure 1: MDRF Source Trends (2006-2017)\n")
cat("2. Figure 2: Silent Zones Sunburst\n")
cat("3. Figure 3: Top 20 Zones Bar Plot\n")
cat("4. Figure 4: Top 20 Review Zones Bar Plot\n")
cat("5. Figure 5: Spatial Distribution Maps\n")
cat("6. Figure 6: Temporal Trends (2010-2017)\n")
cat("7. Figure 7: PDSR Form Source Trends\n")

cat("\n\nANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("=================================\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version$version.string, "\n")
cat("Total processing time: All analyses completed\n")