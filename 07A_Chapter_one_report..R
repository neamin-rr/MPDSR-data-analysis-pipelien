# =========================================
# 1. INITIAL SETUP AND DATA EXPLORATION
# =========================================

# Clear workspace
rm(list = ls())

# Load all required libraries
library(sf)           # Spatial data handling
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(ggspatial)    # Map annotations
library(patchwork)    # Combining plots
library(plotly)       # Interactive plots
library(viridisLite)  # Color palettes
library(scales)       # Formatting scales
library(htmlwidgets)  # Saving interactive plots
library(RColorBrewer) # Color schemes
library(knitr)        # Table formatting
library(gt)           # Publication tables

# Define color palette for maps (used throughout)
ev_colors <- c(
  "Zero reporting (0)" = "red",
  "Low (1-3)" = "#edf8fb",
  "Low (1-4)" = "#edf8fb",
  "Moderate (4-7)" = "#b2e2e2",
  "Moderate (5-10)" = "#b2e2e2",
  "High (8-19)" = "#66c2a4",
  "High (11-21)" = "#66c2a4",
  "Very High (>=20)" = "#238b45",
  "Very High (>=22)" = "#238b45"
)

# =========================================
# 2. ANALYZE ZONES WITH ZERO MATERNAL DEATHS
# =========================================

# Get zones with zero maternal deaths
zones_zero_maternal <- summary_yearly_MPDSR_zone_cleaned_2017_map$Zone_corrected[
  summary_yearly_MPDSR_zone_cleaned_2017_map$sum_maternal_deaths == 0
]

# Create table of regions and zones with zero maternal deaths
zero_maternal_table <- summary_yearly_MPDSR_zone_cleaned_2017_map[
  summary_yearly_MPDSR_zone_cleaned_2017_map$sum_maternal_deaths == 0,
  c("Region", "Zone_corrected")
]

# Display as formatted table
kable(zero_maternal_table, caption = "Zones with Zero Maternal Deaths")

# Create interactive sunburst chart
zero_maternal_table <- zero_maternal_table %>%
  mutate(id = paste(Region, Zone_corrected, sep = "-"),
         parent = Region)

num_regions <- length(unique(zero_maternal_table$Region))
region_colors <- viridisLite::viridis(num_regions)
zone_colors <- sapply(zero_maternal_table$Region, function(r) {
  alpha(region_colors[which(unique(zero_maternal_table$Region) == r)], 0.6)
})
all_colors <- c(region_colors, zone_colors)

fig <- plot_ly(
  labels = c(unique(zero_maternal_table$Region), zero_maternal_table$Zone_corrected),
  parents = c(rep("", num_regions), zero_maternal_table$Region),
  type = 'sunburst',
  branchvalues = 'total',
  hoverinfo = 'label+parent',
  marker = list(colors = all_colors)
) %>%
  layout(
    title = list(
      text = "Silent Zones by Region for Maternal Deaths in 2017 EFY",
      font = list(size = 16, family = "Arial", color = "black"),
      x = 0.5
    ),
    margin = list(l = 0, r = 0, t = 50, b = 0)
  )

print(fig)
saveWidget(fig, "Silent_Zones_Sunburst_2017EFY.html")

# =========================================
# 3. REGIONAL AGGREGATION FOR WEEKLY NOTIFIED DATA
# =========================================

# Aggregate by Region from weekly notification data
region_summary <- summary_yearly_MPDSR_zone_cleaned_2017_map %>%
  group_by(Region) %>%
  summarise(
    total_maternal_deaths = sum(sum_maternal_deaths, na.rm = TRUE),
    total_perinatal_deaths = sum(sum_perinatal_deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Add National total row
national_total <- region_summary %>%
  summarise(
    Region = "National",
    total_maternal_deaths = sum(total_maternal_deaths),
    total_perinatal_deaths = sum(total_perinatal_deaths)
  )

region_summary <- bind_rows(region_summary, national_total)

# =========================================
# 4. REGIONAL AGGREGATION FOR 2017 REVIEWED DATA
# =========================================

# Filter for 2017 EFY and aggregate reviewed maternal deaths
data2017 <- Final_national_MDSR_data_20251216_cleaned %>%
  filter(EFY == 2017)

region_summary_2017 <- data2017 %>%
  group_by(Region_new_1) %>%
  summarise(
    maternal_deaths_review = n(),
    .groups = "drop"
  )

# Add national total
national_total_2017 <- region_summary_2017 %>%
  summarise(
    Region_new_1 = "Ethiopia",
    maternal_deaths_review = sum(maternal_deaths_review)
  )

region_summary_2017 <- bind_rows(region_summary_2017, national_total_2017)

# =========================================
# 5. REGIONAL DATA INTEGRATION AND MERGING
# =========================================

# Function to standardize region names
std_region <- function(x) {
  x %>%
    str_to_upper() %>%
    str_trim()
}

# 1. Reviewed maternal deaths (2017 EFY)
reviewed_2017 <- region_summary_2017 %>%
  mutate(region_std = std_region(Region_new_1)) %>%
  filter(region_std != "ETHIOPIA") %>%
  select(
    region_std,
    maternal_deaths_reviewed = maternal_deaths_review
  )

# 2. Weekly notified deaths (maternal + perinatal)
notified_weekly <- region_summary %>%
  mutate(
    region_std = std_region(Region),
    region_std = case_when(
      region_std == "B-GUMUZ" ~ "BENISHANGUL GUMZ",
      region_std == "SWEPRS" ~ "SOUTH WEST ETHIOPIA",
      region_std == "SOUTH ETHIOPIA" ~ "SOUTHERN ETHIOPIA",
      region_std == "NATIONAL" ~ "ETHIOPIA",
      TRUE ~ region_std
    )
  ) %>%
  filter(region_std != "ETHIOPIA") %>%
  select(
    region_std,
    maternal_deaths_notified = total_maternal_deaths,
    perinatal_deaths_notified = total_perinatal_deaths
  )

# 3. Expected deaths (population-based)
expected_deaths <- Population_data_2024_2025_cleaned %>%
  mutate(
    region_std = std_region(Region),
    region_std = case_when(
      region_std == "BENISHANGUL-GUMUZ" ~ "BENISHANGUL GUMZ",
      region_std == "CENTRAL ETHIOPIA REGION" ~ "CENTRAL ETHIOPIA",
      region_std == "SOUTH WEST ETHIOPIA REGION" ~ "SOUTH WEST ETHIOPIA",
      region_std == "SOUTHER ETHIOPIA REGION" ~ "SOUTHERN ETHIOPIA",
      TRUE ~ region_std
    )
  ) %>%
  filter(region_std != "ETHIOPIA") %>%
  select(
    region_std,
    expected_maternal_deaths = Expected_MD,
    expected_perinatal_deaths = Expected_perinatal_deaths
  )

# 4. Live births data
live_births <- Population_data_2024_2025_cleaned %>%
  mutate(
    region_std = std_region(Region),
    region_std = case_when(
      region_std == "BENISHANGUL-GUMUZ" ~ "BENISHANGUL GUMZ",
      region_std == "CENTRAL ETHIOPIA REGION" ~ "CENTRAL ETHIOPIA",
      region_std == "SOUTH WEST ETHIOPIA REGION" ~ "SOUTH WEST ETHIOPIA",
      region_std == "SOUTHER ETHIOPIA REGION" ~ "SOUTHERN ETHIOPIA",
      TRUE ~ region_std
    )
  ) %>%
  filter(region_std != "ETHIOPIA") %>%
  select(
    region_std,
    estimated_live_births = New_birth
  )

# Merge all regional datasets
merged_region_2017 <- reviewed_2017 %>%
  full_join(notified_weekly, by = "region_std") %>%
  full_join(expected_deaths, by = "region_std") %>%
  left_join(live_births, by = "region_std") %>%
  arrange(region_std)

# Add Ethiopia national row
ethiopia_row <- merged_region_2017 %>%
  summarise(
    region_std = "ETHIOPIA",
    maternal_deaths_reviewed = sum(maternal_deaths_reviewed, na.rm = TRUE),
    maternal_deaths_notified = sum(maternal_deaths_notified, na.rm = TRUE),
    perinatal_deaths_notified = sum(perinatal_deaths_notified, na.rm = TRUE),
    expected_maternal_deaths = sum(expected_maternal_deaths, na.rm = TRUE),
    expected_perinatal_deaths = sum(expected_perinatal_deaths, na.rm = TRUE),
    estimated_live_births = sum(estimated_live_births, na.rm = TRUE)
  )

merged_region_2017 <- bind_rows(merged_region_2017, ethiopia_row)
saveRDS(merged_region_2017, "merged_region_2017_performance.rds")

# =========================================
# 6. CREATE PUBLICATION-READY COVERAGE TABLE
# =========================================

# Create summary table with coverage calculations
summary_table_2017 <- merged_region_2017 %>%
  mutate(
    prop_notified_vs_est = maternal_deaths_notified / expected_maternal_deaths,
    prop_reviewed_vs_notified = maternal_deaths_reviewed / maternal_deaths_notified,
    prop_reviewed_vs_est = maternal_deaths_reviewed / expected_maternal_deaths,
    
    notified_vs_est_fmt = paste0(
      maternal_deaths_notified, " (",
      percent(prop_notified_vs_est, accuracy = 0.1), ")"
    ),
    reviewed_vs_notified_fmt = paste0(
      maternal_deaths_reviewed, " (",
      percent(prop_reviewed_vs_notified, accuracy = 0.1), ")"
    ),
    reviewed_vs_est_pct = percent(prop_reviewed_vs_est, accuracy = 0.1)
  ) %>%
  select(
    Region = region_std,
    `Estimated live births` = estimated_live_births,
    `Estimated maternal deaths` = expected_maternal_deaths,
    `Notified maternal deaths vs estimated` = notified_vs_est_fmt,
    `Reported MDRF vs notified` = reviewed_vs_notified_fmt,
    `% Reported MDRF from estimated` = reviewed_vs_est_pct
  ) %>%
  arrange(desc(Region == "ETHIOPIA"), Region)

# Create gt table for publication
gt_table <- summary_table_2017 %>%
  gt() %>%
  tab_header(
    title = md("**Maternal Death Notification and Review Coverage by Region, Ethiopia, 2017 EFY**")
  ) %>%
  cols_align(
    align = "right",
    columns = c(`Estimated live births`, `Estimated maternal deaths`)
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Notified maternal deaths vs estimated`, 
                `Reported MDRF vs notified`, 
                `% Reported MDRF from estimated`)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    heading.title.font.size = 14
  )

print(gt_table)
gtsave(gt_table, "Maternal_Death_Coverage_2017EFY.docx")
gtsave(gt_table, "Maternal_Death_Coverage_2017EFY.html")

# =========================================
# 7. TOP ZONES ANALYSIS - REVIEWED DEATHS
# =========================================

# Aggregate by zone for reviewed deaths
zone_summary_2017 <- data2017 %>%
  group_by(Region_new_1, Zone_final) %>%
  summarise(
    maternal_deaths_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(maternal_deaths_count))

# Top 20 zones for reviewed deaths
top20_zones <- zone_summary_2017 %>%
  arrange(desc(maternal_deaths_count)) %>%
  slice_head(n = 20) %>%
  mutate(
    Zone_label = paste0(Zone_final, " (", Region_new_1, ")")
  )

region_colors <- RColorBrewer::brewer.pal(n = length(unique(top20_zones$Region_new_1)), "Set2")

ggplot(top20_zones, aes(x = reorder(Zone_label, maternal_deaths_count), 
                        y = maternal_deaths_count, 
                        fill = Region_new_1)) +
  geom_col() +
  geom_text(aes(label = maternal_deaths_count), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = region_colors) +
  labs(
    title = "Top 20 Zones by Maternal Death Review in 2017 EFY",
    x = "Zone (Region)",
    y = "Number of Maternal Deaths Reviewed",
    fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 10)
  ) +
  ylim(0, max(top20_zones$maternal_deaths_count) * 1.1)

# =========================================
# 8. TOP ZONES ANALYSIS - NOTIFIED DEATHS
# =========================================

# Prepare notified data
MPDSR_notification_updated <- summary_yearly_MPDSR_zone_cleaned_2017_map

# Add subzone data for Addis Ababa
addis_subzones <- data.frame(
  Region = "Addis Ababa",
  Zone_corrected = c("Addis_Ketema", "Akaki_Kality", "Arada", "Bole", "Gulele",
                     "Kirkos", "Kolfe_keraniyo", "Lemi_Kura", "Lideta", 
                     "Nifas_silk_lafto", "Yeka"),
  count = c(6,4,4,3,7,13,8,3,3,1,6),
  sum_maternal_deaths = c(6,4,4,3,7,13,8,3,3,1,6),
  sum_perinatal_deaths = c(141,238,121,66,351,554,235,192,218,36,141)
)

MPDSR_final <- MPDSR_notification_updated %>%
  filter(!(Region == "Addis Ababa" & Zone_corrected == "Region 14")) %>%
  bind_rows(addis_subzones) %>%
  arrange(Region, Zone_corrected)

saveRDS(MPDSR_final, "MPDSR_updated_2017_notification.rds")

# Top 20 zones for notified deaths
top20_notified <- MPDSR_final %>%
  arrange(desc(sum_maternal_deaths)) %>%
  slice_head(n = 20) %>%
  mutate(
    Zone_label = paste0(Zone_corrected, " (", Region, ")")
  )

region_colors_notified <- RColorBrewer::brewer.pal(n = length(unique(top20_notified$Region)), "Set2")

ggplot(top20_notified, aes(x = reorder(Zone_label, sum_maternal_deaths), 
                           y = sum_maternal_deaths, 
                           fill = Region)) +
  geom_col() +
  geom_text(aes(label = sum_maternal_deaths), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = region_colors_notified) +
  labs(
    title = "Top 20 Zones by Notified Maternal Deaths in 2017 EFY",
    x = "Zone (Region)",
    y = "Number of Maternal Deaths Notified",
    fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 10)
  ) +
  ylim(0, max(top20_notified$sum_maternal_deaths) * 1.1)

# =========================================
# 9. SPATIAL DATA PREPARATION AND MAPPING
# =========================================
library(sf)
# Load shapefile
shp_path <- "F:/Cholera/shape_22/Adm_2/eth_admbnda_adm2_csa_bofedb_2021.shp"
eth_adm2 <- st_read(shp_path, quiet = TRUE)



# Prepare datasets for mapping
notified <- MPDSR_final %>%
  rename(
    Region_merge = Region,
    Zone_merge = Zone_corrected,
    notified_maternal_deaths = sum_maternal_deaths
  ) %>%
  select(Region_merge, Zone_merge, notified_maternal_deaths)

reviewed <- zone_summary_2017 %>%
  rename(
    Region_merge = Region_new_1,
    Zone_merge = Zone_final,
    reviewed_maternal_deaths = maternal_deaths_count
  )

# Merge notified and reviewed data
maternal_merged <- full_join(notified, reviewed, by = c("Region_merge", "Zone_merge")) %>%
  mutate(reviewed_maternal_deaths = ifelse(is.na(reviewed_maternal_deaths), 0, reviewed_maternal_deaths))

write.csv(maternal_merged, "maternal_merged.csv", row.names = FALSE)

# Name correction mapping
region_name_mapping <- tribble(
  ~Region_merge, ~ADM1_EN_correct,
  "B-Gumuz", "Benishangul Gumz",
  "Gambella", "Gambela",
  "SWEPRS", "South West Ethiopia",
  "South Ethiopia", "Southern Ethiopia",
  "Oromia", "Oromia"
)

additional_corrections <- tribble(
  ~Region, ~Zone, ~Zone_correct,
  "Oromia", "West Hararghe", "West Hararge"
)

# Create final shapefile with corrected names
maternal_with_geometry_final <- maternal_merged %>%
  left_join(region_name_mapping, by = "Region_merge") %>%
  left_join(additional_corrections, by = c("Region_merge" = "Region", "Zone_merge" = "Zone")) %>%
  mutate(
    ADM1_final = ifelse(!is.na(ADM1_EN_correct), ADM1_EN_correct, Region_merge),
    ADM2_final = ifelse(!is.na(Zone_correct), Zone_correct, Zone_merge)
  ) %>%
  left_join(
    eth_adm2 %>% select(ADM1_EN, ADM2_EN, geometry),
    by = c("ADM1_final" = "ADM1_EN", "ADM2_final" = "ADM2_EN")
  ) %>%
  mutate(
    notified_category = factor(
      case_when(
        notified_maternal_deaths == 0 ~ "Zero reporting (0)",
        notified_maternal_deaths >= 1 & notified_maternal_deaths <= 4 ~ "Low (1-4)",
        notified_maternal_deaths >= 5 & notified_maternal_deaths <= 10 ~ "Moderate (5-10)",
        notified_maternal_deaths >= 11 & notified_maternal_deaths <= 21 ~ "High (11-21)",
        notified_maternal_deaths >= 22 ~ "Very High (>=22)"
      ),
      levels = c("Zero reporting (0)", "Low (1-4)", "Moderate (5-10)", "High (11-21)", "Very High (>=22)")
    ),
    reviewed_category = factor(
      case_when(
        reviewed_maternal_deaths == 0 ~ "Zero reporting (0)",
        reviewed_maternal_deaths >= 1 & reviewed_maternal_deaths <= 3 ~ "Low (1-3)",
        reviewed_maternal_deaths >= 4 & reviewed_maternal_deaths <= 7 ~ "Moderate (4-7)",
        reviewed_maternal_deaths >= 8 & reviewed_maternal_deaths <= 19 ~ "High (8-19)",
        reviewed_maternal_deaths >= 20 ~ "Very High (>=20)"
      ),
      levels = c("Zero reporting (0)", "Low (1-3)", "Moderate (4-7)", "High (8-19)", "Very High (>=20)")
    )
  ) %>%
  select(Region = Region_merge, Zone = Zone_merge,
         notified_maternal_deaths, reviewed_maternal_deaths,
         notified_category, reviewed_category, geometry)

# Save final shapefile
final_shapefile <- maternal_with_geometry_final %>% filter(!st_is_empty(geometry))
st_write(final_shapefile, "maternal_deaths_all_zones.gpkg", delete_layer = TRUE)
st_write(final_shapefile, "maternal_deaths_all_zones.shp", delete_layer = TRUE)
saveRDS(final_shapefile, "corrected_shapefile_MDN.rds")

cat("Final shapefile saved with", nrow(final_shapefile), "zones\n")

# =========================================
# 10. CREATE PUBLICATION-READY MAPS
# =========================================

# Prepare data for mapping
final_shapefile <- final_shapefile %>%
  mutate(
    notified_maternal_deaths = ifelse(is.na(notified_maternal_deaths), 0, notified_maternal_deaths),
    reviewed_maternal_deaths = ifelse(is.na(reviewed_maternal_deaths), 0, reviewed_maternal_deaths)
  )

# Create Notification Map
notif_map <- ggplot(final_shapefile) +
  geom_sf(aes(fill = notified_category), 
          color = "black",
          linewidth = 0.1,
          alpha = 0.95) +
  scale_fill_manual(
    values = ev_colors, 
    name = "Notified Deaths",
    drop = FALSE,
    guide = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      keyheight = unit(0.5, "cm"),
      keywidth = unit(0.5, "cm"),
      ncol = 1,
      override.aes = list(linewidth = 0.5)
    )
  ) +
  coord_sf(datum = st_crs(final_shapefile)) +
  annotation_north_arrow(
    location = "tl", 
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    height = unit(0.1, "cm"),
    text_cex = 0.7
  ) +
  labs(
    title = "A). MATERNAL DEATH NOTIFICATION",
    subtitle = "Ethiopia, 2017 Ethiopian Fiscal Year"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.88, 0.75),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.3),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, 
                              margin = margin(b = 3)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40",
                                 margin = margin(b = 5)),
    axis.text = element_text(size = 8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
    plot.margin = margin(5, 5, 5, 5)
  )

# Create Review Map
review_map <- ggplot(final_shapefile) +
  geom_sf(aes(fill = reviewed_category), 
          color = "black",
          linewidth = 0.1,
          alpha = 0.95) +
  scale_fill_manual(
    values = ev_colors, 
    name = "Reviewed Deaths",
    drop = FALSE,
    guide = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      keyheight = unit(0.5, "cm"),
      keywidth = unit(0.5, "cm"),
      ncol = 1,
      override.aes = list(linewidth = 0.5)
    )
  ) +
  coord_sf(datum = st_crs(final_shapefile)) +
  annotation_north_arrow(
    location = "tl", 
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    height = unit(0.1, "cm"),
    text_cex = 0.7
  ) +
  labs(
    title = "B). MATERNAL DEATH REVIEW",
    subtitle = "Ethiopia, 2017 Ethiopian Fiscal Year"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.88, 0.75),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.1, "cm"),
    legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.3),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14,
                              margin = margin(b = 3)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40",
                                 margin = margin(b = 5)),
    axis.text = element_text(size = 8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
    plot.margin = margin(5, 5, 5, 5)
  )

# Combine maps
combined_map <- notif_map + review_map + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Maternal Death Surveillance and Response in Ethiopia",
    subtitle = "Administrative Zone-Level Analysis of Death Notification and Review Systems",
    caption = paste("Data Source: Ethiopian Ministry of Health | Zones:", nrow(final_shapefile)),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5,
                                margin = margin(t = 5, b = 3)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40",
                                   margin = margin(b = 8)),
      plot.caption = element_text(size = 9, hjust = 1, color = "gray50",
                                  margin = margin(t = 5)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

# Display and save
print(combined_map)
ggsave("maternal_deaths_with_grid.png", plot = combined_map,
       width = 20, height = 11, dpi = 600, bg = "white")

# =========================================
# 11. FINAL SUMMARY
# =========================================
cat("\n" + strrep("=", 60) + "\n")
cat("ANALYSIS COMPLETE - SUMMARY\n")
cat(strrep("=", 60) + "\n")
cat("1. Zones with zero deaths analyzed and visualized\n")
cat("2. Regional coverage table created and saved\n")
cat("3. Top 20 zones for reviewed deaths identified\n")
cat("4. Top 20 zones for notified deaths identified\n")
cat("5. Spatial data prepared with", nrow(final_shapefile), "zones matched\n")
cat("6. Publication-quality maps created and saved\n")
cat("7. All output files saved in working directory\n")
cat(strrep("=", 60) + "\n")