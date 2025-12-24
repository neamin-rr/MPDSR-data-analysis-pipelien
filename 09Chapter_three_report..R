# ==============================================================================
# PERINATAL DEATH SURVEILLANCE AND RESPONSE (PDSR) ANALYSIS
# Data preparation and descriptive analysis for EFY 2010-2017
# ==============================================================================

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(scales)
library(stringr)
library(patchwork)
library(tidytext)

# ==============================================================================
# 1. DATA PREPARATION AND CLEANING
# ==============================================================================

# Check initial data structure
table(corrected_PDSR_data_2010_2017$Reportinghealthfacilitytype, useNA = "always")

# Create working dataset
PDSR_2009_2017 <- corrected_PDSR_data_2010_2017

# Check EFY distribution
table(PDSR_2009_2017$EFY)

# ==============================================================================
# 2. FILTER EFY 2017 FOR SPECIFIC ANALYSES
# ==============================================================================

PDSR_2017 <- PDSR_2009_2017 %>%
  filter(EFY == 2017)

N_total_2017 <- nrow(PDSR_2017)  # Total observations for EFY 2017: 5675

# ==============================================================================
# 3. DATA CLEANING AND RECODING
# ==============================================================================

# Clean Region variable
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(Region_clean = if_else(is.na(Region_clean), "Amhara", Region_clean))

# Clean reporting facility type (EFY 2017 only)
PDSR_2017 <- PDSR_2017 %>%
  mutate(Reportinghealthfacilitytype =
           if_else(Reportinghealthfacilitytype == "NGO Facility",
                   "Private Health Facility",
                   Reportinghealthfacilitytype))

# ==============================================================================
# 4. TABLE 1: REPORTING CHARACTERISTICS FOR EFY 2017
# ==============================================================================

# Helper function to create frequency tables
make_np_table <- function(data, var, section_name) {
  data %>%
    count({{ var }}) %>%
    mutate(
      Section = section_name,
      `Reporting characteristics` = as.character({{ var }}),
      Frequency = n,
      Percentage = round(100 * n / N_total_2017, 1)
    ) %>%
    select(
      Section,
      `Reporting characteristics`,
      Frequency,
      Percentage
    )
}

# Build sections for Table 1
facility_tbl <- make_np_table(
  PDSR_2017,
  Reportinghealthfacilitytype,
  "Type of reporting facility"
)

source_tbl <- make_np_table(
  PDSR_2017,
  ThisPDRFisextractedfrom,
  "Data source"
)

region_tbl <- make_np_table(
  PDSR_2017,
  Region_clean,
  "Reporting region"
)

# Combine into one table
table_2017 <- bind_rows(
  facility_tbl,
  source_tbl,
  region_tbl
)

# Create formatted Table 1
table_1_final <- table_2017 %>%
  gt(groupname_col = "Section") %>%
  cols_label(
    Frequency = md("**Frequency (N = 5675)**"),
    Percentage = md("**Percentage (%)**")
  ) %>%
  fmt_number(
    columns = Percentage,
    decimals = 1
  ) %>%
  tab_header(
    title = "Table 1: Reporting Characteristics of Perinatal Death Reviews",
    subtitle = "Ethiopian Fiscal Year 2017"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Frequency, Percentage)
  ) %>%
  tab_source_note(
    source_note = "Source: Perinatal Death Surveillance and Response (PDSR) data, EFY 2017"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display Table 1
table_1_final

# ==============================================================================
# 5. PERINATAL CHARACTERISTICS ANALYSIS
# ==============================================================================

# Check initial distributions
table(PDSR_2017$Sexofthedeceased, useNA = "always")
table(PDSR_2017$Statusofthebabyatbirth, useNA = "always")
table(PDSR_2017$Placeofdeath, useNA = "always")
table(PDSR_2017$Modeofdeliveryofthedeceasedbaby, useNA = "always")

# Create gestational age categories for both datasets
create_ga_categories <- function(data) {
  data %>%
    mutate(
      GA_category = case_when(
        Estimatedgestationalageatdeliveryinweeks <= 36 ~ "Preterm",
        Estimatedgestationalageatdeliveryinweeks >= 37 &
          Estimatedgestationalageatdeliveryinweeks <= 41 ~ "Term",
        Estimatedgestationalageatdeliveryinweeks >= 42 ~ "Post-term",
        TRUE ~ NA_character_
      )
    )
}

# Apply to both datasets
PDSR_2017 <- create_ga_categories(PDSR_2017)
PDSR_2009_2017 <- create_ga_categories(PDSR_2009_2017)

# Check distribution
table(PDSR_2017$GA_category, useNA = "always")

# ==============================================================================
# 6. TABLE 2: PERINATAL CHARACTERISTICS FOR EFY 2017
# ==============================================================================

# Helper function for Table 2
make_desc_table <- function(data, var, section_name) {
  data %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}) %>%
    mutate(
      Variable = section_name,
      Frequency = n,
      Percentage = round(100 * n / sum(n), 1)
    ) %>%
    rename(Category = {{ var }}) %>%
    select(Variable, Category, Frequency, Percentage)
}

# Build tables for each characteristic
sex_tbl <- make_desc_table(PDSR_2017, Sexofthedeceased, "Sex of Perinate")
outcome_tbl <- make_desc_table(PDSR_2017, Statusofthebabyatbirth, "Outcome at Birth")
ga_tbl <- make_desc_table(PDSR_2017, GA_category, "Gestational Age at Delivery")
place_tbl <- make_desc_table(PDSR_2017, Placeofdeath, "Place of Death")

# Combine all tables
desc_table <- bind_rows(sex_tbl, outcome_tbl, ga_tbl, place_tbl)

# Create formatted Table 2
table_2_final <- desc_table %>%
  gt(groupname_col = "Variable") %>%
  cols_label(
    Category = "Category",
    Frequency = md("Frequency"),
    Percentage = md("Percentage (%)")
  ) %>%
  fmt_number(
    columns = Percentage,
    decimals = 1
  ) %>%
  cols_align(
    align = "center",
    columns = c(Frequency, Percentage)
  ) %>%
  tab_header(
    title = "Table 2: Perinatal Characteristics of Deceased Perinates",
    subtitle = paste0("Ethiopian Fiscal Year 2017 (N = ", N_total_2017, ")")
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2017"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display Table 2
table_2_final

# ==============================================================================
# 7. TREND ANALYSES (2010-2017)
# ==============================================================================

# 7.1. Trend of Birth Outcomes 2010-2017
# --------------------------------------

# Prepare trend data for birth outcomes
trend_birth_outcome <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  filter(!is.na(Statusofthebabyatbirth)) %>%
  group_by(EFY, Statusofthebabyatbirth) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  ungroup()

# Create summary table for birth outcomes trend
trend_table_outcome <- trend_birth_outcome %>%
  pivot_wider(
    names_from = Statusofthebabyatbirth,
    values_from = Frequency,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -EFY))) %>%
  mutate(
    Alive_pct = round(100 * `Alive birth` / Total, 1),
    Stillbirth_pct = round(100 * `Stillbirth` / Total, 1)
  )

# Add total row
totals_outcome <- trend_table_outcome %>%
  summarise(
    EFY = "Total",
    `Alive birth` = sum(`Alive birth`),
    `Stillbirth` = sum(`Stillbirth`),
    Total = sum(Total),
    Alive_pct = round(100 * sum(`Alive birth`) / sum(Total), 1),
    Stillbirth_pct = round(100 * sum(`Stillbirth`) / sum(Total), 1)
  )

trend_table_outcome$EFY <- as.character(trend_table_outcome$EFY)
trend_table_outcome_final <- bind_rows(trend_table_outcome, totals_outcome)

# Create formatted trend table
trend_table_1_final <- trend_table_outcome_final %>%
  gt() %>%
  tab_header(
    title = "Table 3: Trend of Birth Outcomes Among Reviewed Perinatal Deaths",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_label(
    EFY = "EFY",
    `Alive birth` = "Alive Birth (n)",
    `Stillbirth` = "Stillbirth (n)",
    Total = "Total (n)",
    Alive_pct = "Alive Birth (%)",
    Stillbirth_pct = "Stillbirth (%)"
  ) %>%
  fmt_number(
    columns = c(Alive_pct, Stillbirth_pct),
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2010-2017"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display trend table
trend_table_1_final

# 7.2. Trend of Gestational Age by Birth Outcome
# ----------------------------------------------

# Prepare data for gestational age trend
trend_ga <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  filter(!is.na(Statusofthebabyatbirth) & !is.na(GA_category)) %>%
  group_by(EFY, Statusofthebabyatbirth, GA_category) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(EFY, Statusofthebabyatbirth) %>%
  mutate(Percentage = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

# Create summary table
table_ga <- trend_ga %>%
  mutate(
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(EFY, Statusofthebabyatbirth, GA_category, Display) %>%
  pivot_wider(
    names_from = GA_category,
    values_from = Display,
    values_fill = "0 (0%)"
  )

table_ga$EFY <- as.character(table_ga$EFY)

# Add total row
totals_ga <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  filter(!is.na(Statusofthebabyatbirth) & !is.na(GA_category)) %>%
  group_by(Statusofthebabyatbirth, GA_category) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(Statusofthebabyatbirth) %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(Statusofthebabyatbirth, GA_category, Display) %>%
  pivot_wider(
    names_from = GA_category,
    values_from = Display,
    values_fill = "0 (0%)"
  ) %>%
  mutate(EFY = "Total") %>%
  select(EFY, everything())

final_ga_table <- bind_rows(table_ga, totals_ga)

# Create formatted table
table_ga_final <- final_ga_table %>%
  gt() %>%
  tab_header(
    title = "Table 4: Gestational Age Distribution by Birth Outcome",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_label(
    EFY = "EFY",
    `Preterm` = "Preterm",
    `Term` = "Term",
    `Post-term` = "Post-term"
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2010-2017"
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "0 (0%)"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_ga_final

# 7.3. Trend of Mode of Delivery by Birth Outcome
# -----------------------------------------------

# Recode vacuum delivery
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    Modeofdeliveryofthedeceasedbaby = recode(
      Modeofdeliveryofthedeceasedbaby,
      "Vacuum" = "Operative vaginal delivery"
    )
  )

# Prepare mode of delivery trend data
trend_mode <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  filter(!is.na(Statusofthebabyatbirth) & !is.na(Modeofdeliveryofthedeceasedbaby)) %>%
  group_by(EFY, Statusofthebabyatbirth, Modeofdeliveryofthedeceasedbaby) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY, Statusofthebabyatbirth) %>%
  mutate(Percentage = round(100 * Frequency / sum(Frequency), 1)) %>%
  ungroup()

# Create summary table
table_mode <- trend_mode %>%
  mutate(
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(EFY, Statusofthebabyatbirth, Modeofdeliveryofthedeceasedbaby, Display) %>%
  pivot_wider(
    names_from = Modeofdeliveryofthedeceasedbaby,
    values_from = Display,
    values_fill = "0 (0%)"
  )

table_mode$EFY <- as.character(table_mode$EFY)

# Add total row
totals_mode <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  filter(!is.na(Statusofthebabyatbirth) & !is.na(Modeofdeliveryofthedeceasedbaby)) %>%
  group_by(Statusofthebabyatbirth, Modeofdeliveryofthedeceasedbaby) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(Statusofthebabyatbirth) %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(Statusofthebabyatbirth, Modeofdeliveryofthedeceasedbaby, Display) %>%
  pivot_wider(
    names_from = Modeofdeliveryofthedeceasedbaby,
    values_from = Display,
    values_fill = "0 (0%)"
  ) %>%
  mutate(EFY = "Total") %>%
  select(EFY, everything())

final_mode_table <- bind_rows(table_mode, totals_mode)

# Create formatted table
table_mode_final <- final_mode_table %>%
  gt() %>%
  tab_header(
    title = "Table 5: Mode of Delivery by Birth Outcome",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_label(
    EFY = "EFY"
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "0 (0%)"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2010-2017"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_mode_final

# ==============================================================================
# 8. MATERNAL CHARACTERISTICS ANALYSIS
# ==============================================================================

# 8.1. Create maternal age categories
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    MaternalAgeCategory = case_when(
      AgeofthemotherYrs %in% 13:19 ~ "15-19",
      AgeofthemotherYrs %in% 20:24 ~ "20-24",
      AgeofthemotherYrs %in% 25:29 ~ "25-29",
      AgeofthemotherYrs %in% 30:34 ~ "30-34",
      AgeofthemotherYrs %in% 35:39 ~ "35-39",
      AgeofthemotherYrs %in% 40:44 ~ "40-44",
      AgeofthemotherYrs %in% 45:50 ~ "45-49",
      TRUE ~ NA_character_
    )
  )

# Check distribution
table(PDSR_2009_2017$MaternalAgeCategory, useNA = "always")

# 8.2. Maternal Age Trend Table 2010-2017
age_table_trend <- PDSR_2009_2017 %>%
  filter(EFY >= 2010 & !is.na(MaternalAgeCategory)) %>%
  group_by(EFY, MaternalAgeCategory) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  ungroup() %>%
  select(EFY, MaternalAgeCategory, Display) %>%
  pivot_wider(
    names_from = MaternalAgeCategory,
    values_from = Display,
    values_fill = "0 (0%)"
  )

age_table_trend$EFY <- as.character(age_table_trend$EFY)

# Add total row
totals_age <- PDSR_2009_2017 %>%
  filter(EFY >= 2010 & !is.na(MaternalAgeCategory)) %>%
  group_by(MaternalAgeCategory) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(MaternalAgeCategory, Display) %>%
  pivot_wider(
    names_from = MaternalAgeCategory,
    values_from = Display
  ) %>%
  mutate(EFY = "Total") %>%
  select(EFY, everything())

final_age_table <- bind_rows(age_table_trend, totals_age)

# Create formatted table
table_age_final <- final_age_table %>%
  gt() %>%
  tab_header(
    title = "Table 6: Maternal Age Distribution Among Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2010-2017"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_age_final

# ==============================================================================
# 9. ANTENATAL CARE (ANC) ANALYSIS
# ==============================================================================

# 9.1. Categorize ANC visits scientifically
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    ANC_category = case_when(
      NumberofANCvisitsinrelationtothedeceasedcasereport0ifNoANCvisits == 0 ~ "No ANC (0 visits)",
      NumberofANCvisitsinrelationtothedeceasedcasereport0ifNoANCvisits %in% 1:3 ~ "Partial ANC (1-3 visits)",
      NumberofANCvisitsinrelationtothedeceasedcasereport0ifNoANCvisits >= 4 ~ "Adequate ANC (4+ visits)",
      TRUE ~ NA_character_
    )
  )

# Check ANC distribution
table(PDSR_2009_2017$ANC_category, useNA = "always")

# 9.2. ANC Donut Chart for EFY 2017
anc_2017 <- PDSR_2009_2017 %>%
  filter(EFY == 2017 & !is.na(ANC_category)) %>%
  count(ANC_category) %>%
  arrange(desc(n)) %>%
  mutate(
    ANC_category = factor(ANC_category, levels = ANC_category),
    Percentage = n / sum(n) * 100,
    Label = paste0(n, " (", round(Percentage, 1), "%)")
  )

# Colors for ANC categories
anc_colors <- c(
  "No ANC (0 visits)" = "#E74C3C",
  "Partial ANC (1-3 visits)" = "#F39C12",
  "Adequate ANC (4+ visits)" = "#2ECC71"
)

# Create donut chart
anc_donut_2017 <- ggplot(anc_2017, aes(x = 2, y = Percentage, fill = ANC_category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y", direction = -1) +
  xlim(0.5, 2.5) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold", color = "black") +
  labs(
    title = "Figure 1: Status of ANC Contact Among Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Year 2017",
    fill = "ANC Category"
  ) +
  scale_fill_manual(values = anc_colors) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Display donut chart
print(anc_donut_2017)

# 9.3. ANC Trend Table 2010-2017
anc_table_data <- PDSR_2009_2017 %>%
  filter(EFY >= 2010 & !is.na(ANC_category)) %>%
  group_by(EFY, ANC_category) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  ungroup() %>%
  select(EFY, ANC_category, Display) %>%
  pivot_wider(
    names_from = ANC_category,
    values_from = Display,
    values_fill = "0 (0%)"
  ) %>%
  mutate(EFY = as.character(EFY))

# Add total row
totals_anc <- PDSR_2009_2017 %>%
  filter(EFY >= 2010 & !is.na(ANC_category)) %>%
  group_by(ANC_category) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 1),
    Display = paste0(Frequency, " (", Percentage, "%)")
  ) %>%
  select(ANC_category, Display) %>%
  pivot_wider(
    names_from = ANC_category,
    values_from = Display,
    values_fill = "0 (0%)"
  ) %>%
  mutate(EFY = "Total") %>%
  select(EFY, everything())

final_anc_table <- bind_rows(anc_table_data, totals_anc)

# Create formatted table
table_anc_final <- final_anc_table %>%
  gt() %>%
  tab_header(
    title = "Table 7: Antenatal Care (ANC) Visits Among Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_label(
    EFY = "EFY",
    `No ANC (0 visits)` = "No ANC (0 visits)",
    `Partial ANC (1-3 visits)` = "Partial ANC (1-3 visits)",
    `Adequate ANC (4+ visits)` = "Adequate ANC (4+ visits)"
  ) %>%
  tab_source_note(
    source_note = "Source: PDSR surveillance data, EFY 2010-2017"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_anc_final

# ==============================================================================
# 10. CAUSES OF DEATH ANALYSIS (ICD-PM)
# ==============================================================================

# 10.1. Create Death Category variable
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    Death_Category = case_when(
      Timingofthedeath == "Antepartum stillbirth" ~ "Antepartum",
      Timingofthedeath %in% c("Intrapartum stillbirth", "Stillbirth of unknown time") ~ "Intrapartum",
      Timingofthedeath %in% c("Death within 24 hours", 
                              "Death between 1-7 days", 
                              "Death between 8-28 days") ~ "Neonate",
      TRUE ~ NA_character_
    )
  )

# Check distribution
table(PDSR_2009_2017$Death_Category, useNA = "always")

# 10.2. Top Causes of Death by ICD-PM Group (EFY 2017)
plot_data_icdpm <- PDSR_2009_2017 %>%
  filter(EFY == 2017) %>%
  mutate(
    Clean_ICDPM = str_remove(ICDPM_Group_Updated, "^[A-Z][0-9]+: "),
    Death_Category = factor(Death_Category, 
                            levels = c("Antepartum", "Intrapartum", "Neonate"),
                            labels = c("Antepartum Deaths", "Intrapartum Deaths", "Neonatal Deaths"))
  ) %>%
  count(Death_Category, Clean_ICDPM) %>%
  filter(n > 0) %>%
  mutate(Clean_ICDPM = reorder_within(Clean_ICDPM, n, Death_Category))

# Create bar plot
causes_plot_2017 <- ggplot(plot_data_icdpm, aes(x = n, y = Clean_ICDPM, fill = Death_Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Death_Category, scales = "free_y", ncol = 1) +
  scale_y_reordered() +
  scale_fill_manual(values = c("Antepartum Deaths" = "#3597E8", 
                               "Intrapartum Deaths" = "#5CB85C", 
                               "Neonatal Deaths" = "#FF9800")) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Figure 2: Causes of Perinatal Death by ICD-PM Group",
    subtitle = "Ethiopian Fiscal Year 2017",
    x = "Frequency (Number of Deaths)",
    y = "ICD-PM Cause of Death"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  ) +
  expand_limits(x = max(plot_data_icdpm$n) * 1.2)

# Display plot
print(causes_plot_2017)

# 10.3. Causes of Death Table for EFY 2017
table_data_icdpm <- PDSR_2009_2017 %>%
  filter(EFY == 2017) %>%
  mutate(
    Clean_ICDPM = str_remove(ICDPM_Group_Updated, "^[A-Z][0-9]+: "),
    Death_Category = factor(Death_Category, 
                            levels = c("Antepartum", "Intrapartum", "Neonate"),
                            labels = c("Antepartum Deaths", "Intrapartum Deaths", "Neonatal Deaths"))
  ) %>%
  count(Death_Category, Clean_ICDPM) %>%
  arrange(Death_Category, desc(n)) %>%
  group_by(Death_Category) %>%
  mutate(
    Category_Total = sum(n),
  ) %>%
  ungroup() %>%
  mutate(
    Overall_Total = sum(n),
    Percent_Category = round(n / Category_Total * 100, 1),
    Percent_Overall = round(n / Overall_Total * 100, 1)
  )

# Create formatted table
table_cod_final <- table_data_icdpm %>%
  gt(groupname_col = "Death_Category") %>%
  fmt_number(
    columns = c(n, Category_Total, Percent_Category, Overall_Total, Percent_Overall),
    decimals = 0
  ) %>%
  cols_label(
    Clean_ICDPM = "ICD-PM Cause of Death",
    n = "Frequency",
    Category_Total = "Total by Category",
    Percent_Category = "Percentage (%) of Category",
    Overall_Total = "Total All Categories",
    Percent_Overall = "Percentage (%) of Total"
  ) %>%
  tab_header(
    title = "Table 8: Causes of Perinatal Death by ICD-PM Group",
    subtitle = "Ethiopian Fiscal Year 2017"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  )

# Display table
table_cod_final

# ==============================================================================
# 11. MATERNAL OUTCOME ANALYSIS
# ==============================================================================

# 11.1. Maternal Status Trend 2010-2017
maternal_outcome_counts <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  mutate(
    Maternal_Status = ifelse(Isthemotherofthedeceasedperinatealive == "Yes", "Alive", "Died"),
    Year_Label = paste0(EFY, " EFY")
  ) %>%
  group_by(Year_Label, Maternal_Status) %>%
  summarise(Count = n(), .groups = "drop")

# Create stacked bar plot
maternal_plot <- ggplot(maternal_outcome_counts, aes(x = Year_Label, y = Count, fill = Maternal_Status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 1.02),
            size = 3, 
            color = "black", 
            fontface = "bold") +
  facet_wrap(~Maternal_Status, ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Figure 3: Perinatal Deaths by EFY and Maternal Outcome",
    subtitle = "Counts of perinatal deaths faceted by maternal outcome (Alive vs Died)",
    x = "EFY",
    y = "Number of Perinatal Deaths",
    fill = "Maternal Outcome"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

# Display plot
print(maternal_plot)

# ==============================================================================
# 12. EDUCATIONAL STATUS ANALYSIS
# ==============================================================================

# Recode educational status
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    Educationalstatus_updated = case_when(
      Educationalstatusofthemother == "College and above" ~ "College and above",
      Educationalstatusofthemother == "Elementary" ~ "Elementary school",
      Educationalstatusofthemother == "High school" ~ "High school",
      Educationalstatusofthemother %in% c(
        "No formal education",
        "No formal, but can read & write"
      ) ~ "No formal Education",
      TRUE ~ NA_character_
    )
  )

# Check distribution
table(PDSR_2009_2017$Educationalstatus_updated, useNA = "always")

# ==============================================================================
# 13. TIMING OF DEATH ANALYSIS
# ==============================================================================

# 13.1. Recode timing of death
PDSR_2009_2017 <- PDSR_2009_2017 %>%
  mutate(
    Timingofthedeath = factor(Timingofthedeath,
                              levels = c(
                                "Antepartum stillbirth",
                                "Intrapartum stillbirth",
                                "Stillbirth of unknown time",
                                "Death within 24 hours",
                                "Death between 1-7 days",
                                "Death between 8-28 days"
                              )),
    Death_type = case_when(
      Timingofthedeath == "Antepartum stillbirth" ~ "Antepartum",
      Timingofthedeath %in% c("Intrapartum stillbirth", "Stillbirth of unknown time") ~ "Intrapartum",
      Timingofthedeath %in% c("Death within 24 hours", "Death between 1-7 days", "Death between 8-28 days") ~ "Neonatal",
      TRUE ~ NA_character_
    )
  )

# 13.2. Timing of Death Trend Table
timing_table <- PDSR_2009_2017 %>%
  filter(EFY >= 2010) %>%
  group_by(EFY, Timingofthedeath) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  ungroup() %>%
  mutate(label = paste0(count, " (", percent, "%)")) %>%
  select(EFY, Timingofthedeath, label) %>%
  pivot_wider(names_from = EFY, values_from = label) %>%
  arrange(Timingofthedeath)

# Create formatted table
table_timing_final <- timing_table %>%
  gt() %>%
  tab_header(
    title = "Table 9: Perinatal Deaths by Timing of Death",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  cols_label(
    Timingofthedeath = "Timing of Death"
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "0 (0%)"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_timing_final

# ==============================================================================
# 14. MOTHER CHARACTERISTICS TABLE (EFY 2017)
# ==============================================================================

# Filter EFY 2017 data
data_2017 <- PDSR_2009_2017 %>% filter(EFY == 2017)

# 14.1. Age group table
age_tbl <- data_2017 %>%
  count(MaternalAgeCategory) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  rename(Category = MaternalAgeCategory,
         Frequency = n) %>%
  mutate(Variable = "Age group") %>%
  select(Variable, Category, Frequency, percent)

# 14.2. ANC follow up table
anc_tbl <- data_2017 %>%
  mutate(ANC_group = case_when(
    ANC_category == "Adequate ANC (4+ visits)" ~ "Four visit above",
    ANC_category == "Partial ANC (1-3 visits)" ~ "At least one visit",
    ANC_category == "No ANC (0 visits)" ~ "No visit"
  )) %>%
  count(ANC_group) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  rename(Category = ANC_group,
         Frequency = n) %>%
  mutate(Variable = "ANC follow up") %>%
  select(Variable, Category, Frequency, percent)

# 14.3. Mode of delivery table
delivery_tbl <- data_2017 %>%
  count(Modeofdeliveryofthedeceasedbaby) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  rename(Category = Modeofdeliveryofthedeceasedbaby,
         Frequency = n) %>%
  mutate(Variable = "Mode of delivery") %>%
  select(Variable, Category, Frequency, percent)

# 14.4. Educational status table
edu_tbl <- data_2017 %>%
  count(Educationalstatus_updated) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  rename(Category = Educationalstatus_updated,
         Frequency = n) %>%
  mutate(Variable = "Educational status") %>%
  select(Variable, Category, Frequency, percent)

# 14.5. Combine all tables
final_mother_tbl <- bind_rows(age_tbl, anc_tbl, delivery_tbl, edu_tbl) %>%
  group_by(Variable) %>%
  mutate(Variable = ifelse(row_number() == 1, Variable, "")) %>%
  ungroup()

# Create formatted table
table_mother_final <- final_mother_tbl %>%
  gt() %>%
  tab_header(
    title = "Table 10: Characteristics of Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Year 2017"
  ) %>%
  cols_label(
    Variable = "Variable",
    Category = "Category",
    Frequency = "Frequency",
    percent = "Percentage (%)"
  ) %>%
  sub_missing(
    columns = c("Variable", "Category", "Frequency", "percent"),
    missing_text = "0"
  ) %>%
  fmt_number(
    columns = c("Frequency"),
    decimals = 0
  ) %>%
  fmt_number(
    columns = c("percent"),
    decimals = 1
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_mother_final

# ==============================================================================
# 15. DELAY ANALYSIS - THREE DELAYS MODEL
# ==============================================================================

# 15.1. Define delay columns
delay_cols <- c(
  "DidNotrecognizethedangersignsofnewborninfants.D1",
  "Unawareofthewarningsignsofproblemsduringpregnancy.D1",
  "DidNotkNowwheretogo.D1",
  "HadNoonetotakecareofotherchildren..D1",
  "Reliantontraditionalpracticemedicine.d1",
  "Lackofdecisiontogotothehealthfacility.D1",
  "TransportwasNotavailable.D2",
  "Transportwastooexpensive.d2",
  "Nofacilitywithinreasonabledistance.D2",
  "Lackofroadaccess.D2",
  "Familylackedmoneyforhealthcare.D3",
  "DelayedarrivaltonextfacilityfromaNotherreferringfacility.D3",
  "Delayedmanagementafteradmission.D3",
  "Feartobescoldedorshoutedatbythestaff.D3",
  "Humanerrorormismanagement.D3",
  "Delayinfirstevaluationbycaregiverafteradmission.D3",
  "Lackofsuppliesorequipment.D3"
)

# 15.2. Individual Delay Factors Table (EFY 2017)
table_2017 <- PDSR_2009_2017 %>%
  filter(EFY == 2017) %>%
  select(all_of(delay_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Value") %>%
  mutate(Value = ifelse(Value == 1 | Value == "Yes", 1, 0)) %>%
  mutate(
    DelayGroup = case_when(
      str_detect(Factor, regex("\\.D1$", ignore_case = TRUE)) ~ "Delay One",
      str_detect(Factor, regex("\\.D2$", ignore_case = TRUE)) ~ "Delay Two",
      str_detect(Factor, regex("\\.D3$", ignore_case = TRUE)) ~ "Delay Three"
    ),
    DelayGroup = factor(DelayGroup, levels = c("Delay One", "Delay Two", "Delay Three")),
    FactorLabel = case_when(
      Factor == "DidNotrecognizethedangersignsofnewborninfants.D1" ~ "Did not recognize danger signs for newborn",
      Factor == "Unawareofthewarningsignsofproblemsduringpregnancy.D1" ~ "Unaware of warning signs during pregnancy",
      Factor == "DidNotkNowwheretogo.D1" ~ "Did not know where to go",
      Factor == "HadNoonetotakecareofotherchildren..D1" ~ "No one to care for other children",
      Factor == "Reliantontraditionalpracticemedicine.d1" ~ "Reliance on traditional medicine",
      Factor == "Lackofdecisiontogotothehealthfacility.D1" ~ "Lack of decision to go to health facility",
      Factor == "TransportwasNotavailable.D2" ~ "Transport not available",
      Factor == "Transportwastooexpensive.d2" ~ "Transport too expensive",
      Factor == "Nofacilitywithinreasonabledistance.D2" ~ "No facility within reasonable distance",
      Factor == "Lackofroadaccess.D2" ~ "Lack of road access",
      Factor == "Familylackedmoneyforhealthcare.D3" ~ "Family lacked money for healthcare",
      Factor == "DelayedarrivaltonextfacilityfromaNotherreferringfacility.D3" ~ "Delayed arrival to next facility",
      Factor == "Delayedmanagementafteradmission.D3" ~ "Delayed management after admission",
      Factor == "Feartobescoldedorshoutedatbythestaff.D3" ~ "Fear of being scolded/shouted at",
      Factor == "Humanerrorormismanagement.D3" ~ "Human error or mismanagement",
      Factor == "Delayinfirstevaluationbycaregiverafteradmission.D3" ~ "Delay in first evaluation",
      Factor == "Lackofsuppliesorequipment.D3" ~ "Lack of supplies or equipment",
      TRUE ~ Factor
    )
  ) %>%
  group_by(DelayGroup, FactorLabel) %>%
  summarise(
    Frequency = sum(Value, na.rm = TRUE),
    Percentage = round((sum(Value, na.rm = TRUE) / nrow(data_2017)) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(DelayGroup, desc(Frequency))

# Create formatted table
table_delays_final <- table_2017 %>%
  gt(groupname_col = "DelayGroup") %>%
  tab_header(
    title = "Table 11: Individual Factors Responsible for Perinatal Deaths Under Three Delays Model",
    subtitle = "Ethiopian Fiscal Year 2017"
  ) %>%
  fmt_number(
    columns = c(Frequency),
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(Percentage),
    decimals = 1
  ) %>%
  cols_label(
    FactorLabel = "Contributing Factor",
    Frequency = "Frequency",
    Percentage = "Percentage (%)"
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    row_group.font.weight = "bold",
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_delays_final

# ==============================================================================
# 16. FINAL DELAY CATEGORIZATION
# ==============================================================================

# 16.1. Identify delay columns
delay1_cols <- names(PDSR_2009_2017)[str_detect(names(PDSR_2009_2017), regex("\\.D1$", ignore_case = TRUE))]
delay2_cols <- names(PDSR_2009_2017)[str_detect(names(PDSR_2009_2017), regex("\\.D2$", ignore_case = TRUE))]
delay3_cols <- names(PDSR_2009_2017)[str_detect(names(PDSR_2009_2017), regex("\\.D3$", ignore_case = TRUE))]

# 16.2. Compute normalized scores and assign Final Delay
PDSR_2009_2017_final <- PDSR_2009_2017 %>%
  rowwise() %>%
  mutate(
    Delay1_score = sum(c_across(all_of(delay1_cols)) == 1 | c_across(all_of(delay1_cols)) == "Yes", na.rm = TRUE) / length(delay1_cols),
    Delay2_score = sum(c_across(all_of(delay2_cols)) == 1 | c_across(all_of(delay2_cols)) == "Yes", na.rm = TRUE) / length(delay2_cols),
    Delay3_score = sum(c_across(all_of(delay3_cols)) == 1 | c_across(all_of(delay3_cols)) == "Yes", na.rm = TRUE) / length(delay3_cols),
    Final_Delay = case_when(
      max(c(Delay1_score, Delay2_score, Delay3_score)) == 0 ~ "No Delay",
      Delay1_score == max(c(Delay1_score, Delay2_score, Delay3_score)) ~ "Delay One",
      Delay2_score == max(c(Delay1_score, Delay2_score, Delay3_score)) ~ "Delay Two",
      Delay3_score == max(c(Delay1_score, Delay2_score, Delay3_score)) ~ "Delay Three"
    )
  ) %>%
  ungroup()

# 16.3. Create binary delay variable
PDSR_2009_2017_final <- PDSR_2009_2017_final %>%
  mutate(
    Delay_vs_NoDelay = ifelse(Final_Delay == "No Delay", "No Delay", "Delay")
  )

# Check distribution
table(PDSR_2009_2017_final$Delay_vs_NoDelay)

# ==============================================================================
# 17. DELAY STATUS TREND 2010-2017
# ==============================================================================

# 17.1. Delay Status Trend Table
delay_trend_table <- PDSR_2009_2017_final %>%
  filter(EFY >= 2010) %>%
  group_by(EFY, Delay_vs_NoDelay) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 1)) %>%
  ungroup() %>%
  mutate(EFY = as.character(EFY))

# Calculate totals
grand_total <- sum(delay_trend_table$Frequency)

total_per_category <- delay_trend_table %>%
  group_by(Delay_vs_NoDelay) %>%
  summarise(
    EFY = "Total",
    Frequency = sum(Frequency),
    Percentage = round((Frequency / grand_total) * 100, 1),
    .groups = "drop"
  )

# Combine tables
delay_trend_final <- bind_rows(delay_trend_table, total_per_category)

# Create formatted table
table_delay_trend_final <- delay_trend_final %>%
  gt() %>%
  tab_header(
    title = "Table 12: Delay Status Among Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Years 2010-2017"
  ) %>%
  fmt_number(
    columns = c(Frequency),
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(Percentage),
    decimals = 1
  ) %>%
  cols_label(
    EFY = "EFY",
    Delay_vs_NoDelay = "Delay Category",
    Frequency = "Frequency",
    Percentage = "Percentage (%)"
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6),
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  )

# Display table
table_delay_trend_final

# ==============================================================================
# 18. DELAY DONUT CHART (EFY 2017)
# ==============================================================================

# Prepare donut chart data
donut_data_2017 <- PDSR_2009_2017_final %>%
  filter(EFY == 2017) %>%
  group_by(Delay_vs_NoDelay) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(
    Delay_vs_NoDelay = ifelse(Delay_vs_NoDelay == "Delay", "With Delay", Delay_vs_NoDelay),
    Percentage = Frequency / sum(Frequency) * 100,
    Label = paste0(Delay_vs_NoDelay, "\n", Frequency, " (", round(Percentage, 1), "%)")
  ) %>%
  arrange(Delay_vs_NoDelay) %>%
  mutate(
    ymax = cumsum(Percentage),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2
  )

# Create donut chart
delay_donut_2017 <- ggplot(donut_data_2017, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Delay_vs_NoDelay)) +
  geom_rect() +
  coord_polar(theta = "y", direction = -1) +
  xlim(c(2, 4)) +
  geom_text(aes(x = 3.5, y = label_pos, label = Label), color = "white", fontface = "bold", size = 4.2) +
  scale_fill_manual(values = c("With Delay" = "#FF5733", "No Delay" = "#3498DB")) +
  theme_void() +
  labs(
    title = "Figure 4: Delay Status Among Mothers of Deceased Perinates",
    subtitle = "Ethiopian Fiscal Year 2017"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )

# Display donut chart
print(delay_donut_2017)

# ==============================================================================
# 19. SPECIFIC DELAY TYPE TREND (EXCLUDING NO DELAY)
# ==============================================================================

# 19.1. Filter only rows with delays
delay_only_trend <- PDSR_2009_2017_final %>%
  filter(Final_Delay != "No Delay") %>%
  group_by(EFY, Final_Delay) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  ungroup()

# 19.2. Create combined table
delay_wide_combined <- delay_only_trend %>%
  select(EFY, Final_Delay, Frequency) %>%
  group_by(EFY) %>%
  mutate(
    Total_Freq_EFY = sum(Frequency),
    Percentage = (Frequency / Total_Freq_EFY) * 100,
    Combined = paste0(Frequency, " (", round(Percentage, 1), "%)")
  ) %>%
  ungroup() %>%
  select(EFY, Final_Delay, Combined) %>%
  pivot_wider(
    names_from = Final_Delay,
    values_from = Combined,
    values_fill = "0"
  ) %>%
  arrange(EFY) %>%
  mutate(EFY = as.character(EFY))

# Calculate totals per EFY
delay_totals <- delay_only_trend %>%
  group_by(EFY) %>%
  summarise(Total_Freq = sum(Frequency), .groups = "drop") %>%
  mutate(Total = paste0(Total_Freq, " (100%)"), EFY = as.character(EFY))

delay_wide_combined <- delay_wide_combined %>%
  left_join(delay_totals %>% select(EFY, Total), by = "EFY")

# Calculate overall totals
delay_sums <- delay_only_trend %>%
  group_by(Final_Delay) %>%
  summarise(Total_Freq = sum(Frequency), .groups = "drop") %>%
  ungroup()

overall_total <- sum(delay_sums$Total_Freq)

total_row <- delay_sums %>%
  mutate(
    Combined = paste0(Total_Freq, " (", round((Total_Freq / overall_total) * 100, 1), "%)")
  ) %>%
  select(Final_Delay, Combined) %>%
  pivot_wider(
    names_from = Final_Delay,
    values_from = Combined,
    values_fill = "0"
  ) %>%
  mutate(
    EFY = "Total",
    Total = paste0(overall_total, " (100%)")
  ) %>%
  select(EFY, `Delay One`, `Delay Two`, `Delay Three`, Total)

# Combine all
delay_wide_final <- bind_rows(delay_wide_combined, total_row)

# Create formatted table
table_delay_types_final <- delay_wide_final %>%
  gt() %>%
  tab_header(
    title = "Table 13: Distribution of Delay Types Among Mothers with Delays",
    subtitle = "Ethiopian Fiscal Years 2010-2017 (Excluding 'No Delay' Cases)"
  ) %>%
  cols_label(
    EFY = "EFY",
    `Delay One` = "Delay One",
    `Delay Two` = "Delay Two",
    `Delay Three` = "Delay Three",
    Total = "Total"
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(5),
    column_labels.font.size = px(12),
    heading.title.font.size = 14,
    heading.title.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = EFY == "Total")
  )

# Display table
table_delay_types_final

# ==============================================================================
# 20. SUMMARY AND FINAL OUTPUTS
# ==============================================================================

cat("\n" + strrep("=", 80) + "\n")
cat("PERINATAL DEATH SURVEILLANCE AND RESPONSE ANALYSIS COMPLETED\n")
cat(strrep("=", 80) + "\n\n")

cat("SUMMARY OF ANALYSIS:\n")
cat("1. Reporting characteristics for EFY 2017 (N =", N_total_2017, ")\n")
cat("2. Perinatal characteristics for EFY 2017\n")
cat("3. Trend analyses for 2010-2017:\n")
cat("   - Birth outcomes\n")
cat("   - Gestational age by outcome\n")
cat("   - Mode of delivery by outcome\n")
cat("   - Maternal age distribution\n")
cat("   - ANC visit patterns\n")
cat("4. Causes of death (ICD-PM classification)\n")
cat("5. Maternal outcomes and characteristics\n")
cat("6. Three delays model analysis\n")
cat("7. Delay categorization and trends\n\n")

cat("KEY OUTPUTS GENERATED:\n")
cat("- 13 Tables (Table 1 to Table 13)\n")
cat("- 4 Figures (Figures 1 to 4)\n")
cat("- Complete trend analysis 2010-2017\n")
cat("- Delay model implementation and categorization\n\n")

cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 80) + "\n")

# Save all tables as HTML files for reporting
# Uncomment to save:
# gtsave(table_1_final, "Table1_Reporting_Characteristics.html")
# gtsave(table_2_final, "Table2_Perinatal_Characteristics.html")
# gtsave(trend_table_1_final, "Table3_Birth_Outcome_Trend.html")
# ... and so on for all tables

# Save plots as PNG files
# Uncomment to save:
# ggsave("Figure1_ANC_Donut_2017.png", anc_donut_2017, width = 10, height = 8, dpi = 300)
# ggsave("Figure2_Causes_of_Death_2017.png", causes_plot_2017, width = 12, height = 10, dpi = 300)
# ggsave("Figure3_Maternal_Outcome.png", maternal_plot, width = 12, height = 8, dpi = 300)
# ggsave("Figure4_Delay_Donut_2017.png", delay_donut_2017, width = 10, height = 8, dpi = 300)