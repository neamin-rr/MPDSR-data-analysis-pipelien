###############################################################################
# MDSR DATA ANALYSIS - MATERNAL DEATHS IN ETHIOPIA (2006-2017)
# Complete Professional Organization with All Original Analyses
###############################################################################

# =============================================================================
# 1. INITIAL SETUP AND DATA LOADING
# =============================================================================

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(stringr)
library(patchwork)
library(grid)
library(viridis)
library(tidytext)
library(gt)

# Load and clean main dataset
MDSR_2006_2017 <- Final_national_MDSR_data_20251216_cleaned

# Filter 2017 data for specific analyses
MDSR_2017 <- MDSR_2006_2017 %>% filter(EFY == 2017)

# =============================================================================
# 2. DEMOGRAPHIC CHARACTERISTICS
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1 Age Distribution Analysis
# -----------------------------------------------------------------------------

# Create age categories
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  mutate(
    AgeCategory = cut(
      AgeAtDeathinyears,
      breaks = c(10, 15, 20, 25, 30, 35, 40, 45, 50),
      labels = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
      right = FALSE
    )
  )

# Median age and range for 2017
median_age_2017 <- median(MDSR_2017$AgeAtDeathinyears, na.rm = TRUE)
age_range_2017 <- range(MDSR_2017$AgeAtDeathinyears, na.rm = TRUE)

cat("Median age in 2017:", median_age_2017, "\n")
cat("Age range in 2017:", age_range_2017[1], "-", age_range_2017[2], "\n")

# Age summary table for 2017
age_summary_2017 <- MDSR_2017 %>%
  group_by(AgeCategory) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# Display table
kable(age_summary_2017, caption = "Age Distribution of Deaths in 2017", align = "c")

# Age trend plot (2006-2017)
age_year_summary <- MDSR_2006_2017 %>%
  group_by(EFY, AgeCategory) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(Proportion = round(Proportion, 1))

# -----------------------------------------------------------------------------
# 2.2 Age Trend Visualization
# -----------------------------------------------------------------------------
age_trend_plot <- ggplot(age_year_summary, 
                         aes(x = EFY, y = Proportion, 
                             color = AgeCategory, group = AgeCategory)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = unique(age_year_summary$EFY),
    labels = paste0(unique(age_year_summary$EFY), " EFY")
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Age Group Distribution Among Reviewed Maternal Deaths (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Proportion of Deaths (%)",
    color = "Age Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(age_trend_plot)

# =============================================================================
# 3. PLACE OF DEATH ANALYSIS
# =============================================================================

# Group place of death categories
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  mutate(
    Placeofdeath_grouped = case_when(
      Placeofdeath_clean %in% c("Other", "Private Health Facility", "Unknown/Unspecified") ~ "Other/Unknown",
      TRUE ~ Placeofdeath_clean
    )
  )

# Update 2017 data similarly
MDSR_2017 <- MDSR_2017 %>%
  mutate(
    Placeofdeath_grouped = case_when(
      Placeofdeath_clean %in% c("Other", "Private Health Facility", "Unknown/Unspecified") ~ "Other/Unknown",
      TRUE ~ Placeofdeath_clean
    )
  )

# Check the grouped table
table(MDSR_2017$Placeofdeath_grouped)

# -----------------------------------------------------------------------------
# 3.1 Place of Death Donut Chart (2017)
# -----------------------------------------------------------------------------
place_summary_2017 <- MDSR_2017 %>%
  group_by(Placeofdeath_grouped) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 1),
    Label = paste0(Count, " (", Percentage, "%)"),
    ymax = cumsum(Count),
    ymin = lag(ymax, default = 0),
    mid = (ymin + ymax)/2,
    Label_display = ifelse(Placeofdeath_grouped == "Other/Unknown",
                           paste0(Label, "."), Label),
    Label_inside = ifelse(Placeofdeath_grouped != "Other/Unknown", Label_display, NA),
    Label_outside = ifelse(Placeofdeath_grouped == "Other/Unknown", Label_display, NA)
  )

place_summary_2017$Placeofdeath_grouped <- factor(
  place_summary_2017$Placeofdeath_grouped,
  levels = place_summary_2017$Placeofdeath_grouped
)

# Donut chart creation
donut_plot <- ggplot(place_summary_2017, 
                     aes(ymax = ymax, ymin = ymin, xmax = 4.5, xmin = 1.2, 
                         fill = Placeofdeath_grouped)) +
  geom_rect(color = "white", linewidth = 1.2) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  scale_fill_brewer(palette = "Set2", name = "Place of Death") +
  xlim(c(0, 5.5)) +
  labs(
    title = "Distribution of the Place of Maternal Death",
    subtitle = "2017 EFY Analysis"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18,
                              margin = margin(b = 9), color = "#2c3e50"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 16,
                                 margin = margin(b = 10), color = "#2c3e50"),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    plot.margin = margin(15, 20, 20, 20),
    plot.background = element_rect(fill = "#f8f9fa", color = NA)
  )

# Donut hole
donut_plot <- donut_plot + 
  annotate("rect", xmin = 0, xmax = 1.2, ymin = 0, ymax = max(place_summary_2017$ymax),
           fill = "white", color = NA)

# Labels inside slices
donut_plot <- donut_plot + geom_text(
  aes(x = 2.8, y = mid, label = Label_inside),
  color = "black", size = 4.5, fontface = "bold",
  hjust = 0.5, na.rm = TRUE
)

# Other/Unknown outside with arrow
other <- place_summary_2017 %>% filter(Placeofdeath_grouped == "Other/Unknown")
donut_plot <- donut_plot +
  geom_segment(
    data = other,
    aes(x = 4.5, xend = 5.1, y = mid, yend = mid),
    arrow = arrow(length = unit(0.04, "npc"), type = "closed"),
    color = "#2c3e50", linewidth = 1.2
  ) +
  geom_label(
    data = other,
    aes(x = 5.25, y = mid, label = Label_outside),
    color = "#2c3e50", fill = "white", size = 4.5,
    fontface = "bold", hjust = 0, label.size = 1,
    label.padding = unit(0.3, "lines"), label.r = unit(0.25, "lines")
  )

# Total count in center
total_count <- sum(place_summary_2017$Count)
donut_plot <- donut_plot + 
  annotate("text", x = 0, y = 0, 
           label = paste("Total:\n", total_count, "Cases"),
           size = 5.5, fontface = "bold", color = "#2c3e50",
           lineheight = 0.9)

print(donut_plot)

# -----------------------------------------------------------------------------
# 3.2 Place of Death Trends (2006-2017)
# -----------------------------------------------------------------------------
place_year_summary <- MDSR_2006_2017 %>%
  group_by(EFY, Placeofdeath_grouped) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1)) %>%
  ungroup()

place_trend_plot <- ggplot(place_year_summary, 
                           aes(x = EFY, y = Proportion, 
                               color = Placeofdeath_grouped, group = Placeofdeath_grouped)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = unique(place_year_summary$EFY), 
                     labels = paste0(unique(place_year_summary$EFY), " EFY")) +
  labs(
    title = "Proportion of Maternal Deaths by Place of Death",
    subtitle = "2006–2017 EFY",
    x = "Fiscal Year (EFY)",
    y = "Proportion (%)",
    color = "Place of Death"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(place_trend_plot)

# -----------------------------------------------------------------------------
# 3.3 Place of Death Wide Table
# -----------------------------------------------------------------------------
place_year_table <- place_year_summary %>%
  mutate(Label = paste0(Count, " (", round(Proportion, 1), "%)")) %>%
  select(EFY, Placeofdeath_grouped, Label) %>%
  pivot_wider(names_from = EFY, values_from = Label) %>%
  arrange(match(Placeofdeath_grouped, 
                c("Hospital", "Home", "Health Center", "In Transit", 
                  "Health Post", "Other/Unknown")))

# Render wide table with gt
place_year_table %>%
  gt() %>%
  tab_header(
    title = md("**Maternal Deaths by Place of Death (Wide Table)**"),
    subtitle = md("2006–2017 EFY")
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.width = pct(100)
  )

# =============================================================================
# 4. PARITY AND ANC VISITS ANALYSIS
# =============================================================================

# Create parity and ANC categories
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  mutate(
    ParityCategory = case_when(
      parity_impute == 0 ~ "Nullipara",
      parity_impute >= 1 & parity_impute <= 4 ~ "Multipara",
      parity_impute >= 5 ~ "Grandmultipara",
      TRUE ~ NA_character_
    ),
    ANC_updated_visit = case_when(
      ANC_visits_imputed == 0 ~ "No",
      ANC_visits_imputed >= 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    ANC_category = case_when(
      ANC_visits_imputed == 0 ~ "No Visit",
      ANC_visits_imputed >= 1 & ANC_visits_imputed <= 3 ~ "1–3 Visits",
      ANC_visits_imputed >= 4 ~ "4 and More Visits",
      TRUE ~ NA_character_
    )
  )

# Check distributions
table(MDSR_2006_2017$ParityCategory)
table(MDSR_2006_2017$ANC_updated_visit)
table(MDSR_2006_2017$ANC_category)

# Update 2017 data
MDSR_2017 <- MDSR_2017 %>%
  mutate(
    ParityCategory = factor(ParityCategory, levels = c("Nullipara", "Multipara", "Grandmultipara"))
  )

# Check 2017 distributions
table(MDSR_2017$ParityCategory)
table(MDSR_2017$ANC_updated_visit)
table(MDSR_2017$ANC_category)

# -----------------------------------------------------------------------------
# 4.1 Parity and ANC Combined Plot (2017)
# -----------------------------------------------------------------------------
# Parity summary
parity_summary_2017 <- MDSR_2017 %>%
  group_by(ParityCategory) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1))

# ANC summary
anc_summary_2017 <- MDSR_2017 %>%
  group_by(ANC_updated_visit) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1))

# Create plots
p1 <- ggplot(parity_summary_2017, aes(x = ParityCategory, y = Proportion, fill = ParityCategory)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", Proportion, "%)")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "A). Maternal Deaths by Parity Category (2017 EFY)",
    x = "", y = "Proportion (%)", fill = "Parity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

p2 <- ggplot(anc_summary_2017, aes(x = ANC_updated_visit, y = Proportion, fill = ANC_updated_visit)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", Proportion, "%)")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "B). Maternal Deaths by ANC Visits (2017 EFY)",
    x = "", y = "Proportion (%)", fill = "ANC Visit"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

# Combine plots
combined_plot <- p1 + p2 + plot_layout(ncol = 2) +
  plot_annotation(
    title = "2017 EFY Maternal Death Analysis",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  )

print(combined_plot)

# -----------------------------------------------------------------------------
# 4.2 Parity Category Wide Table (2006-2017)
# -----------------------------------------------------------------------------
parity_wide <- MDSR_2006_2017 %>%
  group_by(EFY, ParityCategory) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1)) %>%
  ungroup() %>%
  mutate(Label = paste0(Count, " (", Proportion, "%)")) %>%
  select(EFY, ParityCategory, Label) %>%
  pivot_wider(names_from = EFY, values_from = Label) %>%
  arrange(match(ParityCategory, c("Nullipara", "Multipara", "Grandmultipara")))

# Render wide table with gt
parity_wide %>%
  gt() %>%
  tab_header(
    title = md("**Maternal Deaths by Parity Category (Wide Table)**")
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.width = pct(100)
  )

# -----------------------------------------------------------------------------
# 4.3 ANC Visit Status Wide Table (2006-2017)
# -----------------------------------------------------------------------------
anc_wide <- MDSR_2006_2017 %>%
  group_by(EFY, ANC_updated_visit) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1)) %>%
  ungroup() %>%
  mutate(Label = paste0(Count, " (", Proportion, "%)")) %>%
  select(EFY, ANC_updated_visit, Label) %>%
  pivot_wider(names_from = EFY, values_from = Label) %>%
  arrange(match(ANC_updated_visit, c("No", "Yes")))

# Render wide table with gt
anc_wide %>%
  gt() %>%
  tab_header(
    title = md("**Maternal Deaths by ANC Visit Status (Wide Table)**")
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.width = pct(100)
  )

# -----------------------------------------------------------------------------
# 4.4 ANC Category Trends (2006-2017)
# -----------------------------------------------------------------------------
anc_year_summary <- MDSR_2006_2017 %>%
  group_by(EFY, ANC_category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1)) %>%
  ungroup() %>%
  arrange(EFY, ANC_category)

anc_year_summary$ANC_category <- factor(
  anc_year_summary$ANC_category,
  levels = c("No Visit", "1–3 Visits", "4 and More Visits")
)

anc_trend_plot <- ggplot(anc_year_summary, 
                         aes(x = EFY, y = Proportion, 
                             color = ANC_category, group = ANC_category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(
    breaks = unique(anc_year_summary$EFY),
    labels = paste0(unique(anc_year_summary$EFY), " EFY")
  ) +
  labs(
    title = "Trend of ANC Visit Categories Among Maternal Deaths",
    subtitle = "2006–2017 EFY",
    x = "Fiscal Year (EFY)",
    y = "Proportion (%)",
    color = "ANC Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(anc_trend_plot)

# -----------------------------------------------------------------------------
# 4.5 ANC Category Wide Table
# -----------------------------------------------------------------------------
anc_all_years <- MDSR_2006_2017 %>%
  group_by(EFY, ANC_category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1)) %>%
  ungroup() %>%
  arrange(EFY, factor(ANC_category, levels = c("No Visit", "1–3 Visits", "4 and More Visits")))

anc_all_years <- anc_all_years %>%
  mutate(Label = paste0(Count, " (", Proportion, "%)")) %>%
  select(EFY, ANC_category, Label) %>%
  tidyr::pivot_wider(names_from = EFY, values_from = Label)

# Render wide table with gt
anc_all_years %>%
  gt() %>%
  tab_header(
    title = md("**ANC Visit Categories Among Maternal Deaths**"),
    subtitle = md("2006–2017 EFY")
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.width = pct(100)
  )

# =============================================================================
# 5. TIME OF DEATH ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1 Time of Death Trends (2006-2017)
# -----------------------------------------------------------------------------
time_efy_summary <- MDSR_2006_2017 %>%
  filter(!is.na(TimeOfDeath_imputed_prop)) %>%
  group_by(EFY, TimeOfDeath_imputed_prop) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    total = sum(count),
    proportion = count / total * 100
  )

time_trend_plot <- ggplot(time_efy_summary,
                          aes(x = EFY, y = proportion,
                              color = TimeOfDeath_imputed_prop,
                              group = TimeOfDeath_imputed_prop)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = sort(unique(time_efy_summary$EFY)),
    labels = paste0(sort(unique(time_efy_summary$EFY)), " EFY")
  ) +
  labs(
    title = "Trend of Time of Death Among Maternal Deaths (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Proportion of Deaths (%)",
    color = "Time of Death"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(time_trend_plot)

# -----------------------------------------------------------------------------
# 5.2 Time of Death Wide Table
# -----------------------------------------------------------------------------
time_efy_wide <- MDSR_2006_2017 %>%
  filter(!is.na(TimeOfDeath_imputed_prop)) %>%
  group_by(EFY, TimeOfDeath_imputed_prop) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    proportion = round(count / sum(count) * 100, 1)
  ) %>%
  unite("Count (%)", count, proportion, sep = " (") %>%
  mutate(`Count (%)` = paste0(`Count (%)`, "%)")) %>%
  select(EFY, TimeOfDeath_imputed_prop, `Count (%)`) %>%
  pivot_wider(
    names_from = EFY,
    values_from = `Count (%)`
  )

# Display table with kable
kable(
  time_efy_wide,
  caption = "Time of Death Distribution by Ethiopian Fiscal Year (EFY)",
  align = "l",
  booktabs = TRUE
) %>%
  kable_styling(
    full_width = TRUE,
    position = "center",
    font_size = 12
  )

# =============================================================================
# 6. CAUSES OF MATERNAL DEATHS ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# 6.1 Cause Type Trends (2006-2017)
# -----------------------------------------------------------------------------
cause_efy_summary <- MDSR_2006_2017 %>%
  filter(!is.na(Cause_Type), !is.na(EFY)) %>%
  group_by(EFY, Cause_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    total = sum(count),
    proportion = round(count / total * 100, 1)
  )

cause_trend_plot <- ggplot(cause_efy_summary,
                           aes(x = EFY, y = proportion,
                               color = Cause_Type,
                               group = Cause_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = sort(unique(cause_efy_summary$EFY)),
    labels = paste0(sort(unique(cause_efy_summary$EFY)), " EFY")
  ) +
  labs(
    title = "Trend of Maternal Death Causes by EFY (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Proportion of Deaths (%)",
    color = "Cause Type"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(cause_trend_plot)

# -----------------------------------------------------------------------------
# 6.2 Leading Causes Trends
# -----------------------------------------------------------------------------
# Identify top 2 causes in 2017
top2_2017 <- MDSR_2006_2017 %>%
  filter(EFY == 2017, !is.na(Nine_Group_Final)) %>%
  count(Nine_Group_Final, sort = TRUE) %>%
  slice(1:2) %>%
  pull(Nine_Group_Final)

cat("Top 2 causes in 2017:", paste(top2_2017, collapse = ", "), "\n")

# Trend for top 2 causes
trend_top2 <- MDSR_2006_2017 %>%
  filter(Nine_Group_Final %in% top2_2017, !is.na(EFY)) %>%
  group_by(EFY, Nine_Group_Final) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(proportion = count / sum(count) * 100)

top_causes_plot <- ggplot(trend_top2,
                          aes(x = EFY, y = proportion,
                              color = Nine_Group_Final,
                              group = Nine_Group_Final)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 2006:2017,
    labels = paste0(2006:2017, " EFY")
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Trends of the Two Leading Causes of Maternal Deaths\n(2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Proportion of Maternal Deaths (%)",
    color = "Cause Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(top_causes_plot)

# -----------------------------------------------------------------------------
# 6.3 All Causes Trends (Faceted by Cause Type)
# -----------------------------------------------------------------------------
mdsr_clean <- MDSR_2006_2017 %>%
  filter(!is.na(Nine_Group_Final), !is.na(EFY)) %>%
  mutate(Cause_clean = str_remove(Nine_Group_Final, "^Group \\d+:\\s*"))

cause_trend_all <- mdsr_clean %>%
  group_by(EFY, Cause_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ungroup()

# Add Cause_Type for faceting
cause_trend_all <- cause_trend_all %>%
  left_join(
    MDSR_2006_2017 %>%
      select(Nine_Group_Final, Cause_Type) %>%
      distinct(Nine_Group_Final, .keep_all = TRUE) %>%
      mutate(Cause_clean = str_remove(Nine_Group_Final, "^Group \\d+:\\s*")) %>%
      select(Cause_clean, Cause_Type),
    by = "Cause_clean"
  )

# Order causes by overall size for legend
cause_order <- cause_trend_all %>%
  group_by(Cause_clean) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  pull(Cause_clean)

cause_trend_all$Cause_clean <- factor(cause_trend_all$Cause_clean, levels = cause_order)

# Color-blind palette
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
)

all_causes_plot <- ggplot(cause_trend_all,
                          aes(x = EFY, y = proportion,
                              color = Cause_clean,
                              group = Cause_clean)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 2006:2017,
    labels = paste0(2006:2017, " EFY")
  ) +
  scale_color_manual(values = cb_palette) +
  labs(
    title = "Trends of Maternal Death Causes by EFY (2006–2017)",
    x = "Fiscal Year (EFY)",
    y = "Proportion of Maternal Deaths (%)",
    color = "Cause of Death"
  ) +
  facet_wrap(~ Cause_Type, scales = "fixed") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

print(all_causes_plot)

# -----------------------------------------------------------------------------
# 6.4 Causes Distribution 2017 (Bar Chart)
# -----------------------------------------------------------------------------
cause_2017 <- MDSR_2006_2017 %>%
  filter(EFY == 2017, !is.na(Nine_Group_Final)) %>%
  mutate(Cause_clean = str_remove(Nine_Group_Final, "^Group \\d+:\\s*")) %>%
  group_by(Cause_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(proportion = round(count / sum(count) * 100, 1))

causes_2017_plot <- ggplot(cause_2017, 
                           aes(x = reorder(Cause_clean, count), y = count, fill = Cause_clean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", proportion, "%)")),
            hjust = -0.05, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = cb_palette) +
  coord_flip() +
  labs(
    title = "Distribution of Maternal Death Causes in 2017 EFY",
    x = "", y = "Number of Maternal Deaths", fill = "Cause of Death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30)))

print(causes_2017_plot)

# -----------------------------------------------------------------------------
# 6.5 Causes Wide Table
# -----------------------------------------------------------------------------
cause_table_wide <- MDSR_2006_2017 %>%
  filter(!is.na(Nine_Group_Final), !is.na(EFY)) %>%
  mutate(Cause_clean = str_remove(Nine_Group_Final, "^Group \\d+:\\s*")) %>%
  group_by(EFY, Cause_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(proportion = round(count / sum(count) * 100, 1)) %>%
  unite("Count (%)", count, proportion, sep = " (") %>%
  mutate(`Count (%)` = paste0(`Count (%)`, "%)")) %>%
  select(EFY, Cause_clean, `Count (%)`) %>%
  pivot_wider(
    names_from = EFY,
    values_from = `Count (%)`
  ) %>%
  rename_with(~ paste0(.x, " EFY"), -Cause_clean)

kable(
  cause_table_wide,
  caption = "Maternal Death Causes by EFY (Counts and Row-wise Proportions)",
  booktabs = TRUE,
  align = "l"
) %>%
  kable_styling(
    full_width = TRUE,
    position = "center",
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(seq(1, nrow(cause_table_wide), by = 2), background = "#F7F7F7") %>%
  column_spec(1, bold = TRUE)

# =============================================================================
# 7. MODE OF DELIVERY ANALYSIS (2017)
# =============================================================================

# Clean and classify delivery modes
MDSR_2017_clean <- MDSR_2017 %>%
  mutate(
    DeliveryMode_clean = case_when(
      str_detect(tolower(IfdeliveredModeofdelivery), "cs|c/s|abdom|operat") ~ "C-Section / Operative",
      str_detect(tolower(IfdeliveredModeofdelivery), "vd|vagina|vaginal|s/vd|s/v") ~ "Spontaneous Vaginal Delivery",
      str_detect(tolower(IfdeliveredModeofdelivery), "no|none") ~ "No Delivery",
      TRUE ~ NA_character_
    )
  )

# Check initial distribution
table(MDSR_2017_clean$DeliveryMode_clean, useNA = "always")

# Impute missing values proportionally
delivery_props <- MDSR_2017_clean %>%
  filter(!is.na(DeliveryMode_clean)) %>%
  count(DeliveryMode_clean) %>%
  mutate(prop = n / sum(n))

missing_idx <- which(is.na(MDSR_2017_clean$DeliveryMode_clean))
set.seed(123)

MDSR_2017_clean$DeliveryMode_clean[missing_idx] <- sample(
  delivery_props$DeliveryMode_clean,
  size = length(missing_idx),
  replace = TRUE,
  prob = delivery_props$prop
)

# Verify cleaned distribution
table(MDSR_2017_clean$DeliveryMode_clean)
prop.table(table(MDSR_2017_clean$DeliveryMode_clean)) * 100

# Summary and plot
delivery_2017_summary <- MDSR_2017_clean %>%
  group_by(DeliveryMode_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(proportion = round(count / sum(count) * 100, 1))

delivery_plot <- ggplot(delivery_2017_summary, 
                        aes(x = reorder(DeliveryMode_clean, count), y = count, 
                            fill = DeliveryMode_clean)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(count, " (", proportion, "%)")),
            hjust = -0.05, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "C-Section / Operative" = "#D55E00",
    "Spontaneous Vaginal Delivery" = "#0072B2",
    "No Delivery" = "#009E73"
  )) +
  coord_flip() +
  labs(
    title = "Mode of Delivery Distribution in 2017 EFY",
    x = "", y = "Number of Deliveries"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)))

print(delivery_plot)

# =============================================================================
# 8. SPECIFIC CAUSE ANALYSES BY SOURCE
# =============================================================================

# -----------------------------------------------------------------------------
# 8.1 Hemorrhage, Infection, HDP by Source (2017)
# -----------------------------------------------------------------------------
cause_sources_2017 <- MDSR_2017_clean %>%
  filter(EFY == 2017) %>%
  mutate(
    Cause_clean = case_when(
      str_detect(Nine_Group_Final, "Hemorrhage") ~ "Hemorrhage",
      str_detect(Nine_Group_Final, "infection") ~ "Pregnancy-related infection",
      str_detect(Nine_Group_Final, "Hypertensive") ~ "Hypertensive disorders",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Cause_clean != "Other")  # keep only 3 causes of interest

cause_source_table <- cause_sources_2017 %>%
  group_by(Cause_clean, MDRFextractedfrom) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Cause_clean) %>%
  mutate(proportion = round(count / sum(count) * 100, 1)) %>%
  ungroup()

# Pivot wider for table view
cause_source_wide <- cause_source_table %>%
  unite("Count (%)", count, proportion, sep = " (") %>%
  mutate(`Count (%)` = paste0(`Count (%)`, "%)")) %>%
  pivot_wider(
    names_from = MDRFextractedfrom,
    values_from = `Count (%)`,
    values_fill = "0 (0%)"
  )

# Display table
kable(cause_source_wide,
      caption = "Hemorrhage, Infection, and Hypertensive Disorders by Source (2017 EFY)",
      booktabs = TRUE,
      align = "l") %>%
  kable_styling(full_width = TRUE, position = "center", font_size = 12) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# -----------------------------------------------------------------------------
# 8.2 Hemorrhage Cases by Region (2017)
# -----------------------------------------------------------------------------
hemorrhage_region <- MDSR_2017_clean %>%
  filter(EFY == 2017, str_detect(Nine_Group_Final, "Hemorrhage")) %>%
  group_by(Region_new_1) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(proportion = round(count / sum(count) * 100, 1),
         `Count (%)` = paste0(count, " (", proportion, "%)")) %>%
  select(Region_new_1, `Count (%)`)

kable(
  hemorrhage_region,
  caption = "Hemorrhage Cases by Region (2017 EFY) — Percentages Relative to Total Hemorrhage Cases",
  booktabs = TRUE,
  align = "l"
) %>%
  kable_styling(
    full_width = TRUE,
    position = "center",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# =============================================================================
# 9. HEMORRHAGE-SPECIFIC ANALYSES
# =============================================================================

# -----------------------------------------------------------------------------
# 9.1 Hemorrhage Trends by Region (2006-2017)
# -----------------------------------------------------------------------------
hemorrhage_trend <- MDSR_2006_2017 %>%
  filter(str_detect(Nine_Group_Final, "Hemorrhage")) %>%
  group_by(EFY, Region_new_1) %>%
  summarise(count = n(), .groups = "drop")

hemorrhage_trend_plot <- ggplot(hemorrhage_trend, aes(x = EFY, y = count)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~Region_new_1, scales = "free_y") +
  scale_x_continuous(breaks = 2006:2017, labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Trend of Hemorrhage Cases by Region (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Number of Hemorrhage Cases"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(hemorrhage_trend_plot)

# -----------------------------------------------------------------------------
# 9.2 Hemorrhage Percentage by Region (2006-2017)
# -----------------------------------------------------------------------------
region_totals <- MDSR_2006_2017 %>%
  group_by(EFY, Region_new_1) %>%
  summarise(total_deaths = n(), .groups = "drop")

hemorrhage_percent <- MDSR_2006_2017 %>%
  filter(str_detect(Nine_Group_Final, "Hemorrhage")) %>%
  group_by(EFY, Region_new_1) %>%
  summarise(hemorrhage_count = n(), .groups = "drop") %>%
  left_join(region_totals, by = c("EFY", "Region_new_1")) %>%
  mutate(percent = round(hemorrhage_count / total_deaths * 100, 1))

hemorrhage_percent_plot <- ggplot(hemorrhage_percent, aes(x = EFY, y = percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~Region_new_1, scales = "free_y") +
  scale_x_continuous(breaks = 2006:2017, labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Trend of Hemorrhage Cases as Percentage of Total Deaths by Region (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Percentage of Maternal Deaths (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(hemorrhage_percent_plot)

# -----------------------------------------------------------------------------
# 9.3 Hemorrhage Bar Plot by Region (2006-2017)
# -----------------------------------------------------------------------------
hemorrhage_counts <- MDSR_2006_2017 %>%
  filter(str_detect(Nine_Group_Final, "Hemorrhage")) %>%
  group_by(EFY, Region_new_1) %>%
  summarise(count = n(), .groups = "drop")

hemorrhage_bar_plot <- ggplot(hemorrhage_counts, aes(x = factor(EFY), y = count, fill = Region_new_1)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.3, size = 2.2, fontface = "bold") +
  facet_wrap(~Region_new_1, scales = "free_y") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Hemorrhage Cases by Region (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Number of Hemorrhage Cases",
    fill = "Region"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 10)
  )

print(hemorrhage_bar_plot)

# -----------------------------------------------------------------------------
# 9.4 Hemorrhage by Region Table
# -----------------------------------------------------------------------------
hemorrhage_summary_table <- hemorrhage_percent %>%
  select(Region_new_1, EFY, hemorrhage_count, percent) %>%
  rename(
    "Region" = Region_new_1,
    "EFY" = EFY,
    "Hemorrhage Count" = hemorrhage_count,
    "Percentage (%)" = percent
  )

hemorrhage_table_wide <- hemorrhage_summary_table %>%
  select(Region, EFY, `Hemorrhage Count`, `Percentage (%)`) %>%
  unite("Count (%)", `Hemorrhage Count`, `Percentage (%)`, sep = " (") %>%
  mutate(`Count (%)` = paste0(`Count (%)`, "%)")) %>%
  pivot_wider(
    names_from = EFY,
    values_from = `Count (%)`
  )

# Reorder the columns manually from 2006 to 2017
year_order <- as.character(2006:2017)
hemorrhage_table_wide <- hemorrhage_table_wide %>%
  select(Region, all_of(year_order))

hemorrhage_table_wide %>%
  kable(
    caption = "Hemorrhage Cases and Percentage of Total Maternal Deaths by Region (2006–2017 EFY)",
    align = "l",
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = TRUE,
    position = "center",
    font_size = 12
  )

# -----------------------------------------------------------------------------
# 9.5 Hemorrhage by Place of Death (2006-2017)
# -----------------------------------------------------------------------------
hemorrhage_data <- MDSR_2006_2017 %>%
  filter(Nine_Group_Final == "Group 1: Hemorrhage")

hemorrhage_place_summary <- hemorrhage_data %>%
  group_by(EFY, Placeofdeath_grouped) %>%
  summarise(count = n(), .groups = "drop")

total_per_year <- hemorrhage_data %>%
  group_by(EFY) %>%
  summarise(total = n())

hemorrhage_place_summary <- hemorrhage_place_summary %>%
  left_join(total_per_year, by = "EFY") %>%
  mutate(percent = (count / total) * 100)

# Order Placeofdeath_grouped by total deaths
place_order <- hemorrhage_place_summary %>%
  group_by(Placeofdeath_grouped) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  pull(Placeofdeath_grouped)

hemorrhage_place_summary$Placeofdeath_grouped <- factor(
  hemorrhage_place_summary$Placeofdeath_grouped,
  levels = place_order
)

hemorrhage_place_plot <- ggplot(hemorrhage_place_summary, 
                                aes(x = EFY, y = percent, 
                                    color = Placeofdeath_grouped, 
                                    group = Placeofdeath_grouped)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2006:2017, labels = paste0(2006:2017, " EFY")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Trend of Maternal Deaths Due to Hemorrhage\n by Place of Death (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Percentage of Hemorrhage Deaths (%)",
    color = "Place of Death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

print(hemorrhage_place_plot)

# -----------------------------------------------------------------------------
# 9.6 Hemorrhage by Place of Death Table
# -----------------------------------------------------------------------------
hemorrhage_place_table <- hemorrhage_place_summary %>%
  mutate(percent = round(percent, 1)) %>%
  mutate(display = paste0(count, " (", percent, "%)")) %>%
  select(EFY, Placeofdeath_grouped, display) %>%
  pivot_wider(names_from = EFY, values_from = display) %>%
  arrange(desc(rowSums(!is.na(select(., -Placeofdeath_grouped)))))

hemorrhage_place_table %>%
  kable(
    caption = "Hemorrhage Maternal Deaths by Place of Death (2006–2017 EFY)",
    align = c("l", rep("c", ncol(hemorrhage_place_table) - 1)),
    booktabs = TRUE
  ) %>%
  kable_styling(full_width = TRUE, position = "center")

# -----------------------------------------------------------------------------
# 9.7 Hemorrhage by Time of Death (2006-2017)
# -----------------------------------------------------------------------------
timeofdeath_summary <- MDSR_2006_2017 %>%
  filter(Nine_Group_Final == "Group 1: Hemorrhage") %>%
  group_by(EFY, TimeOfDeath_imputed_prop) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order TimeOfDeath_imputed_prop by total deaths
time_order <- timeofdeath_summary %>%
  group_by(TimeOfDeath_imputed_prop) %>%
  summarise(total = sum(count)) %>%
  arrange(desc(total)) %>%
  pull(TimeOfDeath_imputed_prop)

timeofdeath_summary$TimeOfDeath_imputed_prop <- factor(
  timeofdeath_summary$TimeOfDeath_imputed_prop,
  levels = time_order
)

hemorrhage_time_plot <- ggplot(timeofdeath_summary, 
                               aes(x = EFY, y = percent, 
                                   color = TimeOfDeath_imputed_prop, 
                                   group = TimeOfDeath_imputed_prop)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2006:2017, labels = paste0(2006:2017, " EFY")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Trend of Maternal Deaths Due to Hemorrhage\nby Time of Death (2006–2017 EFY)",
    x = "Fiscal Year (EFY)",
    y = "Percentage of Hemorrhage Deaths (%)",
    color = "Time of Death"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

print(hemorrhage_time_plot)

# -----------------------------------------------------------------------------
# 9.8 Hemorrhage by Parity (2006-2017)
# -----------------------------------------------------------------------------
# Create parity category for the whole dataset
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  mutate(
    parity_category = case_when(
      parity_impute == 0 ~ "Nulliparus",
      parity_impute >= 1 & parity_impute <= 4 ~ "Multipara",
      parity_impute >= 5 ~ "GrandMultipara",
      is.na(parity_impute) ~ NA_character_
    )
  )

hemorrhage_parity <- MDSR_2006_2017 %>%
  filter(Nine_Group_Final == "Group 1: Hemorrhage") %>%
  group_by(EFY, parity_category) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(percent = deaths / sum(deaths) * 100) %>%
  ungroup()

hemorrhage_parity_plot <- ggplot(hemorrhage_parity, 
                                 aes(x = EFY, y = percent, 
                                     color = parity_category, 
                                     group = parity_category)) +
  geom_line(size = 1.3) +
  geom_point(size = 3.5) +
  scale_x_continuous(
    breaks = 2006:2017,
    labels = paste0(2006:2017, " EFY")
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c(
    "Nulliparus" = "#1b9e77",
    "Multipara" = "#d95f02",
    "GrandMultipara" = "#7570b3"
  )) +
  labs(
    title = "Trend of Maternal Deaths due to Hemorrhage\nby Parity from 2006–2017 EFY",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Percentage of Hemorrhage Deaths",
    color = "Parity Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12)
  )

print(hemorrhage_parity_plot)

# -----------------------------------------------------------------------------
# 9.9 Hemorrhage by Parity Table
# -----------------------------------------------------------------------------
hemorrhage_parity_table <- MDSR_2006_2017 %>%
  filter(str_detect(Nine_Group_Final, "Hemorrhage")) %>%
  group_by(EFY, parity_category) %>%
  summarise(Deaths = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    Total_Deaths = sum(Deaths),
    Percent = round(Deaths / Total_Deaths * 100, 1),
    Label = paste0(Deaths, " (", Percent, "%)")
  ) %>%
  ungroup() %>%
  select(EFY, parity_category, Label) %>%
  pivot_wider(
    names_from = EFY,
    values_from = Label,
    values_fill = "0 (0%)"
  ) %>%
  arrange(factor(parity_category, levels = c("Nulliparus", "Multipara", "GrandMultipara")))

hemorrhage_parity_table %>%
  kable(
    caption = "Maternal Deaths due to Hemorrhage by Parity Category (2006–2017 EFY)",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(hemorrhage_parity_table) - 1))
  ) %>%
  kable_styling(
    full_width = FALSE,
    font_size = 12,
    position = "center",
    latex_options = c("striped", "hold_position")
  )

# =============================================================================
# 10. DELAY ANALYSIS AND SCORING
# =============================================================================

# Define delay columns
delay_cols <- c("Delay1TraditionalPractices", "Delay1Familypoverity",
                "Delay1FailureofrecognitioNoftheproblem", 
                "Delay1Lackofdecisiontogotohealthfacility",
                "Delay1Delayedreferralfromhome", "Delay2Lackofroads",
                "Delay2Delayedarrivaltoreferredfacility", 
                "Delay2Lackofmoneyfortransport",
                "Delay2Lackoftransporation", 
                "Delay2Nofacilitywithinreasonabledistance",
                "Delay3DelayedArrivalToNextFacilityFromANotherFacilityonreRerral",
                "Delay3DelayedOrLackingSuppliesAndEquipments",
                "Delay3DelayedManagementAfterAdmission", 
                "Delay3HumanErrororMismanagement")

# Convert delay columns to numeric
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  mutate(across(all_of(delay_cols),
                ~ case_when(
                  . == "Yes" ~ 1,
                  . == "No" ~ 0,
                  TRUE ~ NA_real_
                )))

# Calculate total delay score and categories
MDSR_2006_2017 <- MDSR_2006_2017 %>%
  rowwise() %>%
  mutate(Total_Delay_Score = sum(c_across(all_of(delay_cols)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Delay_Category = case_when(
      Total_Delay_Score <= quantile(Total_Delay_Score, 0.25, na.rm = TRUE) ~ "Low Delay",
      Total_Delay_Score <= quantile(Total_Delay_Score, 0.5, na.rm = TRUE) ~ "Moderate Delay",
      Total_Delay_Score <= quantile(Total_Delay_Score, 0.75, na.rm = TRUE) ~ "High Delay",
      Total_Delay_Score > quantile(Total_Delay_Score, 0.75, na.rm = TRUE) ~ "Very High Delay",
      TRUE ~ NA_character_
    )
  )

# Create final delay classification
MDSR_final <- MDSR_2006_2017 %>%
  rowwise() %>%
  mutate(
    Delay1_score = sum(c_across(Delay1TraditionalPractices:Delay1Delayedreferralfromhome), 
                       na.rm = TRUE) / 5,
    Delay2_score = sum(c_across(Delay2Lackofroads:Delay2Nofacilitywithinreasonabledistance), 
                       na.rm = TRUE) / 5,
    Delay3_score = sum(c_across(Delay3DelayedArrivalToNextFacilityFromANotherFacilityonreRerral:
                                  Delay3HumanErrororMismanagement), 
                       na.rm = TRUE) / 4,
    Final_Delay = case_when(
      Delay1_score == 0 & Delay2_score == 0 & Delay3_score == 0 ~ "No Delay",
      Delay1_score >= Delay2_score & Delay1_score >= Delay3_score ~ "Delay1",
      Delay2_score >= Delay1_score & Delay2_score >= Delay3_score ~ "Delay2",
      Delay3_score >= Delay1_score & Delay3_score >= Delay3_score ~ "Delay3"
    )
  ) %>%
  ungroup()

# Check the results
table(MDSR_final$Final_Delay, MDSR_final$EFY, useNA = "always")
table(MDSR_final$Delay_Category)

# Save the final dataset
saveRDS(MDSR_final, file = "MDSR_final_updated.rds")

# Load the final dataset for further analysis
MDSR_final_updated <- readRDS("MDSR_final_updated.rds")

# =============================================================================
# 11. HEMORRHAGE DELAY ANALYSIS
# =============================================================================

hem_data <- MDSR_final_updated %>%
  filter(Nine_Group_Final == "Group 1: Hemorrhage")

# -----------------------------------------------------------------------------
# 11.1 Hemorrhage by Delay Category
# -----------------------------------------------------------------------------
hem_delay_cat_trend <- hem_data %>%
  filter(!is.na(Delay_Category)) %>%
  group_by(EFY, Delay_Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    total = sum(count),
    percent = (count / total) * 100
  )

# Order by total percent
delay_order <- hem_delay_cat_trend %>%
  group_by(Delay_Category) %>%
  summarise(total = sum(percent, na.rm = TRUE)) %>%
  arrange(desc(total))

hem_delay_cat_trend$Delay_Category <- factor(
  hem_delay_cat_trend$Delay_Category,
  levels = delay_order$Delay_Category
)

hemorrhage_delay_plot <- ggplot(hem_delay_cat_trend,
                                aes(x = EFY, y = percent,
                                    color = Delay_Category,
                                    group = Delay_Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_continuous(
    breaks = sort(unique(hem_delay_cat_trend$EFY)),
    labels = paste0(sort(unique(hem_delay_cat_trend$EFY)), " EFY")
  ) +
  labs(
    title = "Hemorrhage: Trend of Delay Category by EFY",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Hemorrhage Deaths",
    color = "Delay Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top"
  )

print(hemorrhage_delay_plot)

# -----------------------------------------------------------------------------
# 11.2 Hemorrhage by Final Delay
# -----------------------------------------------------------------------------
hem_final_delay_trend <- hem_data %>%
  filter(!is.na(Final_Delay)) %>%
  group_by(EFY, Final_Delay) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    percent = count / sum(count) * 100
  )

hemorrhage_final_delay_plot <- ggplot(hem_final_delay_trend,
                                      aes(x = EFY, y = percent,
                                          color = Final_Delay,
                                          group = Final_Delay)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 2006:2017,
    labels = paste0(2006:2017, " EFY")
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Hemorrhage: Trend of Final Delay by EFY",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Hemorrhage Deaths",
    color = "Final Delay"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(hemorrhage_final_delay_plot)

# -----------------------------------------------------------------------------
# 11.3 Hemorrhage Delay Category Table
# -----------------------------------------------------------------------------
hemorrhage_delay_summary <- hem_data %>%
  filter(!is.na(Delay_Category)) %>%
  group_by(EFY, Delay_Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    percent = round((count / sum(count)) * 100, 1),
    value = paste0(count, " (", percent, "%)")
  ) %>%
  ungroup() %>%
  select(EFY, Delay_Category, value)

hemorrhage_delay_wide <- hemorrhage_delay_summary %>%
  pivot_wider(
    names_from = EFY,
    values_from = value,
    values_fill = "0 (0%)"
  ) %>%
  arrange(factor(Delay_Category, 
                 levels = c("Low Delay", "Moderate Delay", "High Delay", "Very High Delay")))

colnames(hemorrhage_delay_wide) <- c("Delay Category", 
                                     paste0(colnames(hemorrhage_delay_wide)[-1], " EFY"))

hemorrhage_delay_wide %>%
  gt() %>%
  tab_header(
    title = "Maternal Deaths due to Hemorrhage by Delay Category and EFY, 2006–2017",
    subtitle = "Counts and row-wise proportions (%)"
  ) %>%
  cols_align(
    align = "center",
    everything()
  ) %>%
  tab_source_note(
    source_note = "Source: National MDSR database (2006–2017 EFY)"
  ) %>%
  opt_table_outline() %>%
  tab_style(
    style = list(
      cell_fill(color = "#F7F7F7")
    ),
    locations = cells_body(
      rows = seq(1, nrow(hemorrhage_delay_wide), by = 2)
    )
  )

# =============================================================================
# 12. HYPERTENSIVE DISORDERS (HDP) ANALYSIS
# =============================================================================

# Create age groups for HDP analysis
MDSR_final_updated <- MDSR_final_updated %>%
  mutate(
    AgeGroup = case_when(
      AgeAtDeathinyears >= 11 & AgeAtDeathinyears <= 19 ~ "15-19",
      AgeAtDeathinyears >= 20 & AgeAtDeathinyears <= 34 ~ "20-34",
      AgeAtDeathinyears >= 35 & AgeAtDeathinyears <= 49 ~ "35-49",
      TRUE ~ NA_character_
    )
  )

# Check distributions
table(MDSR_final_updated$EFY)
table(MDSR_final_updated$Delay_Category)
table(MDSR_final_updated$Final_Delay)
table(MDSR_final_updated$Nine_Group_Final)
table(MDSR_final_updated$Region_new_1)
table(MDSR_final_updated$Placeofdeath_clean)
table(MDSR_final_updated$TimeOfDeath_imputed_prop)
table(MDSR_final_updated$AgeGroup)
table(MDSR_final_updated$MDRFextractedfrom)

# -----------------------------------------------------------------------------
# 12.1 HDP by Data Source
# -----------------------------------------------------------------------------
hdp_source_summary <- MDSR_final_updated %>%
  filter(Nine_Group_Final == "Group 3: Hypertensive disorders") %>%
  filter(!is.na(MDRFextractedfrom)) %>%
  group_by(EFY, MDRFextractedfrom) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(
    percent = round((count / sum(count)) * 100, 1),
    value = paste0(count, " (", percent, "%)")
  ) %>%
  ungroup()

# Pivot to wide format
hdp_source_table <- hdp_source_summary %>%
  select(EFY, MDRFextractedfrom, value) %>%
  pivot_wider(
    names_from = EFY,
    values_from = value,
    values_fill = "0 (0%)"
  ) %>%
  arrange(MDRFextractedfrom)

# Rename columns
colnames(hdp_source_table) <- c("Source of Data", 
                                paste0(colnames(hdp_source_table)[-1], " EFY"))

# Create styled kable table
hdp_source_table %>%
  kable(
    caption = "Maternal Deaths due to HDP by Source of Data (MDRF) and EFY, 2006–2017",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(hdp_source_table) - 1))
  ) %>%
  kable_styling(
    full_width = TRUE,
    font_size = 12,
    position = "center",
    latex_options = c("striped", "hold_position", "scale_down")
  ) %>%
  add_footnote(
    "Values are count (percentage of total HDP deaths in each EFY).",
    notation = "none"
  )

# -----------------------------------------------------------------------------
# 12.2 HDP by Region
# -----------------------------------------------------------------------------
hdp_region_summary <- MDSR_final_updated %>%
  group_by(Region_new_1, EFY) %>%
  summarise(
    total_deaths = n(),
    hdp_deaths = sum(Nine_Group_Final == "Group 3: Hypertensive disorders", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hdp_percent = round((hdp_deaths / total_deaths) * 100, 1),
    value = paste0(hdp_deaths, " (", hdp_percent, "%)")
  )

# Include all EFYs 2006–2017 even if missing
all_efy <- 2006:2017
hdp_region_summary <- hdp_region_summary %>%
  complete(Region_new_1, EFY = all_efy, 
           fill = list(hdp_deaths = 0, total_deaths = 0, hdp_percent = 0))

hdp_region_plot <- ggplot(hdp_region_summary, aes(x = factor(EFY), y = hdp_percent, group = 1)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(color = "#1f77b4", size = 3) +
  facet_wrap(~Region_new_1, scales = "free_y") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(all_efy, " EFY")) +
  labs(
    title = "Proportion of HDP Deaths among Total Maternal Deaths by Region",
    x = "Ethiopian Fiscal Year",
    y = "HDP / Total Deaths (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(size = 10)
  )

print(hdp_region_plot)

# -----------------------------------------------------------------------------
# 12.3 HDP by Region Table
# -----------------------------------------------------------------------------
hdp_region_table <- hdp_region_summary %>%
  select(Region_new_1, EFY, value) %>%
  pivot_wider(
    names_from = EFY,
    values_from = value,
    values_fill = "0 (0%)"
  ) %>%
  arrange(Region_new_1)

colnames(hdp_region_table) <- c("Region", 
                                paste0(colnames(hdp_region_table)[-1], " EFY"))

hdp_region_table %>%
  kable(
    caption = "HDP-related Maternal Deaths as Percentage of Total Maternal Deaths by EFY and Region, 2006–2017",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(hdp_region_table) - 1))
  ) %>%
  kable_styling(
    full_width = TRUE,
    font_size = 11,
    position = "center",
    latex_options = c("striped", "hold_position", "scale_down")
  ) %>%
  add_footnote(
    "Values are count (percentage of total maternal deaths in each EFY for that region).",
    notation = "none"
  )

# -----------------------------------------------------------------------------
# 12.4 HDP by Place of Death
# -----------------------------------------------------------------------------
hdp_data <- MDSR_final_updated %>%
  filter(Nine_Group_Final == "Group 3: Hypertensive disorders")

# Clean Placeofdeath_clean by merging similar categories
MDSR_final_updated <- MDSR_final_updated %>%
  mutate(
    Placeofdeath_clean = case_when(
      Placeofdeath_clean %in% c("Private Health Facility", "Other") ~ "Health Center",
      TRUE ~ Placeofdeath_clean
    )
  )

hdp_place_efy <- hdp_data %>%
  group_by(EFY, Placeofdeath_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Placeofdeath_clean = unique(Placeofdeath_clean), 
           fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order Placeofdeath_clean by total HDP deaths
legend_order <- hdp_place_efy %>%
  group_by(Placeofdeath_clean) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(Placeofdeath_clean)

hdp_place_efy$Placeofdeath_clean <- factor(hdp_place_efy$Placeofdeath_clean, 
                                           levels = legend_order)

hdp_place_plot <- ggplot(hdp_place_efy, 
                         aes(x = factor(EFY), y = percent, 
                             group = Placeofdeath_clean, 
                             color = Placeofdeath_clean)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of HDP Deaths by Place of Death (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of HDP Deaths",
    color = "Place of Death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  )

print(hdp_place_plot)

# -----------------------------------------------------------------------------
# 12.5 HDP by Time of Death
# -----------------------------------------------------------------------------
hdp_time_efy <- hdp_data %>%
  group_by(EFY, TimeOfDeath_imputed_prop) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, TimeOfDeath_imputed_prop = unique(TimeOfDeath_imputed_prop), 
           fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order TimeOfDeath by total HDP deaths
legend_order_time <- hdp_time_efy %>%
  group_by(TimeOfDeath_imputed_prop) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(TimeOfDeath_imputed_prop)

hdp_time_efy$TimeOfDeath_imputed_prop <- factor(hdp_time_efy$TimeOfDeath_imputed_prop, 
                                                levels = legend_order_time)

hdp_time_plot <- ggplot(hdp_time_efy, 
                        aes(x = factor(EFY), y = percent, 
                            group = TimeOfDeath_imputed_prop, 
                            color = TimeOfDeath_imputed_prop)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of HDP Deaths by Time of Death (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of HDP Deaths",
    color = "Time of Death: "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title.align = 0,
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left"
    )
  )

print(hdp_time_plot)

# -----------------------------------------------------------------------------
# 12.6 HDP by Age Group
# -----------------------------------------------------------------------------
hdp_age_efy <- hdp_data %>%
  group_by(EFY, AgeGroup) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, AgeGroup, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order AgeGroup by total HDP deaths
legend_order_age <- hdp_age_efy %>%
  group_by(AgeGroup) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(AgeGroup)

hdp_age_efy$AgeGroup <- factor(hdp_age_efy$AgeGroup, levels = legend_order_age)

hdp_age_plot <- ggplot(hdp_age_efy, 
                       aes(x = factor(EFY), y = percent, 
                           group = AgeGroup, color = AgeGroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of HDP Deaths by Age Group (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of HDP Deaths",
    color = "Age Group: "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title.align = 0,
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left"
    )
  )

print(hdp_age_plot)

# -----------------------------------------------------------------------------
# 12.7 HDP by Delay Category
# -----------------------------------------------------------------------------
hdp_delaycat_efy <- hdp_data %>%
  group_by(EFY, Delay_Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Delay_Category, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order legend by total HDP deaths
legend_order_delay <- hdp_delaycat_efy %>%
  group_by(Delay_Category) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(Delay_Category)

hdp_delaycat_efy$Delay_Category <- factor(hdp_delaycat_efy$Delay_Category, 
                                          levels = legend_order_delay)

hdp_delaycat_plot <- ggplot(hdp_delaycat_efy, 
                            aes(x = factor(EFY), y = percent, 
                                group = Delay_Category, 
                                color = Delay_Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "HDP Deaths by Delay Category (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of HDP Deaths",
    color = "Delay Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top"
  )

print(hdp_delaycat_plot)

# -----------------------------------------------------------------------------
# 12.8 HDP by Final Delay
# -----------------------------------------------------------------------------
hdp_finaldelay_efy <- hdp_data %>%
  group_by(EFY, Final_Delay) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Final_Delay, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order legend by total HDP deaths
legend_order_final <- hdp_finaldelay_efy %>%
  group_by(Final_Delay) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(Final_Delay)

hdp_finaldelay_efy$Final_Delay <- factor(hdp_finaldelay_efy$Final_Delay, 
                                         levels = legend_order_final)

hdp_finaldelay_plot <- ggplot(hdp_finaldelay_efy, 
                              aes(x = factor(EFY), y = percent, 
                                  group = Final_Delay, 
                                  color = Final_Delay)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "HDP Deaths by Final Delay (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of HDP Deaths",
    color = "Final Delay"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top"
  )

print(hdp_finaldelay_plot)

# =============================================================================
# 13. INFECTION-SPECIFIC ANALYSES
# =============================================================================

# -----------------------------------------------------------------------------
# 13.1 Standardize Place of Delivery
# -----------------------------------------------------------------------------
MDSR_final_updated <- MDSR_final_updated %>%
  mutate(PlaceOfDelivery_clean = case_when(
    grepl("home", tolower(Placeofdelivery)) ~ "Home",
    grepl("hospital", tolower(Placeofdelivery)) ~ "Hospital",
    grepl("health center|health_center|Health center|Health Center", tolower(Placeofdelivery)) ~ "Health Center",
    grepl("health post|Health post|Health Post", tolower(Placeofdelivery)) ~ "Health Post",
    grepl("clinic|private clinic", tolower(Placeofdelivery)) ~ "Clinic",
    grepl("on transit|on trasit|On transit|On trasit", tolower(Placeofdelivery)) ~ "On Transit",
    TRUE ~ "Unknown"
  ))

# Check distribution
delivery_counts <- MDSR_final_updated %>%
  group_by(PlaceOfDelivery_clean) %>%
  summarise(count = n(), .groups = "drop")

# Impute Unknown place of delivery
known_props <- MDSR_final_updated %>%
  filter(PlaceOfDelivery_clean != "Unknown") %>%
  group_by(PlaceOfDelivery_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop = count / sum(count))

unknown_indices <- which(MDSR_final_updated$PlaceOfDelivery_clean == "Unknown")
set.seed(123)
imputed_categories <- sample(
  known_props$PlaceOfDelivery_clean,
  size = length(unknown_indices),
  replace = TRUE,
  prob = known_props$prop
)

MDSR_final_updated$PlaceOfDelivery_clean[unknown_indices] <- imputed_categories

# Verify
table(MDSR_final_updated$PlaceOfDelivery_clean)

# -----------------------------------------------------------------------------
# 13.2 Infection by Data Source
# -----------------------------------------------------------------------------
infection_source_summary <- MDSR_final_updated %>%
  filter(Nine_Group_Final == "Group 4: Pregnancy-related infection") %>%
  group_by(EFY, MDRFextractedfrom) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, MDRFextractedfrom, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

infection_source_table <- infection_source_summary %>%
  mutate(value = paste0(count, " (", percent, "%)")) %>%
  select(EFY, MDRFextractedfrom, value) %>%
  pivot_wider(names_from = EFY, values_from = value, values_fill = "0 (0%)") %>%
  arrange(MDRFextractedfrom)

colnames(infection_source_table) <- c("Data Source", 
                                      paste0("EFY ", colnames(infection_source_table)[-1]))

infection_source_table %>%
  kable(
    caption = "Pregnancy-related Infection Maternal Deaths by Data Source and EFY (2006–2017)",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(infection_source_table) - 1))
  ) %>%
  kable_styling(
    full_width = TRUE,
    font_size = 11,
    position = "center",
    latex_options = c("striped", "hold_position", "scale_down")
  )

# -----------------------------------------------------------------------------
# 13.3 Infection by Region
# -----------------------------------------------------------------------------
infection_data <- MDSR_final_updated %>%
  filter(Nine_Group_Final == "Group 4: Pregnancy-related infection")

infection_region_summary <- infection_data %>%
  group_by(EFY, Region_new_1) %>%
  summarise(infection_count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Region_new_1, fill = list(infection_count = 0)) %>%
  ungroup()

# Total maternal deaths per region per EFY
total_region_year <- MDSR_final_updated %>%
  group_by(EFY, Region_new_1) %>%
  summarise(total_deaths = n(), .groups = "drop")

# Calculate proportion relative to the region itself
infection_region_summary <- infection_region_summary %>%
  left_join(total_region_year, by = c("EFY", "Region_new_1")) %>%
  mutate(percent = round((infection_count / total_deaths) * 100, 1))

# Define colors for regions
region_colors <- c(
  "Addis Ababa" = "#1f77b4", "Afar" = "#ff7f0e",
  "Amhara" = "#2ca02c", "Benishangul Gumz" = "#d62728",
  "Central Ethiopia" = "#9467bd", "Dire Dawa" = "#8c564b",
  "Gambella" = "#e377c2", "Harari" = "#7f7f7f",
  "Oromia" = "#bcbd22", "Sidama" = "#17becf",
  "Somali" = "#d62728", "South West Ethiopia" = "#1f77b4",
  "Southern Ethiopia" = "#2ca090", "Tigray" = "#e377c2"
)

infection_region_plot <- ggplot(infection_region_summary, 
                                aes(x = factor(EFY), y = percent, 
                                    group = Region_new_1, 
                                    color = Region_new_1)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "Proportion of Infection Deaths Within Each Region (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percent of Infection Deaths (within region)"
  ) +
  facet_wrap(~Region_new_1, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none"
  )

print(infection_region_plot)

# -----------------------------------------------------------------------------
# 13.4 Infection by Place of Death
# -----------------------------------------------------------------------------
infection_place_efy <- infection_data %>%
  group_by(EFY, Placeofdeath_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Placeofdeath_clean = unique(Placeofdeath_clean), 
           fill = list(count = 0)) %>%
  group_by(Placeofdeath_clean) %>%
  filter(sum(count) > 0) %>%
  ungroup() %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order places by total infection deaths
legend_order <- infection_place_efy %>%
  group_by(Placeofdeath_clean) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(Placeofdeath_clean)

infection_place_efy$Placeofdeath_clean <- factor(infection_place_efy$Placeofdeath_clean, 
                                                 levels = legend_order)

infection_place_plot <- ggplot(infection_place_efy, 
                               aes(x = factor(EFY), y = percent, 
                                   group = Placeofdeath_clean, 
                                   color = Placeofdeath_clean)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of Infection Deaths by Place of Death (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Infection Deaths",
    color = "Place of Death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  )

print(infection_place_plot)

# -----------------------------------------------------------------------------
# 13.5 Infection by Time of Death
# -----------------------------------------------------------------------------
infection_time_efy <- infection_data %>%
  group_by(EFY, TimeOfDeath_imputed_prop) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, TimeOfDeath_imputed_prop = unique(TimeOfDeath_imputed_prop), 
           fill = list(count = 0)) %>%
  group_by(TimeOfDeath_imputed_prop) %>%
  filter(sum(count) > 0) %>%
  ungroup() %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order TimeOfDeath by total Infection deaths
legend_order <- infection_time_efy %>%
  group_by(TimeOfDeath_imputed_prop) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(TimeOfDeath_imputed_prop)

infection_time_efy$TimeOfDeath_imputed_prop <- factor(infection_time_efy$TimeOfDeath_imputed_prop, 
                                                      levels = legend_order)

infection_time_plot <- ggplot(infection_time_efy, 
                              aes(x = factor(EFY), y = percent, 
                                  group = TimeOfDeath_imputed_prop, 
                                  color = TimeOfDeath_imputed_prop)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of Infection Deaths by Time of Death (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Infection Deaths",
    color = "Time of Death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  )

print(infection_time_plot)

# -----------------------------------------------------------------------------
# 13.6 Infection by Place of Delivery
# -----------------------------------------------------------------------------
infection_delivery_efy <- infection_data %>%
  group_by(EFY, PlaceOfDelivery_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, PlaceOfDelivery_clean = unique(PlaceOfDelivery_clean), 
           fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order PlaceOfDelivery by total Infection deaths
legend_order <- infection_delivery_efy %>%
  group_by(PlaceOfDelivery_clean) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(PlaceOfDelivery_clean)

infection_delivery_efy$PlaceOfDelivery_clean <- factor(infection_delivery_efy$PlaceOfDelivery_clean, 
                                                       levels = legend_order)

infection_delivery_plot <- ggplot(infection_delivery_efy, 
                                  aes(x = factor(EFY), y = percent, 
                                      group = PlaceOfDelivery_clean, 
                                      color = PlaceOfDelivery_clean)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of Infection Deaths by Place of Delivery (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Infection Deaths",
    color = "Place of Delivery"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  )

print(infection_delivery_plot)

# -----------------------------------------------------------------------------
# 13.7 Infection by Delay Category
# -----------------------------------------------------------------------------
infection_delay_cat <- infection_data %>%
  group_by(EFY, Delay_Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Delay_Category, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order by total count
delay_order <- infection_delay_cat %>%
  group_by(Delay_Category) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Delay_Category)

infection_delay_cat$Delay_Category <- factor(infection_delay_cat$Delay_Category, 
                                             levels = delay_order)

infection_delay_plot <- ggplot(infection_delay_cat, 
                               aes(x = factor(EFY), y = percent, 
                                   group = Delay_Category, 
                                   color = Delay_Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of Infection Deaths by Delay Category (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Infection Deaths",
    color = "Delay Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))

print(infection_delay_plot)

# -----------------------------------------------------------------------------
# 13.8 Infection by Final Delay
# -----------------------------------------------------------------------------
infection_final_delay <- infection_data %>%
  group_by(EFY, Final_Delay) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Final_Delay, fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order by total count
final_order <- infection_final_delay %>%
  group_by(Final_Delay) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Final_Delay)

infection_final_delay$Final_Delay <- factor(infection_final_delay$Final_Delay, 
                                            levels = final_order)

infection_final_delay_plot <- ggplot(infection_final_delay, 
                                     aes(x = factor(EFY), y = percent, 
                                         group = Final_Delay, 
                                         color = Final_Delay)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Proportion of Infection Deaths by Final Delay (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Infection Deaths",
    color = "Final Delay"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))

print(infection_final_delay_plot)

# =============================================================================
# 14. FINAL DELAY ANALYSIS (2017 Pie Chart and Trends)
# =============================================================================

# -----------------------------------------------------------------------------
# 14.1 Final Delay Pie Chart (2017)
# -----------------------------------------------------------------------------
final_delay_2017 <- MDSR_final_updated %>%
  filter(EFY == 2017) %>%
  group_by(Final_Delay) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percent = round((count / sum(count)) * 100, 1),
    label = paste0(Final_Delay, "\n", count, " (", percent, "%)")
  ) %>%
  arrange(desc(count)) %>%
  mutate(Final_Delay = factor(Final_Delay, levels = Final_Delay))

final_delay_pie <- ggplot(final_delay_2017, aes(x = 2, y = percent, fill = Final_Delay)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set2") +
  xlim(0.5, 2.5) +
  labs(
    title = "Contributing factor for maternal death in 2017 EFY",
    fill = "Final Delay"
  ) +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(final_delay_pie)

# -----------------------------------------------------------------------------
# 14.2 Final Delay Trends (2006-2017)
# -----------------------------------------------------------------------------
final_delay_trend <- MDSR_final_updated %>%
  group_by(EFY, Final_Delay) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(EFY = 2006:2017, Final_Delay = unique(MDSR_final_updated$Final_Delay), 
           fill = list(count = 0)) %>%
  group_by(EFY) %>%
  mutate(percent = round((count / sum(count)) * 100, 1)) %>%
  ungroup()

# Order Final_Delay by total count for consistent legend
legend_order <- final_delay_trend %>%
  group_by(Final_Delay) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(Final_Delay)

final_delay_trend$Final_Delay <- factor(final_delay_trend$Final_Delay, 
                                        levels = legend_order)

final_delay_trend_plot <- ggplot(final_delay_trend, 
                                 aes(x = factor(EFY), y = percent, 
                                     group = Final_Delay, 
                                     color = Final_Delay)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = paste0(2006:2017, " EFY")) +
  labs(
    title = "Trend of Maternal Deaths by Final Delay (2006–2017 EFYs)",
    x = "Ethiopian Fiscal Year",
    y = "Percentage of Maternal Deaths",
    color = "Final Delay"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  )

print(final_delay_trend_plot)

# -----------------------------------------------------------------------------
# 14.3 Final Delay Table
# -----------------------------------------------------------------------------
final_delay_table <- final_delay_trend %>%
  mutate(value = paste0(count, " (", percent, "%)")) %>%
  select(EFY, Final_Delay, value) %>%
  pivot_wider(names_from = EFY, values_from = value, values_fill = "0 (0%)") %>%
  arrange(Final_Delay)

colnames(final_delay_table) <- c("Final Delay", 
                                 paste0(colnames(final_delay_table)[-1], " EFY"))

final_delay_table %>%
  kable(
    caption = "Maternal Deaths by Final Delay per EFY (2006–2017)",
    booktabs = TRUE,
    align = c("l", rep("c", ncol(final_delay_table) - 1))
  ) %>%
  kable_styling(
    full_width = TRUE,
    font_size = 11,
    position = "center",
    latex_options = c("striped", "hold_position", "scale_down")
  ) %>%
  add_footnote(
    "Values are count (percentage of total maternal deaths in that EFY).",
    notation = "none"
  )

# =============================================================================
# 15. DELAY FACTORS DETAILED ANALYSIS (2017)
# =============================================================================

plot_data <- MDSR_2017 %>%
  select(31:44) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Value") %>%
  filter(Value == 1 | Value == "Yes") %>%
  mutate(
    DelayGroup = case_when(
      str_detect(Factor, "Delay1") ~ "Delay One",
      str_detect(Factor, "Delay2") ~ "Delay Two",
      str_detect(Factor, "Delay3") ~ "Delay Three"
    ),
    FactorLabel = case_when(
      Factor == "Delay1TraditionalPractices" ~ "Traditional Practices",
      Factor == "Delay1Familypoverity" ~ "Family poverty",
      Factor == "Delay1FailureofrecognitioNoftheproblem" ~ "Failure to recognize the problem",
      Factor == "Delay1Lackofdecisiontogotohealthfacility" ~ "Lack of decision to go to a health facility",
      Factor == "Delay1Delayedreferralfromhome" ~ "Delayed referral from home",
      Factor == "Delay2Lackofroads" ~ "Lack of road",
      Factor == "Delay2Delayedarrivaltoreferredfacility" ~ "Delayed arrival to referred HF",
      Factor == "Delay2Lackofmoneyfortransport" ~ "Lack of money for transport",
      Factor == "Delay2Lackoftransporation" ~ "Lack of transportation",
      Factor == "Delay2Nofacilitywithinreasonabledistance" ~ "No facility within a reasonable distance",
      Factor == "Delay3DelayedArrivalToNextFacilityFromANotherFacilityonreRerral" ~ "Delayed arrival to next facility from another facility on referral",
      Factor == "Delay3DelayedOrLackingSuppliesAndEquipments" ~ "Delayed or lacking supplies and equipment",
      Factor == "Delay3DelayedManagementAfterAdmission" ~ "Delayed management after admission",
      Factor == "Delay3HumanErrororMismanagement" ~ "Human error or mismanagement"
    )
  ) %>%
  count(DelayGroup, FactorLabel) %>%
  mutate(FactorLabel = reorder_within(FactorLabel, n, DelayGroup))

delay_factors_plot <- ggplot(plot_data, aes(x = n, y = FactorLabel, fill = DelayGroup)) +
  geom_col() +
  facet_wrap(~DelayGroup, scales = "free_y", ncol = 1) +
  scale_y_reordered() +
  scale_fill_manual(values = c("Delay One" = "#2196F3", 
                               "Delay Two" = "#FF9800", 
                               "Delay Three" = "#4CAF50")) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, fontface = "bold") +
  labs(
    title = "Individual Factors Responsible for Maternal Deaths Under Delays Model",
    x = "Frequency", y = "Contributing Factors", fill = "Delays"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(x = max(plot_data$n) * 1.15)

print(delay_factors_plot)

# =============================================================================
# 16. SAVE ALL PLOTS AND TABLES
# =============================================================================

# Create directory for outputs if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Save plots to files
ggsave("outputs/age_trend_plot.png", age_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/place_donut_plot.png", donut_plot, width = 10, height = 8, dpi = 300)
ggsave("outputs/parity_anc_combined.png", combined_plot, width = 14, height = 8, dpi = 300)
ggsave("outputs/anc_trend_plot.png", anc_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/time_trend_plot.png", time_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/cause_trend_plot.png", cause_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/top_causes_plot.png", top_causes_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/all_causes_plot.png", all_causes_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/causes_2017_plot.png", causes_2017_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/delivery_plot.png", delivery_plot, width = 10, height = 8, dpi = 300)
ggsave("outputs/hemorrhage_trend_plot.png", hemorrhage_trend_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/hemorrhage_percent_plot.png", hemorrhage_percent_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/hemorrhage_bar_plot.png", hemorrhage_bar_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/hemorrhage_place_plot.png", hemorrhage_place_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hemorrhage_time_plot.png", hemorrhage_time_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hemorrhage_parity_plot.png", hemorrhage_parity_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hemorrhage_delay_plot.png", hemorrhage_delay_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hemorrhage_final_delay_plot.png", hemorrhage_final_delay_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hdp_region_plot.png", hdp_region_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/hdp_place_plot.png", hdp_place_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hdp_time_plot.png", hdp_time_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hdp_age_plot.png", hdp_age_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hdp_delaycat_plot.png", hdp_delaycat_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/hdp_finaldelay_plot.png", hdp_finaldelay_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/infection_region_plot.png", infection_region_plot, width = 14, height = 10, dpi = 300)
ggsave("outputs/infection_place_plot.png", infection_place_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/infection_time_plot.png", infection_time_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/infection_delivery_plot.png", infection_delivery_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/infection_delay_plot.png", infection_delay_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/infection_final_delay_plot.png", infection_final_delay_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/final_delay_pie.png", final_delay_pie, width = 8, height = 8, dpi = 300)
ggsave("outputs/final_delay_trend_plot.png", final_delay_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("outputs/delay_factors_plot.png", delay_factors_plot, width = 12, height = 10, dpi = 300)

# Save tables as CSV files
write.csv(age_summary_2017, "outputs/age_summary_2017.csv", row.names = FALSE)
write.csv(place_year_table, "outputs/place_of_death_summary.csv", row.names = FALSE)
write.csv(parity_wide, "outputs/parity_summary.csv", row.names = FALSE)
write.csv(anc_wide, "outputs/anc_summary.csv", row.names = FALSE)
write.csv(anc_all_years, "outputs/anc_category_summary.csv", row.names = FALSE)
write.csv(time_efy_wide, "outputs/time_of_death_summary.csv", row.names = FALSE)
write.csv(cause_table_wide, "outputs/causes_summary.csv", row.names = FALSE)
write.csv(cause_source_wide, "outputs/cause_source_summary.csv", row.names = FALSE)
write.csv(hemorrhage_region, "outputs/hemorrhage_region_2017.csv", row.names = FALSE)
write.csv(hemorrhage_table_wide, "outputs/hemorrhage_region_all_years.csv", row.names = FALSE)
write.csv(hemorrhage_parity_table, "outputs/hemorrhage_parity_summary.csv", row.names = FALSE)
write.csv(hemorrhage_delay_wide, "outputs/hemorrhage_delay_summary.csv", row.names = FALSE)
write.csv(hdp_source_table, "outputs/hdp_source_summary.csv", row.names = FALSE)
write.csv(hdp_region_table, "outputs/hdp_region_summary.csv", row.names = FALSE)
write.csv(infection_source_table, "outputs/infection_source_summary.csv", row.names = FALSE)
write.csv(final_delay_table, "outputs/final_delay_summary.csv", row.names = FALSE)

# Save final datasets
saveRDS(MDSR_final_updated, "outputs/MDSR_final_complete.rds")
saveRDS(MDSR_2017_clean, "outputs/MDSR_2017_clean.rds")

# =============================================================================
# 17. SUMMARY STATISTICS
# =============================================================================

cat("\n" + strrep("=", 80) + "\n")
cat("MDSR DATA ANALYSIS SUMMARY - MATERNAL DEATHS IN ETHIOPIA (2006-2017)\n")
cat(strrep("=", 80) + "\n\n")

# Basic statistics
cat("1. DATA OVERVIEW:\n")
cat("   Total records (2006-2017):", nrow(MDSR_2006_2017), "\n")
cat("   Records in 2017:", nrow(MDSR_2017), "\n")
cat("   Number of regions:", length(unique(MDSR_2006_2017$Region_new_1)), "\n")
cat("   Analysis period: 2006 to 2017 EFY\n\n")

# Age statistics
cat("2. AGE STATISTICS (2017):\n")
cat("   Median age:", median_age_2017, "years\n")
cat("   Age range:", age_range_2017[1], "-", age_range_2017[2], "years\n\n")

# Cause statistics
cat("3. LEADING CAUSES (2017):\n")
for(i in 1:min(3, nrow(cause_2017))) {
  cat("   ", i, ".", cause_2017$Cause_clean[i], ":", 
      cause_2017$count[i], "cases (", cause_2017$proportion[i], "%)\n")
}
cat("\n")

# Delay statistics
cat("4. DELAY ANALYSIS:\n")
delay_summary <- MDSR_final_updated %>%
  filter(EFY == 2017) %>%
  count(Final_Delay) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

for(i in 1:nrow(delay_summary)) {
  cat("   ", delay_summary$Final_Delay[i], ":", 
      delay_summary$n[i], "cases (", delay_summary$percent[i], "%)\n")
}
cat("\n")

# Output information
cat("5. OUTPUTS GENERATED:\n")
cat("   Plots saved in: outputs/ directory (", length(list.files("outputs", pattern = "\\.png$")), " PNG files)\n")
cat("   Tables saved in: outputs/ directory (", length(list.files("outputs", pattern = "\\.csv$")), " CSV files)\n")
cat("   Final datasets saved as RDS files\n")
cat("\n" + strrep("=", 80) + "\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(strrep("=", 80) + "\n")