# ==============================================================================
# MATERNAL DEATH SURVEILLANCE AND RESPONSE (MDSR) ANALYSIS
# Comprehensive analysis of maternal deaths 2006-2017
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
library(forcats)

# ==============================================================================
# 1. DATA PREPARATION AND INITIAL EXPLORATION
# ==============================================================================

# Check initial data structure
cat("MDSR Dataset Overview:\n")
cat("===========================================\n")
cat("Total observations:", nrow(MDSR_final_updated), "\n")
cat("Years covered:", min(MDSR_final_updated$EFY), "-", max(MDSR_final_updated$EFY), "\n\n")

# Check Final_Delay distribution
cat("Initial Final_Delay Distribution:\n")
print(table(MDSR_final_updated$Final_Delay, useNA = "always"))

# ==============================================================================
# 2. CREATE DELAY STATUS VARIABLE
# ==============================================================================

# Add binary Delay_Status variable
MDSR_final_updated <- MDSR_final_updated %>%
  mutate(
    Delay_Status = ifelse(Final_Delay %in% c("Delay1", "Delay2", "Delay3"),
                          "With Delay",
                          "No Delay")
  )

# Verify the new variable
cat("\nDelay Status Distribution:\n")
print(table(MDSR_final_updated$Delay_Status, useNA = "always"))

# ==============================================================================
# 3. FIGURE 1: DELAY STATUS TREND PLOT 2006-2017
# ==============================================================================

# Prepare data for trend plot
delay_trend_plot <- MDSR_final_updated %>%
  group_by(EFY, Delay_Status) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  ungroup()

# Order legend by total frequency across all years
legend_order <- delay_trend_plot %>%
  group_by(Delay_Status) %>%
  summarise(Total = sum(Percentage), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(Delay_Status)

# Convert Delay_Status to factor with levels according to size
delay_trend_plot <- delay_trend_plot %>%
  mutate(Delay_Status = factor(Delay_Status, levels = legend_order))

# Create trend plot
figure1_delay_trend <- ggplot(delay_trend_plot, 
                              aes(x = factor(EFY), y = Percentage, 
                                  color = Delay_Status, group = Delay_Status)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(labels = function(x) paste0(x, " EFY")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(
    title = "Figure 1: Trend of Delay Status Among Reviewed Maternal Deaths",
    subtitle = "Ethiopian Fiscal Years 2006–2017",
    x = "EFY",
    y = "Percentage (%)",
    color = "Delay Status"
  ) +
  scale_color_manual(values = c("With Delay" = "#E41A1C", "No Delay" = "#377EB8")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# Display Figure 1
print(figure1_delay_trend)

# ==============================================================================
# 4. TABLE 1: DELAY STATUS TREND TABLE 2006-2017
# ==============================================================================

# Prepare data with frequency and percentage per year
delay_summary <- MDSR_final_updated %>%
  group_by(EFY, Delay_Status) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  ungroup() %>%
  mutate(Combined = paste0(Frequency, " (", round(Percentage, 1), "%)"))

# Calculate total frequency and percentage across all years
total_summary <- delay_summary %>%
  group_by(Delay_Status) %>%
  summarise(Total = sum(Frequency), .groups = "drop") %>%
  mutate(Total_Combined = paste0(Total, " (", round((Total / sum(Total)) * 100, 1), "%)"))

# Pivot so EFY are columns, Delay_Status are rows
delay_table <- delay_summary %>%
  select(EFY, Delay_Status, Combined) %>%
  pivot_wider(names_from = EFY, values_from = Combined) %>%
  left_join(total_summary %>% select(Delay_Status, Total_Combined), by = "Delay_Status") %>%
  arrange(desc(Total_Combined))  # Order by total frequency

# Create formatted Table 1
table1_delay_trend <- delay_table %>%
  gt() %>%
  tab_header(
    title = "Table 1: Trend of Delay Status Among Reviewed Maternal Deaths",
    subtitle = "Ethiopian Fiscal Years 2006–2017"
  ) %>%
  cols_label(
    Delay_Status = "Delay Status",
    Total_Combined = "Total (n and %)"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(14),
    table.width = pct(100),
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Delay_Status)
  )

# Display Table 1
print(table1_delay_trend)

# ==============================================================================
# 5. SPECIFIC DELAY TYPE ANALYSIS (EXCLUDING 'NO DELAY')
# ==============================================================================

# Check Final_Delay distribution
cat("\nSpecific Delay Type Distribution:\n")
print(table(MDSR_final_updated$Final_Delay))

# ==============================================================================
# 6. FIGURE 2: SPECIFIC DELAY TYPE TREND PLOT
# ==============================================================================

# Filter out 'No Delay'
delay_data <- MDSR_final_updated %>%
  filter(Final_Delay != "No Delay")

# Calculate frequency and percentage per EFY
delay_trend <- delay_data %>%
  group_by(EFY, Final_Delay) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%  # % among delays only
  ungroup()

# Order legend by total frequency across all years
legend_order_detailed <- delay_trend %>%
  group_by(Final_Delay) %>%
  summarise(Total = sum(Frequency), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(Final_Delay)

delay_trend <- delay_trend %>%
  mutate(Final_Delay = factor(Final_Delay, levels = legend_order_detailed))

# Create trend line plot
figure2_delay_types <- ggplot(delay_trend, 
                              aes(x = factor(EFY), y = Percentage, 
                                  color = Final_Delay, group = Final_Delay)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(labels = function(x) paste0(x, " EFY")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  labs(
    title = "Figure 2: Trend of Specific Delay Types Among Maternal Deaths",
    subtitle = "Ethiopian Fiscal Years 2006–2017 (Excluding 'No Delay' Cases)",
    x = "EFY",
    y = "Percentage (%)",
    color = "Delay Type"
  ) +
  scale_color_manual(
    values = c("Delay1" = "#E41A1C", "Delay2" = "#377EB8", "Delay3" = "#4DAF4A"),
    labels = c("Delay1" = "Delay One", "Delay2" = "Delay Two", "Delay3" = "Delay Three")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# Display Figure 2
print(figure2_delay_types)

# ==============================================================================
# 7. TABLE 2: SPECIFIC DELAY TYPE TREND TABLE
# ==============================================================================

# Calculate frequency and percentage per EFY
delay_summary_detailed <- delay_data %>%
  group_by(EFY, Final_Delay) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(EFY) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  ungroup() %>%
  mutate(Combined = paste0(Frequency, " (", round(Percentage, 1), "%)"))

# Calculate total frequency and percentage across all years
total_summary_detailed <- delay_summary_detailed %>%
  group_by(Final_Delay) %>%
  summarise(Total = sum(Frequency), .groups = "drop") %>%
  mutate(Total_Combined = paste0(Total, " (", round((Total / sum(Total)) * 100, 1), "%)"))

# Pivot so EFY are columns, Final_Delay are rows
delay_table_detailed <- delay_summary_detailed %>%
  select(EFY, Final_Delay, Combined) %>%
  pivot_wider(names_from = EFY, values_from = Combined) %>%
  left_join(total_summary_detailed %>% select(Final_Delay, Total_Combined), by = "Final_Delay") %>%
  mutate(Final_Delay = factor(Final_Delay, levels = legend_order_detailed)) %>%
  arrange(Final_Delay)

# Create formatted Table 2
table2_delay_types <- delay_table_detailed %>%
  gt() %>%
  tab_header(
    title = "Table 2: Trend of Specific Delay Types Among Maternal Deaths",
    subtitle = "Ethiopian Fiscal Years 2006–2017 (Excluding 'No Delay' Cases)"
  ) %>%
  cols_label(
    Final_Delay = "Delay Type",
    Total_Combined = "Total (n and %)"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(14),
    table.width = pct(100),
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Final_Delay)
  )

# Display Table 2
print(table2_delay_types)

# ==============================================================================
# 8. FIGURE 3: DONUT CHART FOR DELAY STATUS (EFY 2017)
# ==============================================================================

# Filter for EFY 2017 and summarize
delay_2017 <- MDSR_final_updated %>%
  filter(EFY == 2017) %>%
  group_by(Delay_Status) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  arrange(desc(Frequency)) %>%  # Largest segment first
  mutate(
    ypos = cumsum(Percentage) - 0.5 * Percentage,  # Position for labels
    Label = paste0(Delay_Status, "\n", Frequency, " (", round(Percentage, 1), "%)")
  )

# Create donut chart
figure3_delay_donut <- ggplot(delay_2017, aes(x = 2, y = Percentage, fill = Delay_Status)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y", start = 0, direction = -1) +  # Clockwise
  geom_text(aes(y = ypos, label = Label), 
            color = "white", size = 4.5, fontface = "bold") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c("With Delay" = "#E41A1C", "No Delay" = "#377EB8")) +
  theme_void() +
  labs(
    title = "Figure 3: Distribution of Delay Status Among Reviewed Maternal Deaths",
    subtitle = "Ethiopian Fiscal Year 2017",
    fill = "Delay Status"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40",
                                 margin = margin(b = 20)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 10, b = 10)
  )

# Display Figure 3
print(figure3_delay_donut)

# ==============================================================================
# 9. ADDITIONAL ANALYSES (OPTIONAL)
# ==============================================================================

# 9.1. Annual Maternal Death Counts
annual_counts <- MDSR_final_updated %>%
  group_by(EFY) %>%
  summarise(
    Total_Deaths = n(),
    With_Delay = sum(Delay_Status == "With Delay"),
    No_Delay = sum(Delay_Status == "No Delay"),
    .groups = "drop"
  ) %>%
  mutate(
    Percent_With_Delay = round(With_Delay / Total_Deaths * 100, 1),
    Percent_No_Delay = round(No_Delay / Total_Deaths * 100, 1)
  )

cat("\nAnnual Maternal Death Counts:\n")
print(annual_counts)

# 9.2. Delay Type Distribution for EFY 2017
delay_type_2017 <- MDSR_final_updated %>%
  filter(EFY == 2017) %>%
  group_by(Final_Delay) %>%
  summarise(
    Frequency = n(),
    Percentage = round(n() / nrow(filter(MDSR_final_updated, EFY == 2017)) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Frequency))

cat("\nDelay Type Distribution for EFY 2017:\n")
print(delay_type_2017)

# ==============================================================================
# 10. SUMMARY STATISTICS
# ==============================================================================

cat("\n" + strrep("=", 80) + "\n")
cat("MDSR ANALYSIS SUMMARY (2006-2017)\n")
cat(strrep("=", 80) + "\n\n")

# Overall statistics
total_deaths <- nrow(MDSR_final_updated)
deaths_with_delay <- sum(MDSR_final_updated$Delay_Status == "With Delay")
deaths_no_delay <- sum(MDSR_final_updated$Delay_Status == "No Delay")

cat("OVERALL STATISTICS:\n")
cat("Total maternal deaths reviewed:", total_deaths, "\n")
cat("Deaths with delay identified:", deaths_with_delay, 
    "(", round(deaths_with_delay/total_deaths*100, 1), "%)\n")
cat("Deaths with no delay identified:", deaths_no_delay, 
    "(", round(deaths_no_delay/total_deaths*100, 1), "%)\n\n")

# Annual trend summary
cat("ANNUAL TREND SUMMARY:\n")
for (year in sort(unique(MDSR_final_updated$EFY))) {
  year_data <- MDSR_final_updated %>% filter(EFY == year)
  total_year <- nrow(year_data)
  with_delay_year <- sum(year_data$Delay_Status == "With Delay")
  percent_delay <- round(with_delay_year/total_year*100, 1)
  
  cat(sprintf("EFY %4d: %3d deaths (%3d with delay, %5.1f%%)\n", 
              year, total_year, with_delay_year, percent_delay))
}

cat("\n" + strrep("=", 80) + "\n")
cat("ANALYSIS OUTPUTS GENERATED:\n")
cat("1. Figure 1: Delay Status Trend Plot (2006-2017)\n")
cat("2. Table 1:  Delay Status Trend Table (2006-2017)\n")
cat("3. Figure 2: Specific Delay Type Trend Plot\n")
cat("4. Table 2:  Specific Delay Type Trend Table\n")
cat("5. Figure 3: Delay Status Donut Chart (EFY 2017)\n")
cat(strrep("=", 80) + "\n")

# ==============================================================================
# 11. SAVE OUTPUTS (OPTIONAL)
# ==============================================================================

# Uncomment to save outputs:

# # Save figures
# ggsave("Figure1_MDSR_Delay_Status_Trend.png", figure1_delay_trend, 
#        width = 12, height = 8, dpi = 300, bg = "white")
# ggsave("Figure2_MDSR_Specific_Delay_Types.png", figure2_delay_types, 
#        width = 12, height = 8, dpi = 300, bg = "white")
# ggsave("Figure3_MDSR_Delay_Donut_2017.png", figure3_delay_donut, 
#        width = 10, height = 8, dpi = 300, bg = "white")
# 
# # Save tables as HTML
# gtsave(table1_delay_trend, "Table1_MDSR_Delay_Status_Trend.html")
# gtsave(table2_delay_types, "Table2_MDSR_Specific_Delay_Types.html")
# 
# # Save summary data as CSV
# write.csv(annual_counts, "MDSR_Annual_Counts.csv", row.names = FALSE)
# write.csv(delay_type_2017, "MDSR_Delay_Types_2017.csv", row.names = FALSE)