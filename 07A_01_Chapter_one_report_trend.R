# =========================================
# CORRECTED BEST PLOT (No Warnings)
# =========================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggtext)

# Create the data
surveillance_data <- data.frame(
  EFY = 2006:2017,
  Notification_rate = c(0.00, 3.10, 6.20, 11.30, 14.90, 12.80, 12.60, 18.30, 19.10, 23.20, 22.10, 29.80),
  Case_based_reporting_rate = c(1.40, 4.10, 7.80, 12.50, 11.50, 5.90, 3.90, 12.10, 13.00, 13.10, 15.50, 18.90)
)

# Convert to long format
surveillance_long <- surveillance_data %>%
  pivot_longer(
    cols = c(Notification_rate, Case_based_reporting_rate),
    names_to = "Metric",
    values_to = "Rate"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Notification_rate", "Case_based_reporting_rate"),
                    labels = c("**Notification Rate**", "**Case-based Reporting Rate**"))
  )

# Create the plot WITHOUT confidence bands initially
best_plot <- ggplot(surveillance_long, aes(x = EFY, y = Rate, color = Metric)) +
  # Smooth trend lines WITHOUT confidence bands first
  geom_smooth(
    aes(linetype = Metric),
    method = "loess",
    formula = y ~ x,
    span = 0.65,
    se = FALSE,  # NO confidence bands initially
    linewidth = 1.8
  ) +
  # Actual data points
  geom_point(
    aes(shape = Metric),
    size = 4.5,
    fill = "white",
    stroke = 1.3
  ) +
  # Key data labels (first, middle, last years)
  geom_text(
    data = . %>% 
      group_by(Metric) %>% 
      filter(EFY %in% c(2006, 2011, 2017)),
    aes(label = sprintf("%.1f%%", Rate)),
    vjust = -1.2,
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Color scheme
  scale_color_manual(
    values = c(
      "**Notification Rate**" = "#1F77B4",
      "**Case-based Reporting Rate**" = "#FF7F0E"
    ),
    guide = guide_legend(override.aes = list(fill = NA, linewidth = 1.5))
  ) +
  scale_shape_manual(
    values = c(
      "**Notification Rate**" = 21,
      "**Case-based Reporting Rate**" = 24
    )
  ) +
  scale_linetype_manual(
    values = c(
      "**Notification Rate**" = "solid",
      "**Case-based Reporting Rate**" = "solid"
    ),
    guide = "none"
  ) +
  # Axes and scales with more generous y-limits
  scale_x_continuous(
    breaks = seq(2006, 2017, by = 1),
    labels = function(x) paste0("'", substr(x, 3, 4)),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    limits = c(0, 40),  # Increased to 40% for confidence bands
    breaks = seq(0, 40, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  # Labels and title
  labs(
    title = "Trends in Maternal Death Surveillance Coverage: Ethiopia (2006-2017 EFY)",
    subtitle = "Smoothed trend lines show the evolution of notification and case-based reporting systems<br>over 12 Ethiopian fiscal years",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Coverage Rate",
    caption = "**Data Source:** Ethiopian Public Health Institute | **Note:** Lines show LOESS-smoothed trends (span=0.65)"
  ) +
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_markdown(
      face = "bold",
      size = 18,
      hjust = 0,
      margin = margin(b = 8, t = 10),
      lineheight = 1.2
    ),
    plot.subtitle = element_markdown(
      size = 13,
      hjust = 0,
      color = "gray40",
      margin = margin(b = 20),
      lineheight = 1.3
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 15)
    ),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "gray30"),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_markdown(size = 11, margin = margin(r = 15)),
    legend.spacing.x = unit(0.5, "cm"),
    legend.margin = margin(b = 10),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.width = unit(2.5, "cm"),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "gray96", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(30, 40, 30, 30)
  )

# Display the plot
print(best_plot)

# Save it
ggsave("Maternal_Surveillance_Trends_CLEAN.png", 
       plot = best_plot,
       width = 13, 
       height = 8, 
       dpi = 600, 
       bg = "white")

# =========================================
# ALTERNATIVE 1: With confidence bands but careful limits
# =========================================

# First, calculate the max possible y-value including confidence intervals
# We'll create the smoothed data to see its range
smooth_data_notif <- loess(Notification_rate ~ EFY, 
                           data = surveillance_data, 
                           span = 0.65)
smooth_data_case <- loess(Case_based_reporting_rate ~ EFY, 
                          data = surveillance_data, 
                          span = 0.65)

# Predict with confidence intervals
pred_notif <- predict(smooth_data_notif, se = TRUE)
pred_case <- predict(smooth_data_case, se = TRUE)

# Calculate max y value including confidence
max_y_notif <- max(pred_notif$fit + 1.96 * pred_notif$se.fit, na.rm = TRUE)
max_y_case <- max(pred_case$fit + 1.96 * pred_case$se.fit, na.rm = TRUE)
max_y <- max(max_y_notif, max_y_case, surveillance_data$Notification_rate, 
             surveillance_data$Case_based_reporting_rate, na.rm = TRUE)

cat(sprintf("Maximum y-value including confidence intervals: %.1f%%\n", max_y))

# Now create plot with appropriate y-limits
best_plot_with_ci <- ggplot(surveillance_long, aes(x = EFY, y = Rate, color = Metric)) +
  # Smooth trend lines WITH confidence bands
  geom_smooth(
    aes(fill = Metric),
    method = "loess",
    formula = y ~ x,
    span = 0.65,
    se = TRUE,
    alpha = 0.15,
    linewidth = 1.8,
    level = 0.95
  ) +
  # Actual data points
  geom_point(
    aes(shape = Metric),
    size = 4.5,
    fill = "white",
    stroke = 1.3
  ) +
  # Color scheme
  scale_color_manual(
    values = c(
      "**Notification Rate**" = "#1F77B4",
      "**Case-based Reporting Rate**" = "#FF7F0E"
    )
  ) +
  scale_fill_manual(
    values = c(
      "**Notification Rate**" = "#1F77B4",
      "**Case-based Reporting Rate**" = "#FF7F0E"
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      "**Notification Rate**" = 21,
      "**Case-based Reporting Rate**" = 24
    )
  ) +
  # Adjusted y-limits
  scale_x_continuous(
    breaks = seq(2006, 2017, by = 1),
    labels = function(x) paste0("'", substr(x, 3, 4)),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    limits = c(0, 45),  # Increased to accommodate confidence bands
    breaks = seq(0, 45, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  # Labels and title
  labs(
    title = "Maternal Death Surveillance Trends with Confidence Intervals",
    subtitle = "Shaded areas represent 95% confidence intervals around LOESS-smoothed trends",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Coverage Rate",
    caption = "**Data Source:** Ethiopian Public Health Institute"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    legend.title = element_blank()
  )

print(best_plot_with_ci)
ggsave("Maternal_Surveillance_Trends_with_CI.png", 
       plot = best_plot_with_ci,
       width = 13, height = 8, dpi = 600, bg = "white")

# =========================================
# ALTERNATIVE 2: SIMPLEST CLEAN VERSION (No Warnings)
# =========================================

simple_plot <- ggplot(surveillance_long, aes(x = EFY, y = Rate, color = Metric)) +
  # Simple line connecting points (no smoothing)
  geom_line(linewidth = 1.5, alpha = 0.8) +
  # Data points
  geom_point(
    aes(shape = Metric),
    size = 4.5,
    fill = "white",
    stroke = 1.2
  ) +
  # Add value labels for key years
  geom_text(
    data = . %>% filter(EFY %in% c(2006, 2009, 2012, 2015, 2017)),
    aes(label = sprintf("%.1f%%", Rate)),
    vjust = -1,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Color and shape scales
  scale_color_manual(
    values = c(
      "**Notification Rate**" = "#2E86AB",
      "**Case-based Reporting Rate**" = "#A23B72"
    )
  ) +
  scale_shape_manual(
    values = c(
      "**Notification Rate**" = 19,
      "**Case-based Reporting Rate**" = 17
    )
  ) +
  # Axes
  scale_x_continuous(
    breaks = seq(2006, 2017, by = 1),
    labels = function(x) paste0("'", substr(x, 3, 4)),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  # Labels
  labs(
    title = "Maternal Death Surveillance Coverage Trends (2006-2017 EFY)",
    subtitle = "Direct comparison of notification and case-based reporting rates",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Coverage Rate (%)",
    caption = "Data Source: Ethiopian Public Health Institute"
  ) +
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )

print(simple_plot)
ggsave("Maternal_Surveillance_Trends_SIMPLE.png", 
       plot = simple_plot,
       width = 12, height = 7, dpi = 600, bg = "white")

# =========================================
# RECOMMENDATION
# =========================================
cat("\n" + strrep("=", 60) + "\n")
cat("PLOT RECOMMENDATIONS:\n")
cat(strrep("=", 60) + "\n")
cat("1. **For presentations/publications**: Use 'Maternal_Surveillance_Trends_CLEAN.png'\n")
cat("   - Clean, no warnings\n")
cat("   - Shows trends clearly\n")
cat("   - Professional appearance\n\n")
cat("2. **For statistical audiences**: Use 'Maternal_Surveillance_Trends_with_CI.png'\n")
cat("   - Includes confidence intervals\n")
cat("   - Shows uncertainty in trends\n\n")
cat("3. **For simplest view**: Use 'Maternal_Surveillance_Trends_SIMPLE.png'\n")
cat("   - Just lines connecting actual data points\n")
cat("   - No smoothing, no confidence bands\n")
cat("   - Most straightforward interpretation\n")
cat(strrep("=", 60) + "\n")
#####################
# =========================================
# BEST SINGLE PLOT: MATERNAL DEATH SURVEILLANCE TRENDS
# =========================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggtext)  # For enhanced text formatting

# Create the data
surveillance_data <- data.frame(
  EFY = 2006:2017,
  Notification_rate = c(0.00, 3.10, 6.20, 11.30, 14.90, 12.80, 12.60, 18.30, 19.10, 23.20, 22.10, 29.80),
  Case_based_reporting_rate = c(1.40, 4.10, 7.80, 12.50, 11.50, 5.90, 3.90, 12.10, 13.00, 13.10, 15.50, 18.90)
)

# Convert to long format
surveillance_long <- surveillance_data %>%
  pivot_longer(
    cols = c(Notification_rate, Case_based_reporting_rate),
    names_to = "Metric",
    values_to = "Rate"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Notification_rate", "Case_based_reporting_rate"),
                    labels = c("**Notification Rate**", "**Case-based Reporting Rate**")),
    EFY_label = paste0("'", substr(EFY, 3, 4))  # Format as '06, '07, etc.
  )

# Create the plot
best_plot <- ggplot(surveillance_long, aes(x = EFY, y = Rate, color = Metric)) +
  # Smooth trend lines (loess with optimal span)
  geom_smooth(
    aes(fill = Metric, linetype = Metric),
    method = "loess",
    formula = y ~ x,
    span = 0.65,  # Optimal smoothness for this data
    se = TRUE,    # Show confidence interval
    alpha = 0.15, # Light confidence bands
    linewidth = 1.8,  # Emphasize trends
    level = 0.95  # 95% confidence interval
  ) +
  # Actual data points
  geom_point(
    aes(shape = Metric),
    size = 4.5,
    fill = "white",
    stroke = 1.3
  ) +
  # Key data labels (first, middle, last years)
  geom_text(
    data = . %>% 
      group_by(Metric) %>% 
      filter(EFY %in% c(2006, 2011, 2017)),
    aes(label = sprintf("%.1f%%", Rate)),
    vjust = -1.2,
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Color scheme - professional, accessible
  scale_color_manual(
    values = c(
      "**Notification Rate**" = "#1F77B4",  # Distinct blue
      "**Case-based Reporting Rate**" = "#FF7F0E"  # Distinct orange
    ),
    guide = guide_legend(override.aes = list(fill = NA))
  ) +
  scale_fill_manual(
    values = c(
      "**Notification Rate**" = "#1F77B4",
      "**Case-based Reporting Rate**" = "#FF7F0E"
    ),
    guide = "none"  # Hide fill from legend
  ) +
  scale_shape_manual(
    values = c(
      "**Notification Rate**" = 21,  # Filled circle
      "**Case-based Reporting Rate**" = 24  # Filled triangle
    )
  ) +
  scale_linetype_manual(
    values = c(
      "**Notification Rate**" = "solid",
      "**Case-based Reporting Rate**" = "solid"
    ),
    guide = "none"  # Hide linetype from legend
  ) +
  # Axes and scales
  scale_x_continuous(
    breaks = seq(2006, 2017, by = 1),
    labels = function(x) paste0("'", substr(x, 3, 4)),  # '06 format
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.1))  # Space for labels
  ) +
  # Labels and title
  labs(
    title = "Trends in Maternal Death Surveillance Coverage: Ethiopia (2006-2017 EFY)",
    subtitle = "Smoothed trend lines with 95% confidence intervals show the evolution of<br>notification and case-based reporting systems over 12 Ethiopian fiscal years",
    x = "Ethiopian Fiscal Year (EFY)",
    y = "Coverage Rate",
    caption = "**Data Source:** Ethiopian Ministry of Health | **Note:** Lines show LOESS-smoothed trends (span=0.65) with shaded 95% confidence intervals"
  ) +
  # Professional theme
  theme_minimal(base_size = 13) +
  theme(
    # Title styling
    plot.title = element_markdown(
      face = "bold",
      size = 18,
      hjust = 0,
      margin = margin(b = 8, t = 10),
      lineheight = 1.2
    ),
    plot.subtitle = element_markdown(
      size = 13,
      hjust = 0,
      color = "gray40",
      margin = margin(b = 20),
      lineheight = 1.3
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 15)
    ),
    # Axis styling
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    # Legend styling
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_markdown(size = 11, margin = margin(r = 15)),
    legend.spacing.x = unit(0.5, "cm"),
    legend.margin = margin(b = 10),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.width = unit(2.5, "cm"),
    # Grid and background
    panel.grid.major = element_line(color = "gray92", linewidth = 0.4),
    panel.grid.minor = element_line(color = "gray96", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(30, 40, 30, 30)
  )

# Display the plot
print(best_plot)

# Save high-quality versions
ggsave("Maternal_Surveillance_Trends_BEST.png", 
       plot = best_plot,
       width = 13, 
       height = 8, 
       dpi = 600, 
       bg = "white")

ggsave("Maternal_Surveillance_Trends_BEST.pdf", 
       plot = best_plot,
       width = 13, 
       height = 8, 
       device = cairo_pdf)

# =========================================
# ADDITIONAL INSIGHTS ANNOTATION VERSION
# =========================================

# Same plot with key insights annotated
annotated_plot <- best_plot +
  # Highlight key periods
  annotate(
    "rect",
    xmin = 2006, xmax = 2009,
    ymin = -Inf, ymax = Inf,
    fill = "#E8F4F8",
    alpha = 0.3
  ) +
  annotate(
    "rect",
    xmin = 2015, xmax = 2017,
    ymin = -Inf, ymax = Inf,
    fill = "#FFF4E6",
    alpha = 0.3
  ) +
  # Add text annotations
  annotate(
    "text",
    x = 2007.5, y = 32,
    label = "Early Growth Phase\n(2006-2009)",
    size = 3.5,
    color = "#1F77B4",
    fontface = "italic",
    lineheight = 1.1
  ) +
  annotate(
    "text",
    x = 2016, y = 32,
    label = "Accelerated\nImprovement",
    size = 3.5,
    color = "#1F77B4",
    fontface = "italic",
    lineheight = 1.1
  ) +
  annotate(
    "segment",
    x = 2015, xend = 2017,
    y = 25, yend = 29.8,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "#1F77B4",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2016, y = 23.5,
    label = "+7.7% in 2 years",
    size = 3.2,
    color = "#1F77B4",
    fontface = "bold"
  ) +
  # Update title for annotated version
  labs(
    title = "Maternal Death Surveillance Trends with Key Growth Phases",
    subtitle = "Highlighted periods show early establishment (2006-2009) and recent acceleration (2015-2017)<br>of Ethiopia's maternal death notification and review systems"
  )

# Save annotated version
ggsave("Maternal_Surveillance_Trends_ANNOTATED.png", 
       plot = annotated_plot,
       width = 13, 
       height = 8, 
       dpi = 600, 
       bg = "white")

# =========================================
# PRINT KEY INSIGHTS
# =========================================
cat("\n" + strrep("═", 70) + "\n")
cat("KEY INSIGHTS FROM MATERNAL DEATH SURVEILLANCE TRENDS (2006-2017 EFY)\n")
cat(strrep("═", 70) + "\n\n")

# Calculate key metrics
growth_notif_12yr <- surveillance_data$Notification_rate[12] - surveillance_data$Notification_rate[1]
growth_case_12yr <- surveillance_data$Case_based_reporting_rate[12] - surveillance_data$Case_based_reporting_rate[1]
recent_growth_notif <- surveillance_data$Notification_rate[12] - surveillance_data$Notification_rate[10]
recent_growth_case <- surveillance_data$Case_based_reporting_rate[12] - surveillance_data$Case_based_reporting_rate[10]
avg_gap <- mean(surveillance_data$Notification_rate - surveillance_data$Case_based_reporting_rate)

cat("📈 **OVERALL TRENDS (12-Year Period):**\n")
cat(sprintf("  • Notification rate increased from %.1f%% to %.1f%% (+%.1f%% points)\n", 
            surveillance_data$Notification_rate[1], surveillance_data$Notification_rate[12], growth_notif_12yr))
cat(sprintf("  • Case-based reporting increased from %.1f%% to %.1f%% (+%.1f%% points)\n\n", 
            surveillance_data$Case_based_reporting_rate[1], surveillance_data$Case_based_reporting_rate[12], growth_case_12yr))

cat("🚀 **RECENT ACCELERATION (2015-2017):**\n")
cat(sprintf("  • Notification: +%.1f%% points in 2 years (%.1f%% to %.1f%%)\n", 
            recent_growth_notif, surveillance_data$Notification_rate[10], surveillance_data$Notification_rate[12]))
cat(sprintf("  • Case-based: +%.1f%% points in 2 years (%.1f%% to %.1f%%)\n\n", 
            recent_growth_case, surveillance_data$Case_based_reporting_rate[10], surveillance_data$Case_based_reporting_rate[12]))

cat("📊 **PERFORMANCE GAP ANALYSIS:**\n")
cat(sprintf("  • Average gap (Notification - Case-based): %.1f%% points\n", avg_gap))
cat(sprintf("  • 2017 gap: %.1f%% points (%.1f%% vs %.1f%%)\n", 
            surveillance_data$Notification_rate[12] - surveillance_data$Case_based_reporting_rate[12],
            surveillance_data$Notification_rate[12], surveillance_data$Case_based_reporting_rate[12]))

# Find peak gap year
peak_gap_year <- surveillance_data$EFY[which.max(surveillance_data$Notification_rate - surveillance_data$Case_based_reporting_rate)]
peak_gap_value <- max(surveillance_data$Notification_rate - surveillance_data$Case_based_reporting_rate)
cat(sprintf("  • Largest gap: %.1f%% points in %d EFY\n\n", peak_gap_value, peak_gap_year))

cat("🎯 **SYSTEM MATURITY:**\n")
cat("  • Early phase (2006-2009): Rapid establishment of both systems\n")
cat("  • Middle phase (2010-2014): Consolidation with some fluctuations\n")
cat("  • Recent phase (2015-2017): Accelerated improvement, especially in notification\n")
cat(strrep("═", 70) + "\n")