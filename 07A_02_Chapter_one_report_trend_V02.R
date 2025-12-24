# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Create data frame
quarters <- c("2006Q1", "2006Q2", "2006Q3", "2006Q4", "2007Q1", "2007Q2", "2007Q3", "2007Q4",
              "2008Q1", "2008Q2", "2008Q3", "2008Q4", "2009Q1", "2009Q2", "2009Q3", "2009Q4",
              "2010Q1", "2010Q2", "2010Q3", "2010Q4", "2011Q1", "2011Q2", "2011Q3", "2011Q4",
              "2012Q1", "2012Q2", "2012Q3", "2012Q4", "2013Q1", "2013Q2", "2013Q3", "2013Q4",
              "2014Q1", "2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2", "2015Q3", "2015Q4",
              "2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2", "2017Q3", "2017Q4")

notification <- c(0.0, 0.0, 0.0, 100.0, 16.5, 16.5, 26.6, 40.4, 14.9, 21.7, 31.6, 31.8,
                  13.1, 17.3, 31.4, 38.2, 26.2, 22.8, 25.6, 25.5, 22.7, 26.4, 25.4, 25.5,
                  21.9, 24.3, 22.0, 31.9, 21.1, 18.1, 28.2, 32.6, 29.7, 19.7, 23.4, 27.3,
                  30.0, 21.0, 25.3, 23.7, 24.6, 19.5, 25.7, 30.1, 26.0, 23.3, 23.4, 27.2)

case_review <- c(1.7, 9.1, 49.6, 39.7, 25.9, 28.8, 27.1, 18.2, 23.4, 27.2, 25.6, 23.8,
                 19.7, 24.2, 31.9, 24.2, 38.0, 30.0, 19.6, 12.4, 31.5, 35.7, 26.6, 6.2,
                 36.9, 30.1, 16.7, 16.4, 26.8, 21.5, 25.8, 25.8, 28.9, 22.2, 24.6, 24.3,
                 41.7, 27.9, 16.5, 13.9, 32.7, 24.8, 23.5, 19.0, 32.8, 26.4, 22.3, 18.5)

# Create data frame
data <- data.frame(
  Quarter = quarters,
  Notification = notification,
  CaseReview = case_review
)

# Convert to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = c(Notification, CaseReview),
               names_to = "Metric",
               values_to = "Proportion")

# Convert Quarter to ordered factor for proper x-axis ordering
data_long$Quarter <- factor(data_long$Quarter, levels = quarters)

# Create a numeric index for plotting
data_long$Quarter_num <- as.numeric(data_long$Quarter)

# Pre-calculate smoothed values
data_smooth <- data_long %>%
  group_by(Metric) %>%
  arrange(Quarter_num) %>%
  mutate(
    Smooth_Prop = predict(loess(Proportion ~ Quarter_num, span = 0.2))
  ) %>%
  ungroup()

# Create separate data frames for plotting
data_raw_notification <- data_long %>% filter(Metric == "Notification")
data_raw_review <- data_long %>% filter(Metric == "CaseReview")
data_smooth_notification <- data_smooth %>% filter(Metric == "Notification")
data_smooth_review <- data_smooth %>% filter(Metric == "CaseReview")

# Create the plot with four separate lines
p2 <- ggplot() +
  # 1. Notification Raw Data (thin solid line)
  geom_line(
    data = data_raw_notification,
    aes(x = Quarter, y = Proportion, group = Metric, color = "Notification (Raw)"),
    linewidth = 0.8,
    linetype = "solid",
    alpha = 0.9
  ) +
  
  # 2. Notification Smoothed (thick broken line)
  geom_line(
    data = data_smooth_notification,
    aes(x = Quarter, y = Smooth_Prop, group = Metric, color = "Notification (Smoothed)"),
    linewidth = 1.5,
    linetype = "dashed",
    alpha = 0.9
  ) +
  
  # 3. Case Review Raw Data (thin solid line)
  geom_line(
    data = data_raw_review,
    aes(x = Quarter, y = Proportion, group = Metric, color = "Case Review (Raw)"),
    linewidth = 0.8,
    linetype = "solid",
    alpha = 0.9
  ) +
  
  # 4. Case Review Smoothed (thick broken line)
  geom_line(
    data = data_smooth_review,
    aes(x = Quarter, y = Smooth_Prop, group = Metric, color = "Case Review (Smoothed)"),
    linewidth = 1.5,
    linetype = "dashed",
    alpha = 0.9
  ) +
  
  # Add points for raw data
  geom_point(
    data = data_raw_notification,
    aes(x = Quarter, y = Proportion, color = "Notification (Raw)"),
    size = 2,
    alpha = 0.8
  ) +
  
  geom_point(
    data = data_raw_review,
    aes(x = Quarter, y = Proportion, color = "Case Review (Raw)"),
    size = 2,
    alpha = 0.8
  ) +
  
  # Custom color scale with four distinct colors
  scale_color_manual(
    name = "",
    values = c(
      "Notification (Raw)" = "#1F77B4",        # Blue for raw notification
      "Notification (Smoothed)" = "#FF7F0E",   # Orange for smoothed notification
      "Case Review (Raw)" = "#2CA02C",         # Green for raw review
      "Case Review (Smoothed)" = "#D62728"     # Red for smoothed review
    ),
    breaks = c(
      "Notification (Raw)",
      "Notification (Smoothed)", 
      "Case Review (Raw)",
      "Case Review (Smoothed)"
    ),
    guide = guide_legend(
      nrow = 2,  # Changed from 1 to 2 rows
      byrow = TRUE,
      override.aes = list(
        linetype = c("solid", "dashed", "solid", "dashed"),
        linewidth = c(0.8, 1.5, 0.8, 1.5)
      )
    )
  ) +
  
  # Y-axis scale
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, by = 20),
    minor_breaks = seq(0, 100, by = 10),
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X-axis scale - show all quarters with BOLD labels
  scale_x_discrete(
    breaks = quarters,
    labels = quarters
  ) +
  
  # Labels and titles
  labs(
    title = "MDSR Performance per Quarter from 2006 to 2017 EFY",
    subtitle = "Raw data (thin lines) and smoothed trends (dashed thick lines)",
    x = "Quarter",
    y = "Proportion (%)"
  ) +
  
  # Custom theme
  theme_minimal(base_size = 12) +
  theme(
    # Title styling
    plot.title = element_text(
      face = "bold", 
      hjust = 0.5, 
      size = 16, 
      margin = margin(b = 5),
      color = "black"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 12, 
      color = "gray40", 
      margin = margin(b = 15)
    ),
    
    # Axis styling - QUARTER LABELS IN BOLD
    axis.text.x = element_text(
      angle = 90, 
      hjust = 1, 
      vjust = 0.5, 
      size = 8,
      color = "gray30",
      face = "bold"  # ADDED BOLD FACE
    ),
    axis.text.y = element_text(
      size = 10,
      color = "gray30"
    ),
    axis.title.x = element_text(
      size = 12, 
      face = "bold", 
      margin = margin(t = 10),
      color = "black"
    ),
    axis.title.y = element_text(
      size = 12, 
      face = "bold", 
      margin = margin(r = 10),
      color = "black"
    ),
    
    # Legend styling - changed to 2 rows
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 10),
    legend.margin = margin(b = 0, t = 0),
    legend.box.margin = margin(b = 5, t = 5),
    legend.box = "vertical",  # Changed to vertical for 2 rows
    legend.box.just = "center",
    legend.spacing.x = unit(10, "pt"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    
    # Grid styling
    panel.grid.major.x = element_line(color = "gray95", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray95", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Panel background
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Plot margins (adjusted for 2-row legend)
    plot.margin = margin(25, 25, 25, 25)  # Increased top margin for 2-row legend
  ) +
  
  # Add vertical lines for each year (subtle)
  geom_vline(
    xintercept = seq(1, 48, by = 4) - 0.5, 
    color = "gray90", 
    linewidth = 0.4, 
    linetype = "solid"
  ) +
  
  # Add year labels at the top
  annotate(
    "text",
    x = seq(2.5, 46.5, by = 4),
    y = 103,
    label = 2006:2017,
    size = 3.5,
    fontface = "bold",
    color = "gray40"
  )

# Display the plot
print(p2)

# Save with optimal dimensions
ggsave(
  "MDSR_Performance_Four_Lines.png",
  plot = p2,
  width = 20,    # Wider to accommodate all quarters and legend
  height = 11,    # Increased height for 2-row legend
  dpi = 300,
  bg = "white"
)

