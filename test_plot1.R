###  Install necessary packages
library(ggplot2)

data <- read.delim("Dataset.1", header = TRUE, sep = "\t")

# Extract column names automatically
col1 <- names(data)[1]
col2 <- names(data)[2]

### Prepare data for the plot (Points + Mean ± 95% CI)

df_long <- data %>%
  pivot_longer(cols = everything(),
               names_to = "group",
               values_to = "value")

# Summary stats per group
sumstats <- df_long %>%
  group_by(group) %>%
  summarise(
    n     = n(),
    mean  = mean(value, na.rm = TRUE),
    sd    = sd(value, na.rm = TRUE),
    se    = sd / sqrt(n),
    ci95  = 1.96 * se,
    .groups = "drop"
  )

###  Plot: Points + Mean ± 95% CI

plot_out <- ggplot(df_long, aes(x = group, y = value)) +
  geom_jitter(
    aes(color = group),
    width = 0.21,                 
    size = 2.2, alpha = 0.8,
    show.legend = FALSE
  ) +
  # --- ADD a horizontal mean line per group ---
  geom_segment(
    data = sumstats,
    aes(
      x     = as.numeric(factor(group)) - 0.22,
      xend  = as.numeric(factor(group)) + 0.22,
      y     = mean,
      yend  = mean
    ),
    linewidth = 1.1,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("#2E75B6", "#C00000")) +
  scale_fill_manual(values  = c("#5B9BD5", "#FF6666")) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Group Comparison",
    x = "Fibre Type",
    y = "Heteroplasmy (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(plot_out)