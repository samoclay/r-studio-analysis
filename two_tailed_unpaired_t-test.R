### ============================================================
### 1. Load packages
### ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)

### ============================================================
### 2. Import your tab-delimited dataset
###    (Two numeric columns, any names)
### ============================================================
data <- read.delim("CSV_FILE_NAME/PATH_HERE", header = TRUE, sep = "\t")

# Extract column names automatically
col1 <- names(data)[1]
col2 <- names(data)[2]

### ============================================================
### 3. Compute summary statistics
### ============================================================

# Means
mean1 <- mean(data[[col1]], na.rm = TRUE)
mean2 <- mean(data[[col2]], na.rm = TRUE)

# Standard deviations
sd1 <- sd(data[[col1]], na.rm = TRUE)
sd2 <- sd(data[[col2]], na.rm = TRUE)

# Welch two-sample t-test
tt <- t.test(data[[col1]], data[[col2]])

p_value_exact <- format(tt$p.value, scientific = FALSE, digits = 10) 
lower_CI <- tt$conf.int[1]
upper_CI <- tt$conf.int[2]

### Print numerical output cleanly
cat("==============================================\n")
cat("Column means:\n")
cat(col1, "=", mean1, "\n")
cat(col2, "=", mean2, "\n\n")

cat("Standard deviations:\n")
cat(col1, "=", sd1, "\n")
cat(col2, "=", sd2, "\n\n")

cat("Welch Two-Sample t-test:\n")
cat("p-value =", p_value_exact, "\n")
cat("95% CI of mean difference: [", lower_CI, ", ", upper_CI, "]\n")
cat("==============================================\n\n")

### ============================================================
### 4. Prepare data for the plot (Points + Mean ± 95% CI)
### ============================================================

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

### ============================================================
### 5. Plot: Points + Mean ± 95% CI (NO BOXES)
### ============================================================

# assumes df_long$value is 0–100
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

### ============================================================
### TABLE 1: Unpaired t test 
### ============================================================

# F test for variance equality (GraphPad always reports F test)
f_test <- var.test(data[[col1]], data[[col2]])

# Build the table as a data.frame
table1 <- data.frame(
  Field = c(
    "Table Analyzed",
    "Column A", "vs.", "Column B",
    "P value", "P value summary",
    "Significantly different (P < 0.05)?",
    "One- or two-tailed P value?",
    "t, df",
    "Mean of Column A",
    "Mean of Column B",
    "Difference between means (A - B) ± SEM",
    "95% CI of difference",
    "R squared (eta squared)",
    "F, DFn, DFd",
    "P value (F test)",
    "Significantly different variance?",
    "Sample size, Column A",
    "Sample size, Column B"
  ),
  
  Value = c(
    "Data 1",
    col1, "",
    col2,
    format(tt$p.value, digits = 4),
    ifelse(tt$p.value < 0.0001, "<0.0001", ""),
    ifelse(tt$p.value < 0.05, "Yes", "No"),
    "Two-tailed",
    paste0("t = ", round(tt$statistic,3), ", df = ", round(tt$parameter,2)),
    round(mean1, 4),
    round(mean2, 4),
    paste0(round(mean1 - mean2, 4), " ± ", round(sumstats$se[1],4)),
    paste0(round(lower_CI,3), " to ", round(upper_CI,3)),
    round(tt$statistic^2 / (tt$statistic^2 + tt$parameter), 4),
    paste0(round(f_test$statistic,3), ", ", f_test$parameter[1], ", ", f_test$parameter[2]),
    signif(f_test$p.value, 3),
    ifelse(f_test$p.value < 0.05, "Yes", "No"),
    sumstats$n[ sumstats$group == col1 ],
    sumstats$n[ sumstats$group == col2 ]
  )
)

print(table1, row.names = FALSE)

### ============================================================
### TABLE 2: Descriptive Statistics 
### ============================================================

desc <- data.frame(
  Statistic = c(
    "Number of values",
    "Minimum",
    "25% Percentile",
    "Median",
    "75% Percentile",
    "Maximum",
    "Mean",
    "Std. Deviation",
    "Std. Error of Mean",
    "Lower 95% CI",
    "Upper 95% CI"
  ),
  
  Column_A = c(
    sumstats$n[1],
    min(data[[col1]], na.rm = TRUE),
    quantile(data[[col1]], 0.25, na.rm = TRUE),
    quantile(data[[col1]], 0.50, na.rm = TRUE),
    quantile(data[[col1]], 0.75, na.rm = TRUE),
    max(data[[col1]], na.rm = TRUE),
    mean1,
    sd1,
    sd1 / sqrt(sumstats$n[1]),
    mean1 - qt(0.975, df = sumstats$n[1] - 1) * (sd1/sqrt(sumstats$n[1])),
    mean1 + qt(0.975, df = sumstats$n[1] - 1) * (sd1/sqrt(sumstats$n[1]))
  ),
  
  Column_B = c(
    sumstats$n[2],
    min(data[[col2]], na.rm = TRUE),
    quantile(data[[col2]], 0.25, na.rm = TRUE),
    quantile(data[[col2]], 0.50, na.rm = TRUE),
    quantile(data[[col2]], 0.75, na.rm = TRUE),
    max(data[[col2]], na.rm = TRUE),
    mean2,
    sd2,
    sd2 / sqrt(sumstats$n[2]),
    mean2 - qt(0.975, df = sumstats$n[2] - 1) * (sd2/sqrt(sumstats$n[2])),
    mean2 + qt(0.975, df = sumstats$n[2] - 1) * (sd2/sqrt(sumstats$n[2]))
  )
)

print(desc, row.names = FALSE)

