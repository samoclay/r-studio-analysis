library(ggplot2)

data <- data.frame(
  x = seq(-10, 10, by = 0.1)
)

data$y <- data$x^2

ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Plot of y = x^2",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

