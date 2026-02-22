library(ggplot2)
data <- read.csv("scatter_data.csv")
p <- ggplot(data, aes(x = x, y = y, colour = x <5.1)) +
  geom_point() + 
  theme_minimal()
ggsave("scatter_plot.png", plot = p)