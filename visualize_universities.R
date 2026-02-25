library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)

# Fetch university data from API
url <- "http://universities.hipolabs.com/search"
countries <- c("USA", "UK", "Canada", "Australia", "Germany", "France", "Japan")

university_counts <- data.frame()

for (country in countries) {
  response <- GET(url, query = list(country = country))  
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text"))
    count <- length(data)  
    
    university_counts <- rbind(university_counts, data.frame(
      Country = country,
      Count = count
    ))
  }
}

# Create visualization
plot <- ggplot(university_counts, aes(x = reorder(Country, Count), y = Count, fill = Country)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Universities by Country",
    subtitle = "Data sourced from Universities API",
    x = "Country",
    y = "Number of Universities"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Count), vjust = -0.5)

# Save the plot
ggsave("universities_plot.png", plot = plot, width = 10, height = 6)

cat("Visualization saved to universities_plot.png\n")
print(plot)