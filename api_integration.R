# API Integration Script
# Fetches fun data about random universities and their locations
# Uses the Universities API (free, no authentication required)

library(httr)
library(jsonlite)
library(dplyr)

# Function to fetch random university data
get_university_data <- function(num_universities = 10) {
  tryCatch({
    # Fetch data from Universities API
    url <- "http://universities.hipolabs.com/search"
    
    # Get data for multiple countries to get variety
    countries <- c("USA", "UK", "Canada", "Australia", "Germany", "Japan", "India")
    
    all_universities <- data.frame()
    
    for (country in countries) {
      response <- GET(url, query = list(country = country))
      
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, as = "text"))
        
        # Extract relevant fields
        universities <- data.frame(
          name = sapply(data, function(x) x$name),
          country = sapply(data, function(x) x$country),
          domains = sapply(data, function(x) paste(x$domains, collapse = ", ")), 
          stringsAsFactors = FALSE
        )
        
        all_universities <- rbind(all_universities, universities)
      }
    }
    
    # Return sample of universities
    if (nrow(all_universities) > 0) {
      return(all_universities %>% slice_sample(n = min(num_universities, nrow(all_universities))))
    } else {
      stop("No data retrieved from API")
    }
    
  }, error = function(e) {
    cat("Error fetching data:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Fetch and save the data
cat("Fetching university data from API...\n")
university_data <- get_university_data(15)

if (!is.null(university_data)) {
  cat("Successfully retrieved", nrow(university_data), "universities\n")
  cat("\nSample of data:\n")
  print(head(university_data))
  
  # Save to CSV for reference
  write.csv(university_data, "university_data.csv", row.names = FALSE)
  cat("\nData saved to university_data.csv\n")
} else {
  cat("Failed to retrieve data\n")
}