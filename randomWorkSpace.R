
```{r cleanDataSet}

merge_observations <- function(dataset, district, year) {
  # Identify rows of districts with similar names
  district_rows <- 
    grep(pattern = paste('*', district, '*', sep=''), x = data$DISTRICT, ignore.case = TRUE)
  
  # New observation is returned from the function.
  cbind( # Add the year and district as new columns to the dataframe to for the new observation
    YEAR = year, 
    DISTRICT = district, 
    t( # A transposition is needed to make row observations as columns instead
      as.data.frame( # Converts the vector to a dataframe with column headers as rows headers
        colSums( # Creates a vector
          data[data$YEAR == year, ][district_rows, ][, -c(1:2)]), 
        optional = TRUE)
    )
  )
}

# Get the range of years
# years <- unique(data$YEAR)
# Use factors for these YEAR as it's ordinal data. 
data$YEAR <- as.factor(data$YEAR)

# get the levels. 
years <- levels(data$YEAR)

# A list to capture new records. We shall subsequently merge these items.
new_data <- list()

for(year in years) {
  for(district in districts) {
    new_data[[year]] <- merge_observations(dataset = data[data$YEAR == year, ], district = district, year = year)
  }
}

```