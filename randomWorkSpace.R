
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



### What about merging similar districts?
Now that we have replaced the NA values with NIL/ZERO, we can merge the observations of similar districts into a single observation. We are assuming that:
  
  * Values are independent and do not overlap in each district's consituency/municipality.
* Values will be grouped by year (and district) and associated values summed.

So, our approach will be:

* Group the observations by year
* For each year:
+ identify districts with similar names
+ sum up the values for the similar districts and rename the observations with the official district name
+ merge the new observations with the original dataset whilst dropping the older records.
* Merge the above year groups into a new dataset.

```{r cleanDataset}
# concatenate 2 columns with _ value. 
dat$YEAR_DICT <- paste(dat$YEAR, dat$DISTRICT, sep = "_")

# Using base aggregate to loop through the dataframe. This gives us unique YEAR_DISTRICT as 1 var. 
# Elimidate YEAR_DISTRICT var. 
# Sum all the rows and aggregate into 1 row assigned to YEAR_DISTRICT LINE
dat_df <- aggregate(dat[,3:11], list(dat[,12]), function(x) sum(x, na.rm = TRUE))

# create temporary list to split the merged YEAR_DISTRICT into 2 columns. 
list <- strsplit(dat_df$Group.1, "_")

# Create first part using inbuild Reduce to separate the list while combining them 
# onto a dataframe with rbind. call df_a
dat_df_a <- data.frame(Reduce(rbind, list))

# separate remaining dataframe from YEAR_DISTRICT values. call df_b
dat_df_b <- dat_df[, 2:10]

# Map the 2 dataframes df_a and df_b by columns using cbind. 
dat_df_clean <- cbind(dat_df_a, dat_df_b)

# Rename the column names to something readable or back to previous. 
cols <- c("YEAR","DISTRICT")
colnames(dat_df_clean[, 1:2]) <- cols
