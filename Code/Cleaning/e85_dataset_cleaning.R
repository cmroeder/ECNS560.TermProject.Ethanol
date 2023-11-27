#libraries
library(readxl)
library(dplyr)
library(dplyr)
#path to file
excel_file <- "Data/Raw/historical-station-counts1.xlsx"

#list of all years because sheets are named after years
sheet_names <- as.character(seq(2007, 2019, by = 1))
#making a function
all_data <- lapply(sheet_names, function(sheet) {
  data <- read_excel(excel_file, sheet = sheet, skip = 1)  # Skip first row, read header row as column names
  data <- data[!is.na(data[[1]]), ]  # Remove rows where the first column is NA
  data <- data[!is.na(data$E85), ]   # Remove rows where 'E85' column is NA
  data$year <- sheet  # Add 'year' column with sheet name
  return(data)
})
# Vertically concatenate all data frames into one
combined_data <- bind_rows(all_data)

#making function between 2020 and 2022 because data structure differs a little bit
sheet_names1 <- as.character(seq(2020, 2022, by = 1))
all_data1 <- lapply(sheet_names1, function(sheet) {
  data1 <- read_excel(excel_file, sheet = sheet, skip = 1)  # Skip first row, read header row as column names
  data1 <- data1[!is.na(data1[[1]]), ]  # Remove rows where the first column is NA
  data1 <- data1[!is.na(data1$E85), ]   # Remove rows where 'E85' column is NA
  data1$year <- sheet  # Add 'year' column with sheet name
  return(data1)
})
# Vertically concatenate all data frames into one
combined_data1 <- bind_rows(all_data1)
#selecting only necessary columns
# View the combined data
years_2009_2019=combined_data |>
  select(State,year,E85,Total)
years_2020_2022=combined_data1 |>
  select(State, year, E85, Totald)
#renaming columns
colnames(years_2009_2019) <- c("state", "year", "e85", "total")
colnames(years_2020_2022) <- c("state", "year", "e85", "total")
#concating vertically
e85_df <- rbind(years_2009_2019, years_2020_2022)
#saving the df
e85_df$ratio_e85=e85_df$e85/e85_df$total*100
write.csv(e85_df, "Data/Cleaning/e_85.csv")
