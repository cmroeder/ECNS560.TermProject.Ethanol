library(ggplot2)
library(maps)
library(stringr)
library(tidyr)
library(dplyr)
#reading data
merge_final=read.csv("Data/Merging/merge_final.csv")
#missing values
missing_table <- data.frame(
  Column = names(merge_final),
  Missing_Values = sapply(merge_final, function(x) sum(is.na(x)))
)
print(missing_table)
#we have 230 missing values for eth.production and 331 missing values for 
#corn production and corn prices
#ETH PRODUCTION MISSING
missing_eth <- merge_final[is.na(merge_final$eth.production), ]
#creating flag variable
merge_final$missing_eth <- ifelse(is.na(merge_final$eth.production), 1, 0)
#if all values are 0 impute 0
merge_final=merge_final |>
  arrange(state, year)
nonmissing_eth=merge_final[!is.na(merge_final$eth.production), ]
zero_production_states <- nonmissing_eth %>%
  group_by(state) %>%
  filter(all(eth.production == 0)) %>%
  pull(state) %>%
  unique()
merge_final$eth.production[merge_final$state %in% zero_production_states & is.na(merge_final$eth.production)] = 0
#function imputing moving average until no missing values
custom_impute <- function(series, year_col, state_col, k = 1) {
  for (i in which(is.na(series))) {
    # Find the nearest non-missing values within the same state
    neighbors <- merge_final %>% 
      filter(!is.na(series), state == merge_final$state[i]) %>%
      arrange(abs(merge_final$year[i] - year)) %>%
      head(k)
    
    # Impute missing value with the mean of nearest neighbors
    if (nrow(neighbors) > 0) {
      series[i] <- mean(neighbors$eth.production)
    }
  }
  return(series)
}
#imputing moving average until no missing values
while (sum(is.na(merge_final$eth.production)) > 0) {
  merge_final$eth.production <- custom_impute(merge_final$eth.production, merge_final$year, merge_final$state, k = 3)
}
#CORN PRODUCTION
missing_corn_prod <- merge_final[is.na(merge_final$corn.production), ]
#flag column
merge_final$missing_corn_prod <- ifelse(is.na(merge_final$corn.production), 1, 0)
#if all NAs impute 0 "Alaska" "Connecticut" "District of Columbia" "Hawaii"               "Maine"                "Massachusetts"       
#"Nevada"               "New Hampshire"        "Rhode Island"        "Vermont"         
na_corn_production_states <- merge_final %>%
  group_by(state) %>%
  filter(all(is.na(corn.production))) %>%
  pull(state) %>%
  unique()
merge_final$corn.production[merge_final$state %in% na_corn_production_states & is.na(merge_final$corn.production)] = 0
custom_impute1 <- function(series, year_col, state_col, k = 1) {
  for (i in which(is.na(series))) {
    # Find the nearest non-missing values within the same state
    neighbors <- merge_final %>% 
      filter(!is.na(series), state == merge_final$state[i]) %>%
      arrange(abs(merge_final$year[i] - year)) %>%
      head(k)
    
    # Impute missing value with the mean of nearest neighbors
    if (nrow(neighbors) > 0) {
      series[i] <- mean(neighbors$corn.production)
    }
  }
  return(series)
}
while (sum(is.na(merge_final$corn.production)) > 0) {
  merge_final$corn.production <- custom_impute1(merge_final$corn.production, merge_final$year, merge_final$state, k = 3)
}

##CORN PRICES - similar to corn prices
missing_corn_prices <- merge_final[is.na(merge_final$corn.prices), ]
#flag column
merge_final$missing_corn_prices <- ifelse(is.na(merge_final$corn.prices), 1, 0)
#national average for 0s
merge_final = merge_final |> 
  mutate_at(vars(corn.prices), ~replace_na(., mean(corn.prices, na.rm = TRUE)))
#function for moving average
custom_impute3 <- function(series, year_col, state_col, k = 1) {
  for (i in which(is.na(series))) {
    # Find the nearest non-missing values within the same state
    neighbors <- merge_final %>% 
      filter(!is.na(series), state == merge_final$state[i]) %>%
      arrange(abs(merge_final$year[i] - year)) %>%
      head(k)
    
    # Impute missing value with the mean of nearest neighbors
    if (nrow(neighbors) > 0) {
      series[i] <- mean(neighbors$corn.prices)
    }
  }
  return(series)
}
while (sum(is.na(merge_final$corn.prices)) > 0) {
  merge_final$corn.prices <- custom_impute3(merge_final$corn.prices, merge_final$year, merge_final$state, k = 3)
}

write.csv(merge_final, "Data/Merging/merge_final_final.csv")