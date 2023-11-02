#library needed pacakges
library(tidyverse)
library(stringr)
library(skimr)

#import dirty corn prices
production = readRDS("Data/Cleaning/dirtycornproduction.rds")

#select only year values not forecasted values
production1 = production |> 
  filter(reference_period_desc == "YEAR")

#drop unneeded columns
production2 = production1 |> 
  dplyr::select(state_alpha, year, Value)

#rename variables
production3 = production2 |> 
  rename(corn.production = Value, state_abb = state_alpha)

#convert production to numeric value
#check for observations which are not numbers
alpha = production3 |> 
  filter(str_detect(corn.production, "[:alpha:]"))

#remove commas
production4 = production3 |> 
  mutate(corn.production = str_replace_all(corn.production, ",", ""))

#coerce value and year variables to numeric
production5 = production4 |> 
  mutate(corn.production = as.numeric(corn.production))

#check that state and year combinations are unique
production5 |> 
  count(state_abb, year) |> 
  filter(n>1)

#drop OT (other) state_abb values
table(production5$state_abb)

production6 = production5 |> 
  filter(state_abb != "OT")

#check for missing and extreme values
skim(production6$corn.production)

#check range of values
summary(production6$corn.production)
summary(production7$year)

#drop 2022 and 2023 values
production7 = production6 |> 
  filter(year != 2022, year != 2023)

#save
saveRDS(production7,"Data/Cleaning/cleancornproduction.rds")
