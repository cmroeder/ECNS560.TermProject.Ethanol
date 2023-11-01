#library needed pacakges
library(tidyverse)

#import dirty corn prices
prices = readRDS("C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/CornPrices/dirtycornprices.rds")

#drop unneeded columns
prices1 = prices |> 
  select(state_alpha, year, Value)

#rename variables
prices2 = prices1 |> 
  rename(corn.prices = Value, state_abb = state_alpha)

#convert prices to numeric value
#check for observations which are not numbers
alpha = prices2 |> 
  filter(str_detect(corn.prices, "[:alpha:]")) |> 
  count(corn.prices)

#replace "(NA)" values with "NA"
prices3 = prices2 |> 
  mutate(corn.prices = str_replace(corn.prices, "\\(NA\\)", NA_character_))

#coerce value and year variables to numeric
prices4 = prices3 |> 
  mutate(corn.prices = as.numeric(corn.prices))

#check that state and year combinations are unique
prices4 |> 
  count(state_abb, year) |> 
  filter(n>1)

#check for any miscellaneous categories in state column like OT (other)
table(prices4$state_abb)

#check range of values
summary(prices4$corn.prices)
summary(prices4$year)

#drop 2022 values
prices5 = prices4 |> 
  filter(year != 2022)

#save
saveRDS(prices5,"C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/CornPrices/cleancornprices.rds")