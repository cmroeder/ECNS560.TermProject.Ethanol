#library needed packages
library(tidyverse)
library(fixest)
library(skimr)

load("Merging/merge_final.Rdata")

#take a look at missing values - looks like most recent data for corn is missing commonly
#ethanol production data is more sporadic
skim(merge_final)

merge_final1 = merge_final |> 
  filter(is.na(eth.production) | is.na(corn.production) | is.na(corn.prices))

#fill in missing values
merge_final2 = merge_final |> 
  group_by(state)  |>
  arrange(state, year, decreasing = TRUE)|> 
  fill(eth.production, .direction = 'down') |> 
  fill(corn.production, .direction = "down") |> 
  fill(corn.prices, .direction = "down") |> 
  ungroup()

merge_final3 = merge_final2 |> 
  group_by(state)  |>
  arrange(state, year, decreasing = TRUE)|> 
  fill(eth.production, .direction = 'up') |> 
  fill(corn.production, .direction = "up") |> 
  fill(corn.prices, .direction = "up") |> 
  ungroup()

#replace NA values with zero for corn and ethanol production 
#not for corn prices because zero is not an appropriate replacement
merge_final4 <- merge_final3 |> 
  mutate_at(vars(eth.production, corn.production), ~replace_na(., 0))

merge_final5 = merge_final4 |> 
  mutate_at(vars(corn.prices), ~replace_na(., mean(corn.prices, na.rm = TRUE)))

#compare distributions before and after replacing missing values
ggplot(merge_final) +
  geom_density(aes(corn.prices))
ggplot(merge_final5) +
  geom_density(aes(corn.prices))
summary(merge_final$corn.prices)
summary(merge_final5$corn.prices)

ggplot(merge_final) +
  geom_density(aes(corn.production))
ggplot(merge_final5) +
  geom_density(aes(corn.production))
summary(merge_final$corn.production)
summary(merge_final5$corn.production)

ggplot(merge_final) +
  geom_density(aes(eth.production))
ggplot(merge_final5) +
  geom_density(aes(eth.production))
summary(merge_final$eth.production)
summary(merge_final5$eth.production)

save(merge_final5, file = "Merging/merge_final_missingreplaced.RData")
