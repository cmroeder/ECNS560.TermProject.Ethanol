#libraray needed packages
library(tidyverse)
library(ggplot2)
library(readr)

#setwd

#import data set
mega = read.csv("Data/Merging/merge_final.csv")

#make plot of ethanol and corn production
#first decide which low ethanol values to drop
summary(mega$eth.production)

#make plot
scatter = mega |>
  mutate(eth.production = eth.production/1000,
         corn.production = corn.production/1000000) |> 
  filter(eth.production > 0) |> 
  ggplot(aes(x = corn.production, y = eth.production, color = state)) +
  geom_point(size = 2) +
  labs(x = "Corn Production (1 million bushels)",
       y = "Ethanol Production (1 million barrels)",
       title = "States Producing Ethanol: Ethanol on Corn Production")
scatter

#save plot as png
ggsave("Exploratory_Output/EthCornScatterByState.png", scatter, "png")
