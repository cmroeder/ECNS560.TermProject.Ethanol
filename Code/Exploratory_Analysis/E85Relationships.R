#libraray needed packages
library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)

#setwd

#import data set
mega = read.csv("Data/Merging/merge_final.csv")

mega_sum = mega |> 
  mutate(total_leg = incentives + tax_incentives + grant_incentives + other_incentives + regulations)

legbar = mega_sum |> 
  filter(state != "Total") |> 
  ggplot(aes(y = total_leg, x = year)) +
  geom_bar(stat = "identity", fill = "cadetblue3") + 
  labs(x = "Year", y = "Count of Active Legislation")
legbar

e85bar = mega_sum |> 
  filter(state != "Total") |> 
  ggplot(aes(y = e85, x = year)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "Year", y = "Count of E85 Gas Stations")
e85bar

cornprodbar = mega_sum |> 
  filter(state != "Total") |> 
  mutate(corn.production = corn.production/1000000000) |> 
  ggplot(aes(y = corn.production, x = year)) +
  geom_bar(stat = "identity", fill = "cadetblue3") +
  labs(x = "Year", y = "Corn Production (1 billion bushels)")
cornprodbar

ethprodbar = mega_sum |> 
  filter(state != "Total") |> 
  mutate(eth.production = eth.production/1000) |> 
  ggplot(aes(y = eth.production, x = year)) +
  geom_bar(stat = "identity", fill = "cadetblue3") +
  labs(x = "Year", y = "Ethanol Production (1 million barrels)")
ethprodbar

#use patchwork to compile barcharts into one plot
drivers = ((e85bar|legbar)/(cornprodbar|ethprodbar)) +
  plot_annotation(tag_levels = "A", title = "Visualizing Relationship between Count of E85 Stations and Variables of Interest")
drivers

#save plot
ggsave("Outputs/Exploratory_Analysis/E85Relationships.png", drivers, "png")
