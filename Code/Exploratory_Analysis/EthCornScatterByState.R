#libraray needed packages
library(tidyverse)
library(ggplot2)
library(readr)

#setwd

#import data set
mega = read.csv("Data/Merging/merge_last.csv")

#make plot of ethanol and corn production
#first decide which low ethanol values to drop
summary(mega$eth.production)

#make plot
#create max-min legend order
legend_order = mega |> 
  group_by(state) |> 
  summarize(mean_eth_prod = mean(eth.production, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(mean_eth_prod)) |> 
  pull(state)

mega1 <- mega %>%
  mutate(state = factor(state, levels = legend_order))


scatter = mega1 |>
  mutate(eth.production = eth.production/1000,
         corn.production = corn.production/1000000) |> 
  filter(eth.production > 0) |> 
  ggplot(aes(x = corn.production, y = eth.production, color = state)) +
  geom_point(size = 2) +
  labs(x = "Corn Production (1 million bushels)",
       y = "Ethanol Production (1 million barrels)",
       title = "States Producing Ethanol: Ethanol on Corn Production") +
  scale_color_discrete()
scatter

#save plot as png
ggsave("Outputs/Exploratory_Analysis/EthCornScatterByState_edited.png", scatter, "png")
