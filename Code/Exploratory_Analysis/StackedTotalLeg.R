#libraray needed packages
library(tidyverse)
library(ggplot2)
library(readr)

#setwd

#import data set
mega = read.csv("Data/Merging/merge_last.csv")

#make mega data into long format
table(mega$state)

US = mega |> 
  filter(state == "Total")

#looks like united states totals have not been calculated - let's do that
mega_sort = mega |> 
  filter(year !=2022) |> 
  dplyr::select(year, state_abb, enacted, incentives, tax_incentives, grant_incentives, other_incentives, regulations, eth.production, corn.production)

#columns to be summed
cols = c("enacted", "incentives", "tax_incentives", "grant_incentives", "other_incentives", "regulations", "eth.production", "corn.production")

#data frame containing only totals from the columns
totals = mega_sort |> 
  group_by(year) |> 
  summarise(across(cols, sum, na.rm = TRUE))

#make data frame with state as Total and years to prepare totals data frame for merging
state = data.frame(
  state = c(rep("Total", 15)),
  year = c(2007:2021))

total_state = left_join(state, totals, by = "year")

#make totals data long
sorted = total_state |> 
  dplyr::select(year, incentives, tax_incentives, grant_incentives, other_incentives, regulations)

list = c("incentives", "tax_incentives", "grant_incentives", "other_incentives", "regulations")

long <- sorted %>%
  pivot_longer(cols = list, names_to = "type", values_to = "type.count")

#make stacked bar chart of US total regulations of each category
stacked = long |> 
  ggplot(aes(fill=type, y=type.count, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(title = "Count of US Total Active State-Level Legislation by Class",
       x = "Year",
       y = "Count",
       fill = "Legislation Class") +
  scale_fill_manual(values=c("azure3", "paleturquoise2", "cadetblue3","steelblue", "skyblue4"))
stacked

#save plot
ggsave("Outputs/Exploratory_Analysis/StackedTotalLeg_edited.png", stacked, "png")
