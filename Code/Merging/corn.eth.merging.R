#library needed packages
library(tidyverse)

#load in data sets
cornprices = readRDS("Data/Cleaning/cleancornprices.rds")
cornproduction = readRDS("Data/Cleaning/cleancornproduction.rds")
ethanolproduction = readRDS("Data/Cleaning/clean.ethanolproduction.rds")

mega = ethanolproduction |> 
  full_join(y = cornproduction, by = c("year", "state_abb"))

mega = mega |> full_join(y = cornprices, by = c("year", "state_abb"))

#check that there aren't duplicate state and year combinations
mega |> 
  count(state_abb,year) |> 
  filter(n>1)

#save merged data
saveRDS(mega, "C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/Merging/merged.eth.corn.rds")
