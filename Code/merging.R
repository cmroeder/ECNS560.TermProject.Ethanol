#library needed packages
library(tidyverse)

#load in data sets
cornprices = readRDS("C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/CornPrices/cleancornprices.rds")
cornproduction = readRDS("C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/CornProduction/cleancornproduction.rds")
ethanolproduction = readRDS("C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/EthanolProduction/clean.ethanolproduction.rds")

mega = ethanolproduction |> 
  full_join(y = cornproduction, by = c("year", "state_abb"))

mega = mega |> full_join(y = cornprices, by = c("year", "state_abb"))

#check that there aren't duplicate state and year combinations
mega |> 
  count(state_abb,year) |> 
  filter(n>1)

#save merged data
saveRDS(mega, "C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/Merging/corn.eth.merged.rds")
