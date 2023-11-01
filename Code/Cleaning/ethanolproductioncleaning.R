#library needed packages
library(tidyverse)
library(readxl)
library(skimr)

#import excel spreadsheet
production <- read_excel("C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/EthanolProduction/Prod_dataset.xlsx")

#the msn code ENPRP represents ethanol production - filter for that
production1 = production |> 
  filter(MSN =="ENPRP")

#drop unneeded columns
production2 = production1 |> 
  dplyr::select(-Data_Status, -MSN)

#pivot data longer
production3 = production2 |> 
  pivot_longer(cols = -StateCode, names_to = "year", values_to = "eth.production")

#look at the data using skimr
production3 |> skim(eth.production)

#there are no missing values but lots of states that have zero ethanol production - which is to be expected
#let's make a histogram of ethanol production excluding the zero values
production3 |> 
  filter(eth.production > 0) |> 
  ggplot(aes(eth.production)) +
  geom_histogram()

#it looks like most states do not produce a lot of ethanol but there are some states which do produce a lot of ethanol

#change name of state variable
production4 = production3 |> 
  rename(state_abb = StateCode)

#coerce year to an integer
production5 = production4 |> 
  mutate(year = as.integer(year))

#check summary of values
summary(production5$year)
table(production5$state_abb)
summary(production5$eth.production)

#save data in r format
saveRDS(production5, "C:/Users/cmeta/OneDrive/Documents/GitHub/ECNS561.TermProject.Ethanol/Data/EthanolProduction/clean.ethanolproduction.rds")
