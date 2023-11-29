#library needed packages
library(tidyverse)
library(fixest)
library(skimr)
library(broom)
library(knitr)
library(kableExtra)

merge_final = read.csv("Data/Merging/merge_last.csv")

#fixed effects - all states
a = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices + population| year + state_abb, data = merge_final)
summary(a)

#look at relationship for states which are top 10 for ethanol production
merge_final_eth = merge_final |> 
  filter(state_abb == "IA" |
           state_abb == "NE" |
           state_abb == "IL" |
           state_abb == "SD" |
           state_abb == "MN" |
           state_abb == "IN" |
           state_abb == "OH" |
           state_abb == "WI" |
           state_abb == "KS" |
           state_abb == "ND")

b = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices + population| year + state_abb, data = merge_final_eth)
summary(b)


#look at relationship for states not in top 10 group for number of e85 stations
merge_final_loweth = merge_final |> 
  filter(state_abb != "IA" &
           state_abb != "NE" &
           state_abb != "IL" &
           state_abb != "SD" &
           state_abb != "MN" &
           state_abb != "IN" &
           state_abb != "OH" &
           state_abb != "WI" &
           state_abb != "KS" &
           state_abb != "ND")

c = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices + population| year + state_abb, data = merge_final_loweth)
summary(c)

#create lagged variables to account for time needed to build e85 stations
merge_final2 = merge_final |> 
  group_by(state)  |>
  arrange(state, year, decreasing = TRUE)|> 
  mutate(incentives_lag1 = lag(incentives, n = 1),
         incentives_lag2 = lag(incentives, n = 2),
         incentives_lag3 = lag(incentives, n = 3),
         regulations_lag1 = lag(regulations, n = 1),
         regulations_lag2 = lag(regulations, n = 2),
         regulations_lag3 = lag(regulations, n = 3)) |> 
  ungroup()

d = feols(e85~incentives + incentives_lag1 + incentives_lag2 + incentives_lag3 + regulations + regulations_lag1 + regulations_lag2 + regulations_lag3 + total + eth.production + corn.production + corn.prices + population| year + state_abb, data = merge_final2)
summary(d)

#create results table for all but lagged variables
columnlabels = c("All States", "Top 10 Ethanol Production", "Not Top 10 Ethanol Production")

fixest::etable(a, b, c, tex=TRUE, digits="r2",
               fontsize = "small",
               style.tex=fixest::style.tex("base"),
               dict = c("e85" = "Number of E85 Stations", 
                        "tax_incentives" = "Tax Incentives",
                        "grant_incentives" = "Grant Incentives", 
                        "other_incentives" = "Other Incentives", 
                        "regulations" = "Regulations",
                        "total" = "All Fuel Stations", 
                        "eth.production" = "Ethanol Production (thousand barrels)", 
                        "corn.production" = "Corn Production (bushels)",
                        "corn.prices" = "Corn Prices ($)",
                        "population" = "Population"),
               title = "2007-2021 E85 Stations on State Laws",
               drop = c("A", "E", "C", "P"))

#create results table for lagged variables
fixest::etable(d, tex=TRUE, digits="r2",
               fontsize = "small",
               style.tex=fixest::style.tex("base"),
               dict = c("e85" = "Number of E85 Stations", 
                        "incentives" = "Incentives",
                        "incentives_lag1" = "Incentives (Lag 1 Year)", 
                        "incentives_lag2" = "Incentives (Lag 2 Year)", 
                        "incentives_lag3" = "Incentives (Lag 3 Year)",
                        "regulations" = "Regulations", 
                        "regulations_lag1" = "Regulations (Lag 1 Year)", 
                        "regulations_lag2" = "Regulations (Lag 2 Year)", 
                        "regulations_lag3" = "Regulations (Lag 3 Year)",
                        "eth.production" = "Ethanol Production (thousand barrels)",
                        "total" = "All Fuel Stations",
                        "corn.production" = "Corn Production (bushels)",
                        "corn.prices" = "Corn Prices ($)",
                        "population" = "Population"),
               title = "2007-2021 E85 Stations on State Laws with Lagged Law Variables",
               drop = c("A", "E", "C", "P"))
