#library needed packages
library(tidyverse)
library(fixest)
library(skimr)
library(xlsx)

merge_final = read.csv("Data/Merging/merge_last.csv")

skim(merge_final)


#run regression without state and year fixed effects
simple = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices + population, data = merge_final)
summary(simple)

simple_nocontrols = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final)
summary(simple_nocontrols)

#run regression with state and year fixed effects
fe = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices + population| year + state_abb, data = merge_final)
summary(fe)

fe_nocontrols = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final)
summary(fe_nocontrols)



#look at relationship for states with top 10 number of e85 stations
merge_final_e85 = merge_final |> 
  filter(state_abb == "CA" |
           state_abb == "IA" |
           state_abb == "IL" |
           state_abb == "IN" |
           state_abb == "MI" |
           state_abb == "MN" |
           state_abb == "MO" |
           state_abb == "OH" |
           state_abb == "TX" |
           state_abb == "WI")

simple_85 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final_e85)
summary(simple_85)

simple_nocontrols_85 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final_e85)
summary(simple_nocontrols_85)

fe_85 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final_e85)
summary(fe_85)

fe_nocontrols_85 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final_e85)
summary(fe_nocontrols_85)

#look at relationship for states not in top 10 group for number of e85 stations
merge_final_lowe85 = merge_final |> 
  filter(state_abb != "CA" &
           state_abb != "IA" &
           state_abb != "IL" &
           state_abb != "IN" &
           state_abb != "MI" &
           state_abb != "MN" &
           state_abb != "MO" &
           state_abb != "OH" &
           state_abb != "TX" &
           state_abb != "WI")

simple_low85 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final_lowe85)
summary(simple_low85)

simple_nocontrols_low85 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final_lowe85)
summary(simple_nocontrols_low85)

fe_low85 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final_lowe85)
summary(fe_low85)

fe_nocontrols_low85 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final_lowe85)
summary(fe_nocontrols_low85)


#look at relationship for state with top 10 ethanol production
#six of these states are the same as the states with the most e85 stations
mean_eth_prod = merge_final |> 
  group_by(state) |> 
  summarize(mean_eth_prod = mean(eth.production, na.rm = TRUE)) |> 
  ungroup()

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

simple_eth = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final_eth)
summary(simple_eth)

simple_nocontrols_eth = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final_eth)
summary(simple_nocontrols_eth)

fe_eth = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final_eth)
summary(fe_eth)

fe_nocontrols_eth = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final_eth)
summary(fe_nocontrols_eth)


#look at relationship for states not in the top 10 group for ethanol production
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

simple_loweth = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final_loweth)
summary(simple_loweth)

simple_nocontrols_loweth = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final_loweth)
summary(simple_nocontrols_loweth)

fe_loweth = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final_loweth)
summary(fe_loweth)

fe_nocontrols_loweth = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final_loweth)
summary(fe_nocontrols_loweth)


#look at observations which have either incentives or regulations
merge_final1 = merge_final |> 
  filter(incentives > 0 | regulations > 0)

simple1 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final1)
summary(simple1)

simple_nocontrols1 = lm(e85 ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final1)
summary(simple_nocontrols1)

fe1 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final1)
summary(fe1)

fe_nocontrols1 = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final1)
summary(fe_nocontrols1)


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

simple2 = lm(e85 ~ incentives + incentives_lag1 + incentives_lag2 + incentives_lag3 + regulations + regulations_lag1 + regulations_lag2 + regulations_lag3 + total + eth.production + corn.production + corn.prices, data = merge_final2)
summary(simple2)

simple_nocontrols2 = lm(e85 ~ incentives + incentives_lag1 + incentives_lag2 + incentives_lag3 + regulations + regulations_lag1 + regulations_lag2 + regulations_lag3, data = merge_final2)
summary(simple_nocontrols2)

fe2 = feols(e85~incentives + incentives_lag1 + incentives_lag2 + incentives_lag3 + regulations + regulations_lag1 + regulations_lag2 + regulations_lag3 + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final2)
summary(fe2)

fe_nocontrols2 = feols(e85~ incentives + incentives_lag1 + incentives_lag2 + incentives_lag3 + regulations + regulations_lag1 + regulations_lag2 + regulations_lag3 | year + state_abb, data = merge_final2)
summary(fe_nocontrols2)

#nothing really explains the negative relationship between e85 stations and state-level legislation

#create variable which describes the change in E85 stations year on year
merge_final_flow <- merge_final |> 
  group_by(state)  |>
  arrange(state, year, decreasing = TRUE)|>
  mutate(e85_flow = c(NA, diff(e85))) |> 
  ungroup()

simple_flow = lm(e85_flow ~ tax_incentives + grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices, data = merge_final_flow)
summary(simple_flow)

simple_nocontrols_flow = lm(e85_flow ~ tax_incentives + grant_incentives + other_incentives + regulations, data = merge_final_flow)
summary(simple_nocontrols_flow)

fe_flow = feols(e85_flow~ tax_incentives+ grant_incentives + other_incentives + regulations + total + eth.production + corn.production + corn.prices| year + state_abb, data = merge_final_flow)
summary(fe_flow)

fe_nocontrols_flow = feols(e85~ tax_incentives+ grant_incentives + other_incentives + regulations | year + state_abb, data = merge_final_flow)
summary(fe_nocontrols_flow)
