library(ggplot2)
library(maps)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
merge_final=read.csv("Data/Merging/merge_final_final.csv")
#importing first set of population data
pop_2000=read_excel("Data/Raw/pop_2000_2010.xls")
#selecting necessary rows and columns
pop_2000 <- pop_2000[-c(0,1,2),]
colnames(pop_2000) <- pop_2000[1,]
pop_2000 <- pop_2000[-1,]
pop_2000 <- pop_2000[, -c(2, (ncol(pop_2000) - 1):ncol(pop_2000))]
#renaming columns
colnames(pop_2000)[1] <- "state"
#eliminating . from state names
pop_2000$state <- gsub("\\.", "", pop_2000$state)
#creating pivot longer
cols=colnames(pop_2000)
cols <- cols[cols != "state"]
pop_2000_long <- pop_2000 %>%
  mutate(across(all_of(cols), as.character)) %>%
  pivot_longer(cols = cols, 
               names_to = "year",
               values_to = "population")
pop_2000_long$year <- as.integer(pop_2000_long$year)
#merging with final merge
merge_final=left_join(merge_final, pop_2000_long, by=c("state", "year"))

#POP 2010-2020
pop_2010=read_excel("Data/Raw/pop_2010_2020.xlsx")
#selecting necessary rows and columns
pop_2010 <- pop_2010[-c(0,1,2),]
colnames(pop_2010) <- pop_2010[1,]
pop_2010 <- pop_2010[-1,]
pop_2010 <- pop_2010[, -c(2:3)]
#renaming columns
colnames(pop_2010)[1] <- "state"
#eliminating . from state names
pop_2010$state <- gsub("\\.", "", pop_2010$state)
#creating pivot longer
cols=colnames(pop_2010)
cols <- cols[cols != "state"]
pop_2010_long <- pop_2010 %>%
  mutate(across(all_of(cols), as.character)) %>%
  pivot_longer(cols = cols, 
               names_to = "year",
               values_to = "population")
pop_2010_long$year <- as.integer(pop_2010_long$year)
#merging with final merge
merge_final=left_join(merge_final, pop_2010_long, by=c("state", "year"))

#POP 2020-2022
pop_2020=read_excel("Data/Raw/pop_2020_2022.xlsx")
#selecting necessary rows and columns
pop_2020 <- pop_2020[-c(0,1,2),]
colnames(pop_2020) <- pop_2020[1,]
pop_2020 <- pop_2020[-1,]
pop_2020 <- pop_2020[, -c(2)]
#renaming columns
colnames(pop_2020)[1] <- "state"
#eliminating . from state names
pop_2020$state <- gsub("\\.", "", pop_2020$state)
#creating pivot longer
cols=colnames(pop_2020)
cols <- cols[cols != "state"]
pop_2020_long <- pop_2020 %>%
  mutate(across(all_of(cols), as.character)) %>%
  pivot_longer(cols = cols, 
               names_to = "year",
               values_to = "population")
pop_2020_long$year <- as.integer(pop_2020_long$year)
#merging with final merge
merge_final=left_join(merge_final, pop_2020_long, by=c("state", "year"))
#SELECTING ONLY NECESSARy
merge_final <- merge_final %>%
  mutate(population = coalesce(population, population.x, population.y)) %>%
  select(-population.x, -population.y)
#SAVING
write.csv(merge_final, "Data/Merging/merge_last.csv")