library(ggplot2)
library(maps)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
library(outliers)
#reading final
merge_final=read.csv("Data/Merging/merge_last.csv")
#to keep old data
merge_final$e85_old=merge_final$e85
#visualize
state_data <- merge_final %>% filter(state == "Idaho")
# Create a dot plot
ggplot(state_data, aes(x = year, y = e85)) +
  geom_point() +
  labs(title = "Dot Plot of e85 for Alabama Over Years",
       x = "Year",
       y = "e85")



