#libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(naniar)
library(dplyr)

file_path = "Data/Raw/laws_and_incentives (Oct 24 2023).csv"
laws = read.csv(file_path)
#filling missing data with NA
law1 <- laws %>%
  mutate_all(~ifelse(. == "", NA, .))
#counting NAs
sum(is.na(law1$Enacted.Date))
#changing format of date
law1$Status.Date = as.Date(law1$Status.Date, format = "%Y-%m-%d")
law1$status_year=year(law1$Status.Date)
#creating df with missing enacted date
filtered_data <- law1 %>%
  filter(status_year > 2006) %>%
  filter(is.na(Enacted.Date))
#we have 149 missing enacted dates which is a lot to drop
filtered_data$Significant.Update.Date=as.Date(filtered_data$Significant.Update.Date, format = "%Y-%m-%d")
filtered_data$Amended.Date=as.Date(filtered_data$Amended.Date, format = "%Y-%m-%d")
filtered_data2 <- filtered_data %>%
  filter(is.na(Significant.Update.Date)) |>
  filter(is.na(Amended.Date))
#trying to fill with mean length of law
trial1=laws
print(colnames(trial1))
date_related=list("Enacted.Date", "Amended.Date", "Archived.Date", "Repealed.Date", "Status.Date")
trial1 <- laws %>%
  mutate_all(~ifelse(. == "", NA, .))
#changing data type
trial1$Enacted.Date=as.Date(trial1$Enacted.Date, format = "%Y-%m-%d")
trial1$Amended.Date=as.Date(trial1$Amended.Date, format = "%Y-%m-%d")
trial1$Archived.Date=as.Date(trial1$Archived.Date, format = "%Y-%m-%d")
trial1$Repealed.Date=as.Date(trial1$Repealed.Date, format = "%Y-%m-%d")
trial1$Status.Date=as.Date(trial1$Status.Date, format = "%Y-%m-%d")
#creating proxy of start date
trial1 <- trial1 %>%
  mutate(start_date = ifelse(!is.na(Enacted.Date), as.Date(Enacted.Date),
                             ifelse(!is.na(Amended.Date) & !is.na(Significant.Update.Date), 
                                    pmin(as.Date(Amended.Date), as.Date(Significant.Update.Date)), 
                                    ifelse(!is.na(Amended.Date), as.Date(Amended.Date), 
                                           ifelse(!is.na(Significant.Update.Date), as.Date(Significant.Update.Date), NA))
                             )
  )
  )

#creating proxy of end date
trial1 <- trial1 %>%
  mutate(end_date = ifelse(!is.na(Expired.Date), as.Date(Expired.Date),
                           ifelse(!is.na(Status.Date), as.Date(Status.Date), NA)))
# Calculate days_law_active, ignoring NA values
trial1 = trial1 |>
  mutate(days_law_active =end_date-start_date, na.rm=TRUE) |>
  mutate(days_law_active = ifelse(days_law_active < 0, NA, days_law_active))
#it looks like we have clear median and we will assume that missing laws follow median length
hist(trial1$days_law_active, 
     main = "Histogram of days_law_active", 
     xlab = "Days", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black")         
median_length=median(trial1$days_law_active,na.rm=TRUE) #1540 days which is 4 years
#filling start_date
trial1 <- trial1 %>%
  mutate(missing_start_date = ifelse(is.na(start_date), 1, 0)) %>%
  mutate(start_date = ifelse(is.na(start_date), end_date - median_length, start_date))
#now choosing only relevant columns
laws_reg <- trial1 %>%
  select(State, Title, start_date, end_date, Type, Incentive.Categories, Regulation.Categories,
         missing_start_date) %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"),
         end_date = as.Date(end_date, format = "%Y-%m-%d"),
         start_year = year(start_date), 
         start_month = month(start_date), 
         start_day = day(start_date), 
         end_year = year(end_date), 
         end_month = month(end_date), 
         end_day = day(end_date),
         tax_incentive = ifelse(Incentive.Categories == "TAX", 1, 0),
         grant_incentive = ifelse(Incentive.Categories == "GNT", 1, 0),
         other_incentive = ifelse(Incentive.Categories != "GNT" & Incentive.Categories != "TAX", 1, 0),
         total_incentive = ifelse(!is.na(Incentive.Categories), 1, 0),
         total_regulations = ifelse(!is.na(Regulation.Categories), 1, 0)) |>
  mutate(across(c(tax_incentive, grant_incentive, other_incentive, total_incentive, total_regulations),
                ~replace_na(. , 0)))
#check that everything works
laws_reg$sum_incentives <- rowSums(laws_reg[, c("tax_incentive", "grant_incentive", "other_incentive", "total_regulations")])
#6 incentives are both regulations and incentives

#just seeing different types
type_frequencies <- table(trial1$Type)
print(type_frequencies)
#checking if incentives types and reg types have NAs
laws_reg <- laws_reg %>%
  mutate(missing_law_reg = ifelse(is.na(Incentive.Categories) & is.na(Regulation.Categories), 1, 0))
sum(laws_reg$missing_law_reg)


#Now trying to expand
library(tidyr)
#if start date before June this year counts
laws_reg <- laws_reg %>%
  mutate(start_year = ifelse(start_month > 6, start_year + 1, start_year))
#expanding
expand_rows <- function(row) {
  data.frame(
    State = row$State,
    Year = seq(row$start_year, row$end_year),
    Title = row$Title,
    Type = row$Type,
    tax_incentive = row$tax_incentive,
    grant_incentive = row$grant_incentive,
    other_incentive = row$other_incentive,
    total_incentive = row$total_incentive,
    total_regulations = row$total_regulations,
    isFirstYear = ifelse(seq(row$start_year, row$end_year) == row$start_year, 1, 0)
  )
}

# Apply the function to each row and combine the results into one data frame
expanded_laws_reg <- laws_reg %>%
  rowwise() %>%
  do(expand_rows(.)) %>%
  ungroup()

#Final df
regulations_laws_final <- expanded_laws_reg %>%
  select(State, Year, tax_incentive, grant_incentive, other_incentive, 
         total_incentive, total_regulations, isFirstYear) |>
  group_by(State, Year) %>%
  mutate(enacted = sum(isFirstYear),
         incentives = sum(total_incentive),
         tax_incentives = sum(tax_incentive),
         grant_incentives = sum(grant_incentive),
         other_incentives = sum(other_incentive),
         regulations = sum(total_regulations)) |>
  select(State, Year, enacted, incentives, tax_incentives, grant_incentives, 
         other_incentives, regulations) |>
  distinct()
write.csv(regulations_laws_final, "Data/Cleaning/regulations_laws_final.csv")
