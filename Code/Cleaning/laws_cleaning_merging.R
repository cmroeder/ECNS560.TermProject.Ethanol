#libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(naniar)
library(dplyr)

file_path = "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/laws_and_incentives (Oct 24 2023).csv"
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
write.csv(regulations_laws_final, "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/regulations_laws_final.csv")

##trial merge
#abbr
file_path <- "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/state_names.csv"
state_names <- read_csv(file_path)
#merging abbr and laws
names(regulations_laws_final)[names(regulations_laws_final) == 'State'] <- 'Alpha code'
regulations_laws_merged=merge(x = regulations_laws_final, y = state_names, by = "Alpha code", all.x = TRUE)
regulations_laws_merged=regulations_laws_merged|>
  select(State, `Alpha code`, Year, enacted, incentives, tax_incentives, grant_incentives, 
         other_incentives, regulations)
names(regulations_laws_merged)[names(regulations_laws_merged) == 'State'] <- 'state'
names(regulations_laws_merged)[names(regulations_laws_merged) == 'Year'] <- 'year'
#merge e85 and regulations


e_85_merged=merge(x = e85_df, y = regulations_laws_merged, by = c("state", "year"), all.x = TRUE)
names(state_names)[names(state_names) == 'State'] <- 'state'
e_85_merged=merge(x = e_85_merged, y = state_names, by = c("state"), all.x = TRUE)
e_85_merged=e_85_merged|>
  select(state, year, e85, total, ratio_e85, enacted, incentives, tax_incentives, grant_incentives, 
         other_incentives, regulations, `Alpha code.y`)
sum(e_85_merged$enacted)
#filling NAs with0
e_85_merged <- e_85_merged %>%
  mutate(enacted = ifelse(is.na(enacted), 0, enacted),
         incentives = ifelse(is.na(incentives), 0, incentives),
         tax_incentives = ifelse(is.na(tax_incentives), 0, tax_incentives),
         grant_incentives = ifelse(is.na(grant_incentives), 0, grant_incentives),
         other_incentives = ifelse(is.na(other_incentives), 0, other_incentives),
         regulations = ifelse(is.na(regulations), 0, regulations))
#visualizations?
# Load the ggplot2 package (if not already loaded)
library(ggplot2)
# Assuming df is your data frame
ggplot(data = subset(e_85_merged, state == "Total"), aes(x = year, y = e85)) +
  geom_point() +
  geom_line(aes(group = 1), color = "blue") +
  labs(x = "Year", y = "Number of E85 stations",
       title = "Trend of number of E85 stations across all states",
       caption = "Source: Your Data Source") +
  theme_minimal()
#trying to do top 5
sorted_df <- e_85_merged[order(e_85_merged$e85, decreasing = TRUE), ]

# Get top 5 states with the highest e85 values
top_states <- head(unique(sorted_df$state), 6)
top_states<- top_states[-1]

# Get bottom 5 states with the lowest e85 values
bottom_states <- tail(unique(sorted_df$state), 5)

# Subset data for top and bottom states
top_df <- subset(sorted_df, state %in% top_states)
bottom_df <- subset(sorted_df, state %in% bottom_states)

# Plotting the lines for top 5 and bottom 5 states
# Plotting individual dots for each state
ggplot(data = top_df, aes(x = year, y = e85, color = state, group=state)) +
  geom_point(size = 1) +
  geom_line(size = 1) + 
  labs(x = "Year", y = "e85",
       title = "e85 Trends for Each State (2009-2022)",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue", "orange", "purple", "gray", "pink", "brown", "cyan", "magenta"))

ggplot() +
  geom_line(data = top_df, aes(x = Year, y = e85, color = State, group = 1), size = 1) +
  geom_line(data = bottom_df, aes(x = Year, y = e85, color = State, group = 1), size = 1) +
  labs(x = "Year", y = "e85",
       title = "Trend of e85 for Top 5 and Bottom 5 States",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue", "orange", "purple", "gray", "pink", "brown", "cyan", "magenta"))
#merging with corn 
file_path = "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/clean.ethanolproduction.rds"
merged_corn = readRDS(file_path)
#merging everything with E85
e_85_merged <- e_85_merged %>%
  select(`Alpha code.y`, state, year, e85, total, ratio_e85,
         enacted, incentives,tax_incentives, grant_incentives,
         other_incentives, regulations) %>%  # Rearrange columns A and B
  rename(state_abb=`Alpha code.y`)  # Rename column C to NewColumn

merge_final=merge(x=e_85_merged, y=merged_corn, by=c("state_abb", "year"),all.x = TRUE) 

#states with missing corn production
write.csv(merge_final, "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/clean_merged.rds")