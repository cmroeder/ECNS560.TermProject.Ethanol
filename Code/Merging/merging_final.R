#merging everything
library(dplyr)
file_path <- "Data/Cleaning/state_names.csv"
state_names <- read.csv(file_path)
regulations_laws_final=read.csv("Data/Cleaning/regulations_laws_final.csv")
#merging abbr and laws
names(regulations_laws_final)[names(regulations_laws_final) == 'State'] <- 'Alpha.code'
regulations_laws_merged=merge(x = regulations_laws_final, y = state_names, by = "Alpha.code", all.x = TRUE)
regulations_laws_merged=regulations_laws_merged|>
  select(State, `Alpha.code`, Year, enacted, incentives, tax_incentives, grant_incentives, 
         other_incentives, regulations)
names(regulations_laws_merged)[names(regulations_laws_merged) == 'State'] <- 'state'
names(regulations_laws_merged)[names(regulations_laws_merged) == 'Year'] <- 'year'
#merge e85 and regulations
e85_df=read.csv("Data/Cleaning/e_85.csv")
e_85_merged=merge(x = e85_df, y = regulations_laws_merged, by = c("state", "year"), all.x = TRUE)
names(state_names)[names(state_names) == 'State'] <- 'state'
e_85_merged=merge(x = e_85_merged, y = state_names, by = c("state"), all.x = TRUE)
e_85_merged=e_85_merged|>
  select(state, year, e85, total, ratio_e85, enacted, incentives, tax_incentives, grant_incentives, 
         other_incentives, regulations, `Alpha.code.y`)
sum(e_85_merged$enacted)
#filling NAs with0
e_85_merged <- e_85_merged %>%
  mutate(enacted = ifelse(is.na(enacted), 0, enacted),
         incentives = ifelse(is.na(incentives), 0, incentives),
         tax_incentives = ifelse(is.na(tax_incentives), 0, tax_incentives),
         grant_incentives = ifelse(is.na(grant_incentives), 0, grant_incentives),
         other_incentives = ifelse(is.na(other_incentives), 0, other_incentives),
         regulations = ifelse(is.na(regulations), 0, regulations))
e_85_merged <- subset(e_85_merged, !is.na(Alpha.code.y))
#merging with corn 
file_path = "Data/Merging/merged.eth.corn.rds"
merged_corn = readRDS(file_path)
#merging everything with E85
e_85_merged <- e_85_merged %>%
  select(`Alpha.code.y`, state, year, e85, total, ratio_e85,
         enacted, incentives,tax_incentives, grant_incentives,
         other_incentives, regulations) %>%  # Rearrange columns A and B
  rename(state_abb=`Alpha.code.y`)  # Rename column C to NewColumn

merge_final_copy=merge(x=e_85_merged, y=merged_corn, by=c("state_abb", "year"),all.x = TRUE) 
#looking at NAs
missing_table <- data.frame(
  Column = names(merge_final_copy),
  Missing_Values = sapply(merge_final_copy, function(x) sum(is.na(x)))
)
print(missing_table)
#states with missing corn production
write.csv(merge_final_copy, "Data/Merging/merge_final_copy.csv")