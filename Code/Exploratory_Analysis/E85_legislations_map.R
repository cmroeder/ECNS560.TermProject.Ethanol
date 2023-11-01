# Load required libraries
library(ggplot2)
library(maps)
library(stringr)
#reading data
write.csv(merge_final, "C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory/merge_final.csv") 
marge_final=read.csv("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory/merge_final.csv")
#creating df with total enacted laws
merge_final$total_laws=merge_final$incentives+merge_final$regulations
df1=merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(state != "total") %>%
  group_by(state) %>%
  summarise(total_laws_enacted = sum(enacted)) %>%
  ungroup()

#selecting necessary columns
df <- merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(year == 2021, state != "total") %>%
  select(state, e85, total_laws)

df=merge(df, df1, by="state")

# Get the map data for all states, excluding Alaska and Hawaii
us_map <- map_data("state")
# Merge your data frame with map data
merged_data <- merge(us_map, df, by.x = "region", by.y = "state", all.x = TRUE)

#getting data for each state center to put dots there
state_centers=read_csv("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory/state_centers.csv")
state_centers = state_centers|>
  mutate(state=str_to_lower(state))
#merging data to get enacted laws, e85 and locations together
merged_data1 <- merge(us_map, df, by.x = "region", by.y = "state", all.x = TRUE)
dot_data<- merge(df, state_centers, by = "state")
dot_data=dot_data|>
  filter(state!="alaska") |>
  filter(state!="hawaii")
'''
#ONLY MAP
# Your existing map plotting code
map_plot <-ggplot(merged_data1, aes(x = long, y = lat, group = group, fill = ifelse(is.na(e85), 0, e85))) +
  geom_map(map = merged_data1, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = "white", guide = "legend") +
  labs(fill = "Number of E85 Stations") +
  theme_void()+
  coord_fixed(ratio = 1.3) 
map_plot
ggsave("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/Project/data/map_plot.png", map_plot, width = 12, height = 10, units = "in", dpi = 300)
'''

#MAP with dots 
map_plot2=ggplot() +
  geom_map(data = merged_data1, aes(map_id = region, x = long, y = lat, fill = ifelse(is.na(e85), 0, e85)), map = merged_data1, color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "white", guide = "legend") +
  geom_point(data = filter(dot_data, total_laws_enacted > 0), aes(x = long, y = lat, size = total_laws_enacted * 2), color = "grey", alpha = 0.7) +
  geom_text(data = filter(dot_data, total_laws_enacted > 1), aes(x = long, y = lat, label = total_laws_enacted), size = 3, color = "black") +
  geom_text(data = filter(dot_data, total_laws_enacted == 0), aes(x = long, y = lat, label = "0"), size = 3, color = "black") +
  geom_text(data = filter(dot_data, total_laws_enacted == 1), aes(x = long, y = lat, label = "1"), size = 3, color = "black", nudge_x = 0.7) +
  labs(fill = "Number of E85 Stations", size = "Number of Regulations") +
  ggtitle("E85 stations (2021) and enacted state-level legislations (2007-2021)") +
  theme_void()+
  theme(plot.title = element_text(size = 12, margin = margin(10, 0, 20, 0))) +
  coord_fixed(ratio = 1.3)
map_plot2
ggsave("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory_Output/e85_legislations_map.png", map_plot2, width = 8, height = 6, units = "in", dpi = 300)

#east cost states
east_states<- c("connecticut", "delaware", "maine", "maryland", 
                "massachusetts", "new hampshire", "new jersey", "rhode island")
#df_east
east_merged <- merged_data1 %>% 
  filter(region %in% east_states)
#dots
dots_east=dot_data %>% 
  filter(state %in% east_states)
map_plot3=ggplot() +
  geom_map(data = east_merged, aes(map_id = region, x = long, y = lat, fill = ifelse(is.na(e85), 0, e85)), map = east_merged, color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "white", guide = "legend") +
  geom_point(data = filter(dots_east, total_laws_enacted > 0), aes(x = long, y = lat, size = total_laws_enacted * 2), color = "grey", alpha = 0.7) +
  geom_text(data = filter(dots_east, total_laws_enacted > 1), aes(x = long, y = lat, label = total_laws_enacted), size = 3, color = "black") +
  geom_text(data = filter(dots_east, total_laws_enacted == 0), aes(x = long, y = lat, label = "0"), size = 3, color = "black") +
  geom_text(data = filter(dots_east, total_laws_enacted == 1), aes(x = long, y = lat, label = "1"), size = 3, color = "black", nudge_x = 0.7) +
  labs(fill = "Number of E85 Stations", size = "Number of Regulations") +
  ggtitle("E85 stations (2021) and enacted state-level legislations (2007-2021) East Coast") +
  theme_void() +
  theme(plot.title = element_text(size = 10, margin = margin(10, 0, 20, 0))) +
  coord_fixed(ratio = 1.3)
map_plot3
ggsave("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory_Output/e85_legislations_map_east_coast.png", map_plot3, width = 8, height = 6, units = "in", dpi = 300)

