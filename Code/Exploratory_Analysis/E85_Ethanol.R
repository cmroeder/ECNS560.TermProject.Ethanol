# Load required libraries
library(ggplot2)
library(maps)
library(stringr)

#reading data
merge_final=read.csv("Data/Merging/merge_final.csv")
#creating df with total e85 and ethanol production
df <- merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(year == 2021, state != "total") %>%
  select(state, e85, eth.production)

# Get the map data for all states, excluding Alaska and Hawaii
us_map <- map_data("state")
# Merge your data frame with map data
merged_data <- merge(us_map, df, by.x = "region", by.y = "state", all.x = TRUE)

#getting data for each state center to put dots there
state_centers=read_csv("Data/Cleaning/state_centers.csv")
state_centers = state_centers|>
  mutate(state=str_to_lower(state))
#merging data to get enacted laws, e85 and locations together
merged_data1 <- merge(us_map, df, by.x = "region", by.y = "state", all.x = TRUE)
dot_data<- merge(df, state_centers, by = "state")
dot_data=dot_data|>
  filter(state!="alaska") |>
  filter(state!="hawaii")


#MAP with dots 
map_plot2 <- ggplot() +
  geom_map(data = merged_data1, aes(map_id = region, x = long, y = lat, fill = eth.production), 
           map = merged_data1, color = "black") +
  scale_fill_gradient(low = "white", high = "purple", na.value = "lightgrey", guide = "legend") +
  geom_point(data = dot_data, aes(x = long, y = lat, size = e85), color = "cadetblue3", alpha = 0.7) +
  geom_text(data = dot_data, aes(x = long, y = lat, label = e85), size = 3, color = "black") +
  labs(fill = bquote("Ethanol production (k barrels)\n(grey for NA)"), size = "Number of E85 Stations") +
  ggtitle("E85 stations and Ethanol production (2021)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, margin = margin(10, 0, 0, 0)),
    legend.text = element_text(size = 8)  # Adjust the legend text size here
  ) +
  coord_fixed(ratio = 1)

map_plot2
ggsave("Output/Exploratory_Analysis/ethanol_production_e85.png", map_plot2, width = 12, height = 10, units = "in", dpi = 300)

#east cost states
east_states<- c("connecticut", "delaware", "maine", "maryland", 
                "massachusetts", "new hampshire", "new jersey", "rhode island")
#df_east
east_merged <- merged_data1 %>% 
  filter(region %in% east_states)
#dots
dots_east=dot_data %>% 
  filter(state %in% east_states)

map_plot3 <- ggplot() +
  geom_map(data = east_merged, aes(map_id = region, x = long, y = lat, fill = eth.production), 
           map = east_merged, color = "black") +
  scale_fill_gradient(low = "white", high = "purple", na.value = "lightgrey", guide = "legend") +
  geom_point(data = dots_east, aes(x = long, y = lat, size = e85), color = "cadetblue3", alpha = 0.7) +
  geom_text(data = dots_east, aes(x = long, y = lat, label = e85), size = 3, color = "black") +
  labs(fill = bquote("Ethanol production (k barrels)\n(grey for NA)"), size = "Number of E85 Stations") +
  ggtitle("E85 stations and Ethanol production (2021)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, margin = margin(10, 0, 35, 0)),
    legend.text = element_text(size = 8)  # Adjust the legend text size here
  ) +
  coord_fixed(ratio = 1.3)
map_plot3
ggsave("Output/Exploratory_Analysis/e85_legislations_map_east_coast.png", map_plot3, width = 8, height = 6, units = "in", dpi = 300)

