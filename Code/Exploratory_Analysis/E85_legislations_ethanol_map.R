# Load required libraries
library(ggplot2)
library(maps)
library(stringr)
library(patchwork)
#reading data
merge_final=read.csv("Data/Merging/merge_last.csv")
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
  select(state, e85, total_laws, eth.production)
df=merge(df, df1, by="state")

# Get the map data for all states, excluding Alaska and Hawaii
us_map <- map_data("state")
# Merge your data frame with map data
merged_data <- merge(us_map, df, by.x = "region", by.y = "state", all.x = TRUE)
#Map1 - E85 - 2021
map_e85 <- ggplot(merged_data, aes(x = long, y = lat, fill = e85)) +
  geom_map(map = merged_data, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", guide = "legend", limits = c(0, max(merged_data$e85, na.rm = TRUE))) +
  labs(fill = "E85 stations (2021)") +
  ggtitle("Number of E85 Stations - 2021") +
  theme_void()+
  theme(plot.title = element_text(size = 10, margin = margin(5, 0, 0, 0))) +
  coord_fixed(ratio = 1.3)
map_e85
#Map 2
map_laws= ggplot(merged_data, aes(x = long, y = lat, fill = total_laws_enacted)) +
  geom_map(map = merged_data, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "white", guide = "legend", limits = c(0, max(merged_data$total_laws_enacted, na.rm = TRUE))) +
  labs(fill = "Total laws enacted (2021)") +
  ggtitle("Number of total laws enacted - 2021") +
  theme_void()+
  theme(plot.title = element_text(size = 10, margin = margin(5, 0, 0, 0))) +
  coord_fixed(ratio = 1.3)
map_laws
#Map 3
map_ethanol= ggplot(merged_data, aes(x = long, y = lat, fill = eth.production)) +
  geom_map(map = merged_data, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "white", high = "purple", na.value = "white", guide = "legend", limits = c(0, max(merged_data$eth.production, na.rm = TRUE))) +
  labs(fill = "Total ethanol production (2021)") +
  ggtitle("Total ethanol production (thousands of barrels) - 2021") +
  theme_void()+
  theme(plot.title = element_text(size = 10, margin = margin(5, 0, 0, 0))) +
  coord_fixed(ratio = 1.3)
map_ethanol
#combining maps
library(cowplot)
combined_maps <- plot_grid(
  map_e85 + theme(legend.position = "bottom"),
  map_laws + theme(legend.position = "bottom"),
  map_ethanol + theme(legend.position = "bottom"),
  nrow = 1,
  align = "h",
  axis = "b"
) + theme(legend.position = "bottom")
#saving
ggsave("Outputs/Exploratory_Analysis/e85_legislations_ethanol_map.png", combined_maps, width = 8, height = 6, units = "in", dpi = 300)