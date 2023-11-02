# Load required libraries
library(ggplot2)
library(maps)
library(stringr)
library(tidyverse)
# Load required libraries
library(ggplot2)
library(maps)
library(stringr)

us_map <- map_data("state")

#reading data
merge_final=read.csv("Data/Merging/merge_final.csv")

# Filter data for 2007
df_2007 <- merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(year == 2007, state != "total") %>%
  select(state, e85)

# Merge with map data
merged_data_2007 <- merge(us_map, df_2007, by.x = "region", by.y = "state", all.x = TRUE)

# Filter data for 2014
df_2014 <- merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(year == 2014, state != "total") %>%
  select(state, e85)

# Merge with map data
merged_data_2014 <- merge(us_map, df_2014, by.x = "region", by.y = "state", all.x = TRUE)

# Filter data for 2021
df_2021 <- merge_final %>%
  mutate(state = str_to_lower(state)) %>%
  filter(year == 2021, state != "total") %>%
  select(state, e85)

# Merge with map data
merged_data_2021 <- merge(us_map, df_2021, by.x = "region", by.y = "state", all.x = TRUE)

# Create map for 2007
map_2007 <- ggplot(merged_data_2007, aes(x = long, y = lat, fill = ifelse(is.na(e85), 0, e85))) +
  geom_map(map = merged_data_2007, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", guide = "legend", limits = c(0, max(df_2021$e85, na.rm = TRUE))) +
  labs(fill = "Number of E85 Stations - 2007") +
  theme_void() +
  coord_fixed(ratio = 1.3)

# Create map for 2014 with a different color scheme
map_2014 <- ggplot(merged_data_2014, aes(x = long, y = lat, fill = ifelse(is.na(e85), 0, e85))) +
  geom_map(map = merged_data_2014, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", guide = "legend", limits = c(0, max(df_2021$e85, na.rm = TRUE))) +
  labs(fill = "Number of E85 Stations - 2014") +
  theme_void() +
  coord_fixed(ratio = 1.3)

# Create map for 2021 with a different color scheme
map_2021 <- ggplot(merged_data_2021, aes(x = long, y = lat, fill = ifelse(is.na(e85), 0, e85))) +
  geom_map(map = merged_data_2021, aes(map_id = region), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", guide = "legend", limits = c(0, max(df_2021$e85, na.rm = TRUE))) +
  labs(fill = "Number of E85 Stations - 2021") +
  theme_void() +
  coord_fixed(ratio = 1.3)

# Combine maps vertically
combined_maps <- cowplot::plot_grid(map_2007, map_2014, map_2021, ncol = 1)
library(cowplot)

# Your code for combining maps
combined_maps <- cowplot::plot_grid(map_2007, map_2014, map_2021, ncol = 1)

# Combined title
combined_title <- ggdraw() +
  draw_label("E85 stations distribution in 2007, 2014, and 2021", x = 0.5, y = 0.98, size = 12)

# Plotting the combined maps with the combined title
combined_maps=cowplot::plot_grid(combined_title, combined_maps, ncol = 1, rel_heights = c(0.1, 1))

ggsave("Outputs/Exploratory_Analysis/e85_overtime_map.png", combined_maps, width = 8, height = 6, units = "in", dpi = 300)
