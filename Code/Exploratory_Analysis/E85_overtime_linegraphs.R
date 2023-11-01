# Load required libraries
library(ggplot2)
library(maps)
library(stringr)
library(tidyverse)
library(gridExtra)

#reading data
merge_final=read.csv("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Data/Merging/merge_final.csv")
top_states <- merge_final %>%
  group_by(state_abb) %>%
  summarise(total_e85_stations = sum(e85, na.rm = TRUE)) %>%
  top_n(10, wt = total_e85_stations) %>%
  pull(state_abb)
# Define custom color palette for the top 15 states
custom_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")
top_states_data <- merge_final %>%
  filter(state_abb %in% top_states) %>%
  mutate(avg_e85_stations = e85) %>%
  ungroup () |>
  select(state_abb, avg_e85_stations, year)
all_states=unique(merge_final$state_abb)
other_states_data=merge_final |>
  filter(!(state_abb %in% top_states)) |> 
  group_by(year) |>
  summarise(avg_e85_stations = mean(e85, na.rm = TRUE)) |>
  mutate(state_abb="Average") |>
  ungroup ()
top_states_data=rbind(top_states_data, other_states_data)
# Create the plot with modified aesthetics and theme
graph1 <- ggplot(top_states_data, aes(x = year, y = avg_e85_stations, group = state_abb, color = state_abb)) +
  geom_line(size = 0.7) +
  labs(title = "Number of E85 Stations Across Years for Top 10 States and Average for remaining states",
       x = "Year",
       y = "Number of E85 Stations",
       color = "State Abbreviation") +
  scale_color_manual(values = c(custom_colors, "grey")) +  # Set custom colors and grey for the average line
  theme_minimal() +
  theme(legend.position = "bottom",            # Position legend at the bottom
        legend.direction = "horizontal",       # Arrange legend items horizontally
        legend.key.width = unit(1.5, "cm"),    # Adjust legend key width
        legend.key.height = unit(0.5, "cm"),   # Adjust legend key height
        legend.box.spacing = unit(0.2, "cm"))  + 
  geom_vline(aes(xintercept = 2015), linetype = "dotted", color = "red")+
  geom_vline(xintercept = 2008, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2010, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2009, linetype = "dotted", color = "red")
# Print the modified plot
print(graph1)

aggregate_df=merge_final %>%
  group_by(year) %>%
  summarise(total_e85_stations = sum(e85, na.rm = TRUE)) %>%
  select(total_e85_stations, year)

graph2 <- ggplot(aggregate_df, aes(x = year, y = total_e85_stations)) +
  geom_point(size = 1) +
  geom_line() +
  labs(title = "Number of E85 Stations Across Years for US (Total)",
       x = "Year",
       y = "Number of E85 Stations") +
  theme_minimal() +
  geom_vline(xintercept = 2015, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2010, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2009, linetype = "dotted", color = "red")

print(graph2)

graph2

# Combine graphs vertically with equal heights
combined_graph <- grid.arrange(graph1, graph2, ncol = 1, heights = c(3, 2))
ggsave("C:/Users/aitku/OneDrive/Рабочий стол/Fall 2023/Advanced_Data_Analysis/GitHub/ECNS560.TermProject.Ethanol/Exploratory_Output/e85_overtime_graph.png", 
       combined_graph, 
       width = 8, 
       height = 10,  # Set the height explicitly to ensure equal heights for both graphs
       units = "in", 
       dpi = 300)
# Print the combined graph
print(combined_graph)