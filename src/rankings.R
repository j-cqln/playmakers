library(readr)
library(dplyr)

# Read model data and skaters data
model_data <- read.csv("data/model_data.csv")
skaters_data <- read.csv("data/skaters.csv")

model_data <- model_data %>%
  group_by(season, shooter_id) %>%
  select(season, shooter, shooter_id,
         total_xg, avg_xg,
         total_xrebounds, avg_xrebounds,
         total_rebound_value, avg_rebound_value) %>%
  distinct() %>%
  ungroup()

# Make sure no id corresponds with multiple names
model_data$shooter[model_data$shooter == "Chris Tanev"] <- "Christopher Tanev"
model_data$shooter[model_data$shooter == "Jani Hakanp"] <- "Jani Hakanpaa"
model_data$shooter[model_data$shooter == "Alex Kerfoot"] <- "Alexander Kerfoot"
model_data$shooter[model_data$shooter == "Zach Sanford"] <- "Zachary Sanford"
model_data$shooter[model_data$shooter == "Jake Middleton"] <- "Jacob Middleton"
model_data$shooter[model_data$shooter == "Tommy Novak"] <- "Thomas Novak"
model_data$shooter[model_data$shooter == "Max Lajoie"] <- "Maxime Lajoie"
model_data$shooter[model_data$shooter == "Alex Nylander"] <- "Alexander Nylander"
model_data$shooter[model_data$shooter == "Alex Barr-Boulet"] <- "Alex Barre-Boulet"
model_data$shooter[model_data$shooter == "Marin Studenic"] <- "Marian Studenic"
model_data$shooter[model_data$shooter == "Sammy Walker"] <- "Samuel Walker"
model_data$shooter[model_data$shooter == "Alexei Toropchenko"] <- "Alexey Toropchenko"
model_data$shooter[model_data$shooter == "Mitch Marner"] <- "Mitchell Marner"
model_data$shooter[model_data$shooter == "Bo Groulx"] <- "Benoit-Olivier Groulx"
model_data$shooter[model_data$shooter == "Mitch Marner"] <- "Mitchell Marner"
model_data$shooter[model_data$shooter == "Jesse Ylnen"] <- "Jesse Ylonen"
model_data$shooter[model_data$shooter == "Nick Abruzzese"] <- "Nicholas Abruzzese"
model_data$shooter[model_data$shooter == "Alexis Lafrenire"] <- "Alexis Lafreniere"
model_data$shooter[model_data$shooter == "Tim Sttzle"] <- "Tim Stutzle"

skaters_data <- skaters_data %>%
  # Optionally filter out players with low games played, shot attempts, and assists
  filter(games_played > 15 & shot_attempts > 50 & assists > 5) %>%
  mutate(position_group = case_when(
    position %in% c("C", "L", "R") ~ "F",
    position %in% c("D") ~ "D"
  ))

# Join rank and skaters data
ranks <- left_join(skaters_data, model_data,
                   by = c("season", "shooter", "shooter_id"))

# Build ranks
ranks <- ranks %>%
  group_by(season, position_group) %>%
  arrange(desc(total_xg), shot_attempts) %>%
  mutate(total_xg_rank = row_number()) %>%
  arrange(desc(avg_xg), shot_attempts) %>%
  mutate(avg_xg_rank = row_number()) %>%
  arrange(desc(total_xrebounds), shot_attempts) %>%
  mutate(total_xrebounds_rank = row_number()) %>%
  arrange(desc(avg_xrebounds), shot_attempts) %>%
  mutate(avg_xrebounds_rank = row_number()) %>%
  arrange(desc(total_rebound_value), shot_attempts) %>%
  mutate(total_rebound_value_rank = row_number()) %>%
  arrange(desc(avg_rebound_value), shot_attempts) %>%
  mutate(avg_rebound_value_rank = row_number()) %>%
  arrange(desc(assists), shot_attempts) %>%
  mutate(assists_rank = row_number()) %>%
  arrange(desc(primary_assists), shot_attempts) %>%
  mutate(primary_assists_rank = row_number()) %>%
  mutate(playmaking_total_rank = (
    assists_rank + 
      primary_assists_rank + 
      avg_xrebounds_rank + 
      avg_rebound_value_rank)
    ) %>%
  arrange(playmaking_total_rank, shot_attempts) %>%
  mutate(playmaking_rank = row_number(),
         rank_change = assists_rank - playmaking_rank) %>%
  mutate(assists_z_score = (assists - mean(assists)) / sd(assists),
         primary_assists_z_score = (primary_assists - mean(primary_assists)) / sd(primary_assists),
         avg_xrebounds_z_score = (avg_xrebounds - mean(avg_xrebounds, na.rm = TRUE)) / sd(avg_xrebounds, na.rm = TRUE),
         avg_rebound_value_z_score = (avg_rebound_value - mean(avg_rebound_value, na.rm = TRUE)) / sd(avg_rebound_value, na.rm = TRUE),
         playmaking_z_score = (assists_z_score + primary_assists_z_score + avg_xrebounds_z_score + avg_rebound_value_z_score) / 4)
         
# Save data
write.csv(ranks, "data/ranks.csv", row.names = FALSE)
