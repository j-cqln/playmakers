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

skaters_data <- skaters_data %>%
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
         rank_change = assists_rank - playmaking_rank)

# Save data
write.csv(ranks, "data/ranks.csv", row.names = FALSE)
