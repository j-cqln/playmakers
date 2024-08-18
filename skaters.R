library(readr)
library(dplyr)

# Load skaters data for each season
skaters_files <- c("data/skaters_2021.csv",
                   "data/skaters_2022.csv",
                   "data/skaters_2023.csv")

skaters_data <- NULL

for (i in 1:length(skaters_files)) {
  # Load data
  d <- read.csv(skaters_files[i])
  
  if (is.null(skaters_data)) {
    skaters_data <- d
  } else {
    skaters_data <- rbind(skaters_data, d)
  }
  
  rm(d)
}

# Process skaters data
skaters_data <- skaters_data %>%
  filter(situation == "all") %>%
  rename(points = I_F_points,
         goals = I_F_goals,
         primary_assists = I_F_primaryAssists,
         secondary_assists = I_F_secondaryAssists,
         shooter_id = playerId,
         shooter = name,
         ice_time = icetime,
         shot_attempts = I_F_shotAttempts,
         shots_on_goal = I_F_shotsOnGoal,
         blocked_shot_attempts = I_F_blockedShotAttempts,
         missed_shots = I_F_missedShots) %>%
  group_by(season) %>%
  mutate(assists = primary_assists + secondary_assists) %>%
  ungroup() %>%
  select(season, shooter, shooter_id, position,
         games_played, ice_time, shifts,
         points, goals, assists,
         primary_assists, secondary_assists,
         shot_attempts, shots_on_goal, blocked_shot_attempts, missed_shots) %>%
  filter(shooter_id != "" & position != "")


# Save data
write.csv(skaters_data, "data/skaters.csv", row.names = FALSE)