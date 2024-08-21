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

# Find shooter_id with multiple names associated, display the names
skaters_data %>%
  group_by(shooter_id) %>%
  summarise(n = n_distinct(shooter)) %>%
  filter(n > 1) %>%
  select(shooter_id) %>%
  left_join(skaters_data, by = "shooter_id") %>%
  select(shooter_id, shooter) %>%
  distinct()

# Make sure no id corresponds with multiple names
skaters_data$shooter[skaters_data$shooter == "Chris Tanev"] <- "Christopher Tanev"
skaters_data$shooter[skaters_data$shooter == "Jani Hakanp"] <- "Jani Hakanpaa"
skaters_data$shooter[skaters_data$shooter == "Alex Kerfoot"] <- "Alexander Kerfoot"
skaters_data$shooter[skaters_data$shooter == "Zach Sanford"] <- "Zachary Sanford"
skaters_data$shooter[skaters_data$shooter == "Jake Middleton"] <- "Jacob Middleton"
skaters_data$shooter[skaters_data$shooter == "Tommy Novak"] <- "Thomas Novak"
skaters_data$shooter[skaters_data$shooter == "Max Lajoie"] <- "Maxime Lajoie"
skaters_data$shooter[skaters_data$shooter == "Alex Nylander"] <- "Alexander Nylander"
skaters_data$shooter[skaters_data$shooter == "Alex Barr-Boulet"] <- "Alex Barre-Boulet"
skaters_data$shooter[skaters_data$shooter == "Marin Studenic"] <- "Marian Studenic"
skaters_data$shooter[skaters_data$shooter == "Sammy Walker"] <- "Samuel Walker"
skaters_data$shooter[skaters_data$shooter == "Alexei Toropchenko"] <- "Alexey Toropchenko"
skaters_data$shooter[skaters_data$shooter == "Mitch Marner"] <- "Mitchell Marner"
skaters_data$shooter[skaters_data$shooter == "Bo Groulx"] <- "Benoit-Olivier Groulx"
skaters_data$shooter[skaters_data$shooter == "Mitch Marner"] <- "Mitchell Marner"
skaters_data$shooter[skaters_data$shooter == "Jesse Ylnen"] <- "Jesse Ylonen"
skaters_data$shooter[skaters_data$shooter == "Nick Abruzzese"] <- "Nicholas Abruzzese"
skaters_data$shooter[skaters_data$shooter == "Alexis Lafrenire"] <- "Alexis Lafreniere"
skaters_data$shooter[skaters_data$shooter == "Tim Sttzle"] <- "Tim Stutzle"

# Save data
write.csv(skaters_data, "data/skaters.csv", row.names = FALSE)