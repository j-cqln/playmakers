library(readr)
library(dplyr)
library(lme4)
library(splines)
library(ModelMetrics)

# Process data
process_data <- function(d, season) {
  last_event_time_cutoff = 5
  time_since_last_default = 99
  
  # Shot info
  d <- d %>%
    rename(event_id = id,
           shot_id = shotID,
           season_type = isPlayoffGame,
           game = game_id,
           home_team_code = homeTeamCode,
           away_team_code = awayTeamCode,
           home_team_skaters_on_ice = homeSkatersOnIce,
           away_team_skaters_on_ice = awaySkatersOnIce,
           adj_x = xCordAdjusted,
           adj_y = yCordAdjusted,
           dist = shotDistance,
           abs_angle = shotAngleAdjusted,
           angle_change = shotAnglePlusRebound,
           last_event = lastEventCategory,
           time_since_last = timeSinceLastEvent,
           last_event_team = lastEventTeam,
           shooter = shooterName,
           goalie = goalieNameForShot,
           on_goal = shotWasOnGoal,
           creates_rebound_shot = shotGeneratedRebound,
           rebound_shot = shotRebound,
           frozen = shotGoalieFroze,
           stopped = shotPlayStopped,
           in_zone = shotPlayContinuedInZone,
           out_zone = shotPlayContinuedOutsideZone,
           shot_type = shotType) %>%
    filter(!is.na(shooter)) %>%
    mutate(season = as.numeric(season),
           id = row_number(),
           team = ifelse(team == 'HOME', home_team_code, away_team_code),
           is_home_team = ifelse(team == 'HOME', 1, 0),
           goal_diff = ifelse(is_home_team == 1,
                              homeTeamGoals - awayTeamGoals,
                              awayTeamGoals - homeTeamGoals),
           event = 'shot',
           goalie = ifelse(!is.na(goalie), goalie, ''),
           play_stopped = ifelse((frozen == 1) | (stopped == 1), 1, 0),
           time_since_last = ifelse(time_since_last <= last_event_time_cutoff,
                                    time_since_last,
                                    time_since_last_default),
           last_event = ifelse(time_since_last <= last_event_time_cutoff,
                               last_event,
                               'none'),
           last_event = ifelse(is.na(last_event), 'none', last_event),
           last_event_team = ifelse(last_event == 'none', NA, last_event_team))
  
  x <- d$adj_x
  y <- d$adj_y
  
  x <- -abs(x)
  
  d$x_coord <- -y
  d$y_coord <- x
  
  d <- d %>%
    select(season, season_type, game, event_id, shot_id, period, time,
           team, event, goal_diff, time_since_last, last_event,
           x_coord, y_coord, dist, abs_angle,
           shooter, goalie, shot_type, angle_change,
           on_goal, goal,
           creates_rebound_shot, rebound_shot,
           frozen, play_stopped, in_zone, out_zone)
  
  return(d)
}

# Additional data processing for model use
process_model_data <- function(d) {
  d$goal_diff <- as.factor(d$goal_diff)
  d$shooter <- as.factor(d$shooter)
  d$goalie <- as.factor(d$goalie)
  d$shot_type <- as.factor(d$shot_type)
  
  return(d)
}

# Generalized linear mixed model used for xG
model <- function(x, y) {
  y <- as.factor(y)
  
  fit <- glmer(y ~ 1 +
                 ns(dist, df = 6) + ns(abs_angle, df = 6) +
                 (1 | goal_diff) +
                 (1 | shooter) + (1 | goalie) +
                 (1 | shot_type),
               data = x, family = binomial())
  
  return(fit)
}

# Process data
data_files <- c("data/shots_2021.csv",
                "data/shots_2022.csv",
                "data/shots_2023.csv")

data <- NULL

for (i in 1:length(data_files)) {
  # Load data
  d <- read.csv(data_files[i])
  
  # Process data
  d <- process_data(d, substring(data_files[i], 12, 15))
  
  if (is.null(data)) {
    data <- d
  } else {
    data <- rbind(data, d)
  }
}

rm(d)

# Save data
write.csv(data, "data/processed_shots.csv", row.names = FALSE)

# Build model
model_data <- process_model_data(data)
xg_model <- model(model_data, model_data$goal)

# Save model
saveRDS(xg_model, "models/xg_model.rds")

# Predictions
model_data$xg <- predict(xg_model, model_data, type = "response")

# Group by game and period and assign xG of next shot if it generates rebound
model_data <- model_data %>%
  group_by(game, period) %>%
  mutate(rebound = ifelse(team == lead(team) & creates_rebound_shot == 1  & lead(last_event) == "SHOT", lead(xg), NA))
