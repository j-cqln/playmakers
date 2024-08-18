library(readr)
library(dplyr)
library(lme4)
library(splines)
library(ModelMetrics)

# Process data
process_data <- function(d, season) {
  last_event_time_cutoff = 3
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
           shooter_id = shooterPlayerId,
           goalie = goalieNameForShot,
           goalie_id = goalieIdForShot,
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
           shooter, shooter_id, goalie, goalie_id, shot_type, angle_change,
           on_goal, goal,
           creates_rebound_shot, rebound_shot,
           frozen, play_stopped, in_zone, out_zone)
  
  return(d)
}

# Additional data processing for model use
process_model_data <- function(d) {
  d$goal_diff <- as.factor(d$goal_diff)
  d$shooter_id <- as.factor(d$shooter_id)
  d$goalie_id <- as.factor(d$goalie_id)
  d$shot_type <- as.factor(d$shot_type)
  
  return(d)
}

# Generalized linear mixed model used for xG
model <- function(x, y) {
  y <- as.factor(y)
  
  fit <- glmer(y ~ 1 +
                 ns(dist, df = 6) + ns(abs_angle, df = 6) +
                 (1 | goal_diff) +
                 (1 | shooter_id) + (1 | goalie_id) +
                 (1 | shot_type),
               data = x, family = binomial(), nAGQ = 0)
  
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
  
  rm(d)
}

# Group by season/game/period, create rebound goal
data <- data %>%
  group_by(season, game, period) %>%
  mutate(rebound_goal = ifelse(team == lead(team) &
                                 creates_rebound_shot == 1 &
                                 lead(rebound_shot) == 1 &
                                 lead(goal) == 1,
                                1, 0))

# Save data
write.csv(data, "data/processed_shots.csv", row.names = FALSE)

# Build models
model_data <- process_model_data(data)
model_data <- head(model_data, 40000)

xg_model <- model(model_data, model_data$goal)
model_data$xg <- predict(xg_model, model_data,
                         type = "response", allow.new.levels = TRUE)

xrebounds_model <- model(model_data, model_data$creates_rebound_shot)
model_data$xrebounds <- predict(xrebounds_model, model_data,
                                type = "response", allow.new.levels = TRUE)

xrebound_value_model <- model(model_data, model_data$rebound_goal)
model_data$xrebound_value <- predict(xrebound_value_model, model_data,
                                     type = "response", allow.new.levels = TRUE)

# Save models
saveRDS(xg_model, "models/xg_model.rds")
saveRDS(xrebounds_model, "models/xrebounds_model.rds")
saveRDS(xrebound_value_model, "models/xrebound_value_model.rds")

# Group by shooter ID and compute total rebound xG and average rebound xG
model_data <- model_data %>%
  group_by(season, shooter_id) %>%
  mutate(total_xg = sum(xg, na.rm = TRUE),
         avg_xg = mean(xg, na.rm = TRUE),
         total_xrebounds = sum(xrebounds, na.rm = TRUE),
         avg_xrebounds = mean(xrebounds, na.rm = TRUE),
         total_rebound_value = sum(xrebound_value, na.rm = TRUE),
         avg_rebound_value = mean(xrebound_value, na.rm = TRUE))

# Save model data
write.csv(model_data, "data/model_data.csv", row.names = FALSE)