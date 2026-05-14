# =========================================================
# Project: Final Soccer Project
# Purpose: Import StatsBomb event JSON files, extract shot
#          events, and build a clean dataset for analysis
# Author: Pranav Kapoor
# Date: 2026-04-14
# Updated: 2026-04-21 (Calculate match state from goals)
# =========================================================

# Load required packages
library(tidyverse)
library(jsonlite)

# ---------------------------------------------------------
# Step 1: Get all JSON match files from the events folder
# ---------------------------------------------------------

json_files <- list.files(
  path = "events",
  pattern = "\\.json$",
  full.names = TRUE
)

# Check how many files were found
length(json_files)

# ---------------------------------------------------------
# Step 2: Create function to extract shots and match state
# ---------------------------------------------------------

extract_shots <- function(file_name) {
  
  # Read one event file
  events <- fromJSON(file_name, simplifyVector = FALSE)
  
  # Create a tibble of all events to track goals chronologically
  all_events <- tibble(
    event_index = 1:length(events),
    event_type = sapply(events, function(x) x$type$name),
    team = sapply(events, function(x) x$team$name),
    minute = sapply(events, function(x) x$minute),
    second = sapply(events, function(x) x$second),
    is_goal = sapply(events, function(x) {
      if (x$type$name == "Shot") {
        if (!is.null(x$shot$outcome$name)) {
          return(x$shot$outcome$name == "Goal")
        }
      }
      return(FALSE)
    })
  )
  
  # Get the two teams
  teams <- unique(all_events$team)
  team1 <- teams[1]
  team2 <- teams[2]
  
  # Calculate cumulative score for each team
  all_events <- all_events %>%
    mutate(
      team1_goals_cumulative = cumsum(team == team1 & is_goal),
      team2_goals_cumulative = cumsum(team == team2 & is_goal)
    )
  
  # Find shot events
  shot_indices <- which(sapply(events, function(x) x$type$name == "Shot"))
  
  # Build shot-level tibble
  shots <- tibble(
    source_file = basename(file_name),
    event_index = shot_indices,
    team = sapply(events[shot_indices], function(x) x$team$name),
    player = sapply(events[shot_indices], function(x) x$player$name),
    minute = sapply(events[shot_indices], function(x) x$minute),
    second = sapply(events[shot_indices], function(x) x$second),
    x_coord = sapply(events[shot_indices], function(x) x$location[[1]]),
    y_coord = sapply(events[shot_indices], function(x) x$location[[2]]),
    play_pattern = sapply(events[shot_indices], function(x) x$play_pattern$name),
    xg = sapply(events[shot_indices], function(x) {
      val <- x$shot$statsbomb_xg
      if (is.null(val)) return(NA_real_)
      as.double(val)
    }),
    outcome = sapply(events[shot_indices], function(x) x$shot$outcome$name),
    shot_type = sapply(events[shot_indices], function(x) x$shot$type$name),
    body_part = sapply(events[shot_indices], function(x) x$shot$body_part$name),
    technique = sapply(events[shot_indices], function(x) x$shot$technique$name),
    first_time = sapply(events[shot_indices], function(x) {
      if (is.null(x$shot$first_time)) FALSE else x$shot$first_time
    })
  )
  
  # Join with cumulative scores
  shots <- shots %>%
    left_join(
      all_events %>%
        select(event_index, team1_goals_cumulative, team2_goals_cumulative),
      by = "event_index"
    )
  
  # Calculate score before each shot
  shots <- shots %>%
    mutate(
      team1_goals_before = if_else(
        team == team1 & outcome == "Goal",
        team1_goals_cumulative - 1,
        team1_goals_cumulative
      ),
      team2_goals_before = if_else(
        team == team2 & outcome == "Goal",
        team2_goals_cumulative - 1,
        team2_goals_cumulative
      ),
      team_score = if_else(team == team1, team1_goals_before, team2_goals_before),
      opponent_score = if_else(team == team1, team2_goals_before, team1_goals_before)
    )
  
  # Create derived variables
  shots <- shots %>%
    mutate(
      Goal = if_else(outcome == "Goal", 1, 0),
      distance_from_goal = sqrt((120 - x_coord)^2 + (40 - y_coord)^2),
      match_state = case_when(
        team_score > opponent_score ~ "Winning",
        team_score < opponent_score ~ "Losing",
        team_score == opponent_score ~ "Drawing"
      )
    )
  
  # Select and order final columns
  shots <- shots %>%
    select(
      source_file, event_index, team, player, minute, second,
      x_coord, y_coord, play_pattern, xg, outcome, shot_type,
      body_part, technique, first_time,
      team_score, opponent_score, match_state,
      Goal, distance_from_goal
    )
  
  return(shots)
}

# ---------------------------------------------------------
# Step 3: Apply the function to the first 500 match files
# ---------------------------------------------------------

sample_files <- json_files[1:500]

# Combine all shots from 500 matches into one dataset
shots_data <- map_dfr(sample_files, extract_shots)

# Check dataset size and structure
dim(shots_data)
glimpse(shots_data)

# ---------------------------------------------------------
# Step 4: Quality checks on the full dataset
# ---------------------------------------------------------

# Number of matches included
n_distinct(shots_data$source_file)

# Goals vs non-goals
table(shots_data$Goal)

# Match state distribution
table(shots_data$match_state)

# Match state by goal outcome
table(shots_data$match_state, shots_data$Goal)

# Shot distance summary
summary(shots_data$distance_from_goal)

# Body part categories
table(shots_data$body_part)

# Play pattern categories
table(shots_data$play_pattern)

# Missing xG values
sum(is.na(shots_data$xg))

# ---------------------------------------------------------
# Step 5: Save the cleaned shot-level dataset
# ---------------------------------------------------------

write_csv(shots_data, "shots_data_cleaned.csv")

# ---------------------------------------------------------
# Step 6: Create the modeling dataset
# ---------------------------------------------------------

shots_model <- shots_data %>%
  select(
    source_file,
    team,
    minute,
    second,
    x_coord,
    y_coord,
    distance_from_goal,
    xg,
    outcome,
    Goal,
    body_part,
    play_pattern,
    shot_type,
    technique,
    first_time,
    team_score,
    opponent_score,
    match_state
  )

# Preview the modeling dataset
glimpse(shots_model)

# ---------------------------------------------------------
# Step 7: Convert categorical variables to factors
# ---------------------------------------------------------

shots_model <- shots_model %>%
  mutate(
    team         = as_factor(team),
    body_part    = as_factor(body_part),
    play_pattern = as_factor(play_pattern),
    shot_type    = as_factor(shot_type),
    technique    = as_factor(technique),
    first_time   = as_factor(first_time),
    match_state  = factor(match_state, levels = c("Drawing", "Losing", "Winning"))
  )

# Confirm Drawing is the reference level for match_state
levels(shots_model$match_state)

# ---------------------------------------------------------
# Step 8: Create central vs wide shot variable
# ---------------------------------------------------------

# Central is defined as y between 36 and 44 (StatsBomb goalpost coordinates)
# The goal is 8 yards wide, centered at y = 40, with posts at y = 36 and y = 44
shots_model <- shots_model %>%
  mutate(
    central_shot = if_else(y_coord >= 36 & y_coord <= 44, "Central", "Wide"),
    central_shot = as_factor(central_shot)
  )

# Check distribution
table(shots_model$central_shot)
table(shots_model$central_shot, shots_model$Goal)

# ---------------------------------------------------------
# Step 9: Save the final analysis-ready dataset
# ---------------------------------------------------------

write_csv(shots_model, "shots_model.csv")

# ---------------------------------------------------------
# Step 10: Final summary
# ---------------------------------------------------------

# Overall counts
shots_model %>%
  summarize(
    total_shots  = n(),
    total_goals  = sum(Goal),
    goal_rate    = round(mean(Goal), 3),
    n_matches    = n_distinct(source_file)
  )

# Match state breakdown
shots_model %>%
  group_by(match_state) %>%
  summarize(
    total_shots   = n(),
    total_goals   = sum(Goal),
    mean_xg       = round(mean(xg), 4),
    mean_distance = round(mean(distance_from_goal), 2),
    .groups = "drop"
  )