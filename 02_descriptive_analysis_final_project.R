# =========================================================
# Project: Final Soccer Project
# Purpose: Descriptive analysis of shot data including
#          match state effects on shot selection
# Author: Pranav Kapoor
# Date: 2026-04-14
# Updated: 2026-04-21 (Added match state analyses)
# =========================================================

# Load required packages
library(tidyverse)
library(ggrepel)

# Load the analysis-ready dataset
shots_model <- read_csv("shots_model.csv")

# Re-apply factor types after loading from CSV
shots_model <- shots_model %>%
  mutate(
    team         = as_factor(team),
    body_part    = as_factor(body_part),
    play_pattern = as_factor(play_pattern),
    shot_type    = as_factor(shot_type),
    technique    = as_factor(technique),
    first_time   = as_factor(first_time),
    match_state  = factor(match_state, levels = c("Drawing", "Losing", "Winning")),
    central_shot = as_factor(central_shot)
  )

# ---------------------------------------------------------
# Section 1: Overall Summary Statistics
# ---------------------------------------------------------

overall_summary <- shots_model %>%
  summarize(
    total_shots   = n(),
    total_goals   = sum(Goal),
    goal_rate     = mean(Goal),
    mean_xg       = mean(xg, na.rm = TRUE),
    mean_distance = mean(distance_from_goal)
  )

overall_summary

# ---------------------------------------------------------
# Section 2: Compare Goals vs Non-Goals
# ---------------------------------------------------------

goal_comparison <- shots_model %>%
  group_by(Goal) %>%
  summarize(
    total_shots   = n(),
    mean_xg       = mean(xg, na.rm = TRUE),
    mean_distance = mean(distance_from_goal),
    .groups = "drop"
  )

goal_comparison

# ---------------------------------------------------------
# Section 3: Body Part Analysis
# ---------------------------------------------------------

# Count shots by body part
body_part_counts <- shots_model %>%
  group_by(body_part) %>%
  summarize(
    total_shots = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_shots))

body_part_counts

# Goal rate by body part
body_part_summary <- shots_model %>%
  group_by(body_part) %>%
  summarize(
    total_shots = n(),
    total_goals = sum(Goal),
    goal_rate   = mean(Goal),
    mean_xg     = mean(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(goal_rate))

body_part_summary

# ---------------------------------------------------------
# Section 4: Play Pattern Analysis
# ---------------------------------------------------------

# Count shots by play pattern
play_pattern_counts <- shots_model %>%
  group_by(play_pattern) %>%
  summarize(
    total_shots = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_shots))

play_pattern_counts

# Goal rate by play pattern
play_pattern_summary <- shots_model %>%
  group_by(play_pattern) %>%
  summarize(
    total_shots = n(),
    total_goals = sum(Goal),
    goal_rate   = mean(Goal),
    mean_xg     = mean(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(goal_rate))

play_pattern_summary

# ---------------------------------------------------------
# Section 5: Match State Analysis
# ---------------------------------------------------------

# Overall match state distribution
match_state_distribution <- shots_model %>%
  group_by(match_state) %>%
  summarize(
    total_shots = n(),
    percentage  = round(n() / nrow(shots_model) * 100, 1),
    .groups = "drop"
  )

match_state_distribution

# Key statistics by match state
match_state_summary <- shots_model %>%
  group_by(match_state) %>%
  summarize(
    total_shots   = n(),
    total_goals   = sum(Goal),
    goal_rate     = mean(Goal),
    mean_xg       = mean(xg, na.rm = TRUE),
    mean_distance = mean(distance_from_goal),
    .groups = "drop"
  )

match_state_summary

# Shot locations by match state
match_state_locations <- shots_model %>%
  group_by(match_state) %>%
  summarize(
    mean_x        = mean(x_coord),
    mean_y        = mean(y_coord),
    mean_distance = mean(distance_from_goal),
    sd_distance   = sd(distance_from_goal),
    .groups = "drop"
  )

match_state_locations

# ---------------------------------------------------------
# Section 6: Central vs Wide Shot Analysis
# ---------------------------------------------------------

central_shot_summary <- shots_model %>%
  group_by(central_shot) %>%
  summarize(
    total_shots   = n(),
    total_goals   = sum(Goal),
    goal_rate     = mean(Goal),
    mean_xg       = mean(xg, na.rm = TRUE),
    mean_distance = mean(distance_from_goal),
    .groups = "drop"
  )

central_shot_summary

# Central vs wide by match state
central_by_match_state <- shots_model %>%
  group_by(match_state, central_shot) %>%
  summarize(
    total_shots = n(),
    goal_rate   = mean(Goal),
    .groups = "drop"
  ) %>%
  arrange(match_state, central_shot)

central_by_match_state

# ---------------------------------------------------------
# Section 7: First-Time Shot Analysis
# ---------------------------------------------------------

first_time_summary <- shots_model %>%
  group_by(first_time) %>%
  summarize(
    total_shots   = n(),
    total_goals   = sum(Goal),
    goal_rate     = mean(Goal),
    mean_xg       = mean(xg, na.rm = TRUE),
    mean_distance = mean(distance_from_goal),
    .groups = "drop"
  )

first_time_summary

# First-time shots by match state
first_time_by_match_state <- shots_model %>%
  group_by(match_state, first_time) %>%
  summarize(
    total_shots = n(),
    percentage  = round(n() / sum(n()) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(match_state, first_time)

first_time_by_match_state

# ---------------------------------------------------------
# Section 8: Team-Level Analysis
# ---------------------------------------------------------

team_summary <- shots_model %>%
  group_by(team) %>%
  summarize(
    total_shots = n(),
    total_goals = sum(Goal),
    goal_rate   = mean(Goal),
    mean_xg     = mean(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_shots >= 50) %>%
  arrange(desc(goal_rate))

team_summary

# ---------------------------------------------------------
# Section 9: Visualizations - Basic Comparisons
# ---------------------------------------------------------

# Boxplot: Distance by goal outcome
p1 <- ggplot(shots_model) +
  aes(x = as.factor(Goal), y = distance_from_goal) +
  geom_boxplot() +
  labs(
    title = "Distance from Goal by Shot Outcome",
    x     = "Goal Outcome (0 = No Goal, 1 = Goal)",
    y     = "Distance from Goal (yards)"
  ) +
  theme_classic()

p1
ggsave("plot_01_distance_by_goal.png", plot = p1, width = 7, height = 5)

# Boxplot: xG by goal outcome
p2 <- ggplot(shots_model) +
  aes(x = as.factor(Goal), y = xg) +
  geom_boxplot() +
  labs(
    title = "Expected Goals (xG) by Shot Outcome",
    x     = "Goal Outcome (0 = No Goal, 1 = Goal)",
    y     = "xG"
  ) +
  theme_classic()

p2
ggsave("plot_02_xg_by_goal.png", plot = p2, width = 7, height = 5)

# Scatterplot: xG vs distance
p3 <- ggplot(shots_model) +
  aes(x = distance_from_goal, y = xg) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(
    title = "xG and Distance from Goal",
    x     = "Distance from Goal (yards)",
    y     = "xG"
  ) +
  theme_classic()

p3
ggsave("plot_03_xg_vs_distance.png", plot = p3, width = 7, height = 5)

# ---------------------------------------------------------
# Section 10: Visualizations - Match State
# ---------------------------------------------------------

# Boxplot: Distance by match state
p4 <- ggplot(shots_model) +
  aes(x = match_state, y = distance_from_goal) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title    = "Shot Distance by Match State",
    subtitle = "Do trailing teams shoot from farther away?",
    x        = "Match State",
    y        = "Distance from Goal (yards)"
  ) +
  theme_classic()

p4
ggsave("plot_04_distance_by_match_state.png", plot = p4, width = 7, height = 5)

# Boxplot: xG by match state
p5 <- ggplot(shots_model) +
  aes(x = match_state, y = xg) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title    = "Shot Quality (xG) by Match State",
    subtitle = "Do trailing teams take lower-quality shots?",
    x        = "Match State",
    y        = "xG"
  ) +
  theme_classic()

p5
ggsave("plot_05_xg_by_match_state.png", plot = p5, width = 7, height = 5)

# Bar chart: Goal rate by match state
p6 <- ggplot(shots_model) +
  aes(x = match_state, fill = as.factor(Goal)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("0" = "coral", "1" = "darkgreen"),
    labels = c("No Goal", "Goal")
  ) +
  labs(
    title = "Goal Rate by Match State",
    x     = "Match State",
    y     = "Proportion",
    fill  = "Goal"
  ) +
  theme_classic()

p6
ggsave("plot_06_goal_rate_by_match_state.png", plot = p6, width = 7, height = 5)

# ---------------------------------------------------------
# Section 11: Visualizations - Spatial Analysis
# ---------------------------------------------------------

# Scatterplot: Shot locations colored by match state
p7 <- ggplot(shots_model) +
  aes(x = x_coord, y = y_coord, color = match_state) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Shot Locations by Match State",
    x     = "X Coordinate (yards)",
    y     = "Y Coordinate (yards)",
    color = "Match State"
  ) +
  theme_classic()

p7
ggsave("plot_07_locations_by_match_state.png", plot = p7, width = 8, height = 6)

# Density plot: Distance distribution by match state
p8 <- ggplot(shots_model) +
  aes(x = distance_from_goal, fill = match_state) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Shot Distances by Match State",
    x     = "Distance from Goal (yards)",
    y     = "Density",
    fill  = "Match State"
  ) +
  theme_classic()

p8
ggsave("plot_08_distance_density_by_match_state.png", plot = p8, width = 8, height = 5)

# ---------------------------------------------------------
# Section 12: Visualizations - Body Part and Play Pattern
# ---------------------------------------------------------

# Bar chart: Body part by goal outcome
p9 <- ggplot(shots_model) +
  aes(x = body_part, fill = as.factor(Goal)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("0" = "coral", "1" = "darkgreen"),
    labels = c("No Goal", "Goal")
  ) +
  labs(
    title = "Body Part by Shot Outcome",
    x     = "Body Part",
    y     = "Proportion",
    fill  = "Goal"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9
ggsave("plot_09_body_part_by_goal.png", plot = p9, width = 7, height = 5)

# Bar chart: Play pattern by goal outcome
p10 <- ggplot(shots_model) +
  aes(x = play_pattern, fill = as.factor(Goal)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("0" = "coral", "1" = "darkgreen"),
    labels = c("No Goal", "Goal")
  ) +
  labs(
    title = "Play Pattern by Shot Outcome",
    x     = "Play Pattern",
    y     = "Proportion",
    fill  = "Goal"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p10
ggsave("plot_10_play_pattern_by_goal.png", plot = p10, width = 8, height = 5)

# ---------------------------------------------------------
# Section 13: Team Analysis Visualization
# ---------------------------------------------------------

# Scatterplot: Team goal rate vs average xG
p11 <- ggplot(team_summary) +
  aes(x = mean_xg, y = goal_rate, label = team) +
  geom_point(size = 3, color = "darkblue") +
  geom_text_repel(size = 3) +
  labs(
    title    = "Team Goal Rate and Average xG",
    subtitle = "Teams with at least 50 shots",
    x        = "Average xG",
    y        = "Goal Rate"
  ) +
  theme_classic()

p11
ggsave("plot_11_team_goalrate_xg.png", plot = p11, width = 8, height = 6)

# ---------------------------------------------------------
# Section 14: Calculate Key Correlations
# ---------------------------------------------------------

# Correlation between distance and xG
cor_distance_xg <- cor(
  shots_model$distance_from_goal,
  shots_model$xg,
  use = "complete.obs"
)

# Correlation by match state
cor_by_match_state <- shots_model %>%
  group_by(match_state) %>%
  summarize(
    correlation = cor(distance_from_goal, xg, use = "complete.obs"),
    .groups = "drop"
  )

cor_by_match_state

# ---------------------------------------------------------
# Section 15: Save All Summary Tables
# ---------------------------------------------------------

write_csv(overall_summary,          "table_01_overall_summary.csv")
write_csv(goal_comparison,          "table_02_goal_comparison.csv")
write_csv(body_part_summary,        "table_03_body_part_summary.csv")
write_csv(play_pattern_summary,     "table_04_play_pattern_summary.csv")
write_csv(match_state_summary,      "table_05_match_state_summary.csv")
write_csv(match_state_locations,    "table_06_match_state_locations.csv")
write_csv(central_shot_summary,     "table_07_central_shot_summary.csv")
write_csv(first_time_summary,       "table_08_first_time_summary.csv")
write_csv(central_by_match_state,   "table_09_central_by_match_state.csv")
write_csv(team_summary,             "table_10_team_summary.csv")

# ---------------------------------------------------------
# Section 16: Final Summary
# ---------------------------------------------------------

shots_model %>%
  summarize(
    total_shots = n(),
    total_goals = sum(Goal),
    goal_rate   = round(mean(Goal), 3)
  )

shots_model %>%
  group_by(match_state) %>%
  summarize(
    total_shots   = n(),
    mean_distance = round(mean(distance_from_goal), 1),
    mean_xg       = round(mean(xg, na.rm = TRUE), 4),
    .groups = "drop"
  )