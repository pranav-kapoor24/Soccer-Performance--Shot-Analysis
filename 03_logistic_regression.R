# =========================================================
# Project: Final Soccer Project
# Purpose: Logistic regression models to predict goal
#          probability from shot location and match state
# Author: Pranav Kapoor
# Date: 2026-04-14
# Updated: 2026-04-21 (Dynamic model selection, penalty and
#          body_part Other filters, model_1 coefficients saved)
# =========================================================

# Load required packages
library(tidyverse)

# ---------------------------------------------------------
# Step 1: Load data and re-apply factor conversions
# ---------------------------------------------------------

shots_model <- read_csv("shots_model.csv", show_col_types = FALSE)

shots_model <- shots_model %>%
  mutate(
    team         = as_factor(team),
    body_part    = as_factor(body_part),
    play_pattern = as_factor(play_pattern),
    shot_type    = as_factor(shot_type),
    technique    = as_factor(technique),
    first_time   = as_factor(first_time),
    central_shot = as_factor(central_shot),
    match_state  = factor(match_state, levels = c("Drawing", "Losing", "Winning"))
  )

# ---------------------------------------------------------
# Step 2: Filter before modeling
#   - Remove penalty shots (fixed location, structurally
#     different from open-play shots)
#   - Remove body_part "Other" (only 6 shots, 0 goals;
#     causes near-complete separation in the model)
# ---------------------------------------------------------

shots_model <- shots_model %>%
  filter(shot_type != "Penalty") %>%
  filter(body_part != "Other") %>%
  mutate(
    body_part    = droplevels(body_part),
    play_pattern = droplevels(play_pattern),
    shot_type    = droplevels(shot_type)
  )

# Confirm post-filter dataset
dim(shots_model)
table(shots_model$body_part)
table(shots_model$shot_type)
table(shots_model$match_state)

# ---------------------------------------------------------
# Section 1: Model 1 - Distance Only (Baseline)
# ---------------------------------------------------------

model_1 <- glm(
  Goal ~ distance_from_goal,
  data   = shots_model,
  family = binomial
)

summary(model_1)
exp(coef(model_1))

# Extract and save Model 1 coefficients
model_1_results <- as.data.frame(summary(model_1)$coefficients)
model_1_results$odds_ratio <- exp(model_1_results$Estimate)
model_1_results$term       <- rownames(model_1_results)

model_1_results <- model_1_results %>%
  select(term, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`, odds_ratio)

write_csv(
  tibble(
    term       = model_1_results$term,
    estimate   = model_1_results$Estimate,
    std_error  = model_1_results$`Std. Error`,
    z_value    = model_1_results$`z value`,
    p_value    = model_1_results$`Pr(>|z|)`,
    odds_ratio = model_1_results$odds_ratio
  ),
  "model_1_coefficients.csv"
)

# ---------------------------------------------------------
# Section 2: Model 2 - Distance + Body Part + Play Pattern
# ---------------------------------------------------------

model_2 <- glm(
  Goal ~ distance_from_goal + body_part + play_pattern,
  data   = shots_model,
  family = binomial
)

summary(model_2)
exp(coef(model_2))

# ---------------------------------------------------------
# Section 3: Model 3 - Add Central Shot Variable
# ---------------------------------------------------------

model_3 <- glm(
  Goal ~ distance_from_goal + central_shot + body_part + play_pattern,
  data   = shots_model,
  family = binomial
)

summary(model_3)
exp(coef(model_3))

# ---------------------------------------------------------
# Section 4: Model 4 - Add First Time Variable
# ---------------------------------------------------------

model_4 <- glm(
  Goal ~ distance_from_goal + central_shot + body_part + play_pattern + first_time,
  data   = shots_model,
  family = binomial
)

summary(model_4)
exp(coef(model_4))

# ---------------------------------------------------------
# Section 5: Model 5 - Add Match State
# ---------------------------------------------------------

model_5 <- glm(
  Goal ~ distance_from_goal + central_shot + body_part + play_pattern + match_state,
  data   = shots_model,
  family = binomial
)

summary(model_5)
exp(coef(model_5))

# ---------------------------------------------------------
# Section 6: Model 6 - Full Model with All Variables
# ---------------------------------------------------------

model_6 <- glm(
  Goal ~ distance_from_goal + central_shot + body_part + play_pattern +
    first_time + match_state,
  data   = shots_model,
  family = binomial
)

summary(model_6)
exp(coef(model_6))

# ---------------------------------------------------------
# Section 7: Model Comparison
# ---------------------------------------------------------

model_comparison <- tibble(
  Model = c(
    "Model 1: Distance only",
    "Model 2: + Body Part + Play Pattern",
    "Model 3: + Central Shot",
    "Model 4: + First Time",
    "Model 5: + Match State",
    "Model 6: Full Model"
  ),
  AIC = c(
    AIC(model_1), AIC(model_2), AIC(model_3),
    AIC(model_4), AIC(model_5), AIC(model_6)
  ),
  Deviance = c(
    deviance(model_1), deviance(model_2), deviance(model_3),
    deviance(model_4), deviance(model_5), deviance(model_6)
  )
)

model_comparison

# Dynamically select best model by lowest AIC
best_model_idx  <- which.min(model_comparison$AIC)
best_model_name <- model_comparison$Model[best_model_idx]
best_model_name

all_models <- list(model_1, model_2, model_3, model_4, model_5, model_6)
best_model  <- all_models[[best_model_idx]]

# ---------------------------------------------------------
# Section 8: Best Model - Detailed Coefficients
# ---------------------------------------------------------

# Extract coefficients
best_model_results <- as.data.frame(summary(best_model)$coefficients)

# Add odds ratios and confidence intervals
best_model_results$odds_ratio <- exp(best_model_results$Estimate)
conf_int <- exp(confint(best_model))
best_model_results$ci_lower <- conf_int[, 1]
best_model_results$ci_upper <- conf_int[, 2]
best_model_results$term     <- rownames(best_model_results)

best_model_results <- best_model_results %>%
  select(
    term, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`,
    odds_ratio, ci_lower, ci_upper
  )

best_model_results

# ---------------------------------------------------------
# Section 9: Match State Effects
# ---------------------------------------------------------

# Match state coefficients (reference level = Drawing)
match_state_coefs <- best_model_results %>%
  filter(grepl("match_state", term)) %>%
  select(term, odds_ratio, `Pr(>|z|)`, ci_lower, ci_upper)

match_state_coefs

# ---------------------------------------------------------
# Section 10: Add Predictions to Dataset
# ---------------------------------------------------------

shots_model <- shots_model %>%
  mutate(
    predicted_prob = predict(best_model, type = "response"),
    predicted_goal = if_else(predicted_prob > 0.5, 1, 0)
  )

# ---------------------------------------------------------
# Section 11: Model Performance Evaluation
# ---------------------------------------------------------

# Confusion matrix
confusion_matrix <- table(
  Actual    = shots_model$Goal,
  Predicted = shots_model$predicted_goal
)

confusion_matrix

# Performance metrics
accuracy    <- mean(shots_model$Goal == shots_model$predicted_goal)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
precision   <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

tibble(
  metric = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
  value  = round(c(accuracy, sensitivity, specificity, precision), 3)
)

# ---------------------------------------------------------
# Section 12: Predicted Probability by Match State
# ---------------------------------------------------------

predicted_by_match_state <- shots_model %>%
  group_by(match_state) %>%
  summarize(
    mean_predicted_prob = mean(predicted_prob, na.rm = TRUE),
    mean_actual_goal    = mean(Goal, na.rm = TRUE),
    .groups = "drop"
  )

predicted_by_match_state

# ---------------------------------------------------------
# Section 13: Visualizations
# ---------------------------------------------------------

# Predicted probability distribution by match state
p1 <- ggplot(shots_model) +
  aes(x = predicted_prob, fill = match_state) +
  geom_density(alpha = 0.5) +
  labs(
    title    = "Predicted Goal Probability by Match State",
    subtitle = paste("Based on", best_model_name),
    x        = "Predicted Probability",
    y        = "Density",
    fill     = "Match State"
  ) +
  theme_classic()

p1
ggsave("plot_12_predicted_prob_by_match_state.png", plot = p1, width = 8, height = 5)

# Predicted probability vs distance, coloured by actual outcome
p2 <- shots_model %>%
  mutate(Goal_factor = as.factor(Goal)) %>%
  ggplot() +
  aes(x = distance_from_goal, y = predicted_prob, color = Goal_factor) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(
    title    = "Predicted Probability vs Distance",
    subtitle = "Colored by actual outcome",
    x        = "Distance from Goal (yards)",
    y        = "Predicted Probability",
    color    = "Actual Goal"
  ) +
  scale_color_manual(
    values = c("0" = "coral", "1" = "darkgreen"),
    labels = c("No Goal", "Goal")
  ) +
  theme_classic()

p2
ggsave("plot_13_predicted_vs_distance.png", plot = p2, width = 8, height = 5)

# Calibration plot
calibration_data <- shots_model %>%
  mutate(prob_bin = cut(predicted_prob, breaks = seq(0, 1, 0.1))) %>%
  group_by(prob_bin) %>%
  summarize(
    mean_predicted = mean(predicted_prob, na.rm = TRUE),
    observed_rate  = mean(Goal, na.rm = TRUE),
    n              = n(),
    .groups = "drop"
  )

p3 <- ggplot(calibration_data) +
  aes(x = mean_predicted, y = observed_rate) +
  geom_point(aes(size = n)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Model Calibration Plot",
    subtitle = "Perfect calibration = points on diagonal line",
    x        = "Mean Predicted Probability",
    y        = "Observed Goal Rate",
    size     = "Number of Shots"
  ) +
  theme_classic()

p3
ggsave("plot_14_calibration.png", plot = p3, width = 7, height = 5)

# ---------------------------------------------------------
# Section 14: Save Model Results
# ---------------------------------------------------------

saveRDS(best_model, "best_model_final.rds")

write_csv(
  tibble(
    term       = best_model_results$term,
    estimate   = best_model_results$Estimate,
    std_error  = best_model_results$`Std. Error`,
    z_value    = best_model_results$`z value`,
    p_value    = best_model_results$`Pr(>|z|)`,
    odds_ratio = best_model_results$odds_ratio,
    ci_lower   = best_model_results$ci_lower,
    ci_upper   = best_model_results$ci_upper
  ),
  "best_model_coefficients.csv"
)

write_csv(model_comparison, "model_comparison.csv")
write_csv(predicted_by_match_state, "predicted_prob_by_match_state.csv")

performance_metrics <- tibble(
  metric = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
  value  = c(accuracy, sensitivity, specificity, precision)
)

write_csv(performance_metrics, "best_model_performance.csv")

# ---------------------------------------------------------
# Section 15: Final Summary
# ---------------------------------------------------------

# Model comparison ranked by AIC
model_comparison %>%
  arrange(AIC)

# Key coefficients from best model
best_model_results %>%
  select(term, odds_ratio, `Pr(>|z|)`, ci_lower, ci_upper) %>%
  arrange(`Pr(>|z|)`)

# Performance summary
performance_metrics