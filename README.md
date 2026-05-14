# Soccer Shot Analysis: Location and Match State Effects on Goal Probability

**MGMT 4270: Introduction to Data Science — Final Project**
Pranav Kapoor | B00874135

---

## Overview

This project investigates how shot location and match state jointly influence goal probability
in professional soccer. Using 12,866 shot events extracted from StatsBomb Open Data JSON files,
I built a series of logistic regression models to quantify the combined effects of spatial
positioning and in-game context on scoring outcomes.

**Key Finding:** Distance from goal is the strongest predictor of scoring — each additional yard
reduces the odds of scoring by 11.4%. Teams that are winning have 35.5% higher odds of scoring
compared to teams that are drawing, even after controlling for shot distance and type.

---

## Research Questions

1. To what extent does shot location (distance from goal) significantly predict goal probability?
2. How does match state (winning, drawing, or losing) influence the spatial distribution of shot attempts?
3. Do teams take lower-quality or longer-distance shots when trailing compared to when leading?

---

## Repository Structure
soccer-shot-analysis/
│
├── README.md
│
├── scripts/
│   ├── 01_data_prep_final_project_actual.R        # JSON extraction, variable construction, cleaning
│   ├── 02_descriptive_analysis_final_project.R    # Descriptive statistics and visualizations
│   └── 03_logistic_regression.R                   # Six progressive logistic regression models
│
├── data/
│   ├── shots_data_cleaned.csv                     # Full cleaned shot-level dataset (12,866 rows)
│   ├── shots_model.csv                            # Analysis-ready modeling dataset
│   ├── model_comparison.csv                       # AIC and deviance for all 6 models
│   ├── model_1_coefficients.csv                   # Baseline model coefficients
│   ├── best_model_coefficients.csv                # Best model full coefficient table
│   ├── best_model_performance.csv                 # Accuracy, sensitivity, specificity, precision
│   ├── predicted_prob_by_match_state.csv          # Mean predicted probability by match state
│   ├── table_01_overall_summary.csv
│   ├── table_02_goal_comparison.csv
│   ├── table_03_body_part_summary.csv
│   ├── table_04_play_pattern_summary.csv
│   ├── table_05_match_state_summary.csv
│   ├── table_06_match_state_locations.csv
│   ├── table_07_central_shot_summary.csv
│   ├── table_08_first_time_summary.csv
│   ├── table_09_central_by_match_state.csv
│   └── table_10_team_summary.csv
│
├── plots/
│   ├── plot_01_distance_by_goal.png
│   ├── plot_02_xg_by_goal.png
│   ├── plot_03_xg_vs_distance.png
│   ├── plot_04_distance_by_match_state.png
│   ├── plot_05_xg_by_match_state.png
│   ├── plot_06_goal_rate_by_match_state.png
│   ├── plot_07_locations_by_match_state.png
│   ├── plot_08_distance_density_by_match_state.png
│   ├── plot_09_body_part_by_goal.png
│   ├── plot_10_play_pattern_by_goal.png
│   ├── plot_11_team_goalrate_xg.png
│   ├── plot_12_predicted_prob_by_match_state.png
│   ├── plot_13_predicted_vs_distance.png
│   └── plot_14_calibration.png
│
└── report/
└── Soccer_Shot_Analysis_Final_Report.docx

> The `events/` folder containing raw StatsBomb JSON files is not included due to file size.
> See the Data Source section below for download instructions.

---

## Data Source

Raw data comes from the StatsBomb Open Data repository, freely available on GitHub:

https://github.com/statsbomb/open-data

To reproduce this project:

1. Clone the StatsBomb Open Data repository
2. Copy the `events/` folder into the root of this project directory
3. Run the three scripts in order (see How to Run below)

The analysis uses the first 500 match JSON files, yielding 12,866 shot events across
multiple leagues and competitions.

---

## Methods Summary

### Data Processing — 01_data_prep_final_project_actual.R

- Extracted all shot events from StatsBomb JSON files using `jsonlite`
- Calculated match state (Winning / Drawing / Losing) by tracking cumulative goals
  chronologically within each match, before each shot was taken
- Computed distance from goal using: distance = sqrt[(120 - x)^2 + (40 - y)^2]
- Classified shots as central (y between 36 and 44 yards) or wide
- Excluded penalty shots and the "Other" body part category before modeling to avoid
  structural confounds and near-complete separation

### Descriptive Analysis — 02_descriptive_analysis_final_project.R

- Summarized shots by match state, body part, play pattern, shot position, and team
- Produced 14 visualizations and 10 summary tables exported as CSV files

### Logistic Regression — 03_logistic_regression.R

Six progressive models were built, each adding variables incrementally:

| Model   | Variables                              | AIC    |
|---------|----------------------------------------|--------|
| Model 1 | Distance only                          | 1827.8 |
| Model 2 | + Body part + Play pattern             | 1735.1 |
| Model 3 | + Central shot                         | 1717.4 |
| Model 4 | + First-time shot                      | 1714.3 |
| Model 5 | + Match state  [selected]              | 1706.5 |
| Model 6 | All variables                          | 1705.0 |

Model selection used AIC, where lower values indicate better fit. Model 5 was selected
as the best model, offering the strongest balance of explanatory power and parsimony.

Reference levels: Head (body part), From Corner (play pattern), Central (shot position),
False (first-time shot), Drawing (match state). Significance level: alpha = 0.05.

---

## Key Results

### Shot Location Effects

| Variable              | Odds Ratio | Interpretation                                    |
|-----------------------|------------|---------------------------------------------------|
| Distance from goal    | 0.886      | Each additional yard reduces scoring odds by 11.4% |
| Wide shot vs central  | 0.683      | Wide shots have 31.7% lower odds of scoring        |

### Match State Summary

| Match State | Total Shots | Goals | Goal Rate | Mean xG | Mean Distance |
|-------------|-------------|-------|-----------|---------|---------------|
| Drawing     | 1,250       | 128   | 10.2%     | 0.101   | 19.2 yds      |
| Losing      | 703         | 68    | 9.7%      | 0.109   | 19.4 yds      |
| Winning     | 830         | 124   | 14.9%     | 0.131   | 18.4 yds      |

Winning teams had 35.5% higher odds of scoring compared to drawing teams
(OR = 1.355, p = 0.037), after controlling for distance, body part, play pattern,
and shot position.

### Model Performance — Model 5

| Metric      | Value  |
|-------------|--------|
| Accuracy    | 89.1%  |
| Sensitivity | 10.3%  |
| Specificity | 99.4%  |
| Precision   | 67.3%  |

High specificity and low sensitivity is expected for rare-event prediction,
as goals represent only 11.5% of all shots in the dataset.

---

## How to Run

### Requirements

- R version 4.3 or higher
- The following packages:

```r
install.packages(c("tidyverse", "jsonlite", "ggrepel"))
```

### Reproduction Steps

```bash
# 1. Clone this repository
git clone https://github.com/yourusername/soccer-shot-analysis.git
cd soccer-shot-analysis

# 2. Add StatsBomb event data
#    Download from https://github.com/statsbomb/open-data
#    Place the events/ folder in the project root

# 3. Run scripts in order
Rscript 01_data_prep_final_project_actual.R
Rscript 02_descriptive_analysis_final_project.R
Rscript 03_logistic_regression.R
```

All output CSVs and plots will be generated in the working directory and match
the files included in this repository.

---

## Variable Dictionary

| Variable               | Type    | Description                                               |
|------------------------|---------|-----------------------------------------------------------|
| source_file            | String  | Match JSON filename                                       |
| team                   | Factor  | Shooting team name                                        |
| minute / second        | Integer | Time of shot                                              |
| x_coord / y_coord      | Float   | Shot location in StatsBomb coordinate system              |
| distance_from_goal     | Float   | Euclidean distance to goal center (yards)                 |
| xg                     | Float   | StatsBomb expected goals value                            |
| Goal                   | Integer | Binary outcome: 1 = Goal, 0 = No Goal                    |
| body_part              | Factor  | Right Foot / Left Foot / Head                             |
| play_pattern           | Factor  | How possession began before the shot                      |
| first_time             | Factor  | Whether the shot was a first-touch attempt                |
| match_state            | Factor  | Drawing / Losing / Winning (reference level = Drawing)    |
| central_shot           | Factor  | Central (y: 36–44 yards) or Wide                          |
| team_score             | Integer | Shooting team's score at time of shot                     |
| opponent_score         | Integer | Opposing team's score at time of shot                     |

---

## Limitations

- The 500-match sample may not fully represent all leagues, seasons, or playing styles
- The analysis cannot establish causation — match state effects may reflect underlying
  team quality rather than a direct effect of game context
- The binary outcome (goal / no goal) does not account for goalkeeper positioning,
  shot placement, or defensive pressure
- Score margin is not considered; a team losing 1-0 likely behaves differently
  from one losing 3-0

---

## Future Directions

- Expand the sample across more leagues, seasons, and competitions
- Incorporate score margin and time remaining as finer-grained match state variables
- Analyze passing sequences and defensive structure leading up to each shot
- Add tracking data on goalkeeper positioning and defensive pressure at the moment of shot

---

## References

StatsBomb Open Data Repository
https://github.com/statsbomb/open-data

StatsBomb Data Specification and Documentation
https://github.com/statsbomb/statsbombpy

---

## Contact

Pranav Kapoor
MGMT 4270 — Introduction to Data Science
