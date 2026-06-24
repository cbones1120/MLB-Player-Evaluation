# MLB Player Evaluation

Predicting future offensive Runs Above Average (RAA) for MLB players using an ensemble of machine learning models trained on Lahman Baseball Database statistics.

## Overview

This project builds a multi-model ensemble (linear regression, gradient boosted trees, and random forest) to predict offensive RAA for players under age 30, projecting performance from 2018 through 2027. Rather than predicting WAR directly, the model focuses on offensive RAA, which allows for more accurate and customizable projections — a GM can combine predicted offensive RAA with their own defensive assumptions to derive a full WAR estimate.

Positional breakdown percentages are also included in the output, enabling position-specific filtering and defensive RAA calculation for players of interest.

## Repository Structure

| File | Description |
|------|-------------|
| `Future_RAA_MLB.R` | Main analysis script — data prep, modeling, and projections |
| `Batting.csv` | Lahman batting statistics |
| `Appearances.csv` | Lahman appearance/positional data |
| `People.csv` | Lahman player biographical data |
| `historic_prospect_ranking_with_Lahman_ID.xlsx` | Historical prospect rankings linked to Lahman IDs |

## Requirements

**R packages:**
```r
install.packages(c(
  "tidyverse", "ggplot2", "gganimate", "broom",
  "lfe", "tidyr", "dplyr", "radiant",
  "xgboost", "caret", "superml"
))
```

## Setup

1. Clone the repository
2. Open `Future_RAA_MLB.R` and update the two path variables at the top:

```r
DATA_DIR   <- "path/to/your/data/folder"
OUTPUT_DIR <- "path/to/your/output/folder"
```

3. Ensure all data files are in `DATA_DIR`
4. Run the script top to bottom in RStudio or via `Rscript Future_RAA_MLB.R`

## Methodology

**Feature engineering:** Historical cumulative HR%, K%, and BB% (excluding current season to prevent data leakage), up to 3 years of lagged offensive RAA, age, and best prospect ranking.

**Models trained:** Linear regression, gradient boosted trees (XGBoost via radiant), decision tree, and random forest. An ensemble average of the top three outperforms any individual model on the test set.

**Projections:** Starting from 2017 season data, the script iterates forward year by year through 2027, predicting each player's next-season offensive RAA while they remain under age 30.

**Outputs:**
- `Final_Predictions.csv` — projected total and average offensive RAA per player (2018–2027) with career positional percentages
- `Top_20_Disappointments.csv` — players whose actual RAA fell furthest below model expectations

## Notes

- Minimum 100 plate appearances required per season to be included
- Players without a prospect ranking are assigned a rank of 101 (one below the lowest ranked prospect)
- Defensive RAA is not predicted directly due to limited predictive power from position-only data; the positional percentage columns in the output allow a GM to calculate it manually
