#-------------------------#
#-------------------------#
# INTRODUCTION
#-------------------------#
#-------------------------#

# Before I dive into the code I want to clarify my methodology and my choices. I know the task is to predict WAR for a GM, however I am deciding to predict offensive RAA instead. The reason behind this is because of how we are defining defensive RAA. Since defensive RAA is only based on the position a player played, and not on any defensive metrics, I don't believe there is any predictive power in predicting defensive RAA. Because of the data we are using it is also impossible to determine if lack of playing time is due to injury, lack of performance, late call up, or any other reason. It is also impossible to predict future injuries, which would cause a decrease in defensive playing time. Along with this most players have played multiple positions and the Orioles may want to play certain players at only one of these positions, which would affect their defensive RAA. There are possible solutions to this, or partial solutions. One is that I would find the percentage that players play at each position throughout their career, then multiply by the average games started by the player in their career. Again this could be biased by injuries, or if the player has only played one year and had a late call up. Similarly I could multiply the percentages by a set amount of games played for all players, such as 150 games for all players except maybe catchers at half that. This would assume the same level of playing time for all players. These solutions are ones that I would not use without discussing with the GM first, so for this project I elected to focus on offensive RAA only.
#
# However, for this project I have included the percentage played at each position in each player's career. This will allow to filter by position when looking at offensive RAA and compare players at certain positions of interest. Also only predicting offensive RAA will allow the GM to better personalize WAR predictions by selecting the percentage that a player of interest plays at each position. Then the defensive RAA can be better predicted for the player of interest and combined with the predicted offensive RAA to get a predicted WAR. Say a player of interest is expected to play 100% at CF, we can then calculate the defensive RAA for that player and then find their overall predicted WAR.

#------------------#
library(tidyverse)
library(gganimate)
library(ggplot2)
library(broom)
library(lfe)
library(tidyr)
library(radiant)
library(dplyr)
library(xgboost)
library(caret)
library(superml)
library(torch)
library(luz)
library(yardstick)
#------------------#

#-------------------------#
# Load data
#-------------------------#

# Update these paths if re-running on a different machine
DATA_DIR   <- "C:/Users/Connor/Downloads"
OUTPUT_DIR <- "C:/Users/Connor/Documents/Orioles Interview"

Batting <- readr::read_csv(file.path(DATA_DIR, "Batting.csv"), n_max = Inf) %>%
  fix_names() %>%
  to_fct()

Appearances <- readr::read_csv(file.path(DATA_DIR, "Appearances.csv"), n_max = Inf) %>%
  fix_names() %>%
  to_fct()

prospect_ranking <- readr::read_csv(file.path(DATA_DIR, "historic_prospect_ranking_with_Lahman_ID.csv"), n_max = Inf) %>%
  fix_names() %>%
  to_fct()

People <- readr::read_csv(file.path(DATA_DIR, "People.csv"), n_max = Inf) %>%
  fix_names() %>%
  to_fct()

#-------------------------#
# Filtering
#-------------------------#

Batting <- Batting %>%
  filter(yearID >= 1994, AB != 0)

Appearances <- Appearances %>%
  filter(yearID >= 1994, G_batting > 0)

#-------------------------#
# Creating Variables
#-------------------------#

# Sum games at each fielding position, then estimate games started per position
Appearances <- Appearances %>%
  mutate(
    G_field  = G_c + G_1b + G_2b + G_3b + G_ss + G_lf + G_cf + G_rf + G_dh,
    G_c_n    = (G_c  / G_field) * GS,
    G_1b_n   = (G_1b / G_field) * GS,
    G_2b_n   = (G_2b / G_field) * GS,
    G_3b_n   = (G_3b / G_field) * GS,
    G_ss_n   = (G_ss / G_field) * GS,
    G_lf_n   = (G_lf / G_field) * GS,
    G_cf_n   = (G_cf / G_field) * GS,
    G_rf_n   = (G_rf / G_field) * GS,
    G_dh_n   = (G_dh / G_field) * GS,
    RAA_D    = ( 9    / 150) * G_c_n  +
               ( 7    / 150) * G_ss_n +
               ( 3    / 150) * G_2b_n +
               ( 2.5  / 150) * G_cf_n +
               ( 2    / 150) * G_3b_n +
               (-7    / 150) * G_lf_n +
               (-7    / 150) * G_rf_n +
               (-9.5  / 150) * G_1b_n +
               (-15   / 150) * G_dh_n
  )

# Batting rate stats
Batting <- Batting %>%
  mutate(
    AVG = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB,
    OPS = OBP + SLG
  )

# League-average OPS per year (AL only)
OPS_weighted_avg <- Batting %>%
  filter(lgID == "AL") %>%
  group_by(yearID) %>%
  summarise(across(c(AB, H, BB, HBP, SF, X2B, X3B, HR), sum), .groups = "drop") %>%
  mutate(
    OBP     = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG     = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB,
    OPS_AVG = OBP + SLG
  ) %>%
  select(yearID, OPS_AVG)

# Aggregate multi-stint seasons into single rows per player-year
Batting_cmb <- Batting %>%
  group_by(playerID, yearID) %>%
  summarise(across(c(G, SO, AB, H, BB, HBP, SF, X2B, X3B, HR), sum), .groups = "drop") %>%
  mutate(
    AVG = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB,
    OPS = OBP + SLG
  )

# Attach league OPS, compute OPS_DIFF and RAA_O
Batting_OPS <- combine_data(
  x = Batting_cmb,
  y = OPS_weighted_avg,
  by = "yearID",
  add = "OPS_AVG"
) %>%
  mutate(
    OPS_DIFF = OPS - OPS_AVG,
    RAA_O    = (AB + BB + HBP + SF) * OPS_DIFF / 3.2135
  )

# Attach defensive RAA
Batting_OPS <- combine_data(
  x = Batting_OPS,
  y = Appearances,
  by = c("playerID", "yearID"),
  add = "RAA_D"
) %>%
  mutate(
    RAA = RAA_D + RAA_O,
    RAR = pmax(RAA + 20, 0),
    WAR = RAR / 10
  )

#-------------------------#
# Cumulative Sums (exclusive — exclude current year so we can predict it)
#-------------------------#

# Minimum 100 PA per season required
Batting_OPS <- Batting_OPS %>%
  filter(AB + BB + HBP + SF > 99)

# Save a copy before filtering RAA_O_Mean == 0 for use in final projections
Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS

Batting_OPS <- Batting_OPS %>%
  group_by(playerID) %>%
  mutate(
    HR_sum  = cumsum(HR)  - HR,
    AB_sum  = cumsum(AB)  - AB,
    SO_sum  = cumsum(SO)  - SO,
    BB_sum  = cumsum(BB)  - BB,
    HBP_sum = cumsum(HBP) - HBP,
    SF_sum  = cumsum(SF)  - SF
  ) %>%
  ungroup()

#-----------------------------------#
# Historical Percentages
#-----------------------------------#

PA_sum <- quote(AB_sum + BB_sum + HBP_sum + SF_sum)

Batting_OPS <- Batting_OPS %>%
  mutate(
    HR_Perc = HR_sum / (AB_sum + BB_sum + HBP_sum + SF_sum),
    SO_Perc = SO_sum / (AB_sum + BB_sum + HBP_sum + SF_sum),
    BB_Perc = BB_sum / (AB_sum + BB_sum + HBP_sum + SF_sum)
  )

#----------------------------#
# Adding Prospect Ratings
#----------------------------#

min_prospect <- prospect_ranking %>%
  group_by(lahman_id) %>%
  summarise(prospect_rank = min(prospect_rank), .groups = "drop") %>%
  dplyr::rename(playerID = lahman_id)

Batting_OPS_prospect <- combine_data(
  x    = Batting_OPS,
  y    = min_prospect,
  by   = "playerID",
  add  = "prospect_rank",
  type = "left_join"
) %>%
  mutate(prospect_rank = replace_na(prospect_rank, 101))

#------------------------------#
# Lagged Variables
#------------------------------#

Batting_OPS_prospect_lagged <- Batting_OPS_prospect %>%
  group_by(playerID) %>%
  mutate(
    laggedRAA_O   = lag(RAA_O),
    lagged_2_RAA_O = lag(laggedRAA_O),
    lagged_3_RAA_O = lag(lagged_2_RAA_O)
  ) %>%
  ungroup()

Batting_OPS_prospect_lagged$RAA_O_Mean <- rowMeans(
  Batting_OPS_prospect_lagged[, c("laggedRAA_O", "lagged_2_RAA_O", "lagged_3_RAA_O")],
  na.rm = TRUE
)

#-------------------------#
# Adding Age
#-------------------------#

Batting_Birth <- combine_data(
  x   = Batting,
  y   = People,
  by  = "playerID",
  add = c("nameFirst", "nameLast", "nameGiven", "birthYear", "birthMonth", "birthDay")
) %>%
  mutate(age = if_else(birthMonth < 7, yearID - birthYear, yearID - birthYear - 1L))

Batting_OPS_prospect_lagged_birth <- combine_data(
  x   = Batting_OPS_prospect_lagged,
  y   = Batting_Birth,
  by  = c("playerID", "yearID"),
  add = "age"
) %>%
  distinct()

#-------------------------#
# EDA Transformations
#-------------------------#

Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth %>%
  mutate(
    BB_Perc_sq       = BB_Perc^2,
    HR_Perc_sq       = HR_Perc^2,
    SO_Perc_sq       = SO_Perc^2,
    RAA_O_Mean_ln    = log(RAA_O_Mean),
    RAA_O_Mean_ln_sq = RAA_O_Mean_ln^2
  )

#-------------------------#
# Visualize
#-------------------------#

visualize(
  Batting_OPS_prospect_lagged_birth,
  xvar        = c("HR_Perc", "SO_Perc", "BB_Perc", "RAA_O_Mean"),
  yvar        = "RAA_O",
  type        = "scatter",
  nrobs       = -1,
  check       = c("line", "loess"),
  data_filter = "RAA_O_Mean != 0",
  custom      = FALSE
)

visualize(
  Batting_OPS_prospect_lagged_birth,
  xvar        = c("BB_Perc_sq", "HR_Perc_sq", "SO_Perc_sq", "RAA_O_Mean_ln", "RAA_O_Mean_ln_sq"),
  yvar        = "RAA_O",
  type        = "scatter",
  nrobs       = -1,
  check       = c("line", "loess"),
  data_filter = "RAA_O_Mean != 0",
  custom      = FALSE
)

visualize(
  Batting_OPS_prospect_lagged_birth,
  xvar        = c("age", "prospect_rank"),
  yvar        = "RAA_O",
  type        = "scatter",
  nrobs       = -1,
  check       = c("line", "loess"),
  data_filter = "RAA_O_Mean != 0",
  custom      = FALSE
)

#-------------------------#
# Interpretation
#-------------------------#

# Age and prospect_rank have mild negative correlations with RAA_O (expected).
# RAA_O_Mean has the steepest slope — strongest predictor.
# HR_Perc fits better linearly than squared.
# SO_Perc_sq fits better than SO_Perc: strikeout rate has increasing marginal drag.
# BB_Perc_sq fits better than BB_Perc: walk rate has increasing marginal benefit.

#-------------------------#
# Training / Test Split
#-------------------------#

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth
Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth %>%
  filter(RAA_O_Mean != 0)

Batting_OPS_prospect_lagged_birth <- mutate(
  Batting_OPS_prospect_lagged_birth,
  training = make_train(0.7, n_obs(Batting_OPS_prospect_lagged_birth$playerID), seed = 1234)
)

#-------------------------#
# Model Variables
#-------------------------#

EVARS <- c("HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "SO_Perc_sq", "BB_Perc_sq")

#-------------------------#
# Linear Model
#-------------------------#

result_reg <- regress(
  Batting_OPS_prospect_lagged_birth,
  rvar        = "RAA_O",
  evar        = EVARS,
  int         = "prospect_rank:age",
  data_filter = "training == 1"
)
summary(result_reg, sum_check = c("rmse", "vif"))
plot(result_reg, plots = "dashboard", lines = "line", nrobs = 1000, custom = FALSE)

pred <- predict(result_reg, pred_data = Batting_OPS_prospect_lagged_birth)
Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_reg")

#---------------------------------#
# Standardized Linear Regression
#---------------------------------#

result <- regress(
  Batting_OPS_prospect_lagged_birth,
  rvar        = "RAA_O",
  evar        = EVARS,
  int         = "prospect_rank:age",
  check       = "standardize",
  data_filter = "RAA_O_Mean != 0 & training == 1"
)
summary(result, sum_check = c("rmse", "vif"))
plot(result, plots = "coef", lines = "line", custom = FALSE)

# Grid search for optimal XGBoost hyperparameters
X_Y_train <- Batting_OPS_prospect_lagged_birth %>%
  filter(training == 1) %>%
  select(all_of(c(EVARS, "RAA_O")))

clf <- XGBTrainer$new()
gst <- GridSearchCV$new(
  trainer    = clf,
  parameters = list(n_estimators = c(100, 200, 300, 400, 500), max_depth = c(1, 3, 5)),
  n_folds    = 4,
  scoring    = c("accuracy", "auc")
)
# WARNING: may take a couple of minutes
gst$fit(X_Y_train, "RAA_O")
gst$best_iteration()

#----------------------------------#
# Gradient Boosted Model
#----------------------------------#

result_gbt <- gbt(
  Batting_OPS_prospect_lagged_birth,
  rvar                 = "RAA_O",
  evar                 = EVARS,
  type                 = "regression",
  max_depth            = 1,
  early_stopping_rounds = 3,
  seed                 = 1234,
  data_filter          = "training == 1"
)
summary(result_gbt, prn = TRUE)
plot(result_gbt, plots = "dashboard", nrobs = 1000, custom = FALSE)

pred <- predict(result_gbt, pred_data = Batting_OPS_prospect_lagged_birth)
Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_gbt")

#----------------------------------#
# Decision Tree Model
#----------------------------------#

result_crtree <- crtree(
  Batting_OPS_prospect_lagged_birth,
  rvar        = "RAA_O",
  evar        = EVARS,
  type        = "regression",
  pcp         = 0,
  data_filter = "training == 1"
)
summary(result_crtree, prn = TRUE)
plot(result_crtree, plots = "prune", custom = FALSE)

# Re-run with optimal node count from pruning plot
result_crtree <- crtree(
  Batting_OPS_prospect_lagged_birth,
  rvar        = "RAA_O",
  evar        = EVARS,
  type        = "regression",
  pcp         = 0,
  nodes       = 20,
  data_filter = "training == 1"
)
summary(result_crtree, prn = TRUE)

pred <- predict(result_crtree, pred_data = Batting_OPS_prospect_lagged_birth)
Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_crtree")

#----------------------------------#
# Random Forest Model
#----------------------------------#

result_rforest <- rforest(
  Batting_OPS_prospect_lagged_birth,
  rvar        = "RAA_O",
  evar        = EVARS,
  type        = "regression",
  mtry        = 1,
  seed        = 1234,
  data_filter = "training == 1"
)
summary(result_rforest)
plot(result_rforest, plots = "dashboard", nrobs = 1000, custom = FALSE)

pred <- predict(result_rforest, pred_data = Batting_OPS_prospect_lagged_birth, OOB = FALSE)
Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_rf")

#-------------------------#
# Compare Models
#-------------------------#

result <- evalreg(
  Batting_OPS_prospect_lagged_birth,
  pred        = c("pred_reg", "pred_gbt", "pred_crtree", "pred_rf"),
  rvar        = "RAA_O",
  train       = "Test",
  data_filter = "training == 1"
)
summary(result)
plot(result, custom = FALSE)

# Decision tree performs worst. Linear regression and random forest are close;
# random forest slightly outperforms. Averaging the top three is tested next.

#-------------------------#
# Ensemble (average of top 3)
#-------------------------#

Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth %>%
  mutate(pred_avg = (pred_reg + pred_gbt + pred_rf) / 3)

result <- evalreg(
  Batting_OPS_prospect_lagged_birth,
  pred        = c("pred_reg", "pred_gbt", "pred_crtree", "pred_rf", "pred_avg"),
  rvar        = "RAA_O",
  train       = "Test",
  data_filter = "training == 1"
)
summary(result)
plot(result, custom = FALSE)

# Ensemble (pred_avg) beats any single model — used for all projections below.

# Biggest disappointments: players whose actual RAA_O fell furthest below prediction
Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth %>%
  mutate(RAA_O_Diff = RAA_O - pred_avg)

Top_20_Disappointments <- Batting_OPS_prospect_lagged_birth %>%
  slice_min(RAA_O_Diff, n = 20)

#-------------------------#
# Prepare Inclusive Cumulative Stats for Projection Base
#-------------------------#

# For the projection starting point we include the current year (inclusive cumsum),
# since 2017 is historical — we know those stats in full.
Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>%
  group_by(playerID) %>%
  mutate(
    HR_sum  = cumsum(HR),
    AB_sum  = cumsum(AB),
    SO_sum  = cumsum(SO),
    BB_sum  = cumsum(BB),
    HBP_sum = cumsum(HBP),
    SF_sum  = cumsum(SF)
  ) %>%
  ungroup() %>%
  mutate(
    HR_Perc  = HR_sum  / (AB_sum + BB_sum + HBP_sum + SF_sum),
    SO_Perc  = SO_sum  / (AB_sum + BB_sum + HBP_sum + SF_sum),
    BB_Perc  = BB_sum  / (AB_sum + BB_sum + HBP_sum + SF_sum),
    BB_Perc_sq = BB_Perc^2,
    SO_Perc_sq = SO_Perc^2
  )

#-------------------------#
# Helper: predict one year forward
#-------------------------#

predict_next_year <- function(df) {
  df <- df %>%
    filter(age < 30) %>%
    select(playerID, age, prospect_rank, RAA_O_pred,
           laggedRAA_O, lagged_2_RAA_O,
           HR_Perc, SO_Perc_sq, BB_Perc_sq) %>%
    dplyr::rename(
      laggedRAA_O    = RAA_O_pred,
      lagged_2_RAA_O = laggedRAA_O,
      lagged_3_RAA_O = lagged_2_RAA_O
    ) %>%
    mutate(
      age        = age + 1,
      RAA_O_Mean = rowMeans(cbind(laggedRAA_O, lagged_2_RAA_O, lagged_3_RAA_O), na.rm = TRUE)
    )

  df$pred_reg <- predict(result_reg,    pred_data = df)$Prediction
  df$pred_gbt <- predict(result_gbt,    pred_data = df)$Prediction
  df$pred_rf  <- predict(result_rforest, pred_data = df)$Prediction

  df %>% mutate(RAA_O_pred = (pred_reg + pred_gbt + pred_rf) / 3)
}

#-------------------------#
# Creating 2018-2027 Projections
#-------------------------#

# Seed: 2017 players under age 30
Pred_2018 <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>%
  filter(age < 30, yearID == 2017) %>%
  select(playerID, age, prospect_rank, RAA_O, laggedRAA_O, lagged_2_RAA_O,
         HR_Perc, SO_Perc_sq, BB_Perc_sq) %>%
  dplyr::rename(
    laggedRAA_O    = RAA_O,
    lagged_2_RAA_O = laggedRAA_O,
    lagged_3_RAA_O = lagged_2_RAA_O
  ) %>%
  mutate(
    age        = age + 1,
    RAA_O_Mean = rowMeans(cbind(laggedRAA_O, lagged_2_RAA_O, lagged_3_RAA_O), na.rm = TRUE)
  )

Pred_2018$pred_reg <- predict(result_reg,    pred_data = Pred_2018)$Prediction
Pred_2018$pred_gbt <- predict(result_gbt,    pred_data = Pred_2018)$Prediction
Pred_2018$pred_rf  <- predict(result_rforest, pred_data = Pred_2018)$Prediction
Pred_2018 <- Pred_2018 %>% mutate(RAA_O_pred = (pred_reg + pred_gbt + pred_rf) / 3)

# Roll forward year by year through 2027
projection_years <- list(Pred_2018)
for (i in seq_len(9)) {  # 2019 through 2027
  projection_years[[i + 1]] <- predict_next_year(projection_years[[i]])
}

# Name each element for clarity
names(projection_years) <- paste0("Pred_", 2018:2027)

# Combine all projection years
All_years <- bind_rows(projection_years)

# After 2027 no players who started in our data are still under 30.

#-------------------------#
# Summarise Projections
#-------------------------#

Final_Predictions <- All_years %>%
  group_by(playerID) %>%
  summarise(
    Total_RAA_O   = sum(RAA_O_pred),
    Average_RAA_O = mean(RAA_O_pred),
    Number_of_Years = n(),
    .groups = "drop"
  )

Final_Predictions <- combine_data(
  x   = Final_Predictions,
  y   = People,
  by  = "playerID",
  add = c("nameFirst", "nameLast", "nameGiven")
) %>%
  select(playerID, nameFirst, nameLast, nameGiven, Total_RAA_O, Average_RAA_O, Number_of_Years)

# Career positional percentages
Appearances_combine <- Appearances %>%
  group_by(playerID) %>%
  summarise(across(c(GS, G_c_n, G_1b_n, G_2b_n, G_3b_n, G_ss_n,
                     G_lf_n, G_cf_n, G_rf_n, G_dh_n), sum),
            .groups = "drop") %>%
  mutate(
    perc_c  = G_c_n  / GS,
    perc_1b = G_1b_n / GS,
    perc_2b = G_2b_n / GS,
    perc_3b = G_3b_n / GS,
    perc_ss = G_ss_n / GS,
    perc_lf = G_lf_n / GS,
    perc_cf = G_cf_n / GS,
    perc_rf = G_rf_n / GS,
    perc_dh = G_dh_n / GS
  )

Final_Predictions <- combine_data(
  x   = Final_Predictions,
  y   = Appearances_combine,
  by  = "playerID",
  add = c("perc_c", "perc_1b", "perc_2b", "perc_3b", "perc_ss",
          "perc_lf", "perc_cf", "perc_rf", "perc_dh")
)

#-------------------------#
# Write Outputs
#-------------------------#

write.csv(Final_Predictions,
          file.path(OUTPUT_DIR, "Final_Predictions.csv"),
          row.names = FALSE)

Top_20_Disappointments <- combine_data(
  x   = Top_20_Disappointments,
  y   = People,
  by  = "playerID",
  add = c("nameFirst", "nameLast", "nameGiven")
)

write.csv(Top_20_Disappointments,
          file.path(OUTPUT_DIR, "Top_20_Disappointments.csv"),
          row.names = FALSE)

#-------------------------#
#-------------------------#
# Conclusion
#-------------------------#
#-------------------------#

# There are definitely some strengths and weaknesses to my result. I believe by excluding defensive RAA my predictions are more accurate and can be more easily customized to players of interest. However, by excluding defensive RAA I could not predict WAR, but instead offensive RAA. I also include multiple lagged data points which makes the model more accurate by taking multiple years into consideration which can help prevent outliers from drastically affecting results. However, this makes players with less data harder to predict and may make their predictions less accurate. Overall with the fact that baseball is extremely variable at its core, I believe my model does a good job in trying to predict the parts that aren't as variable. In using my predictions I recommend analyzing players of interest based on predicted offensive RAA or average RAA, then looking at the percentage played at each position to find players in the positions of interest with the highest RAA. After the WAR can be calculated by selecting the number of games played at each position and combined with the predicted offensive RAA to more accurately predict WAR.
