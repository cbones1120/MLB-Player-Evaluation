#-------------------------#
#-------------------------#
# INTODUCTION
#-------------------------#
#-------------------------#

# Before I dive into the code I want to clarify my methodology and my choices. I know the task is to predict WAR for a GM, however I am deciding to predict offensive RAA instead. The reason behind this is because of how we are defining defensive RAA. Since defensive RAA is only based on the position a player played, and not on any defensive metrics, I don’t believe there is any predictive power in predicting defensive RAA. Because of the data we are using it is also impossible to determine if lack of playing time is due to injury, lack of performance, late call up, or any other reason. It is also impossible to predict future injuries, which would cause a decrease in defensive playing time. Along with this most players have played multiple positions and the Orioles may want to play certain players at only one of these positions, which would affect their defensive RAA. There are possible solutions to this, or partial solutions. One is that I would find the percentage that players play at each position throughout their career, then multiply by the average games started by the player in their career. Again this could be biased by injuries, or if the player has only played one year and had a late call up. Similarly I could multiply the percentages by a set amount of games played for all players, such as 150 games for all players except maybe catchers at half that. This would assume the same level of playing time for all players. These solutions are ones that I would not use without discussing with the GM first, so for this project I elected to focus on offensive RAA only. 
# 
# However, for this project I have included the percentage played at each position in each player’s career. This will allow to filter by position when looking at offensive RAA and compare players at certain positions of interest. Also only predicting offensive RAA will allow the GM to better personalize WAR predictions by selecting the percentage that a player of interest plays at each position. Then the defensive RAA can be better predicted for the player of interest and combined with the predicted offensive RAA to get a predicted WAR. Say a player of interest is expected to play 100% at CF, we can then calculate the defensive RAA for that player and then find their overall predicted WAR.


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
#------------------#

#-------------------------#
# Load data
#-------------------------#

# Select location of the file if re-running

Batting <- readr::read_csv("/mnt/c/Users/Connor/Downloads/Batting.csv", n_max = Inf) %>%
  fix_names() %>%
  to_fct()

Appearances <- readr::read_csv("/mnt/c/Users/Connor/Downloads/Appearances.csv", n_max = Inf) %>%
  fix_names() %>%
  to_fct()

prospect_ranking <- readr::read_csv("/mnt/c/Users/Connor/Downloads/historic_prospect_ranking_with_Lahman_ID.csv", n_max = Inf) %>%
  fix_names() %>%
  to_fct()

People <- readr::read_csv("/mnt/c/Users/Connor/Downloads/People.csv", n_max = Inf) %>%
  fix_names() %>%
  to_fct()

#-------------------------#
# Filtering
#-------------------------#

#filters data to only include years 1994 and on and excludes anyone without ABs
Batting <- Batting %>%
  filter(yearID>=1994, AB != 0)

Appearances <- Appearances %>%
  filter(yearID>=1994, G_batting>0)

#-------------------------#
# Creating Variables
#-------------------------#

## create new variable to sum up games played at each position
Appearances <- mutate(Appearances, G_field = G_c+G_1b+G_2b+G_3b+G_ss+G_lf+G_cf+G_rf+G_dh)

## creating new variables for GS at each position:
Appearances <- mutate(Appearances, G_c_n = (G_c/G_field)*GS)

Appearances <- mutate(Appearances, G_1b_n = (G_1b/G_field)*GS)

Appearances <- mutate(Appearances, G_2b_n = (G_2b/G_field)*GS)

Appearances <- mutate(Appearances, G_3b_n = (G_3b/G_field)*GS)

Appearances <- mutate(Appearances, G_ss_n = (G_ss/G_field)*GS)

Appearances <- mutate(Appearances, G_lf_n = (G_lf/G_field)*GS)

Appearances <- mutate(Appearances, G_cf_n = (G_cf/G_field)*GS)

Appearances <- mutate(Appearances, G_rf_n = (G_rf/G_field)*GS)

Appearances <- mutate(Appearances, G_dh_n = (G_dh/G_field)*GS)

## create new variable for Defensive RAA
Appearances <- mutate(Appearances, RAA_D = 9/150*(G_c_n)+(7/150)*(G_ss_n)+(3/150)*(G_2b_n)+(2.5/150)*(G_cf_n)+(2/150)*(G_3b_n)-(7/150)*(G_lf_n)-(7/150)*(G_rf_n)-(9.5/150)*(G_1b_n)-(15/150)*(G_dh_n))

# Creating new variables for AVG, OBP, SLG, and OPS:
Batting <- mutate(Batting, AVG = H/AB)

## create new variable(s)
Batting <- mutate(Batting, OBP = (H+BB+HBP)/(AB+BB+HBP+SF))

## create new variable(s)
Batting <- mutate(Batting, SLG = ((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)

## create new variable(s)
Batting <- mutate(Batting, OPS = OBP + SLG)

## Filtering out NL for future League OPS Calculations
Batting_AL <- Batting %>% filter(lgID == 'AL')

# Aggregating AB,H,BB,HBP,SF,X2B,X3B,HR per year in order to calculate league average OPS per year
OPS_weighted_avg <- aggregate(cbind(AB,H,BB,HBP,SF,X2B,X3B,HR)~yearID, data = Batting_AL, FUN = sum ) 

# Creating new variables for AVG, OBP, SLG, and OPS per year:
OPS_weighted_avg <- mutate(OPS_weighted_avg, AVG = H/AB)

OPS_weighted_avg <- mutate(OPS_weighted_avg, OBP = (H+BB+HBP)/(AB+BB+HBP+SF))

OPS_weighted_avg <- mutate(OPS_weighted_avg, SLG = ((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)

OPS_weighted_avg <- mutate(OPS_weighted_avg, OPS = OBP + SLG)            

## Renaming OPS to OPS_AVG to represent that this is the Avergae OPS for that year
OPS_weighted_avg <- dplyr::rename(OPS_weighted_avg, OPS_AVG = OPS)

## Combining multiple stints in a year to get a players aggregated data for the year
Batting_cmb <- aggregate(cbind(G,SO,AB,H,BB,HBP,SF,X2B,X3B,HR)~playerID+yearID, data = Batting, FUN = sum )

Batting_cmb <- mutate(Batting_cmb, AVG = H/AB)

Batting_cmb <- mutate(Batting_cmb, OBP = (H+BB+HBP)/(AB+BB+HBP+SF))

Batting_cmb <- mutate(Batting_cmb, SLG = ((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)

Batting_cmb <- mutate(Batting_cmb, OPS = OBP + SLG) 


## Combining the table with League Average OPS per year with the rest of the batting data
Batting_OPS <- combine_data(
  x = Batting_cmb, 
  y = OPS_weighted_avg, 
  by = "yearID", 
  add = "OPS_AVG", 
)

## Creating new variable that is the difference between a players OPS and league average OPS
Batting_OPS <- mutate(Batting_OPS, OPS_DIFF = OPS-OPS_AVG)

## Creating new variable for Offensive RAA
Batting_OPS <- mutate(Batting_OPS, RAA_O = (AB+BB+HBP+SF)*OPS_DIFF/3.2135)

# Putting defensive RAA in the Batting_OPS data frame

Batting_OPS <- combine_data(
  x = Batting_OPS, 
  y = Appearances, 
  by = c("playerID","yearID"), 
  add = "RAA_D", 
)

# Creating RAA, RAR, and WAR variables

Batting_OPS$RAA <- Batting_OPS$RAA_D+Batting_OPS$RAA_O

Batting_OPS$RAR <- Batting_OPS$RAA + 20

Batting_OPS$RAR[Batting_OPS$RAR<0]<-0

Batting_OPS$WAR <- Batting_OPS$RAR/10

#-------------------------#
# Cumulative Sums
#-------------------------#

## Creating cumulative sums of counting stats in order to calculate HR%, K%, and BB%

## Note that I am subtracting current years counting stats in order to make it a historical cumulative sum. This is because if we want to predict current year WAR we will not know the current year's counting stats so we must exclude them. I will also filter by 100 PAs because of the variance in baseball I do not want to include any seasons that do not represent the players true abilities. I am assuming at least 100 PAs to represent a player's true abilities that year, so anything less will not produce accurate results. This number can be changed if we believe a different number of PAs represents their true ability.

Batting_OPS <- Batting_OPS %>% filter(AB+BB+HBP+SF>99)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( HR_sum = cumsum(HR)-HR)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( AB_sum = cumsum(AB)-AB)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( SO_sum = cumsum(SO)-SO)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( BB_sum = cumsum(BB)-BB)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( HBP_sum = cumsum(HBP)-HBP)

Batting_OPS <- Batting_OPS %>% group_by(playerID) %>%
  dplyr::mutate( SF_sum = cumsum(SF)-SF)

#-----------------------------------#
# Creating Historical Percentages
#-----------------------------------#

## I am taking historical cumulative sums of HR, Ks, and BBs and dividing them by PAs to get HR%, K%, and BB%
## HR_Perc is homerun percent which is HRs/AB
## SO_Perc is strike out percent which is SOs/AB
## BB_Perc is walk percent which is BBs/AB

Batting_OPS <- mutate(Batting_OPS, HR_Perc = HR_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))

Batting_OPS <- mutate(Batting_OPS, SO_Perc = SO_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))

Batting_OPS <- mutate(Batting_OPS, BB_Perc = BB_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))


#----------------------------#
# Adding Prospect Ratings
#----------------------------#

## Taking the lowest prospect ranking for each player ranked
min_prospect <- aggregate(prospect_rank~lahman_id, data = prospect_ranking, FUN = min)

## Changing the name of lahman_id to playerID making table joining easier
min_prospect <- dplyr::rename(min_prospect, playerID = lahman_id)

## Combing the prospect ranking with the batting data
Batting_OPS_prospect <- combine_data(
  x = Batting_OPS, 
  y = min_prospect, 
  by = "playerID", 
  add = "prospect_rank",
  type = "left_join"
)

## Changing any players prospect ranking to 101 if they do not have a ranking. 
## Since 100 is the highest ranking given I will assume all players without a ranking to be at 101. 

Batting_OPS_prospect <- Batting_OPS_prospect %>% dplyr::mutate(prospect_rank = replace_na(prospect_rank, 101))

#------------------------------#
# Creating Lagged Variables
#------------------------------#

## Creating lagged variables for offensive RAA
## The lag of RAA_O will take the previous years RAA_O and allow me to include prior years RAA_O for regression models.
## This will allow me to take prior data into consideration in determining future RAA_O.
## I will lag up to 3 years and I will later take the mean of these lagged variables to account for possible down years.

Batting_OPS_prospect_lagged <- Batting_OPS_prospect %>%                           
  group_by(playerID) %>%
  dplyr::mutate(laggedRAA_O = lag(RAA_O))

Batting_OPS_prospect_lagged <- Batting_OPS_prospect_lagged %>%                           
  group_by(playerID) %>%
  dplyr::mutate(lagged_2_RAA_O = lag(laggedRAA_O))

Batting_OPS_prospect_lagged <- Batting_OPS_prospect_lagged %>%                           
  group_by(playerID) %>%
  dplyr::mutate(lagged_3_RAA_O = lag(lagged_2_RAA_O))


## Taking the mean of the last two lagged RAA_O, if there is only one lagged then it just takes that one.
Batting_OPS_prospect_lagged$RAA_O_Mean = rowMeans(subset(Batting_OPS_prospect_lagged, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)



#-------------------------#
# Adding in Age
#-------------------------#

## Here I will add in an age variable to the data by taking the year they were born minus the year of the season.
## Note that if the player was born in July or later I will subtract a year from their age representing the assumption in the instructions.

Batting_Birth <- combine_data(
  x = Batting, 
  y = People, 
  by = "playerID", 
  add = c("nameFirst", "nameLast", "nameGiven", "birthYear", "birthMonth", "birthDay"), 
)

Batting_Birth <- mutate(Batting_Birth, age = case_when(birthMonth < 7 ~ yearID - birthYear, birthMonth > 6 ~ yearID - birthYear-1 ) )

Batting_OPS_prospect_lagged_birth <- combine_data(
  x= Batting_OPS_prospect_lagged,
  y= Batting_Birth,
  by= c("playerID","yearID"),
  add = "age"
)

Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth[!duplicated(Batting_OPS_prospect_lagged_birth), ]

## Now in the Batting_OPS_prospect_lagged_birth table we should have all the variables needed to try different regressions.
## The table is large but this is needed to try out different regressions.

#-------------------------#
# EDA
#-------------------------#

## The following will be a lot of Exploratory Data Analysis (EDA).
## I will try different transformations to see what data seems the most informative.

## transform variable squaring BB_Perc
Batting_OPS_prospect_lagged_birth <- mutate_ext(Batting_OPS_prospect_lagged_birth, .vars = vars(BB_Perc), .funs = square, .ext = "_sq")

## transform variable Squaring HR_Perc
Batting_OPS_prospect_lagged_birth <- mutate_ext(Batting_OPS_prospect_lagged_birth, .vars = vars(HR_Perc), .funs = square, .ext = "_sq")

## transform variable Squaring SO_Perc
Batting_OPS_prospect_lagged_birth <- mutate_ext(Batting_OPS_prospect_lagged_birth, .vars = vars(SO_Perc), .funs = square, .ext = "_sq")

## transform variable taking ln of RAA_O_Mean
Batting_OPS_prospect_lagged_birth <- mutate_ext(Batting_OPS_prospect_lagged_birth, .vars = vars(RAA_O_Mean), .funs = log, .ext = "_ln")

## transform variable squaring the ln of RAA_O_Mean
Batting_OPS_prospect_lagged_birth <- mutate_ext(Batting_OPS_prospect_lagged_birth, .vars = vars(RAA_O_Mean_ln), .funs = square, .ext = "_sq")

#-------------------------#
# Visualize
#-------------------------#

## I will create scatter plots to visualize the relationships between variables and RAA_O.
## I will put in a line of best fit and a loess line that moves more with the data.

visualize(
  Batting_OPS_prospect_lagged_birth, 
  xvar = c(
    "HR_Perc", "SO_Perc", "BB_Perc", "RAA_O_Mean"
  ), 
  yvar = "RAA_O", 
  type = "scatter", 
  nrobs = -1, 
  check = c("line", "loess"), 
  data_filter = "RAA_O_Mean !=0 ", 
  custom = FALSE
)

visualize(
  Batting_OPS_prospect_lagged_birth, 
  xvar = c(
    "BB_Perc_sq", "HR_Perc_sq", "SO_Perc_sq", "RAA_O_Mean_ln", "RAA_O_Mean_ln_sq"
  ), 
  yvar = "RAA_O", 
  type = "scatter", 
  nrobs = -1, 
  check = c("line", "loess"), 
  data_filter = "RAA_O_Mean !=0 ", 
  custom = FALSE
)

visualize(
  Batting_OPS_prospect_lagged_birth, 
  xvar = c(
    "age", "prospect_rank"
  ), 
  yvar = "RAA_O", 
  type = "scatter", 
  nrobs = -1, 
  check = c("line", "loess"), 
  data_filter = "RAA_O_Mean !=0 ", 
  custom = FALSE
)

#-------------------------#
# Interpretation
#-------------------------#

# Let's look at some of these interactions:

# It appears that both age and prospect rank have some sort of correlation with RAA_O, although neither seems to have a very strong affect. Both age and prospect rank have a negative slope, meaning as someone gets older or their a lower rated prospect, then their RAA_O tends to be lower. This is expected. RAA_O_Mean seems to have the highest slope, which means possibly the highest impact on RAA_O. Which again is expected. HR_Perc seems to have a better linear fit than the square of it. It also has a positive correlation, which is expected. SO_Perc_sq, the square of SO_Perc, seems to have a better linear fit than just SO_Perc. It also has a negative slope. This means that as SO rate increases RAA_O decreases which is expected. Further more because the square of the percentage seems like a better fit that means as SO_Perc increases the more it negatively affects RAA_O. BB_Perc_sq, the square of BB_Perc, seems to have a better linear fit than just BB_Perc. It has a positive correlation with RAA_O, as expected. And similarly to strike out percentage, as walks increase the positive affect on RAA_O increases as well.

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth
Batting_OPS_prospect_lagged_birth <- Batting_OPS_prospect_lagged_birth %>% filter(RAA_O_Mean !=0)
#-------------------------#
# Training/Test Split
#-------------------------#
Batting_OPS_prospect_lagged_birth <- mutate(Batting_OPS_prospect_lagged_birth, training = make_train(0.7, n_obs(Batting_OPS_prospect_lagged_birth$playerID),seed = 1234))

#-------------------------#
# Creating Linear Model
#-------------------------#
result_reg <- regress(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "SO_Perc_sq", 
    "BB_Perc_sq"
  ), 
  int = "prospect_rank:age", 
  data_filter = "training == 1"
)
summary(result_reg, sum_check = c("rmse", "vif"))

#-------------------------#
# Plot Residules
#-------------------------#

plot(result_reg, plots = "dashboard", lines = "line", nrobs = 1000, custom = FALSE)

#-------------------------#
# Create Predictions
#-------------------------#
#Predictions will be used to measure model performances

pred <- predict(result_reg, pred_data = Batting_OPS_prospect_lagged_birth)

Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_reg")

#---------------------------------#
# Standardized Linear Regression
#---------------------------------#

#By standardizing we can get a better understanding of which variables are most impactful.
# Standardizing gets rid of units and allows for direct comparison of the coefficients.

result <- regress(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "SO_Perc_sq", 
    "BB_Perc_sq"
  ), 
  int = "prospect_rank:age", 
  check = "standardize", 
  data_filter = "RAA_O_Mean !=0 & training == 1"
)
summary(result, sum_check = c("rmse", "vif"))
plot(result, plots = "coef", lines = "line", custom = FALSE)

# Creating separate data frames with the training data
X_train <- Batting_OPS_prospect_lagged_birth %>% filter(training==1) %>% select(c(
  "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "SO_Perc_sq", 
  "BB_Perc_sq"))
Y_train <- Batting_OPS_prospect_lagged_birth %>% filter(training==1) %>% select(c("RAA_O"))

X_Y_train <- Batting_OPS_prospect_lagged_birth %>% filter(training==1) %>% select(c(
  "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "SO_Perc_sq", 
  "BB_Perc_sq","RAA_O"))

# Here I will be using a grid search cross validation to help find the optimal paramaters for a gradient boosted regression
clf <- XGBTrainer$new()

gst <-GridSearchCV$new(trainer = clf,
                       parameters = list(n_estimators = c(100,200,300,400,500),
                                         max_depth = c(1,3,5)),
                       n_folds = 4,
                       scoring = c('accuracy','auc'))

# Fitting to my data to find optimalnumber of estimators and max depth for a gradiant boosted regression. 
# WARNING: May take a couple minutes to run

gst$fit(X_Y_train,"RAA_O")

# This shows the optimal number of estimators and max depth for a gradiant boosted regression

gst$best_iteration()

#----------------------------------#
# Creating Gradiant Boosted Model
#----------------------------------#
#This is the gradiant boosted regression with optimal parameters

result_gbt <- gbt(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "BB_Perc_sq", 
    "SO_Perc_sq"
  ), 
  type = "regression", 
  max_depth = 1, 
  early_stopping_rounds = 3, 
  seed = 1234, 
  data_filter = "training == 1"
)
summary(result_gbt, prn = TRUE)

#-------------------------#
# Plot Residuals
#-------------------------#
plot(result_gbt, plots = "dashboard", nrobs = 1000, custom = FALSE)

#-------------------------#
# Create Predictions
#-------------------------#
pred <- predict(result_gbt, pred_data = Batting_OPS_prospect_lagged_birth)

Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_gbt")


#----------------------------------#
# Creating Decision Tree Model
#----------------------------------#
result_crtree <- crtree(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "BB_Perc_sq", 
    "SO_Perc_sq"
  ), 
  type = "regression", 
  pcp = 0, 
  data_filter = "training == 1"
)
summary(result_crtree, prn = TRUE)
#Pruning shows the optimal number of nodes, so I will re-run the model with the optimal nodes
plot(result_crtree, plots = "prune", custom = FALSE)

# Re-running model with optimal nodes
result_crtree <- crtree(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "BB_Perc_sq", 
    "SO_Perc_sq"
  ), 
  type = "regression", 
  pcp = 0, 
  nodes = 20, 
  data_filter = "training == 1"
)
summary(result_crtree, prn = TRUE)

#-------------------------#
# Create Predictions
#-------------------------#
pred <- predict(result_crtree, pred_data = Batting_OPS_prospect_lagged_birth)

Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_crtree")

#----------------------------------#
# Creating Random Forrest Model
#----------------------------------#

result_rforest <- rforest(
  Batting_OPS_prospect_lagged_birth, 
  rvar = "RAA_O", 
  evar = c(
    "HR_Perc", "prospect_rank", "RAA_O_Mean", "age", "BB_Perc_sq", 
    "SO_Perc_sq"
  ), 
  type = "regression", 
  mtry = 1, 
  seed = 1234, 
  data_filter = "training == 1"
)
summary(result_rforest)

#-------------------------#
# Plot Residules
#-------------------------#

plot(result_rforest, plots = "dashboard", nrobs = 1000, custom = FALSE)

#-------------------------#
# Create Predictions
#-------------------------#

pred <- predict(result_rforest, pred_data = Batting_OPS_prospect_lagged_birth, OOB = FALSE)

Batting_OPS_prospect_lagged_birth <- store(Batting_OPS_prospect_lagged_birth, pred, name = "pred_rf")


#-------------------------#
# Comparing Models
#-------------------------#
result <- evalreg(
  Batting_OPS_prospect_lagged_birth, 
  pred = c("pred_reg", "pred_gbt", "pred_crtree", "pred_rf"), 
  rvar = "RAA_O", 
  train = "Test", 
  data_filter = "training == 1"
)
summary(result)
plot(result, custom = FALSE)

# I just compared all four model predictions on the test data in order to measure each models performance. In each model we are looking for a high r squared and low RMSE and MAE. From the comparisons it is clear that the decision tree performs the worst. The linear regression and random forest models performances are very similar, with the random forest slightly outperforming the linear regression.

#-------------------------#
# Averaging Models
#-------------------------#

# I will now try to average out the predictions for the 3 best performing models: linear regression, gradient boosted regression, and random forest.

# I will see if the models can combine to create a prediction that is better than any individual model.

Batting_OPS_prospect_lagged_birth <- mutate(Batting_OPS_prospect_lagged_birth, pred_avg = (pred_reg+pred_gbt+pred_rf)/3)



result <- evalreg(
  Batting_OPS_prospect_lagged_birth, 
  pred = c("pred_reg", "pred_gbt", "pred_crtree", "pred_rf", "pred_avg"), 
  rvar = "RAA_O", 
  train = "Test", 
  data_filter = "training == 1"
)
summary(result)
plot(result, custom = FALSE)

# From the results we can see that the average of the top three predictions does in fact create better predictions. To predict player's future performance I will use a combination of the 3 models.

# Now that we have predictions for players, I will calculate the biggest disappointments.

# Here I am creating the difference between actual and predicted offensive RAA.

Batting_OPS_prospect_lagged_birth$RAA_O_Diff <- Batting_OPS_prospect_lagged_birth$RAA_O - Batting_OPS_prospect_lagged_birth$pred_avg

# Here I am ordering by the biggest negative difference

Disappointments <- Batting_OPS_prospect_lagged_birth[order(Batting_OPS_prospect_lagged_birth$RAA_O_Diff,decreasing = F),]

# Here I am only taking the top 20 who had the biggest negative difference, I can change the number to include more or less if needed.

Top_20_Disappointments <- Disappointments[1:20,]


# Here I will be creating a data frame with missing RAA_O_mean because for predictions I want to include players who's first year was 2017. This is needed because during model testing I excluded any data with 0 or missing in RAA_O_mean. This would exclude any player whose first year was 2017. I will be re-creating HR_perc, SO_perc, and BB_perc for all players. Then I will create the square of SO_perc and BB_perc.

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( HR_sum = cumsum(HR))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( AB_sum = cumsum(AB))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( SO_sum = cumsum(SO))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( BB_sum = cumsum(BB))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( HBP_sum = cumsum(HBP))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% group_by(playerID) %>%
  dplyr::mutate( SF_sum = cumsum(SF))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- mutate(Batting_OPS_prospect_lagged_birth_w_RAA_O, HR_Perc = HR_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- mutate(Batting_OPS_prospect_lagged_birth_w_RAA_O, SO_Perc = SO_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))

Batting_OPS_prospect_lagged_birth_w_RAA_O <- mutate(Batting_OPS_prospect_lagged_birth_w_RAA_O, BB_Perc = BB_sum/(AB_sum+BB_sum+HBP_sum+SF_sum))

## transform variable squaring BB_Perc
Batting_OPS_prospect_lagged_birth_w_RAA_O$BB_Perc_sq <- Batting_OPS_prospect_lagged_birth_w_RAA_O$BB_Perc**2

## transform variable Squaring SO_Perc
Batting_OPS_prospect_lagged_birth_w_RAA_O$SO_Perc_sq <- Batting_OPS_prospect_lagged_birth_w_RAA_O$SO_Perc**2

#-------------------------#
# Creating Projections
#-------------------------#

# I will go year by year and select only players who are under 30 to predict next years RAA_O.
# Once I get to a year where all of the players are over 30 I will combine all of the data frames. In combining the data frames I will find combined WAR and WAR per year for all players.

# Creating 2018-2027 projections

#Selecting any player whose age < 30  during the 2017 year.

Pred_2018 <- Batting_OPS_prospect_lagged_birth_w_RAA_O %>% 
             filter(age<30 & yearID == 2017) %>% 
             select(c(playerID,age,prospect_rank, RAA_O, laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
             rename("laggedRAA_O"="RAA_O",
                    "lagged_2_RAA_O"="laggedRAA_O",
                    "lagged_3_RAA_O"="lagged_2_RAA_O")
# Increasing age by 1 
Pred_2018$age <- Pred_2018$age+1

# Recalculating RAA_O_Mean to include the previous 3 years
Pred_2018$RAA_O_Mean = rowMeans(subset(Pred_2018, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2018)
Pred_2018$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2018)
Pred_2018$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2018)
Pred_2018$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2018 <- mutate(Pred_2018, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

# Creating 2019 projections

# Selecting anyone who is still under 30 years old.

Pred_2019 <- Pred_2018 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2019$age <- Pred_2019$age+1

# Recalculating RAA_O_Mean to include predictions for 2018
Pred_2019$RAA_O_Mean = rowMeans(subset(Pred_2019, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2019)
Pred_2019$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2019)
Pred_2019$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2019)
Pred_2019$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2019 <- mutate(Pred_2019, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

# Creating 2020 projections

Pred_2020 <- Pred_2019 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2020$age <- Pred_2020$age+1

# Recalculating RAA_O_Mean
Pred_2020$RAA_O_Mean = rowMeans(subset(Pred_2020, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2020)
Pred_2020$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2020)
Pred_2020$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2020)
Pred_2020$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2020 <- mutate(Pred_2020, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

Pred_2021 <- Pred_2020 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2021$age <- Pred_2021$age+1

# Recalculating RAA_O_Mean
Pred_2021$RAA_O_Mean = rowMeans(subset(Pred_2021, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2021)
Pred_2021$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2021)
Pred_2021$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2021)
Pred_2021$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2021 <- mutate(Pred_2021, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2022 predictions

Pred_2022 <- Pred_2021 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2022$age <- Pred_2022$age+1

# Recalculating RAA_O_Mean
Pred_2022$RAA_O_Mean = rowMeans(subset(Pred_2022, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2022)
Pred_2022$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2022)
Pred_2022$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2022)
Pred_2022$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2022 <- mutate(Pred_2022, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2023 predictions

Pred_2023 <- Pred_2022 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2023$age <- Pred_2023$age+1

# Recalculating RAA_O_Mean
Pred_2023$RAA_O_Mean = rowMeans(subset(Pred_2023, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2023)
Pred_2023$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2023)
Pred_2023$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2023)
Pred_2023$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2023 <- mutate(Pred_2023, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2024 predictions

Pred_2024 <- Pred_2023 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2024$age <- Pred_2024$age+1

# Recalculating RAA_O_Mean
Pred_2024$RAA_O_Mean = rowMeans(subset(Pred_2024, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2024)
Pred_2024$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2024)
Pred_2024$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2024)
Pred_2024$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2024 <- mutate(Pred_2024, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2025 predictions

Pred_2025 <- Pred_2024 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2025$age <- Pred_2025$age+1

# Recalculating RAA_O_Mean
Pred_2025$RAA_O_Mean = rowMeans(subset(Pred_2025, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2025)
Pred_2025$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2025)
Pred_2025$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2025)
Pred_2025$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2025 <- mutate(Pred_2025, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2026 predictions

Pred_2026 <- Pred_2025 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2026$age <- Pred_2026$age+1

# Recalculating RAA_O_Mean
Pred_2026$RAA_O_Mean = rowMeans(subset(Pred_2026, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2026)
Pred_2026$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2026)
Pred_2026$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2026)
Pred_2026$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2026 <- mutate(Pred_2026, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

#Creating 2027 predictions

Pred_2027 <- Pred_2026 %>% 
  filter(age<30) %>% 
  select(c(playerID,age,prospect_rank, RAA_O_pred,laggedRAA_O, lagged_2_RAA_O,HR_Perc,SO_Perc_sq,BB_Perc_sq)) %>% 
  rename("laggedRAA_O"="RAA_O_pred",
         "lagged_2_RAA_O"="laggedRAA_O",
         "lagged_3_RAA_O"="lagged_2_RAA_O")

# Increasing age by 1 

Pred_2027$age <- Pred_2027$age+1

# Recalculating RAA_O_Mean
Pred_2027$RAA_O_Mean = rowMeans(subset(Pred_2027, select = c(laggedRAA_O, lagged_2_RAA_O,lagged_3_RAA_O)), na.rm = T)

# Creating predictions

pred <- predict(result_reg, pred_data = Pred_2027)
Pred_2027$pred_reg <- pred$Prediction

pred <- predict(result_gbt, pred_data = Pred_2027)
Pred_2027$pred_gbt <- pred$Prediction

pred <- predict(result_rforest, pred_data = Pred_2027)
Pred_2027$pred_rf <- pred$Prediction

# Averaging predictions an assigning it to RAA_O_pred
Pred_2027 <- mutate(Pred_2027, RAA_O_pred = (pred_reg+pred_gbt+pred_rf)/3)

# After 2027 there are no more players who are under the age of 30 that started in our data.

# Here I will combine all the years of predictions into one data frame

All_years <- rbind(Pred_2018,Pred_2019,Pred_2020,Pred_2021,Pred_2022,Pred_2023,Pred_2024,Pred_2025,Pred_2026,Pred_2027)

# Now that all the predictions are in one data frame I will calculate total offensive RAA, average RAA, and the number of years that player played before their age 31 season.

Final_Predictions <- All_years %>% group_by(playerID) %>% 
                                                    summarise(Total_RAA_O = sum(RAA_O_pred),
                                                              Average_RAA_O = mean(RAA_O_pred),
                                                              Number_of_Years = n())

# Including each player's names to the projections.

Final_Predictions <- combine_data(
  x = Final_Predictions, 
  y = People, 
  by = "playerID", 
  add = c("nameFirst","nameLast","nameGiven") 
)

# Re-ordering columns

Final_Predictions <- select(Final_Predictions, playerID, nameFirst, nameLast, nameGiven, Total_RAA_O, Average_RAA_O,Number_of_Years)

# Aggregating games started at each position and games started. This will allow me to calculate the percentages started at each position for each player.

Appearances_combine <- aggregate(cbind(GS,G_c_n,G_1b_n,G_2b_n,G_3b_n,G_ss_n,G_lf_n,G_cf_n,G_rf_n,G_dh_n)~playerID, data = Appearances, FUN = sum)

# Creating variables for percentages started at each position

Appearances_combine$perc_c <- Appearances_combine$G_c_n/Appearances_combine$GS

Appearances_combine$perc_1b <- Appearances_combine$G_1b_n/Appearances_combine$GS

Appearances_combine$perc_2b <- Appearances_combine$G_2b_n/Appearances_combine$GS

Appearances_combine$perc_3b <- Appearances_combine$G_3b_n/Appearances_combine$GS

Appearances_combine$perc_ss <- Appearances_combine$G_ss_n/Appearances_combine$GS

Appearances_combine$perc_lf <- Appearances_combine$G_lf_n/Appearances_combine$GS

Appearances_combine$perc_cf <- Appearances_combine$G_cf_n/Appearances_combine$GS

Appearances_combine$perc_rf <- Appearances_combine$G_rf_n/Appearances_combine$GS

Appearances_combine$perc_dh <- Appearances_combine$G_dh_n/Appearances_combine$GS

# Adding in the percentages started at each postition to the final predictions.

Final_Predictions <- combine_data(
  x = Final_Predictions, 
  y = Appearances_combine, 
  by = "playerID", 
  add = c("perc_c","perc_1b","perc_2b","perc_3b","perc_ss","perc_lf","perc_cf","perc_rf","perc_dh") 
)

# Writing the final predictions to file.
# Note if re-running this change the location of where to save the file.

write.csv(Final_Predictions,"/mnt/c/Users/Connor/Documents/Orioles Interview/Final_Predictions.csv", row.names = F)

# Adding in each player's name to the disappointments data frame

Top_20_Disappointments <- combine_data(
  x = Top_20_Disappointments, 
  y = People, 
  by = "playerID", 
  add = c("nameFirst","nameLast","nameGiven") 
)

# Writing the top 20 disappointments to file.
# Note if re-running this change the location of where to save the file.

write.csv(Top_20_Disappointments,"/mnt/c/Users/Connor/Documents/Orioles Interview/Top_20_Disappointments.csv", row.names = F)


#-------------------------#
#-------------------------#
# Conclusion
#-------------------------#
#-------------------------#

# There are definitely some strengths and weaknesses to my result. I believe by excluding defensive RAA my predictions are more accurate and can be more easily customized to players of interest. However, by excluding defensive RAA I could not predict WAR, but instead offensive RAA. I also include multiple lagged data points which makes the model more accurate by taking multiple years into consideration which can help prevent outliers from drastically affecting results. However, this makes players with less data harder to predict and may make their predictions less accurate. Overall with the fact that baseball is extremely variable at its core, I believe my model does a good job in trying to predict the parts that aren't as variable. In using my predictions I recommend analyzing players of interest based on predicted offensive RAA or average RAA, then looking at the percentage played at each position to find players in the positions of interest with the highest RAA. After the WAR can be calculated by selecting the number of games played at each position and combined with the predicted offensive RAA to more accurately predict WAR.
