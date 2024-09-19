 #Substance Use Patterns in Young Adults: A Data-Driven Investigation Using Regularization Techniques and Tree-Based Methods.(R-Code)
                                                               

library(dplyr)
library(ggplot2)
data <- read.csv("/Users/balajikolusu/Downloads/data_math678.csv")
head(data)
str(data)

# --------------------------------PRE-PROCESSING--------------------------------

# Handling missing values
data_clean <- na.omit(data)

# Filling missing numeric data with the mean of the column
numeric_columns <- sapply(data_clean, is.numeric)
data_clean[numeric_columns] <- lapply(data_clean[numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Encoding categorical variables into dummy variables
categorical_columns <- c("GENDER", "RACE", "US_BORN", "EMPLOYED", "SCHOOL", "THERAPY", "ALCOHOL", "TOBACCO", 
                         "ECSTASY", "KETAMINE", "METHAMPHETAMINE", "MARIJUANA", "COCAINE_POWEDER", "GHB", 
                         "COCAINE_CRACK", "HEROIN", "MUSHROOM", "LSD", "RX")
data_clean <- data_clean %>%
  mutate(across(all_of(categorical_columns), as.factor)) %>%
  model.matrix(~.-1, data = .) %>%
  as.data.frame()
str(data_clean)
 summary(data)

# -------------------------------METHOD 1: glmnet-------------------------------

library(glmnet)
data_glmnet <- data_clean
# Extracting the response variable
response <- data_glmnet$TOTALDRUGS
# Remove the response variable from the predictor dataset
data_glmnet$TOTALDRUGS <- NULL

# Convert to matrix format for glmnet
predictors <- as.matrix(data_glmnet)

# Setting up the glmnet model for LASSO regression (alpha = 1)
set.seed(123)
cv_lasso_model <- cv.glmnet(predictors, response, alpha = 1) # alpha = 1 for LASSO
cv_ridge_model <- cv.glmnet(predictors, response, alpha = 0) # alpha = 0 for Ridge

# Plotting the cross-validation results to see the performance
plot(cv_lasso_model)
plot(cv_ridge_model)

# Identify the lambda that gives the minimum mean cross-validated error
best_lambda_lasso <- cv_lasso_model$lambda.min
best_lambda_ridge <- cv_ridge_model$lambda.min
best_model_lasso <- glmnet(predictors, response, alpha = 1, lambda = best_lambda_lasso)
best_model_ridge <- glmnet(predictors, response, alpha = 0, lambda = best_lambda_ridge)

# Print the coefficients of the model
print(coef(best_model_lasso))

# 30 x 1 sparse Matrix of class "dgCMatrix"#                           s0# (Intercept)               0.1055241# X                         .        # AGE                       .        # GENDERfemale              .        # GENDERmale                .        # RACEBLACK                 .        #RACEMIXED RACE            .        #RACEWHITE                 .        # US_BORNYes                .        # EMPLOYEDYes               .        # SCHOOLYes                 .        # THERAPYYes                .        # ILLEGALTOTALILLEGALDRUGS1 0.9821027# ILLEGTOTALILLEGALDRUGS2   .        # ALCOHOLYes                0.9348273# TOBACCOYes                0.9061479# ECSTASYYes                .        # KETAMINEYes               .        # METHAMPHETAMINEYes        .        # MARIJUANAYes              .        # COCAINE_POWEDERYes        .        # GHBYes                    .        # COCAINE_CRACKYes          .        # HEROINYes                 .        # MUSHROOMYes               .        # LSDYes                    .        # RXYes                     .        # DEPRESSION                .        # ANXIETY                   .        # LONELINESS                .        

print(coef(best_model_ridge))

# 30 x 1 sparse Matrix of class "dgCMatrix"
# s0
# (Intercept)               -0.2259396041
# X                         -0.0006429291
# AGE                        0.0006294003
# GENDERfemale              -0.0280468813
# GENDERmale                 0.0282115451
# RACEBLACK                  0.0093471692
# RACEMIXED.RACE             0.0028085434
# RACEWHITE                  0.0006104133
# US_BORNYes                 0.0293912099
# EMPLOYEDYes                0.0081754342
# SCHOOLYes                 -0.0163418927
# THERAPYYes                 0.0192084442
# ILLEGALTOTALILLEGALDRUGS1  0.1775974092
# ILLEGTOTALILLEGALDRUGS2    0.1641478734
# ALCOHOLYes                 0.8908836661
# TOBACCOYes                 0.8732955443
# ECSTASYYes                 0.5849378692
# KETAMINEYes                0.6319159302
# METHAMPHETAMINEYes         0.6002392228
# MARIJUANAYes               0.7387923877
# COCAINE_POWEDERYes         0.6027194123
# GHBYes                     0.5575379513
# COCAINE_CRACKYes           0.6344908943
# HEROINYes                  0.6885167850
# MUSHROOMYes                0.5893154889
# LSDYes                     0.5916894442
# RXYes                      0.5786050026
# DEPRESSION                 0.0042086983
# ANXIETY                    0.0146360467
# LONELINESS                -0.0159347180

# view non-zero coefficients
print(coef(best_model_lasso)[coef(best_model_lasso) != 0])
# [1] 0.1055241 0.9821027 0.9348273 0.9061479
print(coef(best_model_ridge)[coef(best_model_ridge) != 0])
# [1] -0.2259396041 -0.0006429291  0.0006294003 -0.0280468813  0.0282115451  0.0093471692
# [7]  0.0028085434  0.0006104133  0.0293912099  0.0081754342 -0.0163418927  0.0192084442
# [13]  0.1775974092  0.1641478734  0.8908836661  0.8732955443  0.5849378692  0.6319159302
# [19]  0.6002392228  0.7387923877  0.6027194123  0.5575379513  0.6344908943  0.6885167850
# [25]  0.5893154889  0.5916894442  0.5786050026  0.0042086983  0.0146360467 -0.0159347180

# --------------------------------METHOD 2: vip---------------------------------

library(randomForest)
library(vip)

data_vip <- data_clean

# Clean up column names to avoid spaces and special characters
names(data_vip) <- make.names(names(data_vip))

# Fitting a Random Forest model
rf_model <- randomForest(TOTALDRUGS ~ ., data = data_vip, ntree = 500, importance = TRUE)

# Plotting variable importance
vi <- vip(rf_model, num_features = 10)
plot(vi)

# Importance score generated by randomForest
importance(rf_model)

                             
#                             %IncMSE IncNodePurity
#X                         11.59651542    20.8523250
#AGE                       -0.71719124     3.3610060
#GENDERfemale               3.66131644     0.9329935
#GENDERmale                 2.54530961     1.1088823
#RACEBLACK                 -0.50541057     0.7953879
#RACEMIXED.RACE             0.35642577     0.8547220
#RACEWHITE                 -0.07555131     1.1363432
#US_BORNYes                 4.53464348     1.3514203
#EMPLOYEDYes                2.17623463     1.4449428
#SCHOOLYes                 -0.06087980     0.6546473
#THERAPYYes                -0.61975128     0.7009120
#ILLEGALTOTALILLEGALDRUGS1 20.91610665   236.7092252
#ILLEGTOTALILLEGALDRUGS2   19.52724934   264.6365508
#ALCOHOLYes                26.01072677    35.0890983
#TOBACCOYes                34.21158857    15.9608705
#ECSTASYYes                 6.10165509    32.2706349
#KETAMINEYes                0.90283349     2.9457813
#METHAMPHETAMINEYes         2.44582640     5.0058209
#MARIJUANAYes              16.25062777    29.8781866
#COCAINE_POWEDERYes         7.73264301    53.2511810
# GHBYes                     1.41929187     3.0679982
# COCAINE_CRACKYes           1.91026913     8.8919438
# HEROINYes                  2.59288522     7.7921750
# MUSHROOMYes                5.80552857    32.3446140
# SDYes                     3.07784616    15.8681280
# RXYes                      3.56592491     4.2908171
# DEPRESSION                10.18361622    95.1610426
# ANXIETY                   24.39519140   153.9924997
# LONELINESS                 5.82760985     8.0290280

# Model summary
print(rf_model)

# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 9
# 
# Mean of squared residuals: 0.1234435
# % Var explained: 97.5

# Check for and handle missing values
data <- na.omit(data)

# Convert relevant columns to factors if they are not already
data$EMPLOYED <- as.factor(data$EMPLOYED)
data$SCHOOL <- as.factor(data$SCHOOL)

# Visualization 1: Boxplot of Total Drugs by Employment Status
ggplot(data, aes(x=EMPLOYED, y=TOTALDRUGS, fill=EMPLOYED)) +
  geom_boxplot() +
  labs(title="Distribution of Total Drugs by Employment Status", x="Employed", y="Total Drugs") +
  theme_minimal()

# Visualization 2: Boxplot of Total Drugs by School Attendance
ggplot(data, aes(x=SCHOOL, y=TOTALDRUGS, fill=SCHOOL)) +
  geom_boxplot() +
  labs(title="Distribution of Total Drugs by School Attendance", x="Attends School", y="Total Drugs") +
  theme_minimal()

# Visualization 3: Density Plots for Depression, Anxiety, and Loneliness
data %>%
  select(DEPRESSION, ANXIETY, LONELINESS) %>%
  pivot_longer(cols = everything(), names_to = "Mental_Health_Variable", values_to = "Score") %>%
  ggplot(aes(x=Score, fill=Mental_Health_Variable)) +
  geom_density(alpha=0.7) +
  labs(title="Density Plots for Depression, Anxiety, and Loneliness", x="Score", y="Density") +
  theme_minimal()


# ------------------------------------------------------------------------------

