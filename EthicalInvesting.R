#This script explores the question of whether ethical investments lead
# to better returns based on ESG scores

# Here I will open and explore two .csv files
# Funds_info.csv contains basic information about EFTs and mutual funds
# Funds_ESG_ratings.csv contains the corresponding ESG rating and their
# risk of exposure to a list of controversial topics.

# Addressing questions such as: 
# 1) Do ethical investments lead to better returns?
# 2) Can we actually predict returns based on these ratings?
# 3) What do these ESG scores reflect in terms of the fund's involvement in certain
# controversial issues?

# The script is split into three main sections:
# 1) Data Exploration
# 2) Modelling
# 3) Discussion

#The data exploration section examines the structure
# of the data and explores the relationships between the variables.
# Ideas of which variables to model will come from this data exploration

# The models sections aims to address the questions of whether 
# there is a relationship between the different ESG scores
# and the fund returns. 

# It also then goes on to test whether you can predict the returns
# based off of the scores

# The discussion section goes into a discussion of the results

##########################
######## PACKAGES ########
##########################

#Loading the various packages that I will be using
# to address the above questions

#Tidyverse is a wonderful package for DS - dplyr, ggplot2, and reader will be used 
library(tidyverse)

#ggrepel allows you to plot labels in a more elegant fashion that standard ggplot2
library(ggrepel)

#lattice allows plotting multiple plots at the same time
library(lattice)

#grid, gridExtra,and GGally all improve upon ggplot styles/plotting
library(grid)
library(gridExtra)
library(GGally)

#Tidymodels is an excellent package for model deployments in R
# that uses the same tidy language as tidyverse
library(tidymodels)

#Corrr library is part of the tidy universe and allows for coorelation plots
library(corrr)

#Glmnet is needed in order to run the linear model with penalty terms
library(glmnet)

#ranger is needed in order to run the random forests model
library(ranger)

##############################
############ DATA ############
##############################

#Here I import the data from the two .csv.gz files:

#First, I import the the Funds_info.csv.gz using readr
# into the variable: fundsInfo

#fundsInfo contains 56606 rows and 15 columns
fundsInfo <- read_csv("Funds_info.csv.gz")

#Looking at the column names
colnames(fundsInfo)

#Quick check of the data in the various columns
glimpse(fundsInfo)

#Quick check of duplicate rows - either in the ticker or fund name category
length(unique(fundsInfo$ticker))
length(unique(fundsInfo$fund_name))

#No duplicates found

#Second, I import the Funds_ESG_rating.csv.gz into the variable: 

#fundsESG contains 30945 rows and 17 columns
fundsESG <- read_csv("Funds_ESG_ratings.csv.gz")

#Looking at the column names
colnames(fundsESG)

#Quick check of the data in the various columns
glimpse(fundsESG)

#Quick check of duplicate rows
length(unique(fundsESG$ticker))
#No duplicates found

#ticker appears to the unique key to join together the two datasets
# there are more companies in the fundsInfo dataset than there
# are in the fundsESG set - so when I bind together the
# two dataset, I expect to find the dataset to be 30,945 rows

#From examining the different variables:
# 1) Need to join the two datasets on ticker
# 2) Will exclude (for now) the investment_strategy
# 3) Will keep all of the variables in fundsESG for exploration

#############################################
######### DATA CLEANING AND MERGING #########
#############################################

#Here I join together the two datasets - those "ticker" values in 
# fundsInfo that do not have a matching ticker in fundsESG will be 
# ignored - without the ESG ratings there is nothing to work on

ESG_and_info <- fundsESG %>%
  left_join(fundsInfo, by = "ticker") %>%
  select(-investment_strategy)

#Checking for NAs in the dataset
# What I find is that there are 13,757 rows with at least one NA
ESG_and_info %>%
  filter(if_any(everything(), ~is.na(.)))

#Most of the NAs are coming from the fund_return_2017 columns
# as well as the risk_rating and performance_rating columns

#The choice now is to fill the NAs using the median averages
# around them or to fill them using k Nearest Neighbors or to
# simply drop the values

# For the sake of time - I am going to combine together
# the three return values when I begin modeling and take
# the averages (median) of the three values to have a single
# "return" value. This removes the majority of the missing data
# values for returns. Those without values still will be dropped.
# This will obvious influence the results of the models
# but will still be informative of the overall relationships 
# I am looking at. 

##############################################
############## DATA EXPLORATION ##############
##############################################

#First step is I want to plot some of the variables that may be of most
# interest to check for variation within the variables and covaration between
# the variables

# Judging from the variables names - the different scores, involvements,
# risk_rating, performance_rating, and fund returns all may be important variables

#I begin by plotting the distribution of the score variables using barcharts

#Here I am grabbing the column names associated with "score" for plotting
ESGColnames_Score <- colnames(ESG_and_info %>%
                                select(contains("score")))

#This creates the plots for the scores - these are in bar plots
#it looks through the colnames and then outputs each into one output screen grid
plot_list <- list()
for(i in ESGColnames_Score){
  plot <- ggplot(ESG_and_info, aes_string(x = i)) + 
    geom_bar()
  plot_list[[i]] <- plot
}

plot_grob <- arrangeGrob(grobs = plot_list)
grid.arrange(plot_grob)

#What we see is that most values for environmental_score center around 0.20
# while for social_score and governance_score, they center around 0.60
# This suggests that most companies score relatively poorly on social
# and governance while pretty good on environmental score

# These variables have a bell-curve distribution but environmental
# is skewed towards the left and the other are centered.


#Next I will plot the distribution of the involvements
#Here I am grabbing the column names associated with "involvement" for plotting
ESGColnames_Involvement <- colnames(ESG_and_info %>%
                                      select(contains("involvement")))

#This creates the plots for involvements - these are histograms
# same as above but with histograms
plot_list <- list()
for(i in ESGColnames_Involvement){
  plot <- ggplot(data = ESG_and_info, 
                 aes_string(x = i)) + 
    geom_histogram(binwidth = .5)
  plot_list[[i]] <- plot
}
plot_grob <- arrangeGrob(grobs = plot_list)
grid.arrange(plot_grob)

#What we see is that most of the involvement variables do not provide very much
# information - abortive_contraceptive, animal_test, military contracting, nuclear, 
# small_arms, thermal_coal, and tobacco may by the ones that have the highest
# involvement and be the most useful for predictions

#The distribution of these involvements are heavily skewed towards zero or
# with a positive skew - these potentially zero-inflated variables
# are largely a consequences of the funds not reporting or claiming
# to have no involvements with these categories. Linear models without
# a possion function may have issues modeling these variables. 

##############################
######## CORRELATIONS ########
##############################

#Another thing to examine is the correlations between some of the variables
# for example - we can examine the correlations between the different scores

#rplot provides a useful was to view correlations between all
# of the variables - not just the ones Ive choosen as being potentially most useful

rplot(ESG_and_info %>%
        select(where(is.numeric)) %>%
        correlate() %>%
        rearrange(absolute = F) %>%
        shave(), print_cor = T) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#What we see is that environmental scores are not correlated or very weakly 
#correlated (-0.01) with social or governance scores
# Social and governance however are fairly highly correlated (0.65)
# We also see that there are correlations among the involvements.
# Military_contracting and controversial_weapons are correlated (0.79),
# as are controversial_weapons and small_arms (0.68).
# The highest correlation though is between animal_testing and 
# abortive_contraceptive (0.82).

#The ESG scores and these various involvements though all have low correlations
# suggesting that there is not a strong relationship between them

#This goes to question #3 - the ESG scores seems to be largely not tightly
# correlated with the fund's involvements in convertroversial issues. However,
# further analysis is obviously needed.


# An additional variable of potential interest are the different asset classes.
# You see a strong negative correlation between asset_bond and asset_stock.
# Also, bonds are associated with negative returns in 2017 and 2019 but positive
# return in 2018. Stocks are positively associated with returns in 2017 and 2019
# but not in 2018. Cash had lower returns in 2017 and 2019 but not as low as
# the other two asset types. 

# Moving forward though, I am most interested in examining how
# the various ESG scores and the involvements influence returns and 
# whether we can use these variables to predict the potential returns. 

# Additional avenues to explore would be to model whether you can
# predict the different scores based on the involvements. 

# Further, examining the relationships between the various risk_ratings
# and performance_ratings to the ESG scores would provide a way of highlighting
# whether those funds that are more "ethical" are better performing.

# For this assessment, however, I will use the scores and involvements to
# examine fund returns.

################################################################
####################### MODELING SECTION #######################
################################################################

#Below I select just the columns that I am aiming to use for the analyses
# Here the predictor variables will be the scores and involvements, and the
# response variable will be the fund returns

reducedDataset <- ESG_and_info %>%
  select(contains("score") | contains("involvement") | contains("fund_return"))

#Here I averaged the yearly returns and created the new column returns
# I then removed all of the NA values that are left.
#I am left with a dataset of 26,222 values

furtherReduced <- reducedDataset %>%
  mutate(returns = rowMeans(select(., contains("return")), na.rm = T)) %>%
  select(-contains("fund_return")) %>%
  filter(!is.na(returns))

#Preprocessing of the data is listed with the models in the recipe stage.

#Model 1) Linear Regression
# I begin the modeling with a linear regression as they
# often have a good predictive ability and are a simple model for 
# providing interpretations of the variables. 

#The large sample size (26,222) suggests that many of the 
# assumptions of normality needed for a linear model
# may be already met (central limit theorem).

# This original model will also include all of the scores and 
# all of the involvements that have a correlation score below 0.8.
# 0.8 was chosen as that is a very high correlation value.

#Model 2) Linear Regression with regularization penalites
# The second model is similar to the first but with the additional of
# regularization penalties that punish models with too many variables.
# The additional of the regularization (either lasso or ridge) has the potential
# to improve the model. 

# Model 3) Random Forests
# The third model is a more complex, random forests model. Random forests
# have been found to have high predictive ability but lose interpretability. 


################################################################
###################  LINEAR REGRESSION MODEL ###################
################################################################

#Now setting up the first model which is just a Linear Model

set.seed(25)

#split is for dividing the dataset into a training and test set
# strata makes the makes sure that the variables are evenally distributed
# across the splits - the split is 80% training and 20% test

split <- initial_split(furtherReduced, prop = 0.8, strata = returns)

train_data <- training(split)

test_data <- testing(split)

#For tidymodels - you can prep a "recipe" that will use for the pre-processing
# The pre-processing that is needed is the removal of the high correlation
# valued variables and to normalize the predictors (mean = 0 and variation = 1).
prep_recipe <- 
  #First you set up the model
  recipe(returns ~ ., data = train_data) %>%
  #Next I remove the correlations greater than a threshold - here is 0.8
  step_corr(all_numeric(), threshold = 0.8) %>%
  #Next I normalize the predictor variables minus the variables that are categorical
  step_normalize(all_predictors(), -all_nominal()) 

prep(prep_recipe)

#Here I "juice" the recipe - essentially applying the steps above to the dataset
dia_juiced <- juice(prep(prep_recipe))

#Examine the dimensions of the data
dim(dia_juiced)

#Here I am using the basic linear regression model with the
# base engine (lm) - I am initializing the model below

lm_model <- 
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

#Fitting the juiced dataset to the model using all variables
lm_fit1 <- fit(lm_model, returns ~.,dia_juiced)
lm_fit1

#Plot to examine the assumptions of the linear model - diagnostic plots
plot(lm_fit1$fit)

#What we see is that by including all of the variables.
# You have a rather terrible model.
# You have a very high AIC which is terrible, a RMSE which is also not
# good (6.25) and we explain little of the variance r.squared (0.0934). 
# Interested to see how we can improve this.

#Glance provides a tidy way to view the fit,
# provides the r.squared, RMSE, p value, and AIC.
# We have a significant model but it does not explain much

glance(lm_fit1$fit)

#Tidy provides a way to examine the importance of the variables
tidy(lm_fit1) %>%
  arrange(desc(abs(statistic)))

#The most important variables are environmental score and governance score
# which are both negative suggesting that the better the ESG score
# the better the return - the reverse appears true in terms of social score
# as social score increases - the return increases

#The results of this model would suggest that those funds with
# the lower environmental scores do have better returns, but 
# that social_scores seems to be less important
# additionally - the fewer involvements with the controversial 
# categories leads to higher returns except military_contracting,
#small_arms, and abortive_contraceptives potentially increase the returns.

#Now will prep the recipe for the test_data
prep_recipe <- 
  #First you set up the model
  recipe(returns ~ ., data = test_data) %>%
  #Next I remove the correlations greater than a threshold - here is 0.8
  step_corr(all_numeric(), threshold = 0.8) %>%
  #Next I normalize the predictor variables minus the variables that are categorical
  step_normalize(all_predictors(), -all_nominal()) 

test_juiced <- juice(prep(prep_recipe))

#Here I try to predict the returns from the test_data set
lm_predict <- predict(lm_fit1, new_data = test_juiced)
lm_predict$returns <- test_data$returns

#The model is pretty poor - 6.15 RMSE, and an r.squared of 0.103
# It performs slightly better on the test_data set than on the training
# data set suggest that it is not overfit but the low r.squared also
# suggests that I missing much of the variance
metrics(lm_predict, truth = returns, estimate = .pred)

#Plot of the predicted versus the actual values for the returns
# What we see is essentially a large blob - there is a slight positive relationship
# but it is clear that the model is not capturing the data perfectly.
ggplot(data = lm_predict,
       aes(x = .pred, y = returns)) + 
  geom_point() 


######################################################################
######### RERUNNING THE LM WITH PENALTY and RIDGE REGRESSION #########
######################################################################

#Here I am attempting to run a linear model using the glmnet
# which allows for hyperparameter optimization
# This is an attempt to use feature selection/dimensonality reduction.

lm_model <- 
  #Glmnet has two parameters that we will tune via the tune package
  #penalty is the same as a the lambda value - tunes how much of a penalty
  #mixture is the same as alpha and determines whether the model
  # is a pure ridge regression (0) or lasso (1) model
  linear_reg(penalty = tune(),
             mixture = tune()) %>%
  #mode is set as regression
  set_mode("regression") %>%
  #engine is set as glmnet
  #Requres the package glmnet be installed and loaded
  set_engine("glmnet")

#Here I am creating a workflow - funnels everything into a pipeline
LM_wflow <-
  #initializes the original workflow
  workflow() %>%
  #add the model - the glmnet model
  add_model(lm_model) %>%
  #adds the recipe that is the same the one used for the original LM
  add_recipe(prep_recipe)

#Provides information about the workflow and the steps that will be taken
# and the order
# The parameters have yet to be tuned
LM_wflow

#Here I am updating the parameters that will be used for the model.
#I am setting the search parameters for the grid itself.
# The grid will use 5 penalty terms and 5 mixture values
LM_param <-
  LM_wflow %>%
  parameters() %>%
  update(penalty = penalty(range = c(-3, 0)),
         mixture = mixture(range = c(0, 1)))

#Here I am setting the number of terms that will be used
LM_grid <- grid_regular(LM_param, levels = 5)


#Set a seed for reproducibility
set.seed(1559)

#Here I will be working with a reduced dataset again

furtherReduced

#Split is for dividing the dataset into a training and test set.
# Strata makes the makes sure that the variables are evenly distributed
# across the splits - the split is 80% training and 20% test.

split <- initial_split(furtherReduced, prop = 0.8, strata = returns)
train_data <- training(split)

test_data <- testing(split)

#Here I am using 10-fold cross-validation on the training data set
# for parameter tuning

train_cv <- vfold_cv(train_data, v = 10, strata = returns)

#Here I am tuning the grid - using the workflow and the 
# 10 fold training dataset.

#A potentially better way to do the grid search would be 
# via a Baysian grid search approach.

return_glmnet_grid <-
  tune_grid(LM_wflow, grid = LM_grid, resamples = train_cv,
            param_info = LM_param)

#What you can see with the autoplot is that as the you increase
# the parameterization towards a lasso regression - you have
# a steep increase in the rmse score - this suggests that it starts
# to drop out too many variables.

autoplot(return_glmnet_grid, metric = "rmse")

show_best(return_glmnet_grid, "rmse", n = 9)

#What we find is that the best model is one with a penalty
# term of 0.001, and a mixture of 0.75 - this is close to a 
# lasso regression model - however at the penalty level of 0.001
# the lasso penalty is not really enforced

select_best(return_glmnet_grid, metric = "rmse")

#Another way to examine is by using the one that contributes the
# smallest std_error - here you have the same penalty but complete
# ridge regression (0) - the mean metric (rmse) is the same as the other
# model

select_by_one_std_err(return_glmnet_grid, penalty, mixture, metric = "rmse")

#I will now use the hyperparameters that were selected to fit the 
# whole training dataset rather than the cross validated
LM_params_final <- select_by_one_std_err(return_glmnet_grid, penalty, mixture,
                                         metric = "rmse")

#Finalize the workflow using the updated parameters
LM_wflow_final <- finalize_workflow(LM_wflow, LM_params_final)

#Now fitting the training data set
LM_wflow_final_fit <- fit(LM_wflow_final, data = train_data)

# I will now use the predict function on the test data
# to examine how the model performed
finalTest <- predict(LM_wflow_final_fit, new_data = test_data)

#Need to add in the actual returns data to the predicted model
# the finalTest has a .pred with the predictions -
# this add back in the returns to compare
finalTest$returns <- test_data$returns

metrics(finalTest, truth = returns, estimate = .pred)

#When predicting with the final model - it is pretty terrible.
# RMSE = 6.32, rsq = 0.089, mae = 4.54.
# This is worse than running the model without the parameterizations.
# Potentially this is because certain variables were pushed to zero or close
# to zero because of the ridge regression - potentially too much so. 

#######################################################
################ RANDOM FORESTS MODELS ################
#######################################################

#Here I will attempt to use the same data to model via random forests

furtherReduced

#Now setting up the dataset for splitting

set.seed(25)

#Split is for dividing the dataset into a training and test set.
# Strata makes the makes sure that the variables are evenally distributed
# across the splits - the split is 80% training and 20% test.

split <- initial_split(furtherReduced, prop = 0.8, strata = returns)

train_data2 <- training(split)
test_data2 <- testing(split)

#The random forests model 
show_model_info("rand_forest")

rf_model <- 
  #The main parameter - mtry is the number of random selected
  # predictors to try at each branching
  rand_forest(trees = 200, min_n = 5) %>%
  #set for regression and not classification
  set_mode("regression") %>%
  #default engine
  set_engine("ranger")

#Again I prepare the recipe for pre-processing the data
prep_recipe2 <- 
  #First you set up the model
  recipe(returns ~ ., data = train_data2) %>%
  #Next I remove the correlations greater than a threshold - here is 0.8
  step_corr(all_numeric(), threshold = 0.8) %>%
  #Next I normalize the predictor variables minus the variables that are categorical
  step_normalize(all_predictors(), -all_nominal())

#Here I "juice" the recipe - essentially applying the steps above to the dataset
dia_juiced2 <- juice(prep(prep_recipe2))

#Now to fit the data 
set.seed(1)
rf_reg_fit <- rf_model %>%
  fit(returns ~., data = dia_juiced2)

#The results of the RF fit
rf_reg_fit

#Now to prep the test_data2
prep_recipe2 <- 
  #First you set up the model
  recipe(returns ~ ., data = test_data2) %>%
  #Next I remove the correlations greater than a threshold - here is 0.8
  step_corr(all_numeric(), threshold = 0.8) %>%
  #Next I normalize the predictor variables minus the variables that are categorical
  step_normalize(all_predictors(), -all_nominal())

test_juiced <- juice(prep(prep_recipe2))

PredRF_model <- predict(rf_reg_fit, test_juiced)
PredRF_model$returns <- test_data2$returns


metrics(PredRF_model, truth = returns, estimate = .pred)

#The results of the random forest are similar rmse values (4.33) to the LM
# but with a better .60 rsquared.
# Overall the model is still fairly poor at predicting but much more
# of the variation is accounted for.


################################################
################## DISCUSSION ##################
################################################

#Going back to the original questions:
# 1) Do ethical investments lead to better returns?
# 2) Can we actually predict returns based on these ratings? 

#For the first question:
#We can see in the results is that ethical investments appear to lead
# to potentially better returns. As you increased along the environmental
# or governance scores you had a lower return. The opposite is true for social
# scores. This may reflect the historical view of the past 50 years that you
# need to appear to be a good steward of the environment or involved in good
# governance. The social score pattern however is more difficult to explain. 
# Generally, for the involvements, the fewer involves the better the returns. 
# This suggests that these controversial topics are potentially avoided
# by the funds. This however, is not true for military contracting or 
# alcohol suggesting that the profit to be made by investing in these categories
# outwise the potential negative public view. 

#For the second question:
# The predictive ability of these models appears to be relatively limited
# with a high RMSE value. However, the random forest model was able to explain
# over half the variance see in the returns suggesting that the variables
# do explain the returns to some degree. A boosted random forest model
# would be a potential next step to improve the models. 

#Future Direction:
# Additional avenues to explore would be to include the risk_ratings and
# performance_ratings. Are the scores and involvements able to predict
# the performance ratings? Another question to explore the degree of relatedness
# between the ESG scores and the involvements? 



