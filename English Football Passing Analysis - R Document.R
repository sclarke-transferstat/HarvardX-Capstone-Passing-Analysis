---
#  title: "Capstone CYO - English Football Passing Analysis"
#author: "Stephen Clarke"
#date: "03/12/2020"


#Install the following packages if required

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(Lahman)) install.packages("Lahman", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")



## Executive Summary Section ##

## Method and Analysis Section ##


#### Data Cleaning ####

# Libraries loaded and memory limit increased so system's RAM can cope

library(tidyverse)
library(dslabs)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
ds_theme_set()
library(Lahman)
library(lattice)
library(e1071)
library(caret)
library(knitr)
library(tidyr)
library(stringr)
library(readr)
memory.limit(56000)


# Data sets downloaded from GitHub

Player_Passing_Data <- read.csv("https://raw.githubusercontent.com/sclarke-transferstat/HarvardX-Capstone-Passing-Analysis/main/English%20Football%20Player%20Passing%20Data%202019-20.csv")

Team_Passing_Data <- read.csv("https://raw.githubusercontent.com/sclarke-transferstat/HarvardX-Capstone-Passing-Analysis/main/English%20Football%20Team%20Passing%20Data%202019-20.csv")

#Remove League column in Team_Passing_Data as we already have that column in the Player_Passing Data. We also remove the last 2 columns as they are blank

Team_Passing_Data <- within(Team_Passing_Data, rm("League", "X", "X.1"))


#Join data sets together by Team

Passing_Data <- left_join(Player_Passing_Data, Team_Passing_Data, by = "Team")

head(Passing_Data)

#Leagues ordered in their real life order, of Premier League, Championship, League One.

Passing_Data$League <- factor(Passing_Data$League, levels=c("Premier League", "Championship", "League One"))



#### Data Exploration ####

#Best passers and worst passers examined

Accurate_Passers <- Passing_Data %>%
  select(Player, Team, League, Position, Pass_Accuracy) %>%
  filter(League== "Premier League") %>%
  arrange(desc(Pass_Accuracy)) %>%
  head(n=15)

Accurate_Passers

Inaccurate_Passers <- Passing_Data %>%
  select(Player, Team, League, Position, Pass_Accuracy) %>%
  filter(League== "Premier League") %>%
  arrange(Pass_Accuracy) %>%
  head(n=15)

Inaccurate_Passers



##### Effect of Team ####

#Data grouped by League before creating and faceting scatter graphs to compare passing accuracies

p <- Passing_Data %>%
  group_by(League) %>%
  ggplot(aes(Pass_Accuracy, Team_Pass_Accuracy)) +
  geom_point(aes(col= factor(League))) +
  geom_smooth(method = "lm")

p + facet_grid(cols = vars(League)) +
  xlab("Player Pass Accuracy") +
  ylab("Team Pass Accuracy") +
  ggtitle("Player Passing Relative to their Team")


#Correlation of above graphs tested

cor.test(Passing_Data$Pass_Accuracy, Passing_Data$Team_Pass_Accuracy, method=c("pearson", "kendall", "spearman"))



##### Effect of Position ####

#Data separated into Main Positions and positions ordered

Passing_Data <- Passing_Data %>%
  separate(Position, c("Main_Position", "Secondary_Position", "Tertiary_Position"), fill = "right")

Passing_Data$Main_Position <- factor(Passing_Data$Main_Position, levels=c("GK", "RCB3", "RCB", "CB", "LCB", "LCB3", "RB", "LB", "RWB", "LWB", "RDMF", "DMF", "LDMF", "RCMF3", "RCMF", "LCMF", "LCMF3", "RAMF", "AMF", "LAMF", "RW", "LW", "RWF", "LWF", "CF"))

#Boxplot created of how different positions effect pass accuracy

Passing_Data %>% ggplot(aes(x = Main_Position, y = Pass_Accuracy, col = Main_Position)) +
  geom_boxplot() +
  xlab("Main Position") +
  ylab("Pass Accuracy") +
  ggtitle("How Pass Accuracy Changes by Position")


##### Effect of Pass Difficulty ####

#Data edited to remove GK from 'Main_Position' and 'Safe_Pass_Rate' metric created

Passing_Data <- Passing_Data %>%
  mutate(Safe_Pass_Rate = (Back_passes_per_90 + Lateral_passes_per_90)/Passes_per_90) %>%
  filter(Main_Position %in% c("RCB3", "RCB", "CB", "LCB", "LCB3", "RB", "LB", "RWB", "LWB", "RDMF", "DMF", "LDMF", "RCMF3", "RCMF", "LCMF", "LCMF3", "RAMF", "AMF", "LAMF", "RW", "LW", "RWF", "LWF", "CF"))

#Player Pass Accuracy plotted against Safe Pass Rate

Passing_Data %>%
  ggplot(aes(Pass_Accuracy, Safe_Pass_Rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Player Pass Accuracy") +
  ylab("Safe Pass Rate") +
  ggtitle("Correlation between 'Safe Pass Rate' and Player Pass Accuracy")


#Team Pass Accuracy plotted against Safe Pass Rate

Passing_Data %>%
  ggplot(aes(Team_Pass_Accuracy, Safe_Pass_Rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Team Pass Accuracy") +
  ylab("Safe Pass Rate") +
  ggtitle("Correlation between 'Safe Pass Rate' and Team Pass Accuracy")



#### Insights Gained Section ####


#### Modelling Approach ####

#Data partitioned and split into test and train sets

set.seed(1, sample.kind="Rounding")

train_index <- createDataPartition(y = Passing_Data$Pass_Accuracy, times = 1, p = 0.9, list = FALSE)

Passing_Data_train <- Passing_Data[train_index,]
Passing_Data_test <- Passing_Data[-train_index,]

#Function created for RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}


##### Naive Baseline RMSE ####

#Mean Pass Accuracy calculated

mu <- mean(Passing_Data_train$Pass_Accuracy)
mu


#RMSE calculated for the Naive Baseline Model

naive_rmse <- RMSE(Passing_Data_test$Pass_Accuracy, mu)

naive_rmse


#Data frame created to put model results

model_results <- data.frame(model = "Naive Baseline Model", RMSE = naive_rmse)

model_results



##### Using Position Bias to Improve RMSE #####

#Train set grouped by position to account for Position Bias

Position_avg <- Passing_Data_train %>%
   group_by(Main_Position) %>%
   summarise(b_P = mean(Pass_Accuracy - mu))

#Positions arranged by the Position Bias

Position_avg %>% arrange(desc(b_P))


#RMSE calculated for Position Bias

predicted_ratings <- Passing_Data_test %>% 
  left_join(Position_avg, by = 'Main_Position') %>%
  mutate(pred1 = mu + b_P) %>%
  .$pred1

#.$ means pull() function

Position_bias <- RMSE(Passing_Data_test$Pass_Accuracy, predicted_ratings)


#Model added to Data Frame to compare to Naive Baseline Model

model_results <- bind_rows(model_results, data_frame(model = "Position Bias Model", RMSE = Position_bias))

model_results


#Position Bias data joined with main data set to look at how the position bias effects the pass accuracy

Passing_Data_With_PBias <- left_join(Passing_Data, Position_avg, by = "Main_Position")

Passing_Data_With_PBias <- Passing_Data_With_PBias %>% mutate(New_Pass_Acc = Pass_Accuracy - b_P) %>% select(Player, Main_Position, Passport_country, Team, League, Minutes_played, Pass_Accuracy, Team_Pass_Accuracy, b_P, New_Pass_Acc) %>% arrange(desc(New_Pass_Acc))

#Graphs done out by League once again, accounting for Position Bias

p <- Passing_Data_With_PBias %>%
group_by(League) %>%
ggplot(aes(New_Pass_Acc, Team_Pass_Accuracy)) +
geom_point(aes(col= factor(League))) +
geom_smooth(method = "lm")

p + facet_grid(cols = vars(League)) +
xlab("Player Pass Accuracy Adjusted for Position Bias") +
  ylab("Team Pass Accuracy") +
  ggtitle("Adjusted Player Passing Relative to their Team")



##### Using Team Bias to Improve RMSE ####


#Train set grouped by Team to account for Team Bias

Team_avg <- Passing_Data_train %>% 
  left_join(Position_avg, by = 'Main_Position') %>% 
  group_by(Team) %>% 
  summarize(b_T = mean(Pass_Accuracy - mu - b_P))

#Positions arranged by the Position Bias

Team_avg


#RMSE calculated for Position Bias

predicted_ratings <- Passing_Data_test %>% 
  left_join(Position_avg, by = 'Main_Position') %>% 
  left_join(Team_avg, by = 'Team') %>% 
  mutate(b_P_b_T = mu + b_P + b_T) %>%
  .$b_P_b_T

Team_And_Position_bias <- RMSE(Passing_Data_test$Pass_Accuracy, predicted_ratings)

#Model added to Data Frame to compare to other Models

model_results <- bind_rows(model_results, data_frame(model = "Position and Team Bias Model", RMSE = Team_And_Position_bias))

model_results


#Team Bias and Position Bias data joined with main data set to look at how these biases effect the pass accuracy

Passing_Data_With_PBias <- left_join(Passing_Data, Position_avg, by = "Main_Position")

Passing_Data_With_PTBias <- left_join(Passing_Data_With_PBias, Team_avg, by = "Team")

#New Metric created for 'New Pass Accuracy'

Passing_Data_With_PTBias <- Passing_Data_With_PTBias %>% 
  mutate(New_Pass_Acc = Pass_Accuracy - b_P - b_T) %>% 
  select(Player, Main_Position, Passport_country, Team, League, Minutes_played, Pass_Accuracy, Passes_per_90, Team_Pass_Accuracy, b_P, b_T, New_Pass_Acc) %>% arrange(desc(New_Pass_Acc))

#New Data set filtered to look at Premier League players' New Pass Accuracy

PL_Best_Passers <- Passing_Data_With_PTBias %>%
  filter(League == "Premier League") %>%
  select(Player, Main_Position, Team, Pass_Accuracy, Passes_per_90, b_P, b_T, New_Pass_Acc) %>%
  head(n=20)

PL_Best_Passers

#New Data set filtered to look at New Pass Accuracy of Irish players in top two English tiers

Irish_Best_Passers <- Passing_Data_With_PTBias %>% filter(str_detect(Passport_country, "Republic of Ireland"), League %in% c("Premier League", "Championship"), Minutes_played >= 1000) %>% 
  select(Player, Main_Position, Passport_country, Team, League, Pass_Accuracy, Passes_per_90, b_P, b_T, New_Pass_Acc) %>% 
  head(n=20)

Irish_Best_Passers



##### Regularisation ####

#Choosing lambda for the Regularization penalty term, through cross-validation. We start by using Regularisation just with the Position Bias.

lambdas <- seq(0, 10, 0.25)

mu <- mean(Passing_Data_train$Pass_Accuracy)
just_the_sum <- Passing_Data_train %>% 
  group_by(Main_Position) %>% 
  summarize(s = sum(Pass_Accuracy - mu), n_P = Passes_per_90)


rmses <- sapply(lambdas, function(l){
  predicted_ratings <- Passing_Data_test %>% 
    left_join(just_the_sum, by='Main_Position') %>% 
    mutate(b_P = s/(n_P+l)) %>%
    mutate(pred = mu + b_P) %>%
    pull(pred)
  return(RMSE(predicted_ratings, Passing_Data_test$Pass_Accuracy))
})

#Plot the lamda against RMSE and find what lowest RMSE is

qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

min(rmses)


#Choosing lambda for the Regularization penalty term, through cross-validation. We then do Regularisation with both Team and Position Bias.

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){

  mu <- mean(Passing_Data_train$Pass_Accuracy)
  
  b_P <- Passing_Data_train %>% 
    group_by(Main_Position) %>%
    summarize(b_P = sum(Pass_Accuracy - mu)/(Passes_per_90 +l))
  
  b_T <- Passing_Data_train %>% 
    left_join(b_P, by="Main_Position") %>%
    group_by(Team) %>%
    summarize(b_T = sum(Pass_Accuracy - b_P - mu)/(Passes_per_90 +l))

  predicted_ratings <- 
    Passing_Data_test %>% 
    left_join(b_P, by = "Main_Position") %>%
    left_join(b_T, by = "Team") %>%
    mutate(pred = mu + b_P + b_T) %>%
    pull(pred)
  
return(RMSE(predicted_ratings, Passing_Data_test$Pass_Accuracy))
})

#Plot the lamda against RMSE and find what lowest RMSE is

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda

#Add Regularised RMSE to model results

model_results <- bind_rows(model_results, data_frame(model = "Regularised Position and Team Bias Model", RMSE = min(rmses)))

model_results


## Results ##


#Print full model results

model_results


## Conclusion Section ##

#Best RMSE achieved was **3.265**
