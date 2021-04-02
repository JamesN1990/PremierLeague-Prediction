#James Noone A00162195

#Premier League Prediction 2019 Season
#Load Libraries
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("caret")
install.packages("nnet")


library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(gmodels)
library(randomForest)
library(lattice)
library(caret)
library(nnet)



#Predicting the 2019/2020 Season


TrainingDataFull <- read.csv("results.csv")

as.data.frame(TrainingDataFull)

View(TrainingDataFull)

#Removing rows with NA

TrainingDataFull2 <- na.omit(TrainingDataFull)
View(TrainingDataFull2)

#Removing 20/21 Season
TrainingDataFull3 <- TrainingDataFull2[!(TrainingDataFull2$Season=="2020-21"),]

View(TrainingDataFull3)

#Removing Date/Time Column
TrainingDataFull4 <- TrainingDataFull3[-c(2)]
View(TrainingDataFull4)

#Make Full Time Result Factor
TrainingDataFull4 <- TrainingDataFull4%>%
  mutate(FTR = as.factor(FTR))

#Removing 19/20 Season
TrainingDataFull5 <- TrainingDataFull4[!(TrainingDataFull4$Season=="2019-20"),]

summary(TrainingDataFull5)

View(TrainingDataFull4)



#Creating Test Sets for Prediction 19/20 Season
TestDataSetLR <- subset(TrainingDataFull4,Season=="2019-20")

View(TestDataSetLR)

TestDataSetRF <- subset(TrainingDataFull4,Season=="2019-20")

View(TestDataSetRF)

plot(TrainingDataFull5$FTHG, TrainingDataFull5$FTAG)


#Creating the linear regression model for the home team and away team
PremTrainHomeTeam <- lm(FTHG~HTHG+HTR+HS+HST+HC+HF+HY+HR, data=TrainingDataFull5)
PremTrainAwayTeam <- lm(FTAG~HTAG+HTR+AS+AST+AC+AF+AY+AR, data=TrainingDataFull5)

plot(PremTrainHomeTeam)



plot(PremTrainAwayTeam)




#Does the linear Model Fit the data
summary(PremTrainHomeTeam)$adj.r.squared
summary(PremTrainAwayTeam)$adj.r.squared

#Creating the predictions
HomePredict <- predict(PremTrainHomeTeam, newdata = TestDataSetLR, interval = "confidence")
AwayPredict <- predict(PremTrainAwayTeam, newdata = TestDataSetLR, interval = "confidence")

#Dropped the 2 columns from the predicted model as was causing complications
HomePredict <-subset(HomePredict, select = -c(lwr,upr))
AwayPredict <-subset(AwayPredict, select = -c(lwr,upr))


#Adding predicted values to new "Predictions" dataframe

Predictions <- data.frame("Home_Predict" = HomePredict, "Away_Predict" = AwayPredict)
Predictions <- Predictions%>%
  rename(Home_Predict = fit,
         Away_Predict = fit.1)

#Rounding the Predicted Values to Nearest Whole Numbers:

Predictions$Home_Predict <- round(Predictions$Home_Predict)
Predictions$Away_Predict <- round(Predictions$Away_Predict)

#Changed predictions to Integers
Predictions$Home_Predict <- as.integer(Predictions$Home_Predict)
Predictions$Away_Predict <- as.integer(Predictions$Away_Predict)

#Create Result Predicted Column

Predictions$Results_Predict <- ifelse(Predictions$Home_Predict > Predictions$Away_Predict,"H", 
                                 ifelse(Predictions$Home_Predict < Predictions$Away_Predict,"A",
                                        ("D")))
#Adding Teams to Predictions
Predictions$HomeTeam <- TestDataSetLR$HomeTeam
Predictions$AwayTeam <- TestDataSetLR$AwayTeam
Predictions$Season <- TestDataSetLR$Season

View(Predictions)

#Making the FTR Predict Column a Factor
TestDataSetLR<- TestDataSetLR%>%
  mutate(FTR = as.factor(FTR))

Predictions<- Predictions%>%
  mutate(Results_Predict = as.factor(Results_Predict))


#Breakdown of Results across the Test Data

table(TestDataSetLR$FTR)
table(Predictions$Results_Predict)

round(prop.table(table(TestDataSetLR$FTR)) *100, digits = 1)
round(prop.table(table(Predictions$Results_Predict)) *100, digits = 1)

#Testing the Accuracy of Model- Predicted Vs Actual

table(x = TestDataSetLR$FTR, y = Predictions$Results_Predict)
CrossTable(x=TestDataSetLR$FTR,y= Predictions$Results_Predict)
confusionMatrix(Predictions$Results_Predict, TestDataSetLR$FTR)



#Creating the Random Forest Regression
set.seed(123)
RF_Prem <-randomForest(FTR~HS+HST+HC+HF+HY+HR+AS+AST+AC+AF+AY+AR,data=TrainingDataFull5, ntree=500) 
print(RF_Prem)

RF_Pred = predict(RF_Prem, newdata = TestDataSetRF)
print(RF_Pred)

#Making the FTR Predict Column a Factor
TestDataSetRF<- TestDataSetRF%>%
  mutate(FTR = as.factor(FTR))

RF_Pred <- as.data.frame(RF_Pred)

RF_Pred$HomeTeam <- TestDataSetRF$HomeTeam
RF_Pred$AwayTeam <- TestDataSetRF$AwayTeam
RF_Pred$Season <- TestDataSetRF$Season

confusionMatrix(TestDataSetRF$FTR,RF_Pred$RF_Pred)

View(RF_Pred)
varImpPlot(RF_Prem)

plot(RF_Prem)





#Creating the multinominal linear regression model for the home team and away team

PremTrainHomeTeamMulti <- multinom(FTHG~HTHG+HTR+HS+HST+HC+HF+HY+HR, data=TrainingDataFull5)
PremTrainAwayTeamMulti <- multinom(FTAG~HTAG+HTR+AS+AST+AC+AF+AY+AR, data=TrainingDataFull5)

#Creating the predictions
HomePredictMulti <- predict(PremTrainHomeTeamMulti, newdata = TestDataSetLR, interval = "confidence")
AwayPredictMulti <- predict(PremTrainAwayTeamMulti, newdata = TestDataSetLR, interval = "confidence")

#Create Result Predicted Column
PredictionsMulti <- data.frame("Home_Predict" = HomePredictMulti, "Away_Predict" = AwayPredictMulti)

#Changed predictions to Integers
PredictionsMulti$Home_Predict <- as.integer(PredictionsMulti$Home_Predict)
PredictionsMulti$Away_Predict <- as.integer(PredictionsMulti$Away_Predict)

PredictionsMulti$Results_Predict <- ifelse(PredictionsMulti$Home_Predict > PredictionsMulti$Away_Predict,"H", 
                                      ifelse(PredictionsMulti$Home_Predict < PredictionsMulti$Away_Predict,"A",
                                             ("D")))

#Making the Results_Predict Column a Factor
PredictionsMulti<- PredictionsMulti%>%
  mutate(Results_Predict = as.factor(Results_Predict))

#Adding Teams to Predictions
PredictionsMulti$HomeTeam <- TestDataSetLR$HomeTeam
PredictionsMulti$AwayTeam <- TestDataSetLR$AwayTeam

View(PredictionsMulti)
#Testing the Accuracy of Multinominal Linear Regression Model- Predicted Vs Actual-Work

table(x = TestDataSetLR$FTR, y = PredictionsMulti$Results_Predict)
CrossTable(x=TestDataSetLR$FTR,y= PredictionsMulti$Results_Predict)
confusionMatrix(PredictionsMulti$Results_Predict, TestDataSetLR$FTR)


var(PremTrainAwayTeamMulti)

#Visualise the Predictions for Linear Regression

season_matrix_LR <- ggplot(Predictions,
                        aes(x = HomeTeam, y = AwayTeam, color=Results_Predict))+ 
  geom_point(size = 2)

season_matrix_LR <- season_matrix_LR + 
  theme(axis.text.x = element_text(angle = 90))

season_matrix_LR <- season_matrix_LR +
 ggtitle("Premier League 2019/20 Results Prediction") 


#Visualise the Predictions for Multinominal Linear Regression

season_matrix_MLR <- ggplot(PredictionsMulti,
                           aes(x = HomeTeam, y = AwayTeam, color=Results_Predict))+ 
  geom_point(size = 2)

season_matrix_MLR <- season_matrix_MLR + 
  theme(axis.text.x = element_text(angle = 90))

season_matrix_MLR <- season_matrix_MLR +
  ggtitle("Premier League 2019/20 Results Prediction")

season_matrix_MLR

#Visualise the Predictions for Random Forest Regression
season_matrix_RFR <- ggplot(RF_Pred,
                            aes(x = HomeTeam, y = AwayTeam, color=RF_Pred))+ 
  geom_point(size = 2)

season_matrix_RFR <- season_matrix_RFR + 
  theme(axis.text.x = element_text(angle = 90))

season_matrix_RFR <- season_matrix_RFR +
  ggtitle("Premier League 2019/20 Results Prediction")

season_matrix_RFR

remove(Test_Table)
#Calculating Premier League Table for the 2019/20 Season

test_table <- TestDataSetLR %>% select(HomeTeam, AwayTeam, FTHG, FTAG)
head(test_table)

calculate.win <- function(ourScore, theirScore){
  return(ourScore > theirScore)
}

calculate.lose <- function(ourScore, theirScore){
  return(ourScore < theirScore)
}

calculate.draw <- function(ourScore, theirScore){
  return(ourScore == theirScore)
}

calculate.points <- function(ourScore, theirScore){
  return(ifelse(ourScore < theirScore, 0, ifelse(ourScore == theirScore, 1, 3)))
}

# Calculate the Home data
home_table <- test_table %>%
  # Remove rows with team names missing
  filter(!(is.na(HomeTeam) | (HomeTeam == "") 
           | is.na(AwayTeam) | (AwayTeam == ""))) %>%
  # Calculate League Points for home team
  mutate(HWin  = calculate.win(FTHG, FTAG),
         HDraw = calculate.draw(FTHG, FTAG),
         HLose = calculate.lose(FTHG, FTAG),
         HPts  = calculate.points(FTHG, FTAG)) %>%
  # Create Home Team table entry
  group_by(HomeTeam) %>% summarise(HPlyd = length(HomeTeam),
                                   HWin  = sum(HWin),
                                   HDraw = sum(HDraw),
                                   HLose = sum(HLose),
                                   HFor  = sum(FTHG), 
                                   HAg   = sum(FTAG),
                                   HPts  = sum(HPts)) %>%
  rename(Team = HomeTeam)

head(home_table)

away_table <- test_table %>%
  # Remove rows with team names missing
  filter(!(is.na(HomeTeam) | (HomeTeam == "") 
           | is.na(AwayTeam) | (AwayTeam == ""))) %>%
  # Calculate League Points for away team
  mutate(AWin  = calculate.win(FTAG, FTHG),
         ADraw = calculate.draw(FTAG, FTHG),
         ALose = calculate.lose(FTAG, FTHG),
         APts  = calculate.points(FTAG, FTHG)) %>%
  # Create Away Team table entry
  group_by(AwayTeam) %>% summarise(APlyd = length(AwayTeam),
                                   AWin  = sum(AWin),
                                   ADraw = sum(ADraw),
                                   ALose = sum(ALose),
                                   AFor  = sum(FTAG), 
                                   AAg   = sum(FTHG),
                                   APts  = sum(APts)) %>%
  rename(Team = AwayTeam)
head(away_table)


final_table <- inner_join(home_table, away_table, by = "Team") %>%
  mutate(P  = HPlyd + APlyd, 
         W  = HWin  + AWin,
         D  = HDraw + ADraw,
         L  = HLose + ALose,
         GF = HFor  + AFor,
         GA = HAg   + AAg,
         GD = GF - GA,
         Pts  = HPts  + APts) %>%
  select(Team, P, W, D, L, GF, GA, GD, Pts) %>%
  arrange(desc(Pts), GD, GA, GF)

View(final_table)



#Predicting Results for the 2020/2021 Season
#Importing 2021 Season, played matches and matches yet to play
results2021<- read.csv("results2021.csv")

#Adding new variables to the results2021
results2021 <- results2021%>%
  mutate(HP= ifelse(FTR== "H","3",
                        ifelse(FTR=="D","1", "0")))%>%
  mutate(AP= ifelse(FTR== "A","3",
                          ifelse(FTR=="D","1", "0")))%>%
  mutate(SHG=FTHG)%>%
  mutate(SAG=FTAG)%>%
  mutate(SHS=HS)%>%
  mutate(SAS=AS)
  
View(results2021)
#Making the FTR Predict Column a Factor

results2021<- results2021%>%
  mutate(FTR = as.factor(FTR),HTR=as.factor(HTR), 
         HP = as.integer(HP),
         AP = as.integer(AP))

#Removing N/A rows
results2021clean <- na.omit(results2021)

#Creating the new variable averages for the HomeTeam for future prediction
         
ave_hometest <- results2021clean %>%
  group_by(HomeTeam) %>%
  summarise(FTHG=mean(FTHG), HTHG=mean(HTHG),HS=mean(HS),HST=mean(HST), HC=mean(HC), HF=mean(HF),
               HY=mean(HY), HR=mean(HR),TSHP=sum(tail(HP)),TSHG=sum(tail(SHG)),TSHS=sum(tail(SHS)),
                SHP=sum(HP),SHG=sum(SHG),SHS=sum(SHS))%>%
  rename(Team = HomeTeam)

ave_hometestteam <- filter(ave_hometest, Team =='Leicester')
View(ave_hometest)
#Creating the new variable averages for the AwayTeam for future prediction
ave_awaytest <- results2021clean %>%
  group_by(AwayTeam) %>%
  summarize(FTAG=mean(FTAG), HTAG = mean(HTAG),AS=mean(AS),AST=mean(AST), AC=mean(AC), AF=mean(AF),
            AY=mean(AY), AR=mean(AR),TSAP =sum(tail(AP)),TSAG=sum(tail(SAG)), TSAS=sum(tail(SAS)), 
            SAP=sum(AP),SAG=sum(SAG),SAS=sum(SAS))%>%
  rename(Team = AwayTeam)

ave_awaytestteam <- filter(ave_awaytest, Team =="West Brom")

#Creating the Linear Regression Models 
LR_2021Home <- lm(FTHG~HTHG+HS+HST+HC+HF+HY+HR+TSHP+TSHG+TSHS+SHP+SHG+SHS,data=ave_hometest)
LR_2021Away <- lm(FTAG~HTAG+AS+AST+AC+AF+AY+AR+TSAP+TSAG+TSAS+SAP+SAG+SAS,data=ave_awaytest)


#Creating the predictions
HomePredict2021 <- predict(LR_2021Home, newdata = ave_hometestteam, interval = "prediction")
AwayPredict2021 <- predict(LR_2021Away, newdata = ave_awaytestteam, interval = "prediction")


#Dropped the 2 columns from the predicted model as was causing complications
HomePredict2021 <-subset(HomePredict2021, select = -c(lwr,upr))
AwayPredict2021 <-subset(AwayPredict2021, select = -c(lwr,upr))

#Adding predicted values to new "Predictions" dataframe

Predictions2021 <- data.frame("Home_Predict" = HomePredict2021, "Away_Predict" = AwayPredict2021)
Predictions2021 <- Predictions2021%>%
  rename(Home_Predict = fit,
         Away_Predict = fit.1)

#Changed predictions to Integers
Predictions2021$Home_Predict <- as.integer(Predictions2021$Home_Predict)
Predictions2021$Away_Predict <- as.integer(Predictions2021$Away_Predict)

#Adding the teams who are to play to the predictions
Predictions2021$HomeTeam <- ave_hometestteam$Team
Predictions2021$AwayTeam <- ave_awaytestteam$Team

#Rounding the Predicted Values to Nearest Whole Numbers:
Predictions2021$Home_Predict <- round(Predictions2021$Home_Predict)
Predictions2021$Away_Predict <- round(Predictions2021$Away_Predict)


Predictions2021$Results_Predict <- ifelse(Predictions2021$Home_Predict > Predictions2021$Away_Predict,"H", 
                                           ifelse(Predictions2021$Home_Predict < Predictions2021$Away_Predict,"A",
                                                  ("D")))

#Making the Results_Predict Column a Factor
Predictions2021<- Predictions2021%>%
  mutate(Results_Predict = as.factor(Results_Predict))

View(Predictions2021)

varImp(LR_2021Home)
varImp(LR_2021Away)


#Calculating Predicted Premier League Table for the 2020/21 Season

Premier_League_Table <- read.csv("Premier League Prediction.csv")
View(Premier_League_Table)

#Strange Unicode altering my column needed removing when importing csv file
colnames(Premier_League_Table)[1] <- gsub('^...','',colnames(Premier_League_Table)[1])

test_table <- Premier_League_Table %>% select(HomeTeam, AwayTeam, FTHG, FTAG)


calculate.win <- function(ourScore, theirScore){
  return(ourScore > theirScore)
}

calculate.lose <- function(ourScore, theirScore){
  return(ourScore < theirScore)
}

calculate.draw <- function(ourScore, theirScore){
  return(ourScore == theirScore)
}

calculate.points <- function(ourScore, theirScore){
  return(ifelse(ourScore < theirScore, 0, ifelse(ourScore == theirScore, 1, 3)))
}

# Calculate the Home data
home_table <- test_table %>%
  # Remove rows with team names missing
  filter(!(is.na(HomeTeam) | (HomeTeam == "") 
           | is.na(AwayTeam) | (AwayTeam == ""))) %>%
  # Calculate League Points for home team
  mutate(HWin  = calculate.win(FTHG, FTAG),
         HDraw = calculate.draw(FTHG, FTAG),
         HLose = calculate.lose(FTHG, FTAG),
         HPts  = calculate.points(FTHG, FTAG)) %>%
  # Create Home Team table entry
  group_by(HomeTeam) %>% summarise(HPlyd = length(HomeTeam),
                                   HWin  = sum(HWin),
                                   HDraw = sum(HDraw),
                                   HLose = sum(HLose),
                                   HFor  = sum(FTHG), 
                                   HAg   = sum(FTAG),
                                   HPts  = sum(HPts)) %>%
  rename(Team = HomeTeam)

head(home_table)

away_table <- test_table %>%
  # Remove rows with team names missing
  filter(!(is.na(HomeTeam) | (HomeTeam == "") 
           | is.na(AwayTeam) | (AwayTeam == ""))) %>%
  # Calculate League Points for away team
  mutate(AWin  = calculate.win(FTAG, FTHG),
         ADraw = calculate.draw(FTAG, FTHG),
         ALose = calculate.lose(FTAG, FTHG),
         APts  = calculate.points(FTAG, FTHG)) %>%
  # Create Away Team table entry
  group_by(AwayTeam) %>% summarise(APlyd = length(AwayTeam),
                                   AWin  = sum(AWin),
                                   ADraw = sum(ADraw),
                                   ALose = sum(ALose),
                                   AFor  = sum(FTAG), 
                                   AAg   = sum(FTHG),
                                   APts  = sum(APts)) %>%
  rename(Team = AwayTeam)
head(away_table)


final_table <- inner_join(home_table, away_table, by = "Team") %>%
  mutate(P  = HPlyd + APlyd, 
         W  = HWin  + AWin,
         D  = HDraw + ADraw,
         L  = HLose + ALose,
         GF = HFor  + AFor,
         GA = HAg   + AAg,
         GD = GF - GA,
         Pts  = HPts  + APts) %>%
  select(Team, P, W, D, L, GF, GA, GD, Pts) %>%
  arrange(desc(Pts), GD, GA, GF)

View(final_table)

