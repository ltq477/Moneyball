#Set working directory
setwd("/Users/Ence477/OneDrive/DataScience/Projects/Moneyball/")

#load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(googleVis)


#read dataframes from dataset
df.batting <- read.csv('Batting.csv')
df.salaries <- read.csv('Salaries.csv')

#view dataframes structure
str(df.batting)
str(df.salaries)

#Quick view of data from dataframes
head(select(df.batting, AB), 5)

head(select(df.batting, X2B))
head(df.batting['X2B'])
head(df.batting$X2B,5)

#create new feature columns with formulas
df.batting$X1B <- df.batting$H - df.batting$X2B - df.batting$X3B - df.batting$HR
df.batting$BA <- df.batting$H / df.batting$AB
df.batting$OBP <- (df.batting$H + df.batting$BB + df.batting$HBP) / (df.batting$AB + df.batting$BB + df.batting$HBP + df.batting$SF)
df.batting$SLG <- (df.batting$X1B) +(2 * df.batting$X2B) + (3 * df.batting$X3B) + (4 * df.batting$HR) / df.batting$AB
head(df.batting, 30)

#view col names for references
colnames(df.batting)
colnames(df.salaries)

#merge 2 dataframes by playerid, yeadid, teamid, lgid
df <- merge(df.batting, df.salaries, by = c('yearID','teamID','lgID','playerID'))
summary(df)
head(df, 30)

#pull players on and after 1985
df.1985 <- subset(df, yearID >=1985)
summary(df.1985)

#create a list of lost players
lost.players.list <- c('giambja01','damonjo01','saenzol01')

#select lost players from list of players after 1985
lost.players <- subset(df.1985, playerID %in% lost.players.list)
head(lost.players)

#create new dataset with lost players and specified specific column/features
lost.players.2001 <- subset(lost.players, yearID==2001)
lost.players.2001[c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB','yearID')]
head(lost.players.2001)
head(df)

avail <- filter(df, yearID==2001) 

#OBP vs Salary
plot.OBP.Sal <- avail[c('OBP','salary')] 
plot(gvisScatterChart(plot.OBP.Sal))

#AB vs Salary
plot.AB.Sal <- avail[c('AB','salary')]
plot(gvisScatterChart(plot.AB.Sal))

#final 3 selected
final <- avail %>% filter(salary<=5000000, OBP>0.34, AB>=600)  %>% arrange(AB, OBP, salary)
final[1:3,]

#Extra Notes 
#1 view head(lost.players.2001) to compate and make decision
#2 Combined salary  must not exceed $15,000,000. $15,000,000 / 3 = $5,000,000
#3 lowest OBP is .34, so anything above that
#4 anything above 600 was consistent so 600 was the chosen AB