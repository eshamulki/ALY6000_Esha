install.packages("FSA")
install.packages("FSAdataset")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyrplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readr")
install.packages("ggrepel")
install.packages("cowplot")

library(FSA)
library(FSAdataset)
library(magrittr)
library(dplyr)
library(tidyrplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggrepel)
library(cowplot)
library(ggthemes)
require(scales)

#Import the dataset

hfd <- read.csv("heart.csv", header = TRUE)
headtail(hfd)
str(hfd)
summary(hfd)
class(hfd)
colnames(hfd)

#Dataframe

hfd <- as.data.frame(read.csv("heart.csv", header= TRUE))

# Checking if there is any missing values
sapply(hfd, function(x)sum(is.na(x)))

#Replace the Heart disease column values || Data Cleaning

hfd$HeartDisease <- gsub("0","No",hfd$HeartDisease)
hfd$HeartDisease <- gsub("1","Yes",hfd$HeartDisease)

# Applying appropriate data structure

hfd$Age <- as.numeric(hfd$Age)
hfd$FastingBS <- as.numeric(hfd$FastingBS)
hfd$MaxHR <- as.numeric(hfd$MaxHR)
hfd$HeartDisease <- as.factor(hfd$HeartDisease)
str(hfd)

#Box plots to visualize any outliers in the dataset

boxplot(hfd$RestingBP,ylab="Resting BP",main="Box plot for Resting BP")

boxplot(hfd$Cholesterol,ylab="Cholestrol",main="Box plot for Cholestrol")

boxplot(hfd$MaxHR,ylab="MaxHR",main="Box plot for MaxHR")

# Fixing zeros for Cholesterol and Resting BPs Column values

hfd$Cholesterol[which(hfd$Cholesterol == 0)] <- mean(hfd$Cholesterol)

hfd$RestingBP[which(hfd$RestingBP == 0)] <- mean(hfd$RestingBP)

# Checking summary again

summary(hfd)

#Plotting of Frequency Tables: For the categorical variable: "RestingECG" 
ECG_Freq <- hfd %>%
  select(RestingECG) %>%
  group_by(RestingECG) %>%
  dplyr:: summarize(ECG_count = n())
ECG_Frequency


##Plotting of Frequency Tables: For the categorical variable: "ST_Slope" 
ST_Slope_Freq <- hfd %>%
  select(ST_Slope) %>%
  group_by(ST_Slope) %>%
  dplyr:: summarize(ST_Slope_count = n())
ST_Slope_Freq

#Doing Cross Tabulation : Sex and Heart

CrossTab1 <- xtabs(~Sex +HeartDisease, data=hfd) 
CrossTab1

##Doing Cross Tabulation : Sex and FastingBS 
xtabs(~Sex +ChestPainType, data=hfd)


# Plotting histogram of heart disease with age
ggplot(hfd,aes(Age)) + 
  geom_histogram(aes(fill=HeartDisease),color='black',
        position='identity',alpha=0.4, binwidth=1)


# Plotting histogram of heart disease with cholesterol

ggplot(hfd,aes(Cholesterol)) + geom_histogram(aes(fill=HeartDisease),
            color='black',position='identity',alpha=0.4)

# Graph of MaxHR with Age

ggplot(hfd, aes(x = Age, y = MaxHR, color=Sex))+
  geom_point() +
  labs(title = "Age vs MaxHR",
       x = 'Age',
       y = 'MaxHR')


#Distribution of heart disease among sexes

ggplot(hfd, aes(x= HeartDisease, fill = HeartDisease)) +
  geom_bar()+
 facet_grid(~Sex) +
  geom_text(aes(label = ..count..), 
            stat = 'count', vjust = 1.2, color = "white") +
  labs(title = 'Diagnosis of Heart Disease between Male and Female',
       y = 'Frequency')


#Distribution of Chest Pain based on heart disease

ggplot(hfd, aes(x= ChestPainType, fill = ChestPainType)) +
  geom_bar()+
  facet_grid(~HeartDisease) + labs(title = 'Types of ChestPain based on HeartDisease')


