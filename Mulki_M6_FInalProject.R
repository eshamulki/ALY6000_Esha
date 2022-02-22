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

#setting the theme for our plots
theme_set(theme_minimal())
options( repr.plot.height=4)

dataset <- read.csv("StudentsPerformance.csv")
str(dataset)
summary(dataset)

# Checking if there is any missing values
sapply(dataset, function(x)sum(is.na(x)))

#calculating total and avg score
dataset$total.score = dataset$math.score +dataset$reading.score +dataset$writing.score
dataset$avg.score = round((dataset$total.score)/3,0)

#assigning grades as per avg.score
dataset <- dataset %>% 
  mutate(grade = case_when(
    avg.score >= 90 & avg.score <= 100 ~ "A",
    avg.score >= 80 & avg.score < 90 ~ "B",
    avg.score >= 70 & avg.score < 80 ~ "C",
    avg.score >= 60 & avg.score < 70  ~ "D",
    avg.score >= 50 & avg.score < 60  ~ "E",
    avg.score < 50 ~ "F"
  )%>% as.factor()
  )
head(dataset, n=10)

#summary again
summary(dataset)

#Box plots to visualize any outliers in the dataset

boxplot(dataset$math.score,ylab="Math Scores",main="Box plot for math scores")

boxplot(dataset$reading.score,ylab="Reading Scores",main="Box plot for Reading scores")

boxplot(dataset$writing.score,ylab="Writing Scores",main="Box plot for Writing scores")

boxplot(dataset$avg.score,ylab="Avg Scores",main="Box plot for Avg scores")

#Grade distribution by gender
ggplot(dataset, aes( x= grade, fill = gender)) + 
  geom_bar(position="dodge", stat="count") + 
  geom_text(stat="count" ,aes(label=..count..), position =  position_dodge(width = 1))+
  scale_y_continuous(breaks = c(50, 100, 150,200, 250))+
  labs(title ="Distribution of Grades by Gender", x ="Grades", y = " No of dataset")

#Distribution of grades by Ethnic group
ggplot(dataset, aes(x=race.ethnicity, fill = grade)) + 
  geom_bar( position ="fill", stat ="count")+
  scale_y_continuous(labels = scales::percent)+
  labs(title="Distribution of Grades by Ethnic Background", y ="Percentage of dataset", x = "Ethnic Group")

#Distribution of grades by Parent Education
ggplot(dataset, aes(x=parental.level.of.education, fill = grade)) + 
  geom_bar( position ="fill", stat ="count")+
  scale_y_continuous(labels = scales::percent)+
  labs(title="Distribution of Grades by Parent Education", y ="Percentage of dataset", x = "Parent Edu")

#Distribution of grades by Parent Education in dodge style
ggplot(dataset, aes(x=parental.level.of.education, fill = grade)) + 
  geom_bar(position="dodge", stat="count") +
  geom_text(stat="count" ,aes(label=..count..), position =  position_dodge(width = 1))+
  scale_y_continuous(breaks = c(50, 100, 150,200, 250))+
  labs(title="Distribution of Grades by Parent Education", y ="Percentage of dataset", x = "Parent Edu")

#Type of lunch by grades

  ggplot(dataset, aes( x= grade, fill = lunch)) + 
  geom_bar(position = "dodge", stat="count") +
  geom_text(stat="count",aes(label=..count..), position = position_dodge(width = 1))+
  scale_y_continuous(breaks = c(50, 100, 150,200, 250))+
  labs(title ="Distribution of Grades by Type of Lunch", x= "Grade", y = "No of dataset")
  
#Grades with test preparation

  ggplot(dataset, aes( x= grade, fill = test.preparation.course)) + 
    geom_bar(position = "dodge", stat="count") +
    geom_text(stat="count",aes(label=..count..), position = position_dodge(width = 1))+
    scale_y_continuous(breaks = c(50, 100, 150,200, 250))+
    labs(title ="Distribution of Grades by Test Preparation", x= "Grade", y = "No of dataset")  
  