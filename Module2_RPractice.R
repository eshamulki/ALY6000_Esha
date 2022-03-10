install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyrplyr")
install.packages("tidyverse")
install.packages('data.table')
install.packages("Rmisc")
install.packages("psych")

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyrplyr)
library(tidyverse)
library(data.table)
library(Rmisc)
library(psych)

#importing the dataset
df <- read.csv("winequality-red.csv", header = TRUE)
describe(df)

#renaming the numerical values for the wine quality

df$quality <- gsub("3","Very Poor",df$quality)
df$quality <- gsub("4","Poor",df$quality)
df$quality <- gsub("5","Average",df$quality)
df$quality <- gsub("6","Good",df$quality)
df$quality <- gsub("7","Very Good",df$quality)
df$quality <- gsub("8","Excellent",df$quality)
df

#subsetting data
SUbset <- df %>% filter(df$quality == "Very Good" | df$quality == "Excellent")
SUbset
describe(SUbset)


#boxplot for total sulfur dioxide column

ggplot(df, aes(factor(quality), `total.sulfur.dioxide`, group=quality))+geom_boxplot(aes(color = quality))
+ labs("Fixed Acidity & Quality")+theme_light()


# creating jitter chart

ggplot(df, aes(x = quality, y = fixed.acidity)) + 
  geom_jitter(position = position_jitter(0.5), 
              aes(colour = quality))

#histogram of residual sugar

ggplot(df, aes(residual.sugar))+geom_histogram(fill="skyblue")+
labs(title = 'Distribution of residual sugar',
     y = 'Count')+theme_light()

#Wine quality distribution
ggplot(df, aes(factor(quality)))+geom_bar(fill="lightpink")+ggtitle("Quality Distribution")+theme_light()

#scatterplot of alcohol vs density

ggplot(data=df,aes(x=alcohol, y=density, col=quality))+geom_point()+
  labs(title = 'Alcohol content vs Density')

#boxplot of Alcohol versus quality

ggplot(df, aes(factor(quality), `alcohol`, group=quality))+geom_boxplot(aes(color = quality))+
labs("Alcohol & Quality")+theme_light()
  abline(h = 12, col="red")

#scatterplot with regression line 
  
plot(x=df$alcohol, y=df$density, pch=16, col="red")
abline(reg = lm(df$density ~ df$alcohol), col="blue", lwd=5, lty=2)
