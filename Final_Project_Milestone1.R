install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyrplyr")
install.packages("tidyverse")
install.packages('data.table')
install.packages("Rmisc")
install.packages("psych")
install.packages("plotly")

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyrplyr)
library(tidyverse)
library(data.table)
library(Rmisc)
library(psych)
library(lubridate)
library(plotly)

#importing the dataset
ndf <- read.csv("netflix_titles.csv")


# data structure
str(ndf)
summary(ndf)

# Data cleaning

#changing the data and year in date format

ndf$date_added <- parse_date_time(ndf$date_added,'mdy')
ndf$release_year <- parse_date_time(ndf$release_year,'y')

# Checking the blank values to NA
ndf$director[ndf$director==""] <- NA
ndf$cast[ndf$cast==""] <- NA
ndf$country[ndf$country==""] <- NA
ndf$rating[ndf$rating==""] <- NA

#create a separate data frame to view the missing values.

subs = subset(ndf,select = -c(show_id))
data.frame("variable"=c(colnames(subs)), 
           "missing values count"=sapply(subs, function(x) sum(is.na(x))),
           row.names=NULL)


# 4. Revise rating mismatch

# Rating has 3 strange values, which look more like duration instead of rating.
unique(ndf$rating)

# Filter down to these records and find that duration column is blank,
# which means those values should be duration instead of rating.
filter(ndf, rating %in% c("74 min", "84 min", "66 min"))

# Revise the mismatch
ndf["duration"][ndf["rating"] == "74 min"] <- "74 min"
ndf["duration"][ndf["rating"] == "84 min"] <- "84 min"
ndf["duration"][ndf["rating"] == "66 min"] <- "66 min"
ndf["rating"][ndf["rating"] == "74 min"] <- ""
ndf["rating"][ndf["rating"] == "84 min"] <- ""
ndf["rating"][ndf["rating"] == "66 min"] <- ""

# Graph for number of TV shows and Movies
ndf %>% 
  ggplot(aes(type,fill=type)) + 
  geom_bar()+
  #scale_fill_brewer()+
  coord_flip() +
  geom_text(stat='count',aes(label=..count..),hjust=1.5)+
  labs(title = 'Number of TV shows and Movies')

# Frequency table count and percentage
type_count <- count(ndf$type)
type_count$percen <- paste0(round((type_count$freq/sum(type_count$freq))*100,2),'%') 
type_count

#Pie chart for freq table
type_count %>% 
  ggplot(aes(x="",y=percen,fill=x)) + 
  geom_bar(stat = 'identity') + 
  coord_polar("y",start=0)+
  scale_fill_brewer(palette='Pastel2')+
  geom_text(aes(y=percen,label=percen),
            position=position_stack(vjust=0.5))+
  theme_void()+labs(title = "Percentage of Movies and TV Shows")

#Bar Graph for the freq table
type_count %>% 
  ggplot(aes(x="",y=freq,fill=x)) + 
  geom_bar(stat = 'identity',position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')

#Cumulative time series
ndf %>% 
  group_by(release_year,type) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  mutate(cummul=cumsum(count)) %>% 
  ggplot(aes(x=release_year,y=cummul,color=type))+
  geom_line(size=2)+ labs(title = "Cumulative Time series of Movies and TV Shows")

#Movie duration along the years

ndf %>% 
  filter(type=='Movie'& release_year<="2020-02-01" & release_year>="2000-01-01") %>% 
  mutate(movie_duration=substr(duration,1,nchar(as.character(duration))-4)) %>% 
  mutate(movie_duration = as.integer(movie_duration)) %>% 
  group_by(release_year) %>% 
  summarise(avg_run = mean(movie_duration)) %>% 
  ggplot(aes(x=release_year,y=avg_run, color="blue")) +
  geom_line(size=2)+ labs(title = "Movie Duration along the years")

# Graph of rating by type
ndf %>%
  filter(!(rating %in% c("74 min", "84 min", "66 min", ""))) %>%
  ggplot() +
  aes(x = rating, fill = rating) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(x = "Rating", y = "Count", title = "Rating by Type") +
  theme_minimal() +
  facet_wrap(vars(type)) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Calculation for getting the top 10 countries

#By country
ndf %>% 
  group_by(country) %>% 
  count()

#Considering the top 10 countrys
top_countries <- ndf %>%  
  separate_rows(country,sep=", ") %>% 
  group_by(country) %>%  
  summarise(countt=n()) %>% 
  remove_missing() %>% 
  arrange(desc(countt))

#top countries stored in vector
tc <- as.vector(top_countries$country[1:10])

ndf %>% 
  separate_rows(country,sep=", ") %>%
  filter(country %in% tc) %>% 
  mutate(country=factor(country,levels = tc)) %>% 
  group_by(country,type) %>% 
  summarise(countt=n()) %>% 
  ggplot(aes(x=country,y=countt,fill=type))+
  geom_bar(stat='identity')+
  coord_flip()+labs(title = "Top 10 countries with Netflix Content")



















  