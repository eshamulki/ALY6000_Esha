## 1.Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse

path1 <- 'C:\\Users\\prita_000\\Desktop\\Northeastern University\\Assignments\\Module3'
setwd(path1)
x = 'Esha Mulki'
print(x)

install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyrplyr")
install.packages("tidyverse")
install.packages('data.table')

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyrplyr)
library(tidyverse)
library(data.table)

## 2.Import the inchBio.csv and name the table <bio>
bio<-read.csv("inchBio.csv",header = TRUE,sep = ",")
print(bio)
class(bio)

## 3.Display the head, tail and structure of <bio>
head(bio)
tail(bio)
str(bio)
summary(bio)

## 4.Create an object, <counts>, that counts and lists all the species records

counts<-bio %>% select(species)
counts


## 5. Display just the 8 levels (names) of the species

species_name <- unique(bio$species)
species_name


## 6. Create a <tmp> object that displays the different species and the number of record of each species in the dataset. 
##Include this information in your report.-

tmp<-bio %>% count(species)
tmp

## 7. Create a subset, <tmp2>, of just the species variable and display the first five records

tmp2 <-bio %>% subset.data.table(species) %>%head(5)
tmp2

## 8. Create a table, <w>, of the species variable. Display the class of w

##By default character variables are converted into factors

w<-table(bio$species)
print(w)
class(w)

## 9. Convert <w> to a data frame named <t> and display the results

t <- data.frame(w)
print(t)
class(t)

## 10. Extract and display the frequency values from the <t> data frame

freq <- xtabs(~w1, data=t)
print(freq)

## 11. Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table
## which displays the number of species in the dataset <bio>

cSpec <- table(bio$species)
cSpec

## 12. Create a table named <cSpecPct> that displays the species and percentage of records for each species. 
#3 Confirm you created a table class.

cSpecPct <- table(bio$species)/676
cSpecPct
class(cSpecPct)


# 13. Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame

u <- data.frame(cSpecPct)
class(u)

# 14. Create a barplot of <cSpec> with the following: titled Fish Count with the following specifications:

barplot(cSpec,main ="Fish Count", ylab="COUNTS", col="Lightgreen", las=2 , 
        cex.axis = 1.6 , cex.lab=1.2, cex.names = 0.7)

# 15. Create a barplot of <cSpecPct>, with the following specifications:

barplot(cSpecPct,main ="Fish Relative Frequency", ylab="Percent", col="Lightblue", 
        ylim=c(0,0.4), las=2 , cex.axis = 1.6 , cex.names = 0.6)

# 16. Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d>

d<- u[with(u,order(-Freq)),]
print(d)

#   17. Rename the <d> columns Var 1 to Species, and Freq to RelFreq

names(d) [names(d)=="Var1"]="Species"
names(d) [names(d)=="Freq"]="RelFreq"
print(d)

# 18. Add new variables to <d> and call them cumfreq, counts, and cumcounts

d <- mutate(d, cumfreq=cumsum(RelFreq), counts=RelFreq*676, cumcounts=cumsum(counts))
print(d)

# 19. Create a parameter variable <def_par> to store parameter variables
def_par <-par(mar = c(6,8,2,19))
def_par

# 20. Create a barplot, <pc>, with the following specifications:

pc<-barplot(d$counts,  
            width = 1, space = 1.5, border = NA, axes = F,
            ylim = c(0, 3.05 * max(d$counts, na.rm = T)), 
            ylab = "Cummulative Counts" ,  
            names.arg = d$Species,cex.names = 0.7,
            main = "Species Pareto",las=2)

# 21. Add a cumulative counts line to the <pc> plot with the following:

lines(pc, d$cumcount, type = "b", cex = 0.7, pch = 19, col="cyan4")

# 22. Place a grey box around the pareto plot (hint: https://www.statmethods.net/advgraphs/parameters.html)

box(col="grey62")

# 23. Add a left side axis with the following specifications

axis(side = 2, at = c(0, d$cumcount), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)

# 24. Add axis details on right side of box with the specifications:

axis(side = 4, at = c(0, d$cumcount), labels = paste(c(0, round(d$cumfreq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "cyan3", col.lab = "cyan4", cex.axis = 0.8)

## 25. Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot

axis(side = 4, at = c(0, d$cumcount), labels = paste(c(0, round(d$cumfreq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col.lab = "cyan4", cex.axis = 0.8)
