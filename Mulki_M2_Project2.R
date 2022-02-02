##Print your name at the top of the script. Include the prefix: "Plotting Basics:"

x = 'Plotting Basics:Mulki'
path1 <- 'C:\\Users\\prita_000\\Desktop\\Northeastern University\\Assignments\\Module2Project1'
setwd(path1)
print(x)

##Import libraries including: FSA, FSAdata, magrittr, dplyr, plotrix, ggplot2, and moments

install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

##Load the BullTroutRML2 dataset (BullTroutRML2.csv)

help(FSAdata)
help.search("BullTroutRML2",package=c("FSAdata","FSA"))
BullTrout <- BullTroutRML2

head(BullTrout)

##Print the first and last 3 records from the BullTroutRMS2 dataset

newdata <- BullTrout[c(1:3,(count(BullTrout)[1,1]-2):(count(BullTrout)[1,1])),]
newdata


##Remove all records except those from Harrison Lake (hint: use the <filterD() function)
Harrisonlake <- filterD(BullTrout,lake=="Harrison")
Harrisonlake

##Display the first and last 5 records from the filtered BullTroutRML2 dataset
newdata1 <- Harrisonlake[c(1:5,(count(Harrisonlake)[1,1]-4):(count(Harrisonlake)[1,1])),]
newdata1

##Display the structure of the filtered BullTroutRML2dataset
str(Harrisonlake)

##Display the summary of the filtered BullTroutRML2dataset
summary(Harrisonlake)

##Create a scatterplot for "age" (y variable) and "fl" (x variable) with the following specifications:

plot(x=Harrisonlake$fl, y=Harrisonlake$age, 
      xlab="Fork Length (mm)", ylab="Age (yrs)", 
     xlim=c(0,500),ylim=c(0,15), pch=19,
     main="Plot 1: Harrison Lake Trout")

##Plot an "Age" histogram with the following specifications

hist(Harrisonlake$age, ylab="Frequency", xlab="Age (yrs)", 
     main="Plot 2: Harrison Fish AgeDistribution",
     xlim=c(0,15), ylim=c(0,15), 
     col="cadetblue", col.main= "cadetblue")

##Create an overdense plot using the same specifications as the previous scatterplot.
plot(x=Harrisonlake$fl, y=Harrisonlake$age, 
     xlab="Fork Length (mm)", ylab="Age (yrs)", 
     xlim=c(0,500),ylim=c(0,15), pch=19,
     col="green",
     main="Plot 3: Harrison Density Shaded by Era")

##Create a new object called "tmp" that includes the first 3 and last 3 records of the BullTroutRML2 data set.
tmp <- data.frame(rbind(head(BullTroutRML2,3),tail(BullTroutRML2, 3)))
tmp

##Display the "era" column (variable) in the new "tmp" object
tmp %>% select(era)

##Create a pchs vector with the argument values for + and x.
pchs <-c("+", "*")
pchs

##Create a cols vector with the two elements "red" and "gray60"
cols <-c ("red", "gray60")

##Convert the tmp era values to numeric values.
tmp$era <- as.numeric(tmp$era)
tmp$era

##Initialize the cols vector with the tmp era values
cols <- tmp$era
cols

##Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the following specifications:
plot(x=Harrisonlake$fl, y=Harrisonlake$age, main="Plot 4: Symbol & Color by Era",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch= pchs,
     col= cols)

##Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay"
plot(x=Harrisonlake$fl, y=Harrisonlake$age, main="Plot 5: Regression Overlay",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch= pchs,
     col= cols)
abline(lm(Harrisonlake$age~Harrisonlake$fl), col="blue", main= "Plot 5: Regression Overlay")

##Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay"
plot(x=Harrisonlake$fl, y=Harrisonlake$age, main="Plot 6: Legend Overlay",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch= pchs,
     col= cols)
abline(lm(Harrisonlake$age~Harrisonlake$fl), col="blue", main= "Plot 6: Legend Overlay")
legend("topleft", inset=0.05, legend=levels(Harrisonlake$era), col=cols, text.col = cols)

