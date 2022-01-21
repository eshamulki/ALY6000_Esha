path <- "C:\\Users\\prita_000\\Desktop\\Northeastern University\\Assignments\\Module1Project1"
setwd(path)

x <-"Esha Mulki"

print(x)            #Print name on top of script

install.packages("vcd")            #install the vcd package

library(vcd)                      #import vcd library

#Plot a sales ~ temp scatter plot using the data below

sales_data <- c(7,11,15,20,19,11,18,10,6,22)
temp_data <- c(69,81,77,84,80,97,87,70,65,90)

plot(temp_data,sales_data)           

mean(temp_data)                    #Find the mean temperature

#Delete the 3rd element from the sales vector

sales_data <- c(7,11,15,20,19,11,18,10,6,22)
sales_data <- sales_data[-3]
sales_data

#Insert 16 as the 3rd element into the sales vector
#sales_data <- c(7,11,15,20,19,11,18,10,6,22)
sales_data <- append(sales_data,16,2)
print(sales_data)

#Create a vector <names> with elements Tom, Dick, Harry
names <- c("Tom","Dick","Harry")
print(names)

#Create a 5 row and 2 column matrix of 10 integers
y <- matrix(1:10, nrow = 5, ncol = 2)
y

#Create a data frame <icSales> with sales and temp attributes
sales_data <- c(7,11,15,20,19,11,18,10,6,22)
temp_data <- c(69,81,77,84,80,97,87,70,65,90)
icSales <- data.frame(sales_data,temp_data)
icSales
#Display the data frame structure of icScales
str(icSales)
#Display a summary of the icScales data frame
summary(icSales)

#Import the dataset Student.csv
student_data <- read.csv("student.csv", header = TRUE, sep = ",")
student_data

#Display only the variable names of the Student.csv dataset
student_data1 <- read.table("student.csv", header = TRUE, sep = ",")
student_data1


