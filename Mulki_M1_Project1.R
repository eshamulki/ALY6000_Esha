x="Esha Mulki"

path <- "C:\\Users\\prita_000\\Desktop\\Northeastern University\\Assignments\\Module1Project1"
setwd(path)

print(x)

##install.packages("vcd")

library(vcd)

sales_data <- c(7,11,15,20,19,11,18,10,6,22)
temp_data <- c(69,81,77,84,80,97,87,70,65,90)

plot(temp_data,sales_data)

mean(temp_data)

sales_data <- c(7,11,15,20,19,11,18,10,6,22)
indices <- c(3)
result <- sales_data[-indices]
print(result)

sales_data <- c(7,11,15,20,19,11,18,10,6,22)
result1 <- append(result,16,2)
print(result1)

names <- c("Tom","Dick","Harry")
print(names)

y <- matrix(1:10, nrow = 5, ncol = 2)
y


sales_data <- c(7,11,15,20,19,11,18,10,6,22)
temp_data <- c(69,81,77,84,80,97,87,70,65,90)
icSales <- data.frame(sales_data,temp_data)
icSales
str(icSales)
summary(icSales)


student_data <- read.csv("student.csv", header = TRUE, sep = ",")
student_data

student_data1 <- read.table("student.csv", header = TRUE, sep = ",")
student_data1

