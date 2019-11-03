###------------------
###Visualization
###------------------

###Students Name:Bhavana Gollamudi
###GNumber:G01178657

library(ggplot2)
rm(list=ls())

data <- read.csv('Gollamudi_AIT580/data/EmployeeAttrition.csv')
hist(data$Age)
ggsave("Age.pdf", width = 4, height = 4)
print("From the histogram you can see that more number of employees working here are between the age of 30 to 40.The number of youngsters .i.e. the age of 15-25 is the least. Over the years after the age of 35 the number of employees working decreases with increase in age.")

plot(data$Age,data$MonthlyIncome)
print("From the scattered plot you can see that the pay is roughly increasinh along with the age. The freshers .i.e. of age 15-30 are paied less. From the age of 30 there is an observerable hike in their salaries.The salaries of the experiences candidates is high.")
ggsave("AgevsMonthlyIncome.pdf", width = 4, height = 4)

