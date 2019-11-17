###------------------

###Regression and Clustering

###------------------



###Students Name:Bhavana Gollamudi

###GNumber:G01178657





rm(list=ls())



install.packages(ggplot2)



library(ggplot2)

#Regression

data <- read.csv('C:/Users/gobha/OneDrive/Desktop/MSDA-1/AIT/GIT/AIT580/data/EmployeeAttrition.csv')
#Q1
with(data,scatter.smooth(TotalWorkingYears,MonthlyIncome))
#From the scatter graph it can be observed that the mounthly income is increasing
#along with the total working years. And also the number of people is also decreasing
#with number of working years.

#Q2
with(data,scatter.smooth(Age,DistanceFromHome))
#The age and distance traveled from home for each employee is evenly distributed.
#So all the employees travel all range of distance from their home irrespective to
# their age.

#Q3
cor(data$TotalWorkingYears,data$MonthlyIncome)
#The correlation is positive and close to one. Thus the Monthly income increases with
#the monthly income. It is the same as we observed in the scattered plot above. 
cor(data$Age,data$DistanceFromHome)
#The correlation is negative and close to zero. Thus the Age and distance from home
#are almost not dependent on each other. It is same as we observed in the scatter plot.

#Q4
x<-lm(data$TotalWorkingYear~data$MonthlyIncome)
summary(x)
#The p-value is very small and less than 0.05. Therefore TotalWorkingYear and MonthlyIncome
# are not related to each other if we consider 95% confidence interval.

#Clustering
library(ggplot2)
x<-data.frame(data['HourlyRate'],data['YearsAtCompany'])
plot(x)
#Q1
colnames(x) <- c("HourlyRate", "YearsAtCompany")
cl <- kmeans(x, 3)

plot(x, col = cl$cluster)

points(cl$centers, col = 1:3, pch = 8, cex = 2)
#All the points have been divided into 3 clusters. The first cluster contains
# the employees whose hourly rate is between 30 to 50 and worked different years
#at the company.The second cluster contains the employees whose hourly rate is 
#between 50 to 75 and worked different years at the company.The last cluster contains
# the employees whose hourly rate is between 75 to 100 and worked different years
#at the company.

#Q2
cl <- kmeans(x, 5)

plot(x, col = cl$cluster)

points(cl$centers, col = 1:5, pch = 8, cex = 2)
#All the points have been divided into 5 clusters. The first cluster contains
# the employees whose hourly rate is between 30 to 45 and worked different years
#at the company.The second cluster contains the employees whose hourly rate is 
#between 45 to 55 and worked different years at the company.The third cluster contains
# the employees whose hourly rate is between 55 to 70 and worked different years
#at the company.The fourth cluster contains the employees whose hourly rate is
#between 70 to 85 and worked different years at the company.The last cluster contains
# the employees whose hourly rate is between 85 to 100 and worked different years
#at the company.
