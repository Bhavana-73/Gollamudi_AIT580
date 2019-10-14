#Student Name:Bhavana Gollamudi
#GNumber:G01178657
###--------------------------------------

rm(list=ls())
setwd("C:/Users/gobha/OneDrive/Desktop/MSDA-1/AIT/GIT/AIT580/data")
data <- read.csv('EmployeeAttrition.csv')

# this is just for testing to use "print" statement.
print(data[1,])



# a. Find the number of rows and columns in the dataset (5 points)
nrow(data)
ncol(data)
print("There are 1470 rows and 35 columns")
# b. Find the maximum Age in the dataset (5 points)
max(data$Age)
print("Maximum age is 60")

# c. Find the minimum DailyRate in the dataset (5 points)
min(data$DailyRate)
print("Minimum Daily rate is 102")
# d. Find the average/mean MontlyIncome in the dataset (5 points)

mean(data$MonthlyIncome)
print("The mean Mouthly income is 6502.931")

# e. How many employees rated WorkLifeBalance as 1 (5 points)
length(which(data$WorkLifeBalance==1))

# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
length(which(data$TotalWorkingYears<5))*100/length(data$TotalWorkingYears)
length(which(data$TotalWorkingYears>5))*100/length(data$TotalWorkingYears)
print("percent working years < 5:15.5102 and percent working years>5:78.5034")
# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
library(sqldf)
print("table:  ")
sqldf("SELECT EmployeeNumber, Department, MaritalStatus FROM data WHERE Attrition='Yes' and RelationshipSatisfaction=1 and YearsSinceLastPromotion>3")

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
print("Mode")
X<-sqldf("SELECT count(EnvironmentSatisfaction) FROM data group by Gender, EnvironmentSatisfaction having Gender='Male' ")
max(X)
X<-sqldf("SELECT count(EnvironmentSatisfaction) FROM data group by Gender, EnvironmentSatisfaction having Gender='Female' ")
 max(X)
sqldf("SELECT gender, avg(EnvironmentSatisfaction) FROM data group by Gender")
      mean(subset(data$EnvironmentSatisfaction,data$Gender=='Female'))
      mean(subset(data$EnvironmentSatisfaction,data$Gender=='Male'))
      print("mean")
      median(subset(data$EnvironmentSatisfaction,data$Gender=='Male'))
      median(subset(data$EnvironmentSatisfaction,data$Gender=='Female'))
      print("Frequency table")
      table(data$Gender=="Male",data$EnvironmentSatisfaction)
      table(data$Gender=="Female",data$EnvironmentSatisfaction)
#Question 2
      data <- read.csv('Acme.csv')
      #1. Identify data types for each attribute in the dataset (5 points) 
          data$Gender<-as.character(data$Gender)
          data$Degree<-as.character(data$Degree)
          sapply(data, typeof)
      #2. Produce a summary statistic for each attribute in the dataset (5 points)
          summary(data)
      #3. Produce visualizations for each attribute (Hint: use hist() function) (5 points)
          hist(data)
      #4. Display the relationship between  
      #    a. Years of Experience and Starting Salary for all employees (5 points) 
              scatterplot(data$Years ~ data$GSalary, data = data)
      #    b. Years of Experience and Starting Salary for each gender (5 points) 
              GSalary<-sqldf("select Years,min(StSalary) from data group by Gender")
              plot(GSalary)
      #    c. Years of Experience and Starting Salary for each degree (5 points)  (Hint: use Scatter Plots) 
              DSalary<-sqldf("select Years,min(StSalary) from data group by Degree")
              plot(DSalary)
      #5. Find the correlation between Starting Salary and Years of Experience? (5 points) 
              lm(formula = Years~StSalary,data=data)
      #    a. Is the correlation different for each gender? (5 points) 
              X<-aov(formula = data$StSalary ~ data$Years + data$Gender, data = data)
              summary(X)
              print("the corelation is different for different genders")
      #    b. Is the correlation different for each degree? (5 points) 
              Y<-aov(formula = data$StSalary ~ data$Years + data$Degree, data = data)
              summary(Y)
              print("The correlation is different for different degrees")
      #6. What can you conclude about Acme with respect to gender bias after your overall analysis? (5 points)       
      print(" There is a serious gender bias in the above case.First the male population is more compared to female and
            the minimum salary of males with 1 year experience is more compared to that of females with 2 years experience.")
      