###------------------
###Hypothesis Testing
###------------------

###Students Name:Bhavana Gollamudi
###GNumber:G01178657



rm(list=ls())

data <- read.csv('Gollamudi_AIT580/data/EmployeeAttrition.csv')

library(sqldf)
# Your hypothesis testings here...
#1. If the MonthlyIncome of Males is greater than Females (5 points) 
X<-sqldf("select MonthlyIncome from data where Gender='Male' ")
Y<-sqldf("select MonthlyIncome from data where Gender='Female' ")
t.test(X,Y,alternative ="greater")
print("the monthly income of male is equal to that of male")
#for the first Question H0:monthly income is male is equal to female
#                       H1:monthly income of male is greater than female
#After applying the t test the the p-value:0.8891
#since it is greater than 0.05 the H0 is accepted rejecting H1. Thus, the monthly income of male is equal to that of male.

#2. If the WorkLifeBalance of Males is less than Females 
X<-sqldf("select WorkLifeBalance from data where Gender='Male' ")
Y<-sqldf("select WorkLifeBalance from data where Gender='Female' ")
t.test(X,Y,alternative ="less")
print("the WorkLifeBalance of male is equal to that of female")
#for the first Question H0:WorkLifeBalance of male is equal to female
#                       H1:WorkLifeBalance of male is less than female
#After applying the t test the p-value = 0.4577
#since it is greater than 0.05 the H0 is accepted rejecting H1. Thus, the WorkLifeBalance of male is equal to that of female.

#3. If the YearsAtCompany of Single is less than Married 
X<-sqldf("select YearsAtCompany from data where MaritalStatus='Single' ")
Y<-sqldf("select YearsAtCompany from data where MaritalStatus='Married' ")
t.test(X,Y,alternative ="less")
print("YearsAtCompany of Singles is less than Married.")
#for the first Question H0:YearsAtCompany of Singles is equal to Married
#                       H1:YearsAtCompany of Singles is less than Married
#After applying the t test the p-value = 0.004973
#since it is less than 0.05 the H0 is rejected accepting H1. Thus, YearsAtCompany of Singles is less than Married.

#4.If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No 
X<-sqldf("select EnvironmentSatisfaction from data where Attrition='Yes' ")
Y<-sqldf("select EnvironmentSatisfaction from data where Attrition='No' ")
t.test(X,Y,alternative ="less")
print("EnvironmentSatisfaction of Attrition=Yes is less than Attrition=No")
#for the first Question H0:EnvironmentSatisfaction of Attrition=Yes is equal to Attrition=No
#                       H1:EnvironmentSatisfaction of Attrition=Yes is less than Attrition=No
#After applying the t test the p-value=0.0001046
#since it is less than 0.05 the H0 is rejected accepting H1. Thus, EnvironmentSatisfaction of Attrition=Yes is less than Attrition=No.


#5.If the MonthlyIncome of Manager is greater than Laboratory Technician.
X<-sqldf("select MonthlyIncome from data where JobRole='Manager' ")
Y<-sqldf("select MonthlyIncome from data where JobRole='Laboratory Technician' ")
t.test(X,Y,alternative ="greater")
print("The MonthlyIncome of Manager is greater than Laboratory Technician")
#for the first Question H0:the MonthlyIncome of Manager is equal to Laboratory Technician
#                       H1:the MonthlyIncome of Manager is greater than Laboratory Technician
#After applying the t test the p-value < 2.2e-16
#since it is less than 0.05 the h0 is rejected accepting h1. Thus, the MonthlyIncome of Manager is greater than Laboratory Technician.

#6.If YearsAtCompany and DailyRate are correlated with each other (5 points) 
summary(lm(YearsAtCompany~DailyRate,data=data))
print("YearsAtCompany and DailyRate are not correlated with each other")
#for the first Question H0:YearsAtCompany and DailyRate are correlated with each other
#                       H1:YearsAtCompany and DailyRate are not correlated with each other
#After applying regression the p-value is 0.1919 but Adjusted R-value is 0.0004793 too less.
#Thus h0 is rejected accepting h1.Thus, YearsAtCompany and DailyRate are not correlated with each other.


#7. If YearsAtCompany and MonthlyIncome are correlated with each other (5 points)
summary(lm(YearsAtCompany~MonthlyIncome,data=data))
print("YearsAtCompany and MonthlyIncome are correlated with each other")
#for the first Question H0:YearsAtCompany and MonthlyIncome are correlated with each other
#                       H1:YearsAtCompany and MonthlyIncome are not correlated with each other
#After applying regression the p-value: < 2.2e-16 but Adjusted R-value is 0.264 which is more.
#Thus h0 is accepted rejecting h1.Thus, YearsAtCompany and MonthlyIncome are correlated with each other.


#8. If YearsAtCompany varies depending on individual's MaritalStatus (5 points)
data$MaritalStatus<-as.factor(data$MaritalStatus)
summary(aov(YearsAtCompany~MaritalStatus,data=data))
print("YearsAtCompany varies not depending on individual's MaritalStatus")
#for the first Question H0:YearsAtCompany varies depending on individual's MaritalStatus
#                       H1:YearsAtCompany varies not depending on individual's MaritalStatus
#After applying the aov the p-value=0.0247
#since it is less than 0.05 the h0 is rejected accepting h1. Thus YearsAtCompany varies not depending on individual's MaritalStatus.

#9. If MonthlyIncome varies depending on individual's PerformanceRating (5 points)
summary(lm(MonthlyIncome~PerformanceRating,data=data))
print("MonthlyIncome varies depending on individual's PerformanceRating")
#for the first Question H0:MonthlyIncome varies depending on individual's PerformanceRating
#                       H1:MonthlyIncome varies not depending on individual's PerformanceRating
#After applying the aov the p-value: 0.5119
#since it is more than 0.05 the h0 is accepted rejecting h1. MonthlyIncome varies depending on individual's PerformanceRating.

#10. If MonthlyIncome varies depending on individual's WorkLifeBalance (5 points) 
summary(lm(MonthlyIncome~WorkLifeBalance,data=data))  
print("MonthlyIncome varies depending on individual's WorkLifeBalance")
#for the first Question H0:MonthlyIncome varies depending on individual's WorkLifeBalance
#                       H1:MonthlyIncome varies not depending on individual's WorkLifeBalance
#After applying the aov the p-value: 0.2397
#since it is more than 0.05 the h0 is accepted rejecting h1. MonthlyIncome varies depending on individual's WorkLifeBalance.


