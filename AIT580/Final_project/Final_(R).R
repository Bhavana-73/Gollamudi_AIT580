
data<-read.csv('Consumer_Com_preprocessed.csv')
str(data)
names(data)[names(data) == 'Timely.response.'] <- 'TimelyResponse'
names(data)[names(data) == 'Consumer.disputed.'] <- 'ConsumerDisputed'
names(data)[names(data) == 'Complaint.ID'] <- 'ID'
#Hypothesis Testing

library(sqldf)
X<-sqldf("select ID from data where TimelyResponse='Yes'")
Y<-sqldf("select ID from data where ConsumerDisputed='No'")
t.test(X,Y,alternative ="greater")
#For this assumption I came up with following Null Hypothesis(H0) and Alternate Hypothesis(H1):
#H0: The number of timely responses is greater than customer will not dispute.
#H1: The number of timely responses is less than customer that will not dispute.
#Alternate hypothesis is accepted


