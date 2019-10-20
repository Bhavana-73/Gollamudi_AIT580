library(micromapST)
debt<-read.csv("Debt-capita.csv",header=T,as.is=TRUE)
str(debt)
type=c('map','id','dot','dot')
lab1=c("","",'Auto Debt Balance','Credit Card Debt')
lab2=c("","",'per capita','Balance per capita')
col1=c(NA,NA,'Mortgage.Debt.Balance.per.Capita....','Total.Debt.Balance.per.Capita....')
panelDesc1<-data.frame(type,lab1,lab2,col1)
t(panelDesc1)
fname="debt1.pdf"
pdf("debt1.pdf",8,10)
micromapST(debt,
           panelDesc1,
           rowNamesCol='State',
           rowNames='full',
           sortVar='Total.Debt.Balance.per.Capita....',ascend=FALSE,
           title=c("Debt per capita in AMERICA"),
           ignoreNoMatches=TRUE)
dev.off()
