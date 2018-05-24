###############################################
#Mental Health Data Survey
###############################################

# Clean the Workspace
rm(list=ls())

#function to normalize
mmnorm <- function(x){
  return((x - min(x)) / (max(x)-min(x)))
}

#Read the dataset C:\Users\vivso\Desktop\KDDM Project
survey <- read.csv("survey.csv")
attach(survey)

survey$Country<-ifelse(survey$Country=='United States',"US","NON US")
(survey)

survey[survey==""]<-NA
data <- data.frame(Age,Gender,self.employed,family.history,sought.treatment,work.remotely,tech.company,mental.health.benefits,health.care.options,mental.health.discussion,mental.health.resources,anonymity.protection,medical.leave.ease,mental.health.negative.outcome,physical.health.negative.outcome,discussion.with.your.coworkers,discussion.with.supervisor,interference.in.work,"Country"=survey$Country)
detach (survey)
attach(data)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
#data$interference.in.work[data$interference.in.work =='Never' & data$interference.in.work=='Rarely'] <- "Rarely"
#data$interference.in.work[data$interference.in.work =='Often' & data$interference.in.work=='Sometimes'] <- "Sometimes"
#View(data_new)
data_new<-cbind(
  Age=(mmnorm(na.zero(data$Age))),
  Gender=mmnorm(as.numeric(na.zero(data$Gender))),
  self.employed=mmnorm(as.numeric(na.zero(data$self.employed))),
  family.history=mmnorm(as.numeric(na.zero(data$family.history))),
  sought.treatment=mmnorm(as.numeric(na.zero(data$sought.treatment))),
  work.remotely=mmnorm(as.numeric(na.zero(data$work.remotely))),
  tech.company=mmnorm(as.numeric(na.zero(data$tech.company))),
  mental.health.benefits=mmnorm(as.numeric(na.zero(data$mental.health.benefits))),
  health.care.options=mmnorm(as.numeric(na.zero(data$health.care.options))),
  mental.health.discussion=mmnorm(as.numeric((na.zero(data$mental.health.discussion)))),
  mental.health.resources=mmnorm(as.numeric((na.zero(data$mental.health.resources)))),
  anonymity.protection=mmnorm(as.numeric((na.zero(data$anonymity.protection)))),
  medical.leave.ease=mmnorm(as.numeric((na.zero(data$medical.leave.ease)))),
  mental.health.negative.outcome=mmnorm(as.numeric((na.zero(data$mental.health.negative.outcome)))),
  discussion.with.your.coworkers=mmnorm(as.numeric((na.zero(data$discussion.with.your.coworkers)))),
  discussion.with.supervisor=mmnorm(as.numeric((na.zero(data$discussion.with.supervisor)))),
  interference.in.work=mmnorm(as.numeric((na.zero(data$interference.in.work)))),
  Country=as.character(data$Country)
  
)



idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#****training  & test dataset***********
training<-data_new[idx,]
test<-data_new[-idx,]
library(class)
#View(data_new)
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20 
# here which k's average error rate is minimumm,that k is best.

#CALCULATING ERROR 
for (j in 1:40){
  counter<- 0
  total<-0
  for (i in 1:50) {
    newpredict<-knn(training[,-18],test[,-18],training[,18],k <- j)
    newresults<-cbind(test,as.character(newpredict) )
    wrong<-newresults[,18]!=newresults[,19]
    rate<-sum(wrong)/length(wrong)
    rates<-rbind(rate,rate)
    total<-total+rate
    counter<-counter+1
  }
  print(j)
  avg=total/counter
  print(avg)
}
newpredict<-knn(training[,-18],test[,-18],training[,18],k=25)
newresults<-cbind(test,as.character(newpredict) )
head(newresults)
table(newresults[,18],newresults[,19])


####################################################
data_n<-as.data.frame(data_new)
#training<-as.data.frame(training)
data_n<-na.omit(data_n)
factor(data_n$Country)
is.data.frame(data_n)
idx<-sample(nrow(data_n),as.integer(.70*nrow(data_n)))
#****training  & test dataset***********
train<-data_n[idx,]
testt<-data_n[-idx,]
#library(kknn)
require(kknn)
library(kknn)

#applying kknn
predict_1 <- kknn(formula= Country~., train, testt, k=25,kernel="optimal")
head(predict_1)
fitWalc <- fitted(predict_1)
results <- cbind(testt$Country, fitWalc)
wrong <- results[,1]!=results[,2]
rateWalc <- sum(wrong)/length(wrong)
rateWalc