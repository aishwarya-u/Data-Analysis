###############################################
#Mental Health Data Survey
###############################################

# Clean the Workspace
rm(list=ls())

#function to normalize
mmnorm <- function(x){
  return((x - min(x)) / (max(x)-min(x)))
}

survey <- read.csv("survey.csv")
survey <- survey[,c(4,7,8,9,13, 15, 26)]
View(survey)


survey$Country<-ifelse(survey$Country=='United States',"US","NON US")

survey$Do.you.have.a.family.history.of.mental.illness.<-ifelse(survey$Do.you.have.a.family.history.of.mental.illness.=='Yes',1,0)

survey$Have.you.sought.treatment.for.a.mental.health.condition.<-ifelse(survey$Have.you.sought.treatment.for.a.mental.health.condition.=='Yes',1,0)

survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.<-ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Often',4,ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Rarely',2,ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Sometimes',3,1)))
survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.<-mmnorm(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.)


survey$Does.your.employer.provide.mental.health.benefits. <- ifelse(survey$Does.your.employer.provide.mental.health.benefits. == "Yes", 1, ifelse(survey$Does.your.employer.provide.mental.health.benefits. == "No",2,3))
survey$Does.your.employer.provide.mental.health.benefits. <- mmnorm(survey$Does.your.employer.provide.mental.health.benefits.)
survey$Does.your.employer.provide.mental.health.benefits.

survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. <- ifelse(survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. == "Yes", 0, 1)                                       
survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. <- mmnorm(survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace.)
survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace.


survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. <- ifelse(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. == "Yes", 1, ifelse(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. == "No",2,3))                                                                       
survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. <- mmnorm(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program.)
survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program.

#------------------------------------------------------
install.packages("C50")

require(C50)
library(C50)

survey$Country<-as.factor(survey$Country)
str(survey$Country)

model1 <- C5.0(survey[1:900,-1], survey[1:900,1])
model1
# Tree size is the number of leaf nodes

summary(model1)

#predict on the testing dataset
p1 <- predict(model1, survey[901:1259,])
p1

#Actual vs predicted
result_table <- table(survey[901:1259,1], Predicted=p1)


#Take the measure of performance


accuracy=(sum(diag(result_table))/nrow(survey[901:1259,]))*100
accuracy


plot(model1)

