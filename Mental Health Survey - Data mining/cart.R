###############################################
#Mental Health Data Survey
###############################################

# Clean the Workspace
rm(list=ls())

#function to normalize
mmnorm <- function(x){
  return((x - min(x)) / (max(x)-min(x)))
}

#Read the dataset
survey <- read.csv("survey.csv")
survey <- survey[,c(2,3,4,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
View(survey)
attach(survey)

#--------------------------------------------------------------------------------------------------------
survey$Country<-ifelse(survey$Country=='United States',"US","NON US")
#clean data
survey$Gender<- ifelse(survey$Gender=="M"|survey$Gender=="m"|survey$Gender=="Male"|survey$Gender=="male"|survey$Gender=="Mail"|survey$Gender=="maile"|survey$Gender=="make"|survey$Gender=="Man"|survey$Gender=="Mal","Male", ifelse(survey$Gender=="femail"|survey$Gender=="Femake"|survey$Gender=="Female"|survey$Gender=="Woman","Female","Unknown"))
survey$Gender <- ifelse(survey$Gender == "Male",0,ifelse(survey$Gender == "Female",1,3))
survey$Gender <- mmnorm(survey$Gender)

survey$Do.you.have.a.family.history.of.mental.illness.<-ifelse(survey$Do.you.have.a.family.history.of.mental.illness.=='Yes',1,0)

survey$Have.you.sought.treatment.for.a.mental.health.condition.<-ifelse(survey$Have.you.sought.treatment.for.a.mental.health.condition.=='Yes',1,0)

survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.<-ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Often',4,ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Rarely',2,ifelse(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.=='Sometimes',3,1)))
survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.<-mmnorm(survey$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.)

#Cleaning the age value
survey$Age <- ifelse(survey$Age<20,NA,survey$Age)
survey$Age <- ifelse(survey$Age>90,NA,survey$Age)

avg_age <- mean(survey$Age,na.rm = TRUE)

#replacing nulls with average
survey$Age[is.na(survey$Age)] <- avg_age
#survey$Age <- mmnorm(survey$Age)
View(survey)
survey$Age

#------------------------------------------------------------------------------------
#clean data
survey$Does.your.employer.provide.mental.health.benefits. <- ifelse(survey$Does.your.employer.provide.mental.health.benefits. == "Yes", 1, ifelse(survey$Does.your.employer.provide.mental.health.benefits. == "No",2,3))
survey$Does.your.employer.provide.mental.health.benefits. <- mmnorm(survey$Does.your.employer.provide.mental.health.benefits.)
survey$Does.your.employer.provide.mental.health.benefits.

survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides. <- ifelse(survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides. == "Yes", 1, ifelse(survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides. == "No",2,3))                                                                       
survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides. <- mmnorm(survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides.)
survey$Do.you.know.the.options.for.mental.health.care.your.employer.provides.

survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. <- ifelse(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. == "Yes", 1, ifelse(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. == "No",2,3))                                                                       
survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program. <- mmnorm(survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program.)
survey$Has.your.employer.ever.discussed.mental.health.as.part.of.an.employee.wellness.program.

survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help. <- ifelse(survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help. == "Yes", 1, ifelse(survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help. == "No",2,3))                                                                       

survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help. <- mmnorm(survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help.)
survey$Does.your.employer.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help.

survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources. <- ifelse(survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources. == "Yes", 1, ifelse(survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources. == "No",2,3))                                                                       
survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources. <- mmnorm(survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.)
survey$Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.

survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. <- ifelse(survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. == "Very difficult", 5, ifelse(survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. == "Somewhat difficult",4,ifelse(survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. == "Somewhat easy",3,ifelse(survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. == "Very easy",2,1))))                                                                       
survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition. <- mmnorm(survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition.)
survey$How.easy.is.it.for.you.to.take.medical.leave.for.a.mental.health.condition.

survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences. <- ifelse(survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences. == "Yes", 1, ifelse(survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences. == "No",2,3))                                                                       
survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences. <- mmnorm(survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences.)
survey$Do.you.think.that.discussing.a.mental.health.issue.with.your.employer.would.have.negative.consequences.


survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences. <- ifelse(survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences. == "Yes", 1, ifelse(survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences. == "No",2,3))                                                                       
survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences. <- mmnorm(survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences.)
survey$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences.

survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers. <- ifelse(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers. == "Yes", 1, ifelse(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers. == "No",2,3))                                                 
survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers. <- mmnorm(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers.)
survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.coworkers.

survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s.. <- ifelse(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s.. == "Yes", 1, ifelse(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s.. == "No",2,3))                                                 
survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s.. <- mmnorm(survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s..)
survey$Would.you.be.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s..

survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview. <- ifelse(survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview. == "Yes", 1, ifelse(survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview. == "No",2,3))                                                 
survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview. <- mmnorm(survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview.)
survey$Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview.

survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview. <- ifelse(survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview. == "Yes", 1, ifelse(survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview. == "No",2,3))                                                 
survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview. <- mmnorm(survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview.)
survey$Would.you.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview.

survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. <- ifelse(survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. == "Yes", 1, ifelse(survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. == "No",2,3))                                                 
survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health. <- mmnorm(survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.)
survey$Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.

survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. <- ifelse(survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. == "Yes", 1, 0)                                       
survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace. <- mmnorm(survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace.)
survey$Have.you.heard.of.or.observed.negative.consequences.for.coworkers.with.mental.health.conditions.in.your.workplace.
View(survey)
#-------------------------------------------------------------------------------------

#install packages required for cart
install.packages("tree")
library(tree)

#divide the dataset into training and test data
set.seed(2)
train=sample(1:nrow(survey),nrow(survey)/2)
test=-train
training_data=survey[train,]
testing_data=survey[test,]
testing_country=survey$Country[test]

#apply CART for prdiction on Country
tree_model = tree(as.factor(Country)~.,training_data)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

#check the prediction
tree_pred = predict(tree_model, testing_data,type="class")
summary(tree_pred)
mean(tree_pred != testing_country) #32.06% error

#pruning
set.seed(3)
cv_tree=cv.tree(tree_model,FUN=prune.misclass)

plot(cv_tree$size,cv_tree$dev,type='b')

#using the best value prune the tree
pruned_model=prune.misclass(tree_model, best = 7)
summary(pruned_model)
plot(pruned_model)
text(pruned_model,pretty = 0)

#check the prediction
tree_pred = predict(pruned_model, testing_data,type="class")
mean(tree_pred != testing_country)#30.7% error
