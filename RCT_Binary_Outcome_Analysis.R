# Trial Design Parameters
nPatients <- 1000
death1 <- 0.5
death2 <- 0.4

library(gtsummary)
library(tidyverse)

set.seed(1) 

pid<-seq(1, by=1, len=nPatients) 
age<-rnorm(nPatients,50,10) #mean age 50, sd=10
smoker<-rbinom(nPatients,1,0.10+age*0.001) #older patients a little more likely to smoke
htn<-rbinom(nPatients,1,0.10+age*0.001+smoker*0.2) #older patients & smokers more likely to have HTN
treatment<-rep(1:2, nPatients/2) #simulation 1:1 allocation (easier to just do alternate 1:2 for simulation)
deathprob <- numeric(nPatients) 
deathprob[treatment==1]=death1 
deathprob[treatment==2]=death2 
death<-rbinom(nPatients, 1, deathprob) 
trial<-data.frame(cbind(pid, age, smoker, htn, treatment, death))
head(trial,n=20)
table(death,treatment)

# https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
# make dataset with a few variables to summarize for "Table 1"
trial2 <- trial %>% select(age, smoker, htn, treatment)
# summarize the data by treatment group
table1 <- 
  tbl_summary(
    trial2,
    by = treatment, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # could use this to add test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

table1

# https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html
# primary statistical analysis
model <- glm(death ~ treatment, family=binomial(link='logit'), data=trial) 
t1 <- tbl_regression(model, exponentiate = TRUE)
t1



