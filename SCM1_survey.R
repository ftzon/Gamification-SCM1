## Dependencies
library(scales)
library(dplyr)
#library(summarytools)
library("likert")
## https://ladal.edu.au/tutorials/surveys/surveys.html

##Read and clean data set
df <- read.csv("Survey.csv", header=TRUE,)
data <- df[, 3:10]
colnames(data) <-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")

cols <- colnames(data)
data[cols] <- lapply(data[cols], factor,levels=c(1,2,3,4,5),labels=c("Strongly disagree","Disagree","Neutral","Agree", "Strongly agree"))

png("results/student-survey.png", height = 7, width = 7, uni ='in', res=300)
plot(likert(data), ordered = F, wrap = 60)
dev.off()






