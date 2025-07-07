## Dependencies
library(scales)
library(dplyr)

## Read and clean data set
df <- read.csv("Dataset for SCM paper.csv", header=TRUE)
View(df)
df2 <- df[which(df$Final.exam.Mark != 0 & df$Midterm.exam.mark != 0),]
df2$pass <- ifelse(df2$Final.grade >= 5.5, 1, 0)

## Make groupings of observations
df2_control <- df2[which(df2$Fresh.connection.Final.exam.1 == 0),]
df2_treatment <-df2[which(df2$Fresh.connection.Final.exam.1 == 1),]

# Get the statistics
paste('All mean',mean(df2$Final.grade),'SD',sd(df2$Final.grade),'N students',length(df2$Final.grade))
paste('All mean',mean(df2_control$Final.grade),'SD',sd(df2_control$Final.grade),'N students',length(df2_control$Final.grade))
paste('All mean',mean(df2_treatment$Final.grade),'SD',sd(df2_treatment$Final.grade),'N students',length(df2_treatment$Final.grade))

#### Plotting
## Box Plot variability of student marks, control and treatment (game)

x=df2_control$Final.grade
y=df2_treatment$Final.grade
t.test(x,y)

## Histogram
x=df2_control$Final.grade
y=df2_treatment$Final.grade

xx <- hist(x,  freq=FALSE) 
yy <- hist(y,  freq=FALSE)

png("results/treatment_control.png", height = 7, width = 7, uni ='in', res=300)
plot( xx, col=alpha("#999999",0.5), xlim=c(0,10),  freq=FALSE,
      xlab="Variability",cex=1.5, main="", cex.axis=1.5, cex.lab=1.5,
      ylim=c(0,0.5))  # first histogram
plot( yy, col=alpha("#E69F00",0.5), xlim=c(0,10), add=T,  freq=FALSE)  # second
legend("topright" ,c("control", "treatment"), pch=15, col=c("#999999","#E69F00") , bty="n",cex=1.5)
dev.off()


png("results/treatment_boxplot.png", height = 7, width = 7, uni ='in', res=300)
boxplot(x, xlim=c(0.5,2.5), outline=F, col= "#999999", ylab="Variability", ylim=c(0, 10), cex.axis=1.5, cex.lab=1.5, names=NA)
boxplot(y, at=2 , add=TRUE , outline=F, col="#E69F00", cex.axis=1.5, names=NA)
dev.off()

################################################################################
## 2) Passing rate : Binary test/ chisq r - 2 by 2 table chi square test in r

# Step 1: Creating a contingency table
data <- df2[c("Fresh.connection.Final.exam.1","pass")]

# Create a contingency table
contingency_table <- table(data$Fresh.connection.Final.exam.1, data$pass)

# View the contingency table
print(contingency_table)

# Step 2: Applying the chi-square test function
result <- chisq.test(data)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# View the results
print(chi_square_test)

# Viewing the result
print(result)
###########################################################
## Drop out rate : Binary test/ chisq r - 2 by 2 table chi square test in r
df3 <- df
df3$student_retention <- ifelse(df$Final.exam.Mark == 0 | df$Midterm.exam.mark == 0, 0, 1)

# Step 1: Creating a contingency table
data <- df3[c("Fresh.connection.Final.exam.1","student_retention")]

# Create a contingency table 
contingency_table <- table(data$Fresh.connection.Final.exam.1, data$student_retention)

# View the contingency table
print(contingency_table)

# Step 2: Applying the chi-square test function
result <- chisq.test(data)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# View the results
print(chi_square_test)

# Viewing the result
print(result)



################################################################################
### Rerun test and plots, this time do not filter out those with 0 scores
################################################################################

df4 <- df
df4$pass <- ifelse(df4$Final.grade >= 5.5, 1, 0)

## Make groupings of observations
df4_control <- df4[which(df4$Fresh.connection.Final.exam.1 == 0),]
df4_treatment <-df4[which(df4$Fresh.connection.Final.exam.1 == 1),]


## Get the statistics
paste('All mean',mean(df4$Final.grade),'SD',sd(df4$Final.grade),'N students',length(df4$Final.grade))
paste('Control mean',mean(df4_control$Final.grade),'SD',sd(df4_control$Final.grade),'N students',length(df4_control$Final.grade))
paste('Treatment mean',mean(df4_treatment$Final.grade),'SD',sd(df4_treatment$Final.grade),'N students',length(df4_treatment$Final.grade))

#### Plotting
## Box Plot variability of grade for control and treatment (game)

x=df4_control$Final.grade
y=df4_treatment$Final.grade
t.test(x,y)

## Histogram
x=df4_control$Final.grade
y=df4_treatment$Final.grade

xx <- hist(x,  freq=FALSE) 
yy <- hist(y,  freq=FALSE)

png("results/treatment_control_no_filter.png", height = 7, width = 7, uni ='in', res=300)
plot( xx, col=alpha("#999999",0.5), xlim=c(0,10),  freq=FALSE,
      xlab="Variability",cex=1.5, main="", cex.axis=1.5, cex.lab=1.5,
      ylim=c(0,0.5))  # first histogram
plot( yy, col=alpha("#E69F00",0.5), xlim=c(0,10), add=T,  freq=FALSE) #second plot
legend("topright" ,c("control", "treatment"), pch=15, col=c("#999999","#E69F00") , bty="n",cex=1.5)
dev.off()


png("results/treatment_boxplot_no_filter.png", height = 7, width = 7, uni ='in', res=300)
boxplot(x, xlim=c(0.5,2.5), outline=F, col= "#999999", ylab="Variability", ylim=c(0, 10), cex.axis=1.5, cex.lab=1.5, names=NA)
boxplot(y, at=2 , add=TRUE , outline=F, col="#E69F00", cex.axis=1.5, names=NA)
dev.off()

### Data is not normal distributed, this should not be an issue since N>25, but 
## non the less we will run a non-parametic Shapiro Wilk test
shapiro.test(df4_control$Final.grade)
