## Dependencies
library(scales)
library(dplyr)
library(diptest)
library(moments)


################################################################################
### UNFILTERED : Not removing students who missed one or more exams, bimodal distributed
################################################################################

## Read and clean data set
df <- read.csv("Dataset for SCM paper.csv", header=TRUE)
#View(df)
df1 <- df
df1$student_retention <- ifelse(df$Final.exam.Mark == 0 | df$Midterm.exam.mark == 0, 0, 1)

## Make groupings of observations
df1_control <- df1[which(df1$Fresh.connection.Final.exam.1 == 0),]
df1_treatment <-df1[which(df1$Fresh.connection.Final.exam.1 == 1),]

## Get the statistics
paste('All mean',mean(df1$Final.grade),'SD',sd(df1$Final.grade),'N students',length(df1$Final.grade))
paste('Control mean',mean(df1_control$Final.grade),'SD',sd(df1_control$Final.grade),'N students',length(df1_control$Final.grade))
paste('Treatment mean',mean(df1_treatment$Final.grade),'SD',sd(df1_treatment$Final.grade),'N students',length(df1_treatment$Final.grade))

#### Plotting
## UNFILTERED: Box Plot variability of grade for control and treatment (game)

x=df1_control$Final.grade
y=df1_treatment$Final.grade
t.test(x,y)

## Histogram
x=df1_control$Final.grade
y=df1_treatment$Final.grade

xx <- hist(x,  freq=FALSE) 
yy <- hist(y,  freq=FALSE)

png("results/treatment_control_unfilter.png", height = 7, width = 7, uni ='in', res=300)
plot( xx, col=alpha("#999999",0.5), xlim=c(0,10),  freq=FALSE,
      xlab="Variability",cex=1.5, main="", cex.axis=1.5, cex.lab=1.5,
      ylim=c(0,0.5))  # first histogram
plot( yy, col=alpha("#26E600",0.5), xlim=c(0,10), add=T,  freq=FALSE) #second plot
legend("topright" ,c("control", "treatment"), pch=15, col=c("#999999","#26E600") , bty="n",cex=1.5)
dev.off()


png("results/treatment_boxplot_unfilter.png", height = 7, width = 7, uni ='in', res=300)
boxplot(x, xlim=c(0.5,2.5), outline=F, col= "#999999", ylab="Variability", ylim=c(0, 10), cex.axis=1.5, cex.lab=1.5, names=NA)
boxplot(y, at=2 , add=TRUE , outline=F, col="#26E600", cex.axis=1.5, names=NA)
dev.off()

### Data is not normal distributed, this should not be an issue since N>25, but 
## non the less we will run a non-parametic Shapiro Wilk test
shapiro.test(df1_control$Final.grade)

# Indeed data is not normal distributed, very significant
# Test if uni-modal/ multi-modal distribution using Hartichans'dip test

data <- c(df1_control$Final.grade, df1_treatment$Final.grade)
dip_test_result <- dip.test(data)
print(dip_test_result)

###########################################################
## UNFILTERED Drop out rate : Binary test/ chisq r - 2 by 2 table chi square test in r


# Step 1: Creating a contingency table
data <- df1[c("Fresh.connection.Final.exam.1","student_retention")]

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

## Indeed, very significant higher student engagement and on average higher result in treatment group



################################################################################
### FILTERED data, removing students which missed one or more exams, normal distributed
################################################################################

df2 <- df[which(df$Final.exam.Mark != 0 & df$Midterm.exam.mark != 0),] ## FILTER
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

png("results/treatment_control_filtered.png", height = 7, width = 7, uni ='in', res=300)
plot( xx, col=alpha("#999999",0.5), xlim=c(0,10),  freq=FALSE,
      xlab="Variability",cex=1.5, main="", cex.axis=1.5, cex.lab=1.5,
      ylim=c(0,0.5))  # first histogram
plot( yy, col=alpha("#26E600",0.5), xlim=c(0,10), add=T,  freq=FALSE)  # second
legend("topright" ,c("control", "treatment"), pch=15, col=c("#999999","#26E600") , bty="n",cex=1.5)
dev.off()


png("results/treatment_boxplot_filtered.png", height = 7, width = 7, uni ='in', res=300)
boxplot(x, xlim=c(0.5,2.5), outline=F, col= "#999999", ylab="Variability", ylim=c(0, 10), cex.axis=1.5, cex.lab=1.5, names=NA)
boxplot(y, at=2 , add=TRUE , outline=F, col="#26E600", cex.axis=1.5, names=NA)
dev.off()

################################################################################
## FILTERED :  Chi-square test, not significant
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

# -> Results show no significant different when filtering on passing rate for filtered data
