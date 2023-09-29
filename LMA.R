#Install psych package
library(psych)

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location

#Import Data
data <- read.csv("LMA_CSV.csv", header=TRUE)  


#Print variable names on the screen
colnames(data)

##Perform exploratory data analysis

#display the internal structure of data
str(data) 
#checking missing values
any(is.na(data))

#restricting the data using subset function and storing it in LMA
LMA <- subset(data, WAGP > 10000 & SCHL>15 & AGEP >18) 

#show the structure of the data set
str(LMA)
colnames(LMA)

#check for missing values
any(is.na(LMA))

#view top 20 rows of data set
head(LMA,20)

# Descriptive Statistics for Quantitative variables
summary(LMA[,c("WAGP","AGEP","WKWN","WKHP")])

summary(LMA)

# Descriptive Statistics for Qualitative variables

LMA$WAGP = as.double(LMA$WAGP) # converts data type from double to integer 

install.packages("ISLR")
install.packages("SmartEDA")
library("ISLR")
library("SmartEDA")

ExpCustomStat(LMA,Cvar = c("MAR","SCHL","SEX","RAC1P"),Nvar=c("WAGP"),stat = c('Count','Prop','min','mean','median','max'),gpby=FALSE)

#Liner model 
Model1<-lm(formula = LMA$WAGP~LMA$AGEP+LMA$SEX+LMA$MAR+LMA$SCHL+LMA$RAC1P+LMA$WKHP+LMA$WKWN)
summary(Model1)

# change column names 
colnames(LMA) <- c('Marital Status','Educational attainment','Gender','Race','Weeks worked','Hours worked per week','Age','Wages or Salary Income')

# change column names to short
colnames(LMA) <- c('MAR','SCHL','SEX','RAC1P','WKWN','WKHP','AGEP','WAGP')

#Creates dummy variables and runs ML model 

M_ = factor(LMA$`Marital Status`, levels=c(1,2,3,4,5), labels=c("Married","Widowed", "Divorced","Separated", "Never Married"))

G_ = factor(LMA$Gender, levels=c(1,2), labels=c("Male","Female"))

E_ = factor(LMA$`Educational attainment`, levels = c(16,17,18,19,20,21,22,23,24) , labels = c("High_School" ,"GED" , "Some_College", "College_with_no_degree", "Associate_Degree", "Bachelor's_Degree", "Master's_Degree", "Professional_Degree", "Doctorate_Degree")) 

R_ = factor(LMA$Race, levels = c(1,2,3,4,5,6,7,8,9) , labels = c("White","African_American","American_Indian",                                                                "Alaska_Native","American_Indian_Alaska_Native","Asian","Native_Hawaiian", "Some_Other", "Two_or_more")) 

Wages_or_Salary_Income <- LMA$`Wages or Salary Income`
Age <- LMA$Age
Hours_worked_per_week  <- LMA$`Hours worked per week`
Weeks_worked <- LMA$`Weeks worked`

mod_1 <- lm(Wages_or_Salary_Income ~ Age + G_ + M_ + E_ + Hours_worked_per_week + Weeks_worked + R_)
summary(mod_1)

OR 
#Estimate MR Model (with dummy columns in excel - not used)
Earnings.Equation = lm(WAGP ~ AGEP + Female + High.School.Degree.or.GED + Some.College + Associates.Degree + Bachelors.Degree + Masters.Degree + Professional.Degree + Doctorate, data=LMA)
summary(Earnings.Equation)



