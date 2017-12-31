#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("MASS")
#install.packages("car")
#install.packages("caret")
#install.packages("e1071")
#install.packages("ROCR")

library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ROCR)

#######################
##importing dataset
emp_data <- read.csv("general_data.csv", stringsAsFactors = F)
mgr_srv_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
emp_srv_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
emp_in_time <- read.csv("in_time.csv", stringsAsFactors = F)
emp_out_time <- read.csv("out_time.csv", stringsAsFactors = F)
colnames(emp_in_time)[1]<- "EmployeeID"
colnames(emp_out_time)[1]<- "EmployeeID"

##Verify if All Dataframes has the same employeeID
sum(duplicated(emp_data$EmployeeID))
sum(duplicated(mgr_srv_data$EmployeeID))
sum(duplicated(emp_srv_data$EmployeeID))
sum(duplicated(emp_in_time$EmployeeID))
sum(duplicated(emp_out_time$EmployeeID))

count(emp_data,EmployeeID)
count(mgr_srv_data,EmployeeID)
count(emp_srv_data,EmployeeID)
count(emp_in_time,EmployeeID)
count(emp_out_time,EmployeeID)

setdiff(emp_data$EmployeeID,mgr_srv_data$EmployeeID)
setdiff(emp_data$EmployeeID,emp_srv_data$EmployeeID)
setdiff(emp_data$EmployeeID,emp_in_time$EmployeeID)
setdiff(emp_data$EmployeeID,emp_out_time$EmployeeID)

identical(emp_data$EmployeeID,mgr_srv_data$EmployeeID)
identical(emp_data$EmployeeID,emp_srv_data$EmployeeID)
identical(emp_data$EmployeeID,emp_in_time$EmployeeID)
identical(emp_data$EmployeeID,emp_out_time$EmployeeID)


## Get Public Holidays
indropCols = c()
outdropCols = c()
for (colnam in colnames(emp_in_time)){
  if(length(unique(emp_in_time[[colnam]])) == 1){
    indropCols <- c(indropCols, colnam)
  }
}
for (colnam in colnames(emp_out_time)){
  if(length(unique(emp_out_time[[colnam]])) == 1){
    outdropCols <- c(outdropCols, colnam)
  }
}

indropCols
outdropCols
setdiff(indropCols,outdropCols)

#Removing Public Holiday Attendence
emp_in_time <- emp_in_time[,-which(names(emp_in_time) %in% indropCols)]
emp_out_time <- emp_out_time[,-which(names(emp_out_time) %in% outdropCols)]

emp_in_time[,-1]<- as.data.frame(sapply(emp_in_time[,-1],FUN = as.POSIXlt))
emp_out_time[,-1]<- as.data.frame(sapply(emp_out_time[,-1],FUN = as.POSIXlt))

#str(emp_in_time)
#str(emp_out_time)

#Verify if there is no in-time employee also has no out-time for the day i.e Employee is on Leave
setdiff(which(is.na(emp_in_time)),which(is.na(emp_out_time)))

## Get Working Hours of the Employees
# workingHours <- aggregate(. ~ EmployeeID, rbind(emp_out_time,emp_in_time), sum)
# workingHours <- aggregate(. ~ EmployeeID, rbind(emp_out_timedf,emp_in_timedf), sum)
# res <- aggregate(. ~ EmployeeID, rbind(emp_out_timedf,emp_in_timedf), FUN = sum)

workingHours <- 
bind_rows(emp_in_time,emp_out_time) %>% 
  group_by(EmployeeID)  %>% 
  summarise_all(diff)
workingHours[,-1]<- sapply(workingHours[,-1],FUN = as.numeric)

## Get Avg Working hours and Leaves of the emplyee for the year
avgWorkingHours<-apply(workingHours[,-1],1,mean,na.rm = TRUE)
leaves<-apply(sapply(workingHours[,-1],FUN = is.na),1,sum)
# sum(avgWorkingHours>9)
#

workingHours <- merge(workingHours, emp_data[,c("EmployeeID","StandardHours")], by="EmployeeID")

# find out How many instances of employee working overtime
overtime<-apply(sapply(workingHours[,-1],FUN = function(x){x>workingHours$StandardHours}),1,sum,na.rm=TRUE)
overtime



#Merge Data into single Dataframe
emp_mrg_data <- merge(emp_data,mgr_srv_data,by="EmployeeID")
emp_mrg_data <- merge(emp_mrg_data,emp_srv_data,by="EmployeeID")
emp_mrg_data <- cbind(emp_mrg_data,avgWorkingHours)
emp_mrg_data <- cbind(emp_mrg_data,leaves)
emp_mrg_data <- cbind(emp_mrg_data,overtime)

emp_mrg_data_orginal <- emp_mrg_data
#########################


#########################
##Outliers treatment

box<-boxplot.stats(emp_mrg_data$Age)
out<-box$out
out
##No outliers

box<-boxplot.stats(emp_mrg_data$DistanceFromHome)
out<-box$out
out
##No outliers

box<-boxplot.stats(emp_mrg_data$EmployeeCount)
out<-box$out
out
##No outliers

box<-boxplot.stats(emp_mrg_data$MonthlyIncome)
out<-box$out
out
boxplot(emp_mrg_data$MonthlyIncome)
##Not removing outliers as this may be important factor for
##attrition. And the outliers are genuine

box<-boxplot.stats(emp_mrg_data$NumCompaniesWorked)
out<-box$out
out
##We are getting 9 as outlier but it is possible for
##an employee to work at 9 companies

box<-boxplot.stats(emp_mrg_data$PercentSalaryHike)
out<-box$out
out
##No outliers

box<-boxplot.stats(emp_mrg_data$StandardHours)
out<-box$out
out
##No outliers


box<-boxplot.stats(emp_mrg_data$StockOptionLevel)
out<-box$out
out
##No outliers


box<-boxplot.stats(emp_mrg_data$TotalWorkingYears)
out<-box$out
out
##No outliers

box<-boxplot.stats(emp_mrg_data$TrainingTimesLastYear)
out<-box$out
out
##No outliers


box<-boxplot.stats(emp_mrg_data$YearsAtCompany)
out<-box$out
out
##No major outliers


box<-boxplot.stats(emp_mrg_data$YearsSinceLastPromotion)
out<-box$out
out
##No major outliers

box<-boxplot.stats(emp_mrg_data$YearsWithCurrManager)
out<-box$out
out
##No major outliers

box<-boxplot.stats(emp_mrg_data$avgWorkingHours)
out<-box$out
out
##No major outliers

box<-boxplot.stats(emp_mrg_data$leaves)
out<-box$out
out
##No major outliers


box<-boxplot.stats(emp_mrg_data$overtime)
out<-box$out
out
##No major outliers
#########################

#########################
##Missing value treatment
sum(is.na(emp_mrg_data))
summary(is.na(emp_mrg_data))

##removing rows with NA in NumCompaniesWorked
sum(is.na(emp_mrg_data$NumCompaniesWorked))
emp_mrg_data <- emp_mrg_data[-which(is.na(emp_mrg_data$NumCompaniesWorked)),]

##removing rows with NA in TotalWorkingYears
sum(is.na(emp_mrg_data$TotalWorkingYears))
emp_mrg_data <- emp_mrg_data[-which(is.na(emp_mrg_data$TotalWorkingYears)),]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


##imputing NAs in EnvironmentSatisfaction with mode of it
##since it is categorical
emp_mrg_data$EnvironmentSatisfaction[which(is.na(emp_mrg_data$EnvironmentSatisfaction))]<-getmode(emp_mrg_data$EnvironmentSatisfaction)
sum(is.na(emp_mrg_data$EnvironmentSatisfaction))


##imputing NAs in JobSatisfaction with mode of it
##since it is categorical
emp_mrg_data$JobSatisfaction[which(is.na(emp_mrg_data$JobSatisfaction))]<-getmode(emp_mrg_data$JobSatisfaction)
sum(is.na(emp_mrg_data$JobSatisfaction))


##imputing NAs in WorkLifeBalance with mode of it
##since it is categorical
emp_mrg_data$WorkLifeBalance[which(is.na(emp_mrg_data$WorkLifeBalance))]<-getmode(emp_mrg_data$WorkLifeBalance)
sum(is.na(emp_mrg_data$WorkLifeBalance))

##removing EmployeeCount as it has only unique value
unique(emp_mrg_data$EmployeeCount)
emp_mrg_data <- emp_mrg_data[,-which("EmployeeCount"==colnames(emp_mrg_data))]


##removing Over18 as it has only unique value
unique(emp_mrg_data$Over18)
emp_mrg_data <- emp_mrg_data[,-which("Over18"==colnames(emp_mrg_data))]


##removing StandardHours as it has only unique value
unique(emp_mrg_data$StandardHours)
emp_mrg_data <- emp_mrg_data[,-which("StandardHours"==colnames(emp_mrg_data))]


##Checking for data integrity NumCompaniesWorked
table(emp_mrg_data$NumCompaniesWorked)
emp_mrg_data[emp_mrg_data$NumCompaniesWorked==0, c("TotalWorkingYears", "YearsAtCompany")]
numcompanies_zero_subset <- emp_mrg_data[emp_mrg_data$NumCompaniesWorked==0,]
unique(numcompanies_zero_subset$TotalWorkingYears-numcompanies_zero_subset$YearsAtCompany)
nrow(numcompanies_zero_subset)/nrow(emp_mrg_data)
##
##Observations:
# We have observed there are two  cases in  NumCompaniesWorked.
# Case1:NumCompaniesWorked:0
# In case of NumCompaniesWorked = 0,There is a difference of 1 year between number of years in current company 
# and total working years.It means minimum NumCompaniesWorked should be 2.
# Case2:NumCompaniesWorked:1
# In some cases where Number of companies worked is equal to 1 There is a difference of 1 year between number of years in current company and 
# total working years which is not correct.In this case both years in current comapny and total working year should be same.
## But since it is approx. 15% of the data we are not removing these facts
## also we cannot impute the correct values as we don't have enough business understanding
## for the incorrect values.

#########################


#########################
###EDA Analysis


ggplot(emp_mrg_data, aes(x=avgWorkingHours, fill=as.factor(Attrition)))+
  geom_histogram(position = "fill", binwidth = 1)+
  ggtitle("Average Working Hours/Attrition")+
  labs(x="Average Working Hours", y="Count")
##Observations:
##Employees who are working more than 8 hours are more likely to leave

ggplot(emp_mrg_data, aes(x=as.factor(EnvironmentSatisfaction), fill=as.factor(Attrition)))+
  geom_bar(position = position_fill())+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("EnvironmentSatisfaction/Attrition")+
  labs(x="EnvironmentSatisfaction", y="Count")
##Observations:
##: Associates with lower Environment satisfaction has higher risk of leaving the company    


##No pattern observed for DistanceFromHome variable on Attrition
ggplot(emp_mrg_data, aes(x=DistanceFromHome, fill=as.factor(Attrition)))+
  geom_histogram(binwidth = 5, position = "fill")+
  # geom_text(stat = "count", aes(label=..count..), position = "fill")+
  ggtitle("DistanceFromHome/Attrition")+
  labs(x="DistanceFromHome", y="Count")


ggplot(emp_mrg_data, aes(x=as.factor(JobSatisfaction), fill=as.factor(Attrition)))+
  geom_bar(position = position_fill())+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("JobSatisfaction/Attrition")+
  labs(x="JobSatisfaction", y="Count")
##Observations:
##Associates with lower job satisfaction has higher risk of leaving the company    



ggplot(emp_mrg_data, aes(x=as.factor(MaritalStatus), fill=as.factor(Attrition)))+
  geom_bar(position = position_fill())+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("MaritalStatus/Attrition")+
  labs(x="MaritalStatus", y="Count")
##Observations:
##Employee having marital status single has high risk of leaving the company  



ggplot(emp_mrg_data, aes(x=WorkLifeBalance, fill=as.factor(Attrition)))+
  geom_bar(position = position_fill())+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("WorkLifeBalance/Attrition")+
  labs(x="WorkLifeBalance", y="Count")
##Observations:
##Associates with lower work life balance  has higher risk of leaving the company


ggplot(emp_mrg_data, aes(x=YearsWithCurrManager, fill=as.factor(Attrition)))+
  geom_bar(position = position_fill())+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = "fill")+
  ggtitle("YearsWithCurrManager/Attrition")+
  labs(x="YearsWithCurrManager", y="Count")
##Observations:
##Associates who are working with same manger for long time are less likely to leave the company.

#########################



#########################
####Feature Standardisation
emp_mrg_data$Age <- scale(emp_mrg_data$Age)
emp_mrg_data$DistanceFromHome <- scale(emp_mrg_data$DistanceFromHome)
emp_mrg_data$MonthlyIncome <- scale(emp_mrg_data$MonthlyIncome)
emp_mrg_data$StockOptionLevel <- scale(emp_mrg_data$StockOptionLevel)
emp_mrg_data$TotalWorkingYears <- scale(emp_mrg_data$TotalWorkingYears)
emp_mrg_data$TrainingTimesLastYear <- scale(emp_mrg_data$TrainingTimesLastYear)
emp_mrg_data$YearsAtCompany <- scale(emp_mrg_data$YearsAtCompany)
emp_mrg_data$NumCompaniesWorked <- scale(emp_mrg_data$NumCompaniesWorked)
emp_mrg_data$PercentSalaryHike <- scale(emp_mrg_data$PercentSalaryHike)
emp_mrg_data$YearsSinceLastPromotion <- scale(emp_mrg_data$YearsSinceLastPromotion)
emp_mrg_data$YearsSinceLastPromotion <- scale(emp_mrg_data$YearsWithCurrManager)
emp_mrg_data$avgWorkingHours <- scale(emp_mrg_data$avgWorkingHours)
emp_mrg_data$leaves <- scale(emp_mrg_data$leaves)
emp_mrg_data$overtime <- scale(emp_mrg_data$overtime)


##Dummy variable creation and checking duplicates
str(emp_mrg_data)

dropCOls = c()

unique(emp_mrg_data$BusinessTravel)
emp_mrg_data$BusinessTravel <- as.factor(emp_mrg_data$BusinessTravel)
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = emp_mrg_data))
dummy_1<-dummy_1[,-1]
dropCOls<-c(dropCOls, "BusinessTravel")


unique(emp_mrg_data$Department)
emp_mrg_data$Department <- as.factor(emp_mrg_data$Department)
dummy_2 <- data.frame(model.matrix( ~Department, data = emp_mrg_data))
dummy_2 <-dummy_2[,-1]
dropCOls<-c(dropCOls, "Department")


unique(emp_mrg_data$Education)
emp_mrg_data$Education <- as.factor(emp_mrg_data$Education)
dummy_3 <- data.frame(model.matrix( ~Education, data = emp_mrg_data))
dummy_3 <-dummy_3[,-1]
dropCOls<-c(dropCOls, "Education")


unique(emp_mrg_data$EducationField)
emp_mrg_data$EducationField <- as.factor(emp_mrg_data$EducationField)
dummy_4 <- data.frame(model.matrix( ~EducationField, data = emp_mrg_data))
dummy_4 <-dummy_4[,-1]
dropCOls<-c(dropCOls, "EducationField")


unique(emp_mrg_data$JobLevel)
emp_mrg_data$JobLevel <- as.factor(emp_mrg_data$JobLevel)
dummy_5 <- data.frame(model.matrix( ~JobLevel, data = emp_mrg_data))
dummy_5 <-dummy_5[,-1]
dropCOls<-c(dropCOls, "JobLevel")


unique(emp_mrg_data$JobRole)
emp_mrg_data$JobRole <- as.factor(emp_mrg_data$JobRole)
dummy_6 <- data.frame(model.matrix( ~JobRole, data = emp_mrg_data))
dummy_6 <-dummy_6[,-1]
dropCOls<-c(dropCOls, "JobRole")


unique(emp_mrg_data$MaritalStatus)
emp_mrg_data$MaritalStatus <- as.factor(emp_mrg_data$MaritalStatus)
dummy_7 <- data.frame(model.matrix( ~MaritalStatus, data = emp_mrg_data))
dummy_7 <-dummy_7[,-1]
dropCOls<-c(dropCOls, "MaritalStatus")


unique(emp_mrg_data$JobInvolvement)
emp_mrg_data$JobInvolvement <- as.factor(emp_mrg_data$JobInvolvement)
dummy_8 <- data.frame(model.matrix( ~JobInvolvement, data = emp_mrg_data))
dummy_8 <-dummy_8[,-1]
dropCOls<-c(dropCOls, "JobInvolvement")


unique(emp_mrg_data$EnvironmentSatisfaction)
emp_mrg_data$EnvironmentSatisfaction <- as.factor(emp_mrg_data$EnvironmentSatisfaction)
dummy_9 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_mrg_data))
dummy_9 <-dummy_9[,-1]
dropCOls<-c(dropCOls, "EnvironmentSatisfaction")


unique(emp_mrg_data$JobSatisfaction)
emp_mrg_data$JobSatisfaction <- as.factor(emp_mrg_data$JobSatisfaction)
dummy_10 <- data.frame(model.matrix( ~JobSatisfaction, data = emp_mrg_data))
dummy_10 <-dummy_10[,-1]
dropCOls<-c(dropCOls, "JobSatisfaction")


unique(emp_mrg_data$WorkLifeBalance)
emp_mrg_data$WorkLifeBalance <- as.factor(emp_mrg_data$WorkLifeBalance)
dummy_11 <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_mrg_data))
dummy_11 <-dummy_11[,-1]
dropCOls<-c(dropCOls, "WorkLifeBalance")


##Dummy for variables having 2 level
unique(emp_mrg_data$Attrition)
emp_mrg_data$Attrition <- as.factor(emp_mrg_data$Attrition)
levels(emp_mrg_data$Attrition)<-c(0,1)
# assigning 1 to yes and 0 to no
emp_mrg_data$Attrition<- as.numeric(levels(emp_mrg_data$Attrition))[emp_mrg_data$Attrition]


unique(emp_mrg_data$Gender)
emp_mrg_data$Gender <- as.factor(emp_mrg_data$Gender)
levels(emp_mrg_data$Gender)<-c(1,0)
# assigning 1 to female and 0 to male
emp_mrg_data$Gender<- as.numeric(levels(emp_mrg_data$Gender))[emp_mrg_data$Gender]


unique(emp_mrg_data$PerformanceRating)
emp_mrg_data$PerformanceRating <- as.factor(emp_mrg_data$PerformanceRating)
levels(emp_mrg_data$PerformanceRating)<-c(1,0)
# assigning 1 to female and 0 to male
emp_mrg_data$PerformanceRating<- as.numeric(levels(emp_mrg_data$PerformanceRating))[emp_mrg_data$PerformanceRating]


emp_mrg_data1 <- emp_mrg_data[,-which(names(emp_mrg_data) %in% dropCOls)]

emp_mrg_data1 <- cbind(emp_mrg_data1 , dummy_1, dummy_2, dummy_3, dummy_4, dummy_5, dummy_6, dummy_7, dummy_8, dummy_9, dummy_10, dummy_11)


######################
###Creating train and test set
set.seed(101)

indices= sample(1:nrow(emp_mrg_data1), 0.7*nrow(emp_mrg_data1))

##Removing EmployeeID for training data as it is not required
train= emp_mrg_data1[indices, -1]
test = emp_mrg_data1[-indices, ]

######################


######################

model_1 <- glm(Attrition~., train, family = "binomial")
summary(model_1)

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check

sort(vif(model_2), decreasing = TRUE)

##Removing EducationFieldLife.Sciences as it high p-value(0.003173) and high vif(8.643654)
##hence insignificant
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + avgWorkingHours + overtime + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)

summary(model_3)
sort(vif(model_3), decreasing = TRUE)


##Removing YearsAtCompany as it high vif(0.019919) and highly vif(3.839649)
##hence insignificant
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + avgWorkingHours + overtime + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)

summary(model_4)
sort(vif(model_4), decreasing = T)
cor(emp_mrg_data1[,c("overtime", "avgWorkingHours")])


##Removing avgWorkingHours as it high p-value(0.000551) and is highly correlated to overtime, which is a significant variable
##hence insignificant
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)

summary(model_5)
sort(vif(model_5), decreasing = T)


##Removing BusinessTravelTravel_Rarely as it high p-value(0.020340) and is highly correlated to BusinessTravelTravel_Frequently
##hence insignificant
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)

summary(model_6)
sort(vif(model_6), decreasing = T)


##Removing WorkLifeBalance4 as it high p-value(0.000294)
##hence insignificant
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3, family = "binomial", 
               data = train)

summary(model_7)
sort(vif(model_7), decreasing = T)


##Removing MaritalStatusMarried as it high p-value(0.130328) and high vif(2.093903)
##hence insignificant
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3, family = "binomial", 
               data = train)

summary(model_8)
sort(vif(model_8), decreasing = T)


##Removing EducationFieldMedical as it high p-value(0.27463)
##hence insignificant
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                 EducationFieldMarketing + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + MaritalStatusSingle + 
                 JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3, family = "binomial", 
               data = train)

summary(model_9)



##Removing EducationFieldMarketing as it high p-value(0.364569)
##hence insignificant
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_10)


##Removing EducationFieldOther as it high p-value(0.207383)
##hence insignificant
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                  EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_11)


##Removing StockOptionLevel as it high p-value(0.120957)
##hence insignificant
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                  EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement2 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_12)


##Removing JobInvolvement2 as it high p-value(0.100389)
##hence insignificant
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                  EducationFieldTechnical.Degree + JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_13)


##Removing EducationFieldTechnical.Degree as it high p-value(0.171999)
##hence insignificant
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + Education5 + 
                  JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_14)


##Removing Education5 as it high p-value(0.085444)
##hence insignificant
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobLevel5 + JobRoleHuman.Resources + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_15)


##Removing JobRoleHuman.Resources as it high p-value(0.071734)
##hence insignificant
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_16)


##Removing WorkLifeBalance2 as it high p-value(0.031484)
##hence insignificant
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  JobInvolvement3 + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_17)


###Removing JobInvolvement3 as it high p-value(0.022433)
##hence insignificant
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_18)


###Removing JobLevel5 as it high p-value(0.015855)
##hence insignificant
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_19)


###Removing JobRoleSales.Executive as it high p-value(0.01091)
##hence insignificant
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_20)


###Removing JobSatisfaction2 as it high p-value(0.00768)
##hence insignificant
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_21)


###Removing JobSatisfaction3 as it high p-value(0.04885)
##hence insignificant
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_22)


###Removing JobRoleResearch.Director as it high p-value(0.00979)
##hence insignificant
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_23)


###Removing TrainingTimesLastYear as it high p-value(0.00321)
##hence insignificant
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_24)


###Removing JobRoleManufacturing.Director as it high p-value(0.00159)
##hence insignificant
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                  YearsSinceLastPromotion + overtime + BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_25)



###Removing YearsSinceLastPromotion as it high p-value(0.00142)
##hence insignificant
model_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears +
                  overtime + BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_26)


###Removing Age as it high p-value(0.00358)
##hence insignificant
model_27 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears +
                  overtime + BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3, family = "binomial", 
                data = train)

summary(model_27)

######################

####
## NumCompaniesWorked, TotalWorkingYears , overtime , BusinessTravelTravel_Frequently , 
#  MaritalStatusSingle , EnvironmentSatisfaction2 , EnvironmentSatisfaction3 , EnvironmentSatisfaction4 , 
#  JobSatisfaction4 , WorkLifeBalance3
####

final_model<- model_27

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Checking the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))



test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#######################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Choosing a cutoff value of 0.1536364 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc ##0.7277567

sens ##0.7351598

spec ##0.7262774

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#### 0.4614372

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attrition, test_pred, groups = 10)
Churn_decile
