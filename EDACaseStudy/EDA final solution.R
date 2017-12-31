install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("reshape2")

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

###############################
##reading loan data
master_loan<-read.csv("loan.csv", stringsAsFactors = TRUE)
###############################

str(master_loan)

###############################

loan_df <- master_loan

## Remove Columns which has only single Value for All Rows
dropCols = c()
for (colnam in colnames(loan_df)){
  if(length(unique(loan_df[[colnam]])) == 1){
    dropCols <- c(dropCols, colnam)
  }
}

##cleaning the dataset -- 54 columns had only NA values, hence removing.
loan_df <- master_loan[,colSums(is.na(master_loan))<nrow(master_loan)]


dropCols

for (colnam in colnames(loan_df)){
  if(length(unique(loan_df[[colnam]])) == 2){
    dropCols <- c(dropCols, colnam)
  }
}

dropCols

##Manual Verification before removing the data
unique(loan_df$pymnt_plan)
unique(loan_df$initial_list_status)
unique(loan_df$policy_code)
unique(loan_df$application_type)
unique(loan_df$acc_now_delinq)
unique(loan_df$delinq_amnt)
unique(loan_df$chargeoff_within_12_mths)
unique(loan_df$collections_12_mths_ex_med)
unique(loan_df$tax_liens)
unique(loan_df$term)

##removing terms from dropcols as we need it.
dropCols <- dropCols[-which(dropCols== "term")] 

##remove columns with single values
loan_df <- loan_df[,vapply(loan_df, function(x){ length(unique(x))>1 }, TRUE)]

##Check duplicate records
#Verifying if there are duplicate unique LC assigned ID for the loan listing.
sum(duplicated(loan_df$id))
#Verifying if there are duplicate unique LC assigned Id for the borrower member.
sum(duplicated(loan_df$member_id))

length(unique(loan_df$desc))

##removing other identified columns manually
dropCols <- c(dropCols, "desc", "member_id", "id")


#cleaning text columns
loan_df$emp_title <- str_to_upper(str_replace_all(loan_df$emp_title, "[:punct:]", ""))
loan_df$title <- str_to_upper(str_replace_all(loan_df$title, "[:punct:]", ""))

length(unique(loan_df$title))
length(unique(loan_df$emp_title))
length(unique(loan_df$url))
count(loan_df,next_pymnt_d)
count(loan_df[loan_df$loan_status=="Current",],next_pymnt_d)
## removing the next payment date as it only available for the Current loan status
##and blank for other loan status
##Remove blank

##removing these columns as cannot conclude from the columns due to high number of ......
dropCols <- c(dropCols, "title","emp_title","next_pymnt_d", "url","zip_code")
dropCols

loan_df <- loan_df[,-which(names(loan_df) %in% dropCols)]

colnames(loan_df)


##removing "%" from int_rate column and revol_util
loan_df$int_rate<-as.numeric(str_replace(loan_df$int_rate, pattern = "%", replacement = ""))
loan_df$revol_util<-as.numeric(str_replace(loan_df$revol_util, pattern = "%", replacement = ""))

##coverting term to numeric
loan_df$term<-as.numeric(str_trim(str_replace(str_to_upper(loan_df$term), pattern = "MONTHS", replacement = "")))


#Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. 
loan_df$emp_length<-gsub("< 1 YEAR", "0", str_to_upper(loan_df$emp_length))
loan_df$emp_length<-str_extract(loan_df$emp_length, "([:digit:]+)")

##coverting date string into date objects
loan_df$issue_d<-str_c("01", loan_df$issue_d, sep="-")
loan_df$issue_d<- as.POSIXct(loan_df$issue_d, format = "%d-%b-%y")

loan_df$earliest_cr_line<-str_c("01", loan_df$earliest_cr_line, sep="-")
loan_df$earliest_cr_line<- as.POSIXct(loan_df$earliest_cr_line, format = "%d-%b-%y")

loan_df$last_credit_pull_d<-str_c("01", loan_df$last_credit_pull_d, sep="-")
loan_df$last_credit_pull_d<- as.POSIXct(loan_df$last_credit_pull_d, format = "%d-%b-%y")

loan_df$last_pymnt_d<-str_c("01", loan_df$last_pymnt_d, sep="-")
loan_df$last_pymnt_d<- as.POSIXct(loan_df$last_pymnt_d, format = "%d-%b-%y")

##outlier remove
ggplot(loan_df, aes(y=annual_inc, x=loan_status))+geom_boxplot()+ggtitle("Before Removing Outlier from Annual Income")+
  theme_update(plot.title = element_text(hjust = 0.5))
quantile(loan_df$annual_inc, probs = c(seq(0,1,0.05)), na.rm = T)
loan_df <- loan_df[loan_df$annual_inc<=quantile(loan_df$annual_inc, 0.95) & loan_df$annual_inc>=quantile(loan_df$annual_inc, 0.05),]
ggplot(loan_df, aes(y=annual_inc, x=loan_status))+geom_boxplot()+ggtitle("Annual Income After Removing Outlier")+
  theme_update(plot.title = element_text(hjust = 0.5))
quantile(loan_df$annual_inc, probs = c(seq(0,1,0.05)), na.rm = T)
###############################



###############################
##Univariate Analysis

#######emp_length - NO patterns observed
ggplot(loan_df, aes(x=emp_length, fill=loan_status))+geom_bar()
ggplot(loan_df, aes(x=emp_length, fill=loan_status))+ geom_bar(position = "fill")


#######interest rate
ggplot(loan_df, aes(x=int_rate, fill=loan_status))+geom_histogram(binwidth = 0.5)
##Finding for PPT: As the int_rate increases the percentage of Chargedoff increases
ggplot(loan_df, aes(x=int_rate, fill=loan_status))+geom_histogram(binwidth = 0.5,position = "fill")+
  ggtitle("Effect of Interest Rate on Charged Off")+
  labs(x="Interest Rate", y="Percentage")

#######Grades and Sub-Grade
##Finding for PPT: As the Grade and Sub Grade Falls the Likelihood of Chargedoff increases
##For Grade above "C" we should be cautious while giving loans as there is high chance of getting charged off
ggplot(loan_df, aes(x=grade, fill=loan_status))+geom_bar(position = "fill")+geom_text(stat="count", aes(label=..count..), position = position_fill())+
  ggtitle("Effect of Grade on Charged Off")+
  labs(x="Grade", y="Percentage")
ggplot(loan_df, aes(x=sub_grade, fill=loan_status))+geom_bar(position = "fill")+geom_text(stat="count", aes(label=..count..), position = position_fill())


########annual_inc
ggplot(loan_df, aes(x=annual_inc, fill=loan_status))+geom_histogram(binwidth = 400)
# Finding for PPT: Maybe we can say as the annual_inc, Chargedoff percentage is droping
ggplot(loan_df, aes(x=annual_inc, fill=loan_status))+geom_histogram(binwidth = 400,position = "fill", na.rm = T) 


########loan_amnt - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=loan_amnt, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=loan_amnt, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=loan_amnt, fill=loan_status))+geom_histogram(binwidth = 400,position = "fill")


########funded_amount - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=funded_amnt, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=funded_amnt, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=funded_amnt, fill=loan_status))+geom_histogram(binwidth = 400,position = "fill")


#######funded_amount_inv - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=funded_amnt_inv, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=funded_amnt_inv, fill=loan_status))+geom_histogram(binwidth = 400)
ggplot(loan_df, aes(x=funded_amnt_inv, fill=loan_status))+geom_histogram(binwidth = 400,position = "fill")


#######term
##Observed - There are no current loans in 36 month term plan.
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=term, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=as.character(term), fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())


########installment - No Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=installment, fill=loan_status))+geom_histogram()
ggplot(loan_df, aes(x=installment, fill=loan_status))+geom_histogram(position = "fill")


#######home_ownership - No significant Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=home_ownership, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=home_ownership, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())


########verification_status - No significant Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=verification_status, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=verification_status, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())

########purpose
##FInding For PPT: "Debt Consolidation"(2540), "Other"(526), "credit card"(503), "small business"(434)
##have higher no of loan defaulters
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=purpose, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=scales::percent(round((..count..)/sum(..count..),2))), vjust=-0.5)+
  ggtitle("Percentage of Charged off - Purpose wise")+
  labs(x="Pupose", y="Total Count")
ggplot(loan_df, aes(x=purpose, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=factor(1), fill=purpose)) +
  geom_bar(width = 1, stat = "count") + coord_polar("y", start=0) + 
  geom_text(stat="count",aes(label=scales::percent(round((..count..)/sum(..count..),2))),position = position_stack(vjust = 0.5))+
  ggtitle("Purpose wise distribution for Charged off")+
  labs(x="",y="")


#Count of each category in purpose in descending order
count(loan_df,purpose,sort=TRUE)
count(loan_df[loan_df$loan_status=="Charged Off",],purpose,sort=TRUE)




#######addr_state - No Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=addr_state, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=addr_state, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())
ggplot(loan_df, aes(x=addr_state, y=annual_inc))+geom_boxplot()


#########dti - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=dti, fill=loan_status))+geom_histogram()
ggplot(loan_df, aes(x=dti, fill=loan_status))+geom_histogram(position = "fill")
ggplot(loan_df, aes(y=dti, x=loan_status))+geom_boxplot()


#########delinq_2yrs - No Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=delinq_2yrs, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=delinq_2yrs, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())

#########inq_last_6mths
##Findings for PPT: as the number of inq_last_6mnths increase the likelihood of loan getting defaulted also increases
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=inq_last_6mths, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=inq_last_6mths, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())+
  ggtitle("Inquiries in Last 6 Months")+
  labs(x="No of inquiries in last 6 months", y="Total Count Percentage Wise")


###########mths_since_last_delinq - No Patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=mths_since_last_delinq, fill=loan_status))+geom_histogram(binwidth = 3, na.rm = T)
ggplot(loan_df, aes(x=mths_since_last_delinq, fill=loan_status))+geom_histogram(binwidth = 20, position = "fill", na.rm = T)+geom_text(stat = "bin", aes(label=..count..), na.rm = T, vjust=-0.5, position = position_fill())
  

###########open_acc - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=open_acc, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=open_acc, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())


###########pub_rec - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=pub_rec, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=pub_rec, fill=loan_status))+
  geom_bar(position = "fill")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_fill())


###########revol_bal - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=revol_bal, fill=loan_status))+geom_histogram()#+geom_smooth(stat="bin")
ggplot(loan_df, aes(x=revol_bal, fill=loan_status))+geom_histogram(position = "fill")


###########revol_util
##Findings for PPT: As the revoling utilisation increases the person is highly likely to default the loan
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=revol_util, fill=loan_status))+geom_histogram(na.rm = T)
ggplot(loan_df, aes(x=revol_util, fill=loan_status))+geom_histogram(binwidth = 20, na.rm = T, position = "fill")+
  geom_text(stat = "bin", aes(label=..count..), vjust=-0.5, position = position_fill(), na.rm = T)+
  ggtitle("Credit Utilisation Trend with Charged off")+
  labs(x="Revoling Util", y="TOtal count")


###########total_acc - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=total_acc, fill=loan_status))+geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
ggplot(loan_df, aes(x=total_acc, fill=loan_status))+
  geom_bar(position = "fill")
ggplot(loan_df, aes(x=loan_status, y=total_acc))+geom_boxplot()


############pub_rec_bankruptcies - No patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=pub_rec_bankruptcies, fill=loan_status))+geom_bar(na.rm = T)
ggplot(loan_df, aes(x=pub_rec_bankruptcies, fill=loan_status))+geom_bar(na.rm = T)+geom_text(stat = "count",na.rm = T, aes(label=..count..), vjust=-0.5, position = position_stack())
ggplot(loan_df, aes(x=pub_rec, fill=loan_status))+geom_bar(position = "fill")#+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_stack())



############collection_recovery_fee - No significant patterns observed
ggplot(loan_df[loan_df$loan_status=="Charged Off",], aes(x=collection_recovery_fee, fill=loan_status))+geom_histogram()
ggplot(loan_df, aes(x=collection_recovery_fee, fill=loan_status))+geom_histogram(position = "fill", na.rm = T)#+geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_stack())

count(loan_df[loan_df$loan_status=="Charged Off",], collection_recovery_fee, sort = T)
count(loan_df, loan_status)
length(loan_df[loan_df$loan_status=="Charged Off" & loan_df$collection_recovery_fee!=0,])

############out_prncp - No patterns observed
ggplot(loan_df, aes(x=out_prncp, fill=loan_status))+geom_histogram()

############total_acc - No patterns observed
ggplot(loan_df, aes(x=total_acc, fill=loan_status))+geom_histogram(position = "fill", na.rm = T)


############total_pymnt
ggplot(loan_df, aes(x=total_pymnt, fill=loan_status))+geom_histogram(position = "fill", binwidth = 1000, na.rm = T)+
  geom_text(stat = "bin", aes(label=..count..), vjust=-0.5, position = position_fill())


############mths_since_last_record - No patterns observed
ggplot(loan_df, aes(x=mths_since_last_record, fill=loan_status))+geom_histogram(position = "fill", na.rm = T)


############last_pymnt_amnt - No patterns observed
ggplot(loan_df, aes(x=last_pymnt_amnt, fill=loan_status))+geom_histogram(position = "fill", na.rm = T)

#############total_rec_late_fee
ggplot(loan_df, aes(x=total_rec_late_fee, fill=loan_status))+geom_histogram(position = "fill", na.rm = T)


#################################
##Derived columns

############Monthly income
loan_df$monthly_income <-loan_df$annual_inc/12
ggplot(loan_df, aes(x=monthly_income, y=installment,col=loan_status))+geom_point(alpha=0.4)+geom_smooth()
ggplot(loan_df, aes(x=monthly_income, y=dti,col=loan_status))+geom_point(alpha=0.4)+geom_smooth()


#################################
##segmented analysis

loan_df_debt_consolidation<-loan_df[loan_df$purpose=="debt_consolidation",]
str(loan_df_debt_consolidation)
summary(loan_df_debt_consolidation)
ggplot(loan_df_debt_consolidation, aes(x=monthly_income, y=installment,col=loan_status))+geom_point(alpha=0.4)+geom_smooth()
ggplot(loan_df_debt_consolidation, aes(x=annual_inc, y=installment, col=loan_status))+geom_point()+facet_wrap(~loan_status)+geom_smooth()

##monthly income - segment
##creating 4 income bins using percentiles
quantile(loan_df$monthly_income, seq(0, 1, 0.25))
loan_df<-mutate(loan_df, monthly_income, income_group = ifelse(monthly_income<=quantile(loan_df$monthly_income, 0.25), "Low",
                                       ifelse(monthly_income<=quantile(loan_df$monthly_income, 0.5), "Medium",
                                              ifelse(monthly_income<=quantile(loan_df$monthly_income, 0.75), "Medium High", "High"))))
loan_df$income_group <- factor(loan_df$income_group, levels = unique(loan_df$income_group[order(loan_df$monthly_income)]), ordered = T)

ggplot(loan_df, aes(x=income_group, y=loan_amnt, col=loan_status))+geom_jitter()+
  ggtitle("Income Group Segment Analyis - Loan Amounts")+
  labs(x="Income Groups", y="LOan Amount")

#################################
##bivariate analysis


##Mulitvariate analysis
ggplot(loan_df, aes(x=funded_amnt, y=loan_amnt, col=loan_status))+geom_point()+geom_smooth()+ggtitle("Multivariate analysis Fund, Loan, Status")


ggplot(loan_df, aes(x=monthly_income, y=loan_amnt, col=loan_status))+geom_point()+geom_smooth()+ggtitle("Multivariate analysis Fund, Loan, Status")


##Grade, Int_rate, Status
ggplot(loan_df,aes(y=int_rate, x=grade, fill=loan_status)) + geom_boxplot()+
  ggtitle("Multivariate analysis Grade, Interest Rate and Status")+
  labs(x="Grade", y="Interest rates")



