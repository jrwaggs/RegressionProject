library(dplyr)
library(ggplot2)
library(readr)
library(GGally)
library(broom)
library(jtools)
library(anytime)
library(car)
library(kableExtra)
library(EnvStats)
library(corrplot)
library(ggstance)

#---------------------------- Data import and Cleaning ------------------------

# import the CSV
loans_charged_off <- read.csv('loans_charged_off.csv')

#drop rows with NA for now, revisit missing data later
#loans_charged_off <-loans_charged_off[complete.cases(loans_charged_off),]  

#convert revolving util from string % to numeric decimal
loans_charged_off$revol_util<- as.numeric(sub("%","",loans_charged_off$revol_util))/100

#convert term to numeric
loans_charged_off$term<- as.numeric(sub("months","",loans_charged_off$term))

#compute target dependent variable, total paid
loans_charged_off <- loans_charged_off %>%
  mutate(tot_paid = total_rec_prncp + total_rec_int)

# there are 15 purpose categories, -> factor 
loans_charged_off$purpose <- factor(loans_charged_off$purpose)

#convert character strings from form like '10-Dec' to form like '2010-dec'
# strings to be used with anytime function
loans_charged_off$issue_d <- sub("11","2011",loans_charged_off$issue_d,fixed = TRUE)
loans_charged_off$issue_d <- sub("10","2010",loans_charged_off$issue_d,fixed = TRUE)
loans_charged_off$issue_d <- sub("9","2009",loans_charged_off$issue_d,fixed = TRUE)
loans_charged_off$issue_d <- sub("8","2008",loans_charged_off$issue_d,fixed = TRUE)
loans_charged_off$issue_d <- sub("7","2007",loans_charged_off$issue_d,fixed = TRUE)

#convert issue_d from factor to to date-time 
loans_charged_off$issue_d <-anytime(loans_charged_off$issue_d)

# convert from '7-Dec' to '2007-Dec' for use with anytime()
loans_charged_off$last_pymnt_d <- sub("16","2016",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d <- sub("15","2015",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d <- sub("14","2014",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d <- sub("13","2013",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d <- sub("12","2012",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d <- sub("11","2011",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d  <- sub("10","2010",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d  <- sub("9","2009", loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d  <- sub("8","2008",loans_charged_off$last_pymnt_d ,fixed = TRUE)
loans_charged_off$last_pymnt_d  <- sub("7","2007",loans_charged_off$last_pymnt_d ,fixed = TRUE)

#convert issue_d from factor to date-time 
loans_charged_off$last_pymnt_d <-anytime(loans_charged_off$last_pymnt_d)

#  pull three records from the dataframe, 
#   plus a fourth with a high loan amount

#copy row @ index 2000 to new DF
#test_case <- loans_charged_off[c(13,1000,156,228),]
#delete row @ 2000 from original DF
#loans_charged_off <- loans_charged_off[-c(13,1000,156,228),]

#------------------------------- data exploration -------------------------------------
#mean(loans_charged_off$pct_paid) # the mean % paid back on a failed loan is 35.38%

# ---------- DISTRIBUTIONS

# distribution of annual incomes
ggplot(loans_charged_off,aes(annual_inc))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 20, color="black", fill="gray65")+
  ggtitle(("Borrower Annual Income"))+
  xlab("Annual Income")+
  xlim(0,200000)+
  ylab("Loan Count")

# distribution of inquiries last 6 months (not included in markdown)
ggplot(loans_charged_off,aes(inq_last_6mths))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_bar(color="black", fill="gray65")+
  ggtitle("Inquries Last 6 Months")+
  xlab("Inquiries")+
  ylab("Loan Count")

# distribution of installment
ggplot(loans_charged_off,aes(installment))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins=20, color="black", fill="gray65")+
  ggtitle("Installment")+
  xlab("Installment")+
  ylab("Loan Count")

# distribution of interest rates
ggplot(loans_charged_off,aes(int_rate))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins=20, color="black", fill="gray65")+
  ggtitle("Loan Interest Rates")+
  xlab("Interest Rate")+
  ylab("Loan Count")

# distribution of last payment amount (not included in markdown)
ggplot(loans_charged_off,aes(last_pymnt_amnt))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins=30, color="black", fill="gray65")+
  ggtitle("Last Payment Amount")+
  xlab("Last Payment Amount")+
  ylab("Loan Count")

# distribution of loan amounts 
ggplot(loans_charged_off,aes(loan_amnt))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 20, color="black", fill="gray65")+
  ggtitle("Loan Amounts")+
  xlab("Loan amount")+
  ylab("Loan Count")

# distribution of months since last delinquency
ggplot(loans_charged_off, aes(mths_since_last_delinq))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins=30, color="black", fill="gray65")+
  ggtitle("Months Since Last Delinquency")+
  xlab("Months")+
  ylab("Loan Count")

# distribution of months since last record  (not included in markdown)
ggplot(loans_charged_off, aes(mths_since_last_record))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 10, color="black", fill="gray65")+
  ggtitle("Months Since Last Record")+
  xlab("Months")+
  ylab("Loan Count")

# distribution of open accounts
ggplot(loans_charged_off,aes(open_acc))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_bar(color="black", fill="gray65")+
  ggtitle("Open Credit Line Accounts")+
  xlab("Open Accounts")+
  ylab("Loan Count")

# distribution of recoveries (not included in markdown)
ggplot(loans_charged_off,aes(recoveries))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 30, color="black", fill="gray65")+
  ggtitle("Recoveries")+
  xlab("Recoveries")+
  ylab("Loan Count")

# distribution of Total credit revolving balance (not included in markdown)
ggplot(loans_charged_off,aes(revol_bal))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 30, color="black", fill="gray65")+
  ggtitle("Total Credit Revolving Balance")+
  xlab("Revolving Balance")+
  ylab("Loan Count")

# distribution of Revolving Credit Line Utilization Rate (not included in markdown)
ggplot(loans_charged_off,aes(revol_util))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 10, color="black", fill="gray65")+
  ggtitle("Revolving Credit Line Utilization Rate")+
  xlab("Utilization Rate")+
  ylab("Loan Count")

# distribution of total paid
ggplot(loans_charged_off, aes(tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_histogram(bins = 30, color="black", fill="gray65")+
  ggtitle("Total Paid")+
  xlab("Total Paid")+
  ylab("Loan Count")

#----------CORRELATION ANALYSIS OF NUMERIC VARIABLES 

#subset of only numeric vaiables
numeric_loans_charged_off <- loans_charged_off %>%
  dplyr::select(int_rate,installment,annual_inc,inq_last_6mths,
                mths_since_last_delinq,mths_since_last_record,open_acc,pub_rec,revol_bal,
                revol_util,recoveries,loan_amnt,tot_paid)

#corrplot of numeric values
corrplot(cor(numeric_loans_charged_off, use = "na.or.complete"), method = "ellipse") 
#potential multicollinearity between "loan_amnt" and "installment"
#potential multicollinearity between "mths_since_last_record" and "pub_rec"
#potential multicollinearity between "annual_inc" and "revol_bal"

#pairwise plots of numeric values  
ggpairs(numeric_loans_charged_off)

# ---------RELATIONSHIPS BETWEEN CATEGORICAL VARIABLES AND TARGET VARIABLE

# boxplot of total paid by purpose, with coordinates flipped 
ggplot(loans_charged_off, aes(purpose, tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Purpose")+
  xlab("Purpose")+
  ylab("Total Paid")

# boxplot of loan amount by purpose 
ggplot(loans_charged_off,aes(purpose,loan_amnt))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Loan Amount by Purpose")+
  xlab("Purpose")+
  ylab("Loan Amount")

# boxplot of total paid by grade
ggplot(loans_charged_off,aes(grade,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Loan Grade")+
  xlab("Loan Grade")+
  ylab("Total Paid")

# boxplot of total paid by home ownership
ggplot(loans_charged_off,aes(home_ownership,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Home Ownership")+
  xlab("Home Ownership")+
  ylab("Total Paid")

# boxplot of total paid by verification status
ggplot(loans_charged_off,aes(verification_status,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Verification Status")+
  xlab("Verification Status")+
  ylab("Total Paid")

# boxplot of total paid by employment length
ggplot(loans_charged_off,aes(emp_length,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Employment Length")+
  xlab("Employment Length")+
  ylab("Total Paid")

# boxplot of total paid by term (not included in markdown)
ggplot(loans_charged_off,aes(term,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Term")+
  xlab("Term")+
  ylab("Total Paid")

# boxplot of total paid by loan status
ggplot(loans_charged_off,aes(loan_status,tot_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Loan Status")+
  xlab("Loan Status")+
  ylab("Total Paid")


# ---------- OTHER PLOTS (not included in markdown)
# DTI vs. pct_paid (not included in markdown)
ggplot(loans_charged_off, aes(dti, pct_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_point()

# scatterplot plot of loan grade vs.pct_paid (not included in markdown)
ggplot(loans_charged_off,aes(sub_grade,pct_paid))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_point()

# boxplot of loan amount by purpose (not included in markdown)
ggplot(loans_charged_off,aes(purpose,loan_amnt))+
  ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Loan Amount by Purpose")+
  xlab("Purpose")+
  ylab("Loan Amount")

#----------------------- MODELING ----------------------
#exclude employee title(free text), sub grade(same as grade), funded/funded inv(same as amount),desc(free text)
#   title (free text), total payment/received_prncp&int/inv/latefee/recoveries (same/components of target variable)
testmodel_charged_off <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+home_ownership+
                  annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+addr_state+
                  dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                  mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                  last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                  ,data=loans_charged_off)

summary(testmodel_charged_off) # adjusted r2 of 99.17 and p-value of .0082

#drop home_ownership
testmodel_charged_off1 <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+
                              annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+addr_state+
                              dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                              mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                              last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                            ,data=loans_charged_off)

summary(testmodel_charged_off1) # adjusted r2 of 99.02 and p-value of 4.033e-06

#drop term
testmodel_charged_off2 <- lm(tot_paid~loan_amnt+int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+addr_state+
                               dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off2) # adjusted r2 of 99.08 and p-value of 4.944e-07

#drop state
testmodel_charged_off3 <- lm(tot_paid~loan_amnt+int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off3) # adjusted r2 of 99.08 and p-value of 4.944e-07

#drop last payment amount
testmodel_charged_off4 <- lm(tot_paid~loan_amnt+int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off4) # adjusted r2 of 98.73 and p-value of 2.538e-07

#drop loan amount, colinear with installment
testmodel_charged_off5 <- lm(tot_paid~int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off5) # adjusted r2 of 96.57 and p-value of 3.608e-06

#drop delinq_2yrs 
testmodel_charged_off6 <- lm(tot_paid~int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off6) # adjusted r2 of 96.45 and p-value of 1.203e-06

#drop inq_last_6mths
testmodel_charged_off7 <- lm(tot_paid~int_rate+installment+grade+emp_length+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+earliest_cr_line+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off7) # adjusted r2 of 96.16 and p-value of 5.319e-07

#drop emp_length
testmodel_charged_off8 <- lm(tot_paid~int_rate+installment+grade+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+earliest_cr_line+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off8) # adjusted r2 of 93.10 and p-value of 4.934e-10

#drop grade
testmodel_charged_off9 <- lm(tot_paid~int_rate+installment+
                               annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+
                               dti+earliest_cr_line+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off9) # adjusted r2 of 93.72 and p-value of 5.951e-13

#drop loan status
testmodel_charged_off10 <- lm(tot_paid~int_rate+installment+
                               annual_inc+verification_status+issue_d+purpose+zip_code+
                               dti+earliest_cr_line+mths_since_last_delinq+
                               mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                               last_pymnt_d+last_credit_pull_d
                             ,data=loans_charged_off)

summary(testmodel_charged_off10) # adjusted r2 of 93.93 and p-value of 1.454e-13

#drop purpose
testmodel_charged_off11 <- lm(tot_paid~int_rate+installment+
                                annual_inc+verification_status+issue_d+zip_code+
                                dti+earliest_cr_line+mths_since_last_delinq+
                                mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off11) # adjusted r2 of 93.04 and p-value of 2.2e-16

#drop annual income
testmodel_charged_off12 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+zip_code+
                                dti+earliest_cr_line+mths_since_last_delinq+
                                mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off12) # adjusted r2 of 92.91 and p-value of 2.2e-16

#drop open_acc, very similar to total_acc
testmodel_charged_off13 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+zip_code+
                                dti+earliest_cr_line+mths_since_last_delinq+
                                mths_since_last_record+pub_rec+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off13) # adjusted r2 of 92.29 and p-value of 2.2e-16

#drop pub_rec
testmodel_charged_off14 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+zip_code+
                                dti+earliest_cr_line+mths_since_last_delinq+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off14) # adjusted r2 of 0.89 and p-value of 1.18e-14

#drop zip_code
testmodel_charged_off15 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                dti+earliest_cr_line+mths_since_last_delinq+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off15) # adjusted r2 of 79.62 and p-value of 2.2e-16

#drop earliest_cr_line
testmodel_charged_off16 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                dti+mths_since_last_delinq+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d+last_credit_pull_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off16) # adjusted r2 of 79.50 and p-value of 2.2e-16

#drop last_credit_pull_d
testmodel_charged_off17 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                dti+mths_since_last_delinq+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off17) # adjusted r2 of 79.32 and p-value of 2.2e-16

#drop dti
testmodel_charged_off18 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                mths_since_last_delinq+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off18) # adjusted r2 of 79.36 and p-value of 2.2e-16

#drop dti
testmodel_charged_off19 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                mths_since_last_record+revol_bal+revol_util+total_acc+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off19) # adjusted r2 of 81.60 and p-value of 2.2e-16

#drop revol_util
testmodel_charged_off20 <- lm(tot_paid~int_rate+installment+
                                verification_status+issue_d+
                                mths_since_last_record+revol_bal+total_acc+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off20) # adjusted r2 of 81.63 and p-value of 2.2e-16

#drop interest rate
testmodel_charged_off21 <- lm(tot_paid~installment+
                                verification_status+issue_d+
                                mths_since_last_record+revol_bal+total_acc+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off21) # adjusted r2 of 81.64 and p-value of 2.2e-16

#drop total_acc
testmodel_charged_off22 <- lm(tot_paid~installment+
                                verification_status+issue_d+
                                mths_since_last_record+revol_bal+
                                last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off22) # adjusted r2 of 81.62 and p-value of 2.2e-16

#drop verification_status
testmodel_charged_off23 <- lm(tot_paid~installment+issue_d+
                                mths_since_last_record+revol_bal+last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off23) # adjusted r2 of 81.58 and p-value of 2.2e-16
anova(testmodel_charged_off23)

#---------------------------Residuals & Analysis ----------

#multiple regression lines against the observed points
leveragePlots(testmodel_charged_off23, pch=16)


#augment model summary to df
modeldf_charged_off <- augment(testmodel_charged_off23)

#plot residiuals
ggplot(modeldf_charged_off,aes(.fitted,.resid))+
  geom_point()+
  #ylim(-15000,15000)+
  geom_line( y = 0, linetype = 2, color = "darkred")

#test for non constant variance, 
#very small P score, non constant variance is present
ncvTest(testmodel_charged_off23)

#quantile plot
qqPlot(testmodel_charged_off23, pch=16)

#density residual plot
ggplot(modeldf_charged_off,aes(x=.resid))+
  stat_function(fun=dnorm, color="red", size=1.2,     
                args=list(mean=mean(testmodel_charged_off23$.resid),
                          sd=sd(testmodel_charged_off23$.resid)))+
  geom_histogram(aes(y=..density..), bins=20, color="black", fill="light gray")+  
  xlim ( -1300, 1300 )+ 
  labs(x="Residuals", y="Density")

#residual normality test
#  Very small p value, residuals not normally distributed
shapiro.test(testmodel_charged_off23$residuals)

#no multicolinesrity present
vif(testmodel_charged_off23)

plot_summs(testmodel_charged_off23)

outlierTest(testmodel_charged_off23)

ggplot(loans_charged_off,aes(tot_paid))+
  geom_histogram()

ggplot(loans_charged_off,aes(sqrt(tot_paid)))+
  geom_histogram()

# ------------------  start of variable transformations -------------------------------



#BOXCOX TEST

box <- boxcox(testmodel_charged_off23)
# boxcox test says lambda of somewhere between 0 and .5 may work for tot_paid

testmodel_charged_off24 <- lm((tot_paid^(1/7))~installment+issue_d+
                                mths_since_last_record+revol_bal+last_pymnt_d
                              ,data=loans_charged_off)

summary(testmodel_charged_off24) #p-value = 2.2e-16 


modeldf_charged_off1 <- augment(testmodel_charged_off24)

#plot residiuals
ggplot(modeldf_charged_off1,aes(.fitted,.resid))+
  geom_point()+
  #ylim(-15000,15000)+
  geom_line( y = 0, linetype = 2, color = "darkred")

ncvTest(testmodel_charged_off24) #p-value = 0.71389 #pass

shapiro.test(testmodel_charged_off24$residuals) #p-value = 2.2e-16 #fail


#log transform installment
testmodel_charged_off25 <- lm((tot_paid^(1/7))~log(installment)+issue_d+
                   mths_since_last_record+revol_bal+last_pymnt_d
                 ,data=loans_charged_off)

summary(testmodel_charged_off25) #p = 2.2e-16
ncvTest(testmodel_charged_off25) #p-value - 0.87978 #pass 
shapiro.test(testmodel_charged_off25$residuals) # did not noticeably improve

# ------------------------------------- Box-Cox of independent variables --------------------------------

summary(powerTransform(loans_charged_off$installment)) #Est Power .2839

summary(powerTransform(loans_charged_off$issue_d)) #Est Power 21.4612

summary(powerTransform(loans_charged_off$mths_since_last_record, family = "bcnPower")) #Est Power 1.0548

summary(powerTransform(loans_charged_off$revol_bal, family = "bcnPower")) #Est Power .2716

summary(powerTransform(loans_charged_off$last_pymnt_d, family = "bcnPower")) #Est Power 2.7278


# create new DF in which input will be transformed.
loantrans_charged_off <- loans_charged_off
loantrans_charged_off$installment <- loantrans_charged_off$installment^(.2839)
loantrans_charged_off$issue_d <- loantrans_charged_off$issue_d #^(21.4612)
loantrans_charged_off$mths_since_last_record <- loantrans_charged_off$mths_since_last_record^(1.0548)
loantrans_charged_off$revol_bal <- loantrans_charged_off$revol_bal^(.2716)
loantrans_charged_off$last_pymnt_d <- loans_charged_off$last_pymnt_d  #^(2.7278)
loantrans_charged_off$tot_paid <-loantrans_charged_off$tot_paid^(1/7)


#copy row @ index 2000 to new DF
test_case <- loantrans_charged_off[c(13,1000,156,228),]
test_case$tot_paid <- test_case$tot_paid^3
#delete row @ 2000 from original DF
loantrans_charged_off <- loantrans_charged_off[-c(13,1000,156,228),]


testmodel_charged_off26 <- lm(tot_paid~installment+issue_d+
                                mths_since_last_record+revol_bal+last_pymnt_d
                              ,data=loantrans_charged_off)

summary(testmodel_charged_off26) # adjusted R^2 of .88.84
ncvTest(testmodel_charged_off26) #fail

#shapiro test 
shapiro.test(testmodel_charged_off26$residuals) # fail
vif(testmodel_charged_off26)

plot_summs(testmodel_charged_off26, scale = TRUE, plot.distributions = TRUE,
           rescale.distributions = T,inner_ci_level = .95)

#https://www.statmethods.net/stats/regression.html
anova(testmodel_charged_off26)


box2 <- boxcox(testmodel_charged_off26)

testmodel_charged_off27 <- lm(tot_paid^3~installment+issue_d+
                                mths_since_last_record+revol_bal+last_pymnt_d
                              ,data=loantrans_charged_off)

summary(testmodel_charged_off27) # adjusted R^2 of 0.8963 
ncvTest(testmodel_charged_off27) #fail
shapiro.test(testmodel_charged_off27$residuals) # fail



#-----------------------Model Application-----------------

#apply model to test case
predictions_charged_off <- predict(testmodel_charged_off25,test_case,interval = "predict")^3

combo <- test_case %>%
  dplyr::select(tot_paid) 

pred_table <- merge(combo,predictions_charged_off,by = 0, all = T)

pred_table <- pred_table%>%
  mutate("% Error" = (abs(tot_paid-fit)/tot_paid)*100)

colnames(pred_table) <- c("","Total Paid","Predicted","Lower PI","Upper PI","% Error")
