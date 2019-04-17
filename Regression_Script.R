
library(dplyr)
library(ggplot2)
library(readr)
library(GGally)

# import the CSV
loandata <- read.csv('LoanStats3a.csv')

loan <- loandata

#convert revolving util from string % to numeric decimal
loan$revol_util<- as.numeric(sub("%","",loan$revol_util))/100

#compute target dependent variable, total paid
loan <- loan %>%
  mutate(tot_paid = total_rec_prncp + total_rec_int)

#drop NAs
loan <-loan[complete.cases(loandata),]  


#factor loan term; 2 levels (3,5 years)
loan$term <- factor(loan$term)

unique(loan$purpose)

# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)


# randomly pull one record from the dataframe

#copy row @ index 291 to new DF
test_case <- loan[2000,]
#delete row @ 291 from original DF
loan <- loan[-c(2000),]

#------------------------------- data exploration -------------------------------------
mean(loan$pct_paid) # the mean % paid back on a failed loan is 35.38%

# ---------- DISTRIBUTIONS

#distribution of loan amounts
ggplot(loan,aes(loan_amnt))+
  geom_histogram(bins = 7)+
  ggtitle("Distribution of Loan Amounts")+
  xlab("Loan amount")

# distribution of interest rates
ggplot(loan,aes(int_rate))+
  geom_histogram(bins=20)+
  ggtitle("Distribution of Loan Interest Rates")+
  xlab("Interest Rate")

# distribution of total paid
ggplot(loan, aes(tot_paid))+
  geom_histogram(bins = 30)+
  ggtitle("Distribution of Total Paid")+
  xlab("Total Paid")

#distribution of of annual incomes
ggplot(loan,aes(annual_inc))+
  geom_histogram(bins = 20)+
  ggtitle(("Distribution of Borrower Annual Income"))+
  xlab("Annual Income")+
  xlim(0,200000)

# Count of loan purpose
ggplot(loan,aes(purpose))+
  geom_bar()+
  ggtitle("Count of Loans by purpose")+
  xlab("Purpose")+
  coord_flip()

#----------CORRELATION ANALYSIS OF NUMERIC VARIABLES 

#subset of only numeric vaiables
numeric_loan <- loan %>%
  select(loan_amnt,int_rate,inq_last_6mths,annual_inc,installment,delinq_2yrs,tot_paid  )

#pairwise ploits of numeric values  
ggpairs(numeric_loan)

# ---------RELATIONSHIPS BETWEEN CATEGORICAL VARIABLES AND TARGET VARIABLE

# boxplot of total repaid by purpose, with coordinates flipped
ggplot(loan, aes(purpose, tot_paid))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Purpose")+
  xlab("Purpose")+
  ylab("Total Paid")

# boxplot of loanterm  vs. total paid
ggplot(loan,aes(term,tot_paid)) +
  geom_boxplot()+
  ggtitle("Total Paid by Loan Term")+
  xlab("Loan Term")+
  ylab("Total Paid")

# boxplot of loan amount by purpose
ggplot(loan,aes(purpose,loan_amnt))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Loan Amount by Purpose")+
  xlab("Purpose")+
  ylab("Loan Amount")

# ---------- OTHER PLOTS
# DTI vs. pct_paid
ggplot(loan, aes(dti, pct_paid))+
  geom_point()

# scatterplot plot of loan grade vs.pct_paid
ggplot(loan,aes(sub_grade,pct_paid))+
  geom_point()

# boxplot of loan amount by purpose
ggplot(loan,aes(purpose,loan_amnt))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Loan Amount by Purpose")+
  xlab("Purpose")+
  ylab("Loan Amount")

#----------------------- MODELING ----------------------
#exclude employee title(free text), sub grade(same as grade), funded/funded inv(same as amount),desc(free text)
#   title (free text), total payment/received_prncp&int/inv/latefee/recoveries (same/components of target variable)
testmodel <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+home_ownership+
                  annual_inc+verification_status+issue_d+loan_status+purpose+zip_code+addr_state+
                  dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+
                  mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                  last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                ,data=loan)

summary(testmodel) # adjusted r2 of 87.35

# drop verification status (group p values of .591 & .9628)
#   zip_code, earliest credit line
testmodel1 <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+addr_state+
                   dti+delinq_2yrs+inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+total_acc+
                   last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                 ,data=loan)

summary(testmodel1) # adjusted r2 of 88.24,

#pull total acc p =.89

testmodel2 <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+addr_state+
                   dti+delinq_2yrs+inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                 ,data=loan)

summary(testmodel2) # adjusted r2 of 88.24,

#remove delinq 2 years, dti
testmodel3 <- lm(tot_paid~loan_amnt+term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+addr_state+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                 ,data=loan)
summary(testmodel3) # adjusted r2 of 88.25







#-----------------------Model Application-----------------
#apply model to test case
predict(testmodel3,test_case)
  
