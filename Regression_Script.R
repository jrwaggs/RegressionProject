
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
#---------------------------- Data import and Cleaning ------------------------
# import the CSV
loan <- read.csv('LoanStats3a.csv')

#drop NAs
loan <-loan[complete.cases(loan),]  

#convert revolving util from string % to numeric decimal
loan$revol_util<- as.numeric(sub("%","",loan$revol_util))/100

#convert term to numeric
loan$term<- as.numeric(sub("months","",loan$term))

#compute target dependent variable, total paid
loan <- loan %>%
  mutate(tot_paid = total_rec_prncp + total_rec_int)

# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)



#convert character strings from form like '10-Dec' to form like '2010-dec'
# strings to be used with anytime function
loan$issue_d <- sub("11","2011",loan$issue_d,fixed = TRUE)
loan$issue_d <- sub("10","2010",loan$issue_d,fixed = TRUE)
loan$issue_d <- sub("9","2009",loan$issue_d,fixed = TRUE)
loan$issue_d <- sub("8","2008",loan$issue_d,fixed = TRUE)
loan$issue_d <- sub("7","2007",loan$issue_d,fixed = TRUE)

#convert issue_d from factor to to date-time 
loan$issue_d <-anytime(loan$issue_d)

# convert from '7-Dec' to '2007-Dec' for use with anytime()
loan$last_pymnt_d <- sub("16","2016",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d <- sub("15","2015",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d <- sub("14","2014",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d <- sub("13","2013",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d <- sub("12","2012",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d <- sub("11","2011",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d  <- sub("10","2010",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d  <- sub("9","2009", loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d  <- sub("8","2008",loan$last_pymnt_d ,fixed = TRUE)
loan$last_pymnt_d  <- sub("7","2007",loan$last_pymnt_d ,fixed = TRUE)

#convert issue_d from factor to date-time 
loan$last_pymnt_d <-anytime(loan$last_pymnt_d)

#  pull three records from the dataframe, 
#   plus a fourth with a high loan amount

#copy row @ index 2000 to new DF
test_case <- loan[c(13,1000,156,228),]
#delete row @ 2000 from original DF
loan <- loan[-c(13,1000,156,228),]

#------------------------------- data exploration -------------------------------------
mean(loan$pct_paid) # the mean % paid back on a failed loan is 35.38%

# ---------- DISTRIBUTIONS

#distribution of loan amounts
ggplot(loan,aes(loan_amnt))+
  geom_histogram(bins = 20)+
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
  select(int_rate,installment,annual_inc,inq_last_6mths,
         mths_since_last_delinq,mths_since_last_record,open_acc,pub_rec,revol_bal,
         revol_util,recoveries,tot_paid  )

#pairwise plots of numeric values  
ggpairs(numeric_loan)

# ---------RELATIONSHIPS BETWEEN CATEGORICAL VARIABLES AND TARGET VARIABLE

# boxplot of total repaid by purpose, with coordinates flipped
ggplot(loan, aes(purpose, tot_paid))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by Purpose")+
  xlab("Purpose")+
  ylab("Total Paid")

# boxplot of loan amount by purpose
ggplot(loan,aes(purpose,loan_amnt))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Loan Amount by Purpose")+
  xlab("Purpose")+
  ylab("Loan Amount")

ggplot(loan,aes(grade,tot_paid))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("Total Paid by loan grade")+
  xlab("Loan Grade")+
  ylab("Total Paid")

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

#pull loan amount variable, it is colinear with installment
testmodel4 <- lm(tot_paid~term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+addr_state+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                 ,data=loan)
summary(testmodel4) # adjusted r2 of 88.11

#drop state, reduce factors/variables

testmodel5 <- lm(tot_paid~term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+last_credit_pull_d
                 ,data=loan)

summary(testmodel5) 
#Adjusted r2 of 87.26 after issue_d & last payment date change and remove state

testmodel6 <- lm(tot_paid~term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+recoveries
                 ,data=loan)

summary(testmodel6)  #adjusted r2 of 87.31 after dropping last credit pulled
                        # added in recoveries, r2 = .8752


#-----------------------Model Application-----------------
#apply model to test case
predict(testmodel6,test_case,interval = "predict")


#---------------------------Residuals & Analysis
#augment model summary to df
modeldf <- augment(testmodel5)

#plot residiuals
ggplot(modeldf,aes(.fitted,.resid))+
  geom_point()+
  #ylim(-15000,15000)+
  geom_line( y = 0, linetype = 2, color = "darkred")
  
  
#test for non constant variance, 
  #very small P score, non constant variance is present
ncvTest(testmodel6)

#quantile plot
qqPlot(testmodel6,pch=16)

#residual normality test
  #  Very small p value, residuals not normally distributed
shapiro.test(testmodel6$residuals)


vif(testmodel6)

plot_summs(testmodel6)

outlierTest(testmodel6)


ggplot(loan,aes(tot_paid))+
  geom_histogram()

ggplot(loan,aes(sqrt(tot_paid)))+
  geom_histogram()

# ------------------------------  start of variable transformations -------------------------------


#BOXCOX TEST

box <- boxcox(testmodel6)
# boxcox test says lambda of somewhere between 0 and .5 may work for tot_paid

testmodel7 <- lm((tot_paid^(1/3))~term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+recoveries
                 ,data=loan)

summary(testmodel7) #p = .8722


modeldf1 <- augment(testmodel7)

#plot residiuals
ggplot(modeldf1,aes(.fitted,.resid))+
  geom_point()+
  #ylim(-15000,15000)+
  geom_line( y = 0, linetype = 2, color = "darkred")

ncvTest(testmodel7) # p value of .726, non-constant variance is not an issue

shapiro.test(testmodel7$residuals) # fail



#log transform  int rate, annual inc,
testmodel8 <- lm((tot_paid^(1/3))~term+log(int_rate)+sqrt(installment)+grade+emp_length+home_ownership+
                   log(annual_inc)+issue_d+loan_status+purpose+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+recoveries
                 ,data=loan)

summary(testmodel8) #p = .9198, 
ncvTest(testmodel8) #fail
shapiro.test(testmodel8$residuals) # did not noticeably improve

# ------------------------------------- Box-Cox of independent variables --------------------------------

summary(powerTransform(loan$int_rate)) #Est Power .3657

summary(powerTransform(loan$installment))#Est Power .2797

summary(powerTransform(loan$annual_inc))#Est Power .076

summary(powerTransform(loan$inq_last_6mths, family = "bcnPower"))#Est Power .128

summary(powerTransform(loan$mths_since_last_delinq, family = "bcnPower"))#Est Power .1972
 
summary(powerTransform(loan$mths_since_last_record, family = "bcnPower"))#Est Power -.0688

summary(powerTransform(loan$open_acc, family = "bcnPower"))#Est Power .3411

summary(powerTransform(loan$pub_rec, family = "bcnPower"))#Est Power -3 ???

summary(powerTransform(loan$revol_bal, family = "bcnPower"))#Est Power .239

summary(powerTransform(loan$revol_util, family = "bcnPower"))#Est Power .644



loantrans <- loan
loantrans$int_rate <- loantrans$int_rate^(.366)
loantrans$installment <- loantrans$installment^(.2797)
#loantrans$annual_inc <- loantrans$annual_inc^(.076)
loantrans$inq_last_6mths <- loantrans$inq_last_6mths^(.128)
loantrans$mths_since_last_delinq <- loantrans$mths_since_last_delinq^(.1972)
#loantrans$mths_since_last_record <- 1/(loantrans$mths_since_last_record^(.0688))
loantrans$open_acc <- loantrans$open_acc^(.3411)
#loantrans$pub_rec <- 1/loantrans$pub_rec^(3)
loantrans$revol_bal <- loantrans$revol_bal^(.239)
loantrans$revol_util <- loantrans$revol_util^(.644)


testmodel10 <- lm(tot_paid^(1/3)~term+int_rate+installment+grade+emp_length+home_ownership+
                   annual_inc+issue_d+loan_status+purpose+
                   inq_last_6mths+mths_since_last_delinq+
                   mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                   last_pymnt_d+last_pymnt_amnt+recoveries
                 ,data=loantrans)

summary(testmodel10) # adjusted R^2 of .9257
ncvTest(testmodel10) #fail
shapiro.test(testmodel10$residuals) # fail


box2 <- boxcox(testmodel10)
box2
testmodel11 <- lm(sqrt(tot_paid)~term+int_rate+installment+grade+emp_length+home_ownership+
                    annual_inc+issue_d+loan_status+purpose+
                    inq_last_6mths+mths_since_last_delinq+
                    mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
                    last_pymnt_d+last_pymnt_amnt+recoveries
                  ,data=loantrans)

summary(testmodel11) # adjusted R^2 of .9201
ncvTest(testmodel11) #fail
shapiro.test(testmodel10$residuals) # fail



boxTidwell(tot_paid^(1/3)~term+int_rate+installment+grade+emp_length+home_ownership+
  annual_inc+issue_d+loan_status+purpose+
  inq_last_6mths+mths_since_last_delinq+
  mths_since_last_record+open_acc+pub_rec+revol_bal+revol_util+
  last_pymnt_d+last_pymnt_amnt+recoveries
,data=loan)




