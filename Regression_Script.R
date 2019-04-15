
library(dplyr)
library(ggplot2)
library(readr)
library(GGally)

# import the CSV
loandata <- read.csv('LoanStats3a.csv')

# create a subset of data features & target variable
loan <- loandata %>%
 # filter(loan_status == "Charged Off") %>%
  mutate(tot_paid = total_rec_prncp + total_rec_int) %>%
  select(tot_paid,loan_amnt,term,int_rate,inq_last_6mths,
         annual_inc,purpose,installment,
         delinq_2yrs,revol_bal)

#factor loan term; 2 levels (3,5 years)
loan$term <- factor(loan$term, levels = c("36 months","60 months"),labels = c("3year","5year"))

unique(loan$purpose)

# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)


# randomly pull one record from the dataframe

#copy row @ index 291 to new DF
test_case <- loan[291,]
#delete row @ 291 from original DF
loan <- loan[-c(291),]

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
  geom_histogram(bins = 50)+
  ggtitle(("Distribution of Borrower Annual Income"))+
  xlab("Annual Income")

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


model = lm(tot_paid~loan_amnt+term+int_rate+inq_last_6mths+
             annual_inc+purpose+installment+delinq_2yrs+
             revol_bal
           ,data = loan)

summary(model)

#-----------------------Model Application-----------------
#apply model to test case
predict(model,test_case)
