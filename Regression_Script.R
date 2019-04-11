
library(dplyr)
library(ggplot2)
library(readr)
library(GGally)

# import the CSV
loandata <- read.csv('LoanStats3a.csv',stringsAsFactors = FALSE)

# create a subset of data limited to those loans that failed + features 
loan <- loandata %>%
  filter(loan_status == "Charged Off") %>%
  mutate(pct_paid = total_rec_prncp/loan_amnt * 100) %>%
  select(loan_amnt,term,int_rate,inq_last_6mths,sub_grade,home_ownership,
         annual_inc,purpose,dti,pct_paid) 

#factor loan_term; 2 levels (3,5 years)
loan$term <- factor(loan$term, labels = c("3year","5year"))

# factor loan grades info @ https://www.lendingclub.com/foliofn/rateDetail.action
#gradelevels <- c("A1","A2","A3","A4","A5","B1","B2"
                 #"B3","B4","B5","C1")

loan$sub_grade <- factor(loan$sub_grade)
summary(loan$sub_grade)
# need to check if levels are ascending or descending

# factor home_ownership
loan$home_ownership <- factor(loan$home_ownership, levels = c("RENT","MORTGAGE","OWN"))


unique(loan$purpose)
# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)



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

# distribution of  % paid
ggplot(loan, aes(pct_paid))+
  geom_histogram()+
  ggtitle("Distribution of % Paid")+
  xlab("% Paid")

# DTI distribution
ggplot(loan, aes(dti))+
  geom_histogram()+
  ggtitle("Distribution of Borrower DTI")+
  xlab("DTI")

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
  select(loan_amnt,int_rate,inq_last_6mths,annual_inc,dti,pct_paid)

#pairwise ploits of numeric values  
ggpairs(numeric_loan)

# ---------RELATIONSHIPS BETWEEN CATEGORICAL VARIABLES AND TARGET VARIABLE

# boxplot of % repaid by purpose, with coordinates flipped
ggplot(loan, aes(purpose, pct_paid))+
  geom_boxplot()+
  coord_flip()+
  ggtitle("% Paid by Purpose")+
  xlab("% Paid")+
  ylab("Purpose")

# boxplot plot of loan grade vs.pct_paid
ggplot(loan,aes(sub_grade,pct_paid))+
  geom_boxplot()+
  ggtitle("% Paid by Sub-grade")+
  xlab("Loan Sub-Grade")+
  ylab("% Paid")

# boxplot of loanterm  vs. pct paid
ggplot(loan,aes(term,pct_paid)) +
  geom_boxplot()+
  ggtitle("% Paid by Loan Term")+
  xlab("Loan Term")+
  ylab("% Paid")

# boxplot of home ownership to pct_paid
ggplot(loan,aes(home_ownership,pct_paid))+
  geom_boxplot()+
  ggtitle("% Paid by Home Ownership")+
  xlab("Home Ownership")+
  ylab("% Paid")

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
