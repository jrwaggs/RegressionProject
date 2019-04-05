
library(dplyr)
library(ggplot2)
library(readr)

# import the CSV
loandata <- read.csv('LoanStats3a.csv')

# create a subset of data limited to those loans that failed + features 
loan <- loandata %>%
  filter(loan_status == "Charged Off") %>%
  select(loan_amnt,term,int_rate,installment,sub_grade,home_ownership,
         annual_inc,pymnt_plan,purpose,dti,total_pymnt,total_rec_prncp) %>%
  mutate(pct_paid = total_rec_prncp/loan_amnt * 100)

#factor loan_term; 2 levels (3,5 years)
loan$term <- factor(loan$term, labels = c("3year","5year"))

# factor loan grades info @ https://www.lendingclub.com/foliofn/rateDetail.action
loan$sub_grade <- factor(loan$sub_grade)
summary(loan$sub_grade)

# factor home_ownership
loan$home_ownership <- factor(loan$home_ownership, levels = c("RENT","MORTGAGE","OWN"))


unique(loan$purpose)
# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)




ggplot(failed_loan, aes(loan_amnt,total_pymnt)) +
  geom_point()

ggplot(failed_loan, aes(loan_amnt,pct_paid)) +
  geom_point()

ggplot(failed_loan, aes(pct_paid))+
  geom_histogram()

pairs(failed_loan[1:5])
