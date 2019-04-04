
library(dplyr)
library(ggplot2)
library(readr)


loandata <- read.csv('LoanStats3a.csv')

failed_loan <- loandata %>%
  filter(loan_status == "Charged Off") %>%
  select(loan_amnt,term,int_rate,installment,sub_grade,home_ownership,
         annual_inc,pymnt_plan,purpose,dti,total_pymnt,total_rec_prncp) %>%
  mutate(pct_paid = total_rec_prncp/loan_amnt * 100)

ggplot(failed_loan, aes(loan_amnt,total_pymnt)) +
  geom_point()

ggplot(failed_loan, aes(loan_amnt,pct_paid)) +
  geom_point()

ggplot(failed_loan, aes(pct_paid))+
  geom_histogram()

pairs(failed_loan[1:5])
