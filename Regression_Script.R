library(dplyr)

loandata <- read.csv('LoanStats3a.csv')

failed_loan <- loandata %>%
  filter(loan_status == "Charged Off") %>%
  select(loan_amnt,term,int_rate,installment,sub_grade,home_ownership,
         annual_inc,pymnt_plan,purpose,dti,total_pymnt,total_rec_prncp) %>%
  mutate(pct_paid = total_rec_prncp/loan_amnt * 100)
