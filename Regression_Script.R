
library(dplyr)
library(ggplot2)
library(readr)

# import the CSV
loandata <- read.csv('LoanStats3a.csv')

# create a subset of data limited to those loans that failed + features 
loan <- loandata %>%
  filter(loan_status == "Charged Off") %>%
  select(loan_amnt,term,int_rate,inq_last_6mths,sub_grade,home_ownership,
         annual_inc,pymnt_plan,purpose,dti,total_pymnt,total_rec_prncp) %>%
  mutate(pct_paid = total_rec_prncp/loan_amnt * 100)

#factor loan_term; 2 levels (3,5 years)
loan$term <- factor(loan$term, labels = c("3year","5year"))

# factor loan grades info @ https://www.lendingclub.com/foliofn/rateDetail.action
loan$sub_grade <- factor(loan$sub_grade)
summary(loan$sub_grade)

# factor home_ownership
loan$home_ownership <- factor(loan$home_ownership, levels = c("RENT","MORTGAGE","OWN"))

#factor payment plan variable
loan$pymnt_plan <- factor(loan$pymnt_plan)

unique(loan$purpose)
# there are 15 purpose categories, -> factor 
loan$purpose <- factor(loan$purpose)
summary(loan$purpose)


# distribution of loans by % paid
ggplot(failed_loan, aes(pct_paid))+
  geom_histogram()

mean(loan$pct_paid) # the mean % paid back on a failed loan is 35.38%


# boxplot of % repaid by purpose, with coordinates flipped
ggplot(loan, aes(purpose, pct_paid)) +
  geom_boxplot() +
  coord_flip()



