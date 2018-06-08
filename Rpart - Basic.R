library(tidyverse)
library(rpart)
library(rpart.plot)

names = c('creditability','account_balance','duration_of_credit_month',
          'payment_status_of_previous_credit','purpose','credit_amount',
          'value_savingsstocks','length_of_current_employment',
          'instalment_per_cent','sex__marital_status','guarantors',
          'duration_in_current_address','most_valuable_available_asset',
          'age_years','concurrent_credits','type_of_apartment',
          'no_of_credits_at_this_bank','occupation','no_of_dependents',
          'telephone','foreign_worker')

col_types = list('creditability'=col_factor(levels=c('0','1')),
                 'account_balance'=col_factor(levels=c('1','2','3','4'), 
                                              ordered = T))

df <- read_csv('~/Documents/Data/german_credit.csv',
               col_names = names,
               col_types = col_types,
               skip = 1)

df %>% glimpse()


#### rpart Modell
model_rpart <- rpart(creditability ~ ., data=df) 
rpart.plot(model_rpart,fallen.leaves = F, type=5,extra=1)

