## R version of FoCS exercise - just for cross-cheking the results
library(data.table)
library(lubridate)
library(dplyr)

## loan lenders
loans_lenders = fread("kiva-kaggle/loans_lenders.csv", data.table = F)
dim(loans_lenders)

## Problem 1 : Normalize the loan_id
lenders = strsplit(loans_lenders[,2], ", ")
lenders.all = unlist(lenders)

lenders.tmp = strsplit(loans_lenders[,2], ",")
lenders.all.tmp = unlist(lenders)

cardinality = sapply(lenders, length)
loan_id_norm = rep(loans_lenders[,1], cardinality)
loans_lenders_norm = data.frame(loan_id = loan_id_norm, lender = lenders.all)

## Problem 2 : For each loan, add a column duration corresponding to the number
## of days between the disburse time and the planned expiration time. If any of
## those two dates is missing, also the duration must be missing.

loans = fread("kiva-kaggle/loans.csv", data.table = F)
dim(loans)
loans1 = loans[,c("loan_id", "disburse_time", "planned_expiration_time")]
loans1$duration = with(loans1, seconds_to_period(planned_expiration_time - disburse_time))
head(loans1)
dim(loans1)


## Problem 3: Find the lenders that have funded at least twice.
## Note: it takes into account loans given by the same funder multiple time for the same loan_id

loans_lenders_norm_grp = group_by(loans_lenders_norm, lender)
lenders_multifunders = summarise(loans_lenders_norm_grp, funds = n())
lenders_multifunders = arrange(lenders_multifunders, funds)
tail(lenders_multifunders)


## Problem 4: For each country, compute how many loans have involved that country as borrowers.
loans1 = loans[ ,c("loan_id", "country_code", "country_name", "loan_amount")]
loans1.null = loans1[loans1$country_code == "", ]

## All Namibia is blank
loans1.null

countries = fread('kiva-kaggle/country_stats.csv', data.table = F)
countries[countries$country_code == "", ]

## Fix the country code to NAM
loans1$country_code[loans1$country_code == ""] = "NAM"
loans1[loans1$country_code == "NAM", ]

loans1.grp = group_by(loans1, country_name, country_code)
loans1.borrowed =  summarise(loans1.grp, times_borrowed = n())
loans1.borrowed = arrange(loans1.borrowed, times_borrowed)
tail(loans1.borrowed)

## Problem 5: For each country, compute the overall amount of money borrowed
loans1.tot =  summarise(loans1.grp, total = sum(loan_amount))
loans1.tot= arrange(loans1.tot, total)
tail(loans1.tot)

total = sum(loans1.tot$total)
total

loans1.tot$perc = loans1.tot$total/total
loans1.tot

## Problem 7: Like the three previous points, but split for each year (with respect to disburse time).
loans2 = loans1
loans2$year = year(loans$disburse_time)

loans2.grp = group_by(loans2, country_name, country_code, year)
loans2.yearwise =  summarise(loans2.grp, total_loans = sum(loan_amount))
loans2.yearwise = arrange(loans2.yearwise, total_loans)
tail(loans2.yearwise)

loans2.yearwise1 = group_by(loans2.yearwise, year)
summarise(loans2.yearwise1, sum(total_loans))

## Problem 8: For each lender, compute the overall amount of money lent. For
## each loan that has more than one lender, you must assume that all lenders
## contributed the same amount.

## Problem: for loan_id 657259, num_lenders_total =11. However, in the loan_lenders, there are only 7!
# Similarly for loan_id 658010. 
# loans_lenders[loans_lenders$loan_id == "657307", ]

loans3 = loans[ ,c("loan_id", "loan_amount", "funded_amount", "num_lenders_total")]
loans3$amt_per_lender = with(loans3, funded_amount/num_lenders_total)

loans_amount = merge(loans_lenders, loans3, all.x = T)
###### We see above that the num_lenders is not the right filed to find the total number of lenders

### Problem 9: Country, lent vs borrowed. For each country, compute the
##difference between the overall amount of money lent and the overall amount of
##money borrowed. Since the country of the lender is often unknown, you can
##assume that the true distribution among the countries is the same as the one
##computed from the rows where the country is known.

