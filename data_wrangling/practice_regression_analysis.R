# load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: GET THE DATA
getwd()
data_loan <- read.csv("loans data.csv", stringsAsFactors = FALSE)
#view the structure
View(data_loan)
str(data_loan)
dim(data_loan)

#1a. Understand the data
str(data_loan)
unique(data_loan$Amount.Requested) # there is a dot; change type to numeric
unique(data_loan$Amount.Funded.By.Investors) # there is a dot; change type to numeric
unique(data_loan$Interest.Rate) # there is a percent sign; change type to numeric
unique(data_loan$Debt.To.Income.Ratio)# there is percent sign, change type to numeric
unique(data_loan$FICO.Range) # there is hyphen;get mean and change to numeric
unique(data_loan$Open.CREDIT.Lines)# there is dot; change type to numeric
unique(data_loan$Revolving.CREDIT.Balance) # there is dot; change type to numeric
# step 2 DATA PREPARATION

# Using dplyr to adjust the above variable/columns
data_loan1 <- data_loan %>% mutate(Amount.Requested = as.numeric(Amount.Requested),
                                   Amount.Funded.By.Investors = as.numeric(Amount.Funded.By.Investors),
                                   Interest.Rate = as.numeric(sub("%","", Interest.Rate)),
                                   Debt.To.Income.Ratio = as.numeric(sub("%", "", Debt.To.Income.Ratio)),
                                   Open.CREDIT.Lines = as.numeric(Open.CREDIT.Lines),
                                   Revolving.CREDIT.Balance = as.numeric(Revolving.CREDIT.Balance),
                                   first_half = as.numeric(substr(FICO.Range, 1,3)),
                                   second_half = as.numeric(substr(FICO.Range, 5,7)),
                                   FICO.Range = (first_half + second_half)/2) %>% select(-first_half, -second_half)



