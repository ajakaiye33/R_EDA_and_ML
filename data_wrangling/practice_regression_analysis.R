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


# verify new data frame
View(data_loan1)
str(data_loan1)
# detection and treatment of missing values
lapply(data_loan1, function(x) sum(is.na(x)))
#Remove the entire row in ID column with na
data_loan2 <- data_loan1[!is.na(data_loan1$ID),]
sum(is.na(data_loan2$ID))
# verify new data frame
View(data_loan2)
lapply(data_loan2, function(x) sum(is.na(x)))


# replace other missing values with mean if type is numeric, mode if type is categorical/character
#function for calculating mode

data_loan3 <- data_loan2

get_mode <- function(x){
  sey <- x[!is.na(x)]
  uni <- unique(sey)
  return(uni[which.max(tabulate(match(x, uni)))])}

for(i in 1:ncol(data_loan3)){
  if(class(data_loan3[,i])  %in% c("numeric","integer")){
    data_loan3[is.na(data_loan3[,i]), i] = mean(data_loan3[,i], na.rm = TRUE)
    
  }else if(class(data_loan3[,i]) == "character"){
    data_loan3[is.na(data_loan3[,i]),i] = get_mode(data_loan3[,i])
  }
  
  
}

  
lapply(data_loan3, function(x) sum(is.na(x)))

# drop amount funded by investor as its superflous

data_loan4 <- data_loan3 %>% select(-Amount.Funded.By.Investors)
data_loan5 <- data_loan4 %>% select(-Employment.Length, -State)



View(data_loan5)

# detect and treat outliers: mean +- 3*stddev
summary(data_loan5)
str(data_loan5)

#data_loan6 <- data_loan5 %>% summarize(Uc_monthly = mean(Monthly.Income) + 3*sd(Monthly.Income),
                                      # Uc_revolve = mean(Revolving.CREDIT.Balance) + 3* sd(Revolving.CREDIT.Balance)) %>% filter(
                                         #Monthly.Income > Uc_monthly, Revolving.CREDIT.Balance>Uc_revolve)



UC_monthly <- mean(data_loan5$Monthly.Income) + 3*sd(data_loan5$Monthly.Income)
data_loan5$Monthly.Income[data_loan5$Monthly.Income > UC_monthly] <- UC_monthly
UC_revolve <- mean(data_loan5$Revolving.CREDIT.Balance) + 3*sd(data_loan5$Revolving.CREDIT.Balance)
data_loan5$Revolving.CREDIT.Balance[data_loan5$Revolving.CREDIT.Balance > UC_revolve] <- UC_revolve

summary(data_loan5)

# convert categorical variable to numeric
# before converting the categorical variable they need to be grouped base on their frequency and group
# the approach would depend on the frequency of the various groups
str(data_loan5)
# some categorical variable
## Loan.Purpose, Home Ownership, Loan length
table(data_loan$Loan.Purpose)
table(data_loan5$Home.Ownership)
table(data_loan5$Loan.Length)
# repplace the dot with the mode in loan length
data_loan5$Loan.Length[data_loan5$Loan.Length == "."] <- get_mode(data_loan5[,"Loan.Length"])
#verify otput
table(data_loan5$Loan.Length)

# check percentage of frequency
round(prop.table(table(data_loan5$Loan.Purpose)),2)

# group other categorical variable
#sort(round(prop.table(table(data_loan5$Loan.Purpose)),2))

#check the relationship between the target and the interested category
round(tapply(data_loan5$Interest.Rate, data_loan5$Loan.Purpose,mean))
# from this we see that car, major_purchase,and educational are similar but interms
# of frequency percentage major_purpose is more frequent/popular
data_loan6 <- data_loan5 %>% mutate(Loan.Purpose = ifelse(Loan.Purpose %in% c("car","educational","major_purchase"),"major_purchase", Loan.Purpose),
                                    Loan.Purpose = ifelse(Loan.Purpose %in% c("home_improvement", "medical","vacation","wedding"), "home_improvement", Loan.Purpose),
                                    Loan.Purpose = ifelse(Loan.Purpose %in% c("credit_card", "house", "other","small_business"),"credit_card", Loan.Purpose),
                                    Loan.Purpose = ifelse(Loan.Purpose %in% c("debt_consolidation", "moving"), "debt_consolidation", Loan.Purpose))

#check/verify
table(data_loan6$Loan.Purpose)

#treat home_ownership categorical variable
#check for relation ship

round(prop.table(table(data_loan6$Home.Ownership)),2)

round(tapply(data_loan6$Interest.Rate, data_loan6$Home.Ownership, mean))
# we can see that mortgage, own and rent have a relationship,we can geoup them

#data_loan7 <- data_loan6 %>% mutate(Home.Ownership = ifelse(Home.Ownership %in% c("MORTGAGE", "OWN","RENT"),))
str(data_loan6)
library(caret)
dmy <- dummyVars("~.", data = data_loan6, fullRank = TRUE)
#View(dmy)

data_loan7 <- data.frame(predict(dmy,newdata = data_loan6))
View(data_loan7)
str(data_loan7)

corr_matrix <- data.frame(round(cor(data_loan7[,-c(1)], method = c("pearson")),2))
write.csv(corr_matrix,"Correlation_loanmart.csv",row.names = FALSE)