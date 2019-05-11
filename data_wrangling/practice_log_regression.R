
################## LOGISTIC REGRESSION WITH R ############################
# Load necessary packages
library(dplyr)
library(caret)
library(car)
library(ggplot2)

#get working dicrectory
getwd()
#step 1. get the data
logreg_data <- read.csv("rg_train.csv", stringsAsFactors = FALSE) 
View(logreg_data)
str(logreg_data)

#observation from the data
unique(logreg_data$children) ## children and age band have been corerce into character type owing to issues in the data
unique(logreg_data$age_band)## converted to numeric and get the average band


#use dplyr to ajust the above observed infractions

logreg_data1 <- logreg_data %>% mutate(children = sub("Zero","0", children),
                                       children = as.numeric(sub("+", "", children)),
                                       age_band = sub("71+", "71-71", age_band),
                                       age_band = sub("Unknown","",age_band),
                                       lower_band = as.numeric(substr(age_band, 1,2)),
                                       upper_band = as.numeric(substr(age_band,4,5)),
                                       age_band = (lower_band + upper_band)/2) %>% select(-lower_band, -upper_band)

#check to verify
View(logreg_data1)
unique(logreg_data1$children)
unique(logreg_data1$age_band)

#detect mising values
lapply(logreg_data1, function(x) sum(is.na(x)))

#treatment of missing values. we also observed that the missing 
#values are only in age_band column and children column,
#we can safely replace them with their respective mean

logreg_data2 = logreg_data1
for(i in 1:ncol(logreg_data2)){
  logreg_data2[is.na(logreg_data2[,i]),i] = mean(logreg_data2[,i], na.rm = T)
  }

#verify columns
lapply(logreg_data2, function(x) sum(is.na(x)))
View(logreg_data2)

##prone the categorical variable on the basis of their respective p-value relative 
##to the target variable through the chi square teest: accept if p-value < 5%
#this is necessary as a way of determining categorical variable that need to be converted to numerical

#check the structure

str(logreg_data2)
#checking chi-square test for significance
chisq.test(logreg_data2$status, as.character(logreg_data2$Revenue.Grid)) # not significant
chisq.test(logreg_data2$occupation, as.character(logreg_data2$Revenue.Grid)) #0.6301
chisq.test(logreg_data2$occupation_partner, as.character(logreg_data2$Revenue.Grid)) #0.7081
chisq.test(logreg_data2$home_status, as.character(logreg_data2$Revenue.Grid)) #0.8562
chisq.test(logreg_data2$self_employed, as.character(logreg_data2$Revenue.Grid)) #0.2762
chisq.test(logreg_data2$self_employed_partner, as.character(logreg_data2$Revenue.Grid)) #0.2762
chisq.test(logreg_data2$TVarea, as.character(logreg_data2$Revenue.Grid))  #0.793
chisq.test(logreg_data2$post_code, as.character(logreg_data2$Revenue.Grid)) #0.5537
chisq.test(logreg_data2$post_area, as.character(logreg_data2$Revenue.Grid)) #0.5686
chisq.test(logreg_data2$gender, as.character(logreg_data2$Revenue.Grid)) #0.032
chisq.test(logreg_data2$region, as.character(logreg_data2$Revenue.Grid)) #0.7179

# we can wirte the above code better through a for loop

categorical_var <- names(logreg_data2)[sapply(logreg_data2, function(x) is.character(x))]
for(i in 1:length(categorical_var)){
  print(categorical_var[i])
  print(chisq.test(logreg_data2[categorical_var[i]], as.character(logreg_data2$Revenue.Grid)))
}
#from the above, only gender categorical variable has a p-value less than 5%
# remove all other categorical variable other than "gender" from the data set

names(logreg_data2)[sapply(logreg_data2, function(x) is.character(x))]

logreg_data3 <- logreg_data2 %>% select(-status,-home_status,-self_employed_partner,
                                        -post_area,-occupation,-family_income,-TVarea,
                                        -occupation_partner, -self_employed, -post_code,
                                        -region)
View(logreg_data3)

##feature selection: continuous variable
##similar to the categorical varaible we can determine which variable is 
##significant enough to make our model
#Isolate the continuous variable in the data set
#str(logreg_data3)
continuos_variable <-  names(logreg_data3)[sapply(logreg_data3, function(x) is.numeric(x))]

## check the p-value of each variable through getting their respective ttest

#names(logreg_data3)[-1]
#for(i in 1:length(continuos_variable)){
  #print(continuos_variable[i])
  #print(t.test(logreg_data3[continuos_variable[i]][logreg_data3$Revenue.Grid==1],
              # logreg_data3[continuos_variable[i]][logreg_data3$Revenue.Grid==2]))}


# children pvalue > 5% drop
t.test(logreg_data3$children[logreg_data3$Revenue.Grid==1],
       logreg_data3$children[logreg_data3$Revenue.Grid==2])
#bal trans pvalue <5%
t.test(logreg_data3$Balance.Transfer[logreg_data3$Revenue.Grid==1],
       logreg_data3$Balance.Transfer[logreg_data3$Revenue.Grid==2])
#life insurance pvalue < 5%
t.test(logreg_data3$Life.Insurance[logreg_data3$Revenue.Grid==1],
       logreg_data3$Life.Insurance[logreg_data3$Revenue.Grid==2])
# average aac bal -----> pvalue <5%
t.test(logreg_data3$Average.A.C.Balance[logreg_data3$Revenue.Grid==1],
       logreg_data3$Average.A.C.Balance[logreg_data3$Revenue.Grid==2])
# inbestment in mutual funds ----> pvalue< 5%
t.test(logreg_data3$Investment.in.Mutual.Fund[logreg_data3$Revenue.Grid==1],
       logreg_data3$Investment.in.Mutual.Fund[logreg_data3$Revenue.Grid==2])
#homeloan pvalue <5% 
t.test(logreg_data3$Home.Loan[logreg_data3$Revenue.Grid==1],
       logreg_data3$Home.Loan[logreg_data3$Revenue.Grid==2])
#investment in equity ---> pvalue < 5%
t.test(logreg_data3$Investment.in.Equity[logreg_data3$Revenue.Grid==1],
       logreg_data3$Investment.in.Equity[logreg_data3$Revenue.Grid==2])
# portfolio balance ---> pvalue <5%
t.test(logreg_data3$Portfolio.Balance[logreg_data3$Revenue.Grid==1],
       logreg_data3$Portfolio.Balance[logreg_data3$Revenue.Grid==2])
#ageband ---->  pvalue < 5% 
t.test(logreg_data3$age_band[logreg_data3$Revenue.Grid==1],
       logreg_data3$age_band[logreg_data3$Revenue.Grid==2])
#average credit card trans ----> pvalue < 5%
t.test(logreg_data3$Average.Credit.Card.Transaction[logreg_data3$Revenue.Grid==1],
       logreg_data3$Average.Credit.Card.Transaction[logreg_data3$Revenue.Grid==2])

# term deposite ----> pvalue > 5%
t.test(logreg_data3$Term.Deposit[logreg_data3$Revenue.Grid==1],
       logreg_data3$Term.Deposit[logreg_data3$Revenue.Grid==2])
#investment in taxsaving bond -----> pvalue<5%
t.test(logreg_data3$Investment.Tax.Saving.Bond[logreg_data3$Revenue.Grid==1],
       logreg_data3$Investment.Tax.Saving.Bond[logreg_data3$Revenue.Grid==2])
#online purchase amount -----> pvalue <5%
t.test(logreg_data3$Online.Purchase.Amount[logreg_data3$Revenue.Grid==1],
       logreg_data3$Online.Purchase.Amount[logreg_data3$Revenue.Grid==2])
#investment in commodity ----> pvalue <5%
t.test(logreg_data3$Investment.in.Commudity[logreg_data3$Revenue.Grid==1],
       logreg_data3$Investment.in.Commudity[logreg_data3$Revenue.Grid==2])

# medical insurance ----> pvalue <5%
t.test(logreg_data3$Medical.Insurance[logreg_data3$Revenue.Grid==1],
       logreg_data3$Medical.Insurance[logreg_data3$Revenue.Grid==2])
#Investmentderivative ----> pvalue < 5%
t.test(logreg_data3$Investment.in.Derivative[logreg_data3$Revenue.Grid==1],
       logreg_data3$Investment.in.Derivative[logreg_data3$Revenue.Grid==2])
#personalloan -----> pvalue < 5%
t.test(logreg_data3$Personal.Loan[logreg_data3$Revenue.Grid==1],
       logreg_data3$Personal.Loan[logreg_data3$Revenue.Grid==2])

#drop home loan and age band


logreg_data4 <- logreg_data3 %>% select(-children, -Term.Deposit)

str(logreg_data4)

# convert the categorical variable to numerical;gender column
# since its just one, we may not use dummyvarse but used a shortcut: mutate with ifelse
#table(logreg_data4$gender)

logreg_data5 <- logreg_data4 %>% mutate(gender = ifelse(gender=="Uknown", "Female", gender),
                                        gender = ifelse(gender == "Female", 0,1),
                                        gender = as.numeric(gender))

str(logreg_data5)

#years_last_move need to be change into years_from_last_move(derived variable)
logreg_data6 <- logreg_data5 %>% mutate(years_from_last_move = 2019 - year_last_moved) %>% select(-year_last_moved)
str(logreg_data6)

lapply(logreg_data6, function(x) sum(is.na(x)))
#logreg_data7 <- logreg_data6

#UC_RCB <- mean(ldata5$Revolving.CREDIT.Balance) + 3*sd(ldata5$Revolving.CREDIT.Balance)
#ldata5$Revolving.CREDIT.Balance[ldata5$Revolving.CREDIT.Balance > UC_RCB] <- UC_RCB
#detecting and treatment of outlier in log regression mean +- 3std dev
#logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 1]
#logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 2]

#Uc_grd1 <- mean(logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 1]) + 3* sd(logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 1])
#logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 1] > Uc_grd1] <- Uc_grd1

#Uc_grd2 <- mean(logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 2]) + 3* sd(logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 2])
#logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 2] > Uc_grd2] <- Uc_grd2

#boxplot( logreg_data7$Average.Credit.Card.Transaction[logreg_data7$Revenue.Grid == 2])
#logreg_data7 <- logreg_data6$Average.Credit.Card.Transaction %>% filter(Revenue.Grid == 1) 

# convert the target's binary indicator from 2,1 to 0,1

logreg_data7 <- logreg_data6 %>% mutate(Revenue.Grid = ifelse(Revenue.Grid == 2,0,1))

str(logreg_data7)

##use "stratified sampling" to split the data into train and test
train_index <- createDataPartition(logreg_data7$Revenue.Grid,p = 0.7,list = FALSE)
train_data <-  logreg_data7[train_index,]
test_data <- logreg_data7[-train_index,]

#Stratified sampling is prefered in logistic regression
prop.table(table(train_data$Revenue.Grid))
prop.table(table(test_data$Revenue.Grid))

#see whether there is a differnce when Random sampling is used instead
#train_random_index <- sample(1:nrow(logreg_data7), round(0.7*nrow(logreg_data7)))
#train_data_random <- logreg_data7[train_random_index,]
#test_data_random <- logreg_data7[-train_random_index,]

#prop.table(table(train_data_random$Revenue.Grid))
#prop.table(table(test_data_random$Revenue.Grid))
#we will do a linear regression before a logistic regression so as to treat multincolinearity

#linregfit <- lm(Revenue.Grid ~. -REF_NO, data = train_data)
library(car)
vif(linregfit) # according to industry standard multicolinearity should not exceed 3

linregfit <- lm(Revenue.Grid ~. -REF_NO -Investment.in.Derivative -Investment.in.Equity -Investment.in.Commudity -Portfolio.Balance, data = train_data)
sort(vif(linregfit),decreasing = T) #remove:Investment.in.Derivative,Investment.in.Equity,Investment.in.Commudity,Portfolio.Balance

#logistic regression;copy the above arguement

#log_reg_fit <- glm(as.factor(Revenue.Grid) ~. -REF_NO -Investment.in.Derivative -Investment.in.Equity -Investment.in.Commudity -Portfolio.Balance, family = "binomial", data = train_data)
summary(log_reg_fit)
#use the step function to remove variable thats not significant
step(log_reg_fit)

log_reg_fit <- glm(formula = as.factor(Revenue.Grid) ~ Average.Credit.Card.Transaction + 
                     Balance.Transfer + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                     Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
                     Home.Loan + Online.Purchase.Amount, family = "binomial", 
                   data = train_data)
summary(log_reg_fit)

# prediction
revenue_grid_pred_test <- predict(log_reg_fit, newdata = test_data, type = c("response"))

# evaluation