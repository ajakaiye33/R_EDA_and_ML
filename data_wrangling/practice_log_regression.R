
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

# include prediction as a column in both the train and test data
test_data$prediction <- predict(log_reg_fit, newdata = test_data, type = c("response"))
train_data$prediction <- predict(log_reg_fit, newdata = train_data, type = c("response"))

# view to verify
View(train_data)
View(test_data)

#the final outcome using 0.5 as a threshold/cut off probability
test_data$prediction_outcome <- ifelse(test_data$prediction > 0.5,1,0)
train_data$prediction_outcome <- ifelse(train_data$prediction > 0.3,1,0) # reduce the threshold from 0.5 to 0.3 to cut down on the false negative FN

# view to verify
View(test_data)
View(train_data)

# doing the confusion matrix
confusionMatrix(as.factor(test_data$prediction_outcome), as.factor(test_data$Revenue.Grid),
                positive = levels(as.factor(test_data$Revenue.Grid))[2])

confusionMatrix(as.factor(train_data$prediction_outcome), as.factor(train_data$Revenue.Grid),
                positive = levels(as.factor(train_data$Revenue.Grid))[2])


#Graphical Ilustration of imbalance learning
#ie Alot My Actual 1 observations have a predicted probability btw 0.1 - 0.5 or closer to 0
ggplot(train_data,aes(prediction, color = as.factor(Revenue.Grid))) + geom_density(size=1) +
  ggtitle("Training data predicted score")


#methods used to handle the imbalance:
#1.OVERSAMPLING
#2. UNDERSAMPLING
#3. SMOTE

# USING OVERSAMPLING
colnames_train <- colnames(train_data)
colname_x <- c(colnames_train[c(1:12)], colnames_train[c(14:21)])
up_train <- upSample(x = train_data[,colnames(train_data) %in% colname_x],
                     y = as.factor(train_data$Revenue.Grid))
#better way of doing oversampling
up_train_t <- upSample(x = train_data[,colnames(train_data) != "Revenue.Grid"],
                       y = as.factor(train_data$Revenue.Grid))
View(up_train_t)
#oversampling, using the upsample function creates an additonal column called CLASS*
View(up_train)
prop.table(table(up_train$Class))
prop.table(table(up_train_t$Class))


# apply logistic regression using upsample data(change just revenuegride and train data to class and upsampling data respectively)
logref_upsample <- glm(formula = as.factor(Class) ~ Average.Credit.Card.Transaction + 
                         Balance.Transfer + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
                         Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
                         Home.Loan + Online.Purchase.Amount, family = "binomial", 
                       data = up_train)

#get a prediction with the predict function
up_train$prediction <- predict(logref_upsample,newdata = up_train, type = c("response"))

# applying a probability threshold/cut-off of 0.5
up_train$predicted_outcome <- ifelse(up_train$prediction > 0.5,1,0)
#turn to factor why?
str(up_train)
up_train$predicted_outcome <- as.factor(up_train$predicted_outcome)
View(up_train)
str(up_train)

# apply confusion matrix on upsample data
confusionMatrix(up_train$predicted_outcome, up_train$Class)

# # graph again with the upsamle train data to see difference

ggplot(up_train,aes(prediction, color = as.factor(Class))) + geom_density(size=1) +
  ggtitle("Upsampled Training data predicted score")


# applying predict on test data; prediction is based on the model from the upsample data
View(test_data)

test_data$upsample_prediction <- predict(logref_upsample, newdata = test_data,type = "response")
test_data$upsample_prediction_outcome <- ifelse(test_data$upsample_prediction > 0.5,1,0)

# look at the performance from the confusion matrix
# using the original prediction and the prediction from the upsample data

confusionMatrix(as.factor(test_data$prediction_outcome), as.factor(test_data$Revenue.Grid),
                positive = levels(as.factor(test_data$Revenue.Grid))[2])# poor very low prediction of 1's our optimization objective(sensitivity 55% and FN=111)

confusionMatrix(as.factor(test_data$upsample_prediction_outcome), as.factor(test_data$Revenue.Grid),
                positive = levels(as.factor(test_data$Revenue.Grid))[2]) # with the upsampleprediction outcome our model perfomed better with sensitivity incresaing to about 90 % and FN reducing to 24 


# this is achieve with abitrarilly using the threshold of 0.5 but we can 
#finetune our model by determining the sweet spot of threshold to apply
threshold <- seq(0.1,0.9,0.05)
accuracy <- NULL

for(i in seq(along = threshold)){
  prediction_check <- ifelse(logref_upsample$fitted.values >= threshold[i],1,0)
  total_correct = length(which(up_train$Class == prediction_check))
  percentage_correct = total_correct/length(prediction_check)
  accuracy <- c(accuracy, percentage_correct)
  
}

plot(threshold,accuracy,pch=19,type='b',xlab ="Cutoffs/threshold",ylab = 'Accuracy%')

# from this graph we can see that our model would perform to its utmost at the point of 0.4

up_train$predicted_outcome <- ifelse(up_train$prediction > 0.5,1,0)


confusionMatrix(as.factor(test_data$upsample_prediction_outcome), as.factor(test_data$Revenue.Grid),
                positive = levels(as.factor(test_data$Revenue.Grid))[2]) # with the upsampleprediction outcome our model perfomed better with sensitivity incresaing to about 90 % and FN reducing to 24 
