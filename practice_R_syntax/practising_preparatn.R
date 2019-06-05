# preparing a data set for linear regression or any model application is the core of the
# data science profession 
# some of the steps involve in data preparation are as follows

#  read your data into rstudio or any tool you are using
loan_data <- read.csv("loans data.csv", stringsAsFactors = FALSE)

#View your data set on the pane
View(loan_data)

#Get a feel of the size and columns names of your data set
colnames(loan_data)
dim(loan_data)

# get a feel of the structure of your data set
str(loan_data)

 # transform column to their appropriate data type which were transformed to strings beacuse of inherent issues
# for instance loan_Amount_requested and similar columns ought to be numeric in order to be useful in our objective

# we would employ the use of dplyr to effect this transformation
library(dplyr)
loan_data1 <- loan_data %>% mutate(
  Revolving.CREDIT.Balance = as.numeric(Revolving.CREDIT.Balance),
  Amount.Requested = as.numeric(Amount.Requested),
  Amount.Funded.By.Investors = as.numeric(Amount.Funded.By.Investors),
  Interest.Rate = as.numeric(sub("%", "",Interest.Rate)),
  Open.CREDIT.Lines = as.numeric(Open.CREDIT.Lines),
  Debt.To.Income.Ratio = as.numeric(sub("%", "", Debt.To.Income.Ratio))
  )
# view your transformed data set and their respetive data type
View(loan_data1)
str(loan_data1)
# check and treat missing values. It would be helpful to know the missing values in each columns
lapply(loan_data1, function(x) sum(is.na(x)))
# using Dplyer package 
loan_data2 = loan_data1 %>% filter(!ID=="NA")
sum(is.na(loan_data2$ID))
View(loan_data2)
#check the values in ID
#loan_data2 <- loan_data1[!is.na(loan_data1$ID),]
#sum(is.na(loan_data2$ID))
# Treating other missing values
lapply(loan_data2, function(x) sum(is.na(x)))

#function on geting mode
get_mode <- function(x){
  sey <- x[!is.na(x)]
  uni <- unique(sey)
  return(uni[which.max(tabulate(match(x, uni)))])
}


# if the mising value is in a categorical column, replace NA with the mode of that column
#whereas if the missing value is in a numerical column, replace NA with the mean of that column
loan_data3 <- loan_data2

for(i in 1:ncol(loan_data3)){
  if(class(loan_data3[,i]) == "numeric"){
    loan_data3[is.na(loan_data3[,i]),i] = mean(loan_data3[,i],na.rm = T)
  }else if(class(loan_data3[,i]) == "character"){
    loan_data3[is.na(loan_data3[,i]),i] = get_mode(loan_data3[,i])
  }
 
  
 }
  
  
  #for(i in 1:ncol(data_loan2)){
    #if(class(data_loan2[,i])  == "numeric"){
      #data_loan2[is.na(data_loan2[,i]), i] = mean(data_loan2[,i], na.rm = TRUE)
      
    #}else if(class(data_loan2[,i]) == "character"){
     # data_loan2[is.na(data_loan[,i]),i] = get_mode(data_loan2[,i])
   # }
    
  
#}

# check for effect
lapply(loan_data3,function(x) sum(is.na(x)))

# Handling and treatment of outliers
#using the formular: "mean + 3*stddev
summary(loan_data3)

p <- ggplot(data = loan_data3, mapping = aes(x=loan_data3$Loan.Length, y=loan_data3$Interest.Rate))
p + geom_boxplot()


#clustering example

getwd()
library(dplyr)

wine_data <- read.csv("winequality-red.csv", sep = ";", stringsAsFactors = FALSE)
str(wine_data)
View(wine_data)

wine_data_cluster <-  wine_data %>% select(-quality)
#helping to visualiz missing values
install.packages("VIM")

library(VIM)
#no missing values
aggr(wine_data_cluster)

#get the scales variable of data
wine_data_cluster_scale <- scale(wine_data_cluster)# -3 +3
summary(wine_data_cluster_scale)

#do the k-means clustering

?kmeans
km_result <- kmeans(wine_data_cluster_scale, centers = 3, iter.max = 25, nstart = 15)

km_result$cluster
km_result$centers
km_result$withinss #intra cluster distance
km_result$betweenss# inter cluster distance



##Random forest

install.packages("randomForest")

hr_data <- read.csv("hr_train.csv", stringsAsFactors = FALSE)

View(hr_data)

hr_data %>% filter(promotion_last_5years == 1) %>% summarize( count = n())

hr_data %>% filter(left == 0) %>% select(satisfaction_level) %>% summarize(variance = round(var(satisfaction_level),4))
round(var(hr_data[hr_data$left==0, "satisfaction_level"]),4)

library(ggplot2)
ggplot(data = hr_data, aes(x="average_monthly_hours")) + geom_histogram()
plot(hr_data$average_montly_hours)
?plot
histogram(hr_data$average_montly_hours, hr_data, type ='count')
?histogram


which.max(table(hr_data[hr_data$left == 1, "salary"]))
round(cor(hr_data$last_evaluation, hr_data$average_montly_hours),2)
unique(hr_data$last_evaluation)
unique(hr_data$average_montly_hours)
coefficients()
lapply(hr_data, function(x) sum(is.na(x)))
chisq.test(hr_data$salary, hr_data$left)
library(caret)
library(car)
#feature selection


for(i in 1:length(hr_data)){
  if(class(hr_data[,i]) == "charater"){
  chisq.test(hr_data[,i], hr_data$left)
  }
}
#train_index <- createDataPartition(logreg_data7$Revenue.Grid,p = 0.7,list = FALSE)

#dmy <- dummyVars("~.",data = ldata8, fullRank = TRUE) # FullRank = TRUE  --> for n categories , it will ctreate n-1 variables
#View(dmy)
#ldata9 <- data.frame(predict(dmy,newdata = ldata8))
View(ldata9)

dmy <- dummyVars("~.", data = hr_data, fullRank = TRUE)
hr_data1 <- data.frame(predict(dmy,newdata = hr_data))
str(hr_data1)
View(hr_data1)


hr_index <- createDataPartition(hr_data1$left, p = 0.9, list = FALSE)
train_hr <- hr_data1[hr_index,]
test_hr <- hr_data1[-hr_index,]
nrow(train_hr)
nrow(test_hr)
View(train_hr)

prop.table(table(hr_data1$left))

log_fit <- lm(left~.-salessales -salarylow, data = train_hr )
summary(log_fit)
sort(vif(log_fit), decreasing = T)

#logreg_fitty <- glm(as.factor(left)~.-salessales -salarylow,family = "binomial", data = train_hr )
summary(logreg_fitty)

step(logreg_fitty)

logreg_fitty <- glm(formula = as.factor(left) ~ satisfaction_level + last_evaluation + 
                      number_project + average_montly_hours + time_spend_company + 
                      Work_accident + promotion_last_5years + saleshr + salesIT + 
                      salesmanagement + salesRandD + salarymedium -saleshr -salesIT -salesRandD, family = "binomial", 
                    data = train_hr)

summary(logreg_fitty)

test_hr$prediction <- predict(logreg_fitty, newdata = test_hr, type = c("response"))
View(test_hr)


median(hr_data[hr_data$left==1, "time_spend_company"])

hr_data %>% group_by(sales) %>% summarize( max_median = max(median(average_montly_hours))) %>% arrange(desc(max_median) )

t.test(hr_data[hr_data$number_project], hr_data[hr_data$left ==1])

chisq.test(hr_data$number_project[hr_data$left==1],
       hr_data$number_project[hr_data$left==2])


round(1/length(hr_data$left[hr_data$Work_accident == 1]),2)