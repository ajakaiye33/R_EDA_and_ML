# Load necessary packages
library(dplyr)
library(caret)
library(car)
library(rpart)
library(rpart.plot)


#####################################Theory Notes on Decision Tree Algorithm############################:

# It has a structre like a tree. In the leaf-note lies the prediction and decision
# spliting is base on the order of importance of the variable with the highest posibility/level of reduction of impurity
# the  binary split will continue until both spectrum of classifaction are 100% each
# which leaf-note is giving a more homogenous group(the one with the less degree of impurity)
## the less the homogeneity the more the level of impurity and vic
# reduction in impurity need to be quantified to grow the tree
# the quantification of reduction in impurity is done in 2 ways:

###1.Gini Index/Impure
# - the higher the value of Gini the higher the homoenity
# Gini fomular is (p^2 + q^2) where p= Probability of playing q= probability of not playing

### 2. Entropy/Information gain (information in nodes)- How much of impurity are there in the system
# - the more pure node, the more homogenuos and the less information required to explain it
# the more impure node the less homogenous and the more information required to explain it
# Entropy formula ---> -p log2p - qlog2q
# In the trees, the most important node are positioned at the top whereas the least important node are at the bottom
## disadvantge of DT
# - Higher tendency to overfit ---> Modeling the noice in the data(mugging up the training data point)

### Methods of preventing this overfitting
## 1.Prunning
#- preprunning prevent: tree  the from growing in the first place
#-postprunning:- Allow the tree to grow but cut it afterwards
## 2. Use Random Forest Instead

# with DT, there wont be the need of using t-test and chis-square for feature selection
# DT automatically position the important variable at the top of the tree structure


#predict_test <- predict(tree_model_prune, newdata = test_house_data)
#length(predict_test)
#dim(test_house_data)
#dim(train_house_data$Price)

#in our data, there is no issue of overfittng. if the xerro values had been fluctuating 
#and the plotcp curve depicts growth( becomming u in shape) then overfitting could have been said
# have taken place and would cut or prun accordingly

# where there is overfitting and there is need for prunning(post prunning) we would run the code:

#tree_model_prune <- prune(tree_model,cp = related CP value)
# the lower value of xerro before there was an unexpected increase, the related CP(complexity parameter) value would be used as the parameter 
#value of the cp
# the value of the prune version is used for prediction for the training and testing data
############################################## End of Theory Notes###################################

#setup directory
setwd("~/Documents/edvancer/Data")

house_train <- read.csv("housing_train.csv", stringsAsFactors = FALSE)

house_test <- read.csv("housing_test.csv", stringsAsFactors = FALSE)
 ?read.csv()
# Add target column to test data and populate with NA
house_test$Price = NA

# add a column for delineation
house_test$data = "test"
house_train$data = "train"

dim(house_test)
dim(house_train)

#combin dataframe
all_house_data <- rbind(house_test,house_train)
str(all_house_data)
glimpse(all_house_data)
View(all_house_data)




# get ride of some columns
all_house_data1 <- all_house_data %>% select(-Address,-Postcode,-Suburb, -CouncilArea,-SellerG)

#str(all_house_data1)
#unique(all_house_data1$CouncilArea)
#replace empty string under councilarea with unknown
#all_house_data1$CouncilArea[all_house_data1$CouncilArea==""] <- "Unknown"
#str(all_house_data1)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

str(all_house_data1)

 
 
 for(var in c("Type","Method")){
   all_house_data2 = CreateDummies(all_house_data1,var,100)
 }

glimpse(all_house_data2)

all_house_data2 <- all_house_data2 %>% select(-Type)
View(all_house_data2)

#all_house_data2 = CreateDummies(all_house_data1, "Type",50)
#all_house_data3 = CreateDummies(all_house_data2, "Method",50)
#all_house_data3 = CreateDummies(all_house_data3, "CouncilArea",50)




glimpse(all_house_data2)

# detect and treat missing values
all_house_data3 <- all_house_data2[!((is.na(all_house_data2$Price)) & all_house_data2$data== "train"),]

all_house_data4 <- all_house_data3

for(col in names(all_house_data4)){
  if(sum(is.na(all_house_data4[,col])) > 0 & !(col %in% c("data", "Price"))){
    all_house_data4[is.na(all_house_data4[,col]), col] = median(all_house_data4[all_house_data4$data == "train",col],na.rm = T)
  }
  
}

lapply(all_house_data4, function(x) sum(is.na(x)))

str(all_house_data4)
summary(all_house_data4)

#separate the training data set from the test data set

train_house_data <- all_house_data4 %>% filter(data == "train") %>% select(-data) 
test_house_data <- all_house_data4 %>% filter(data == "test") %>% select(-data, -Price)

View(train_house_data)
View(test_house_data)
# both train and test data are free of missing values
lapply(train_house_data, function(x) sum(is.na(x)))
lapply(test_house_data, function(x) sum(is.na(x)))

lin_fit_train <- lm(Price ~.- Bedroom2,data = train_house_data)
summary(lin_fit_train)

sort(vif(lin_fit_train),decreasing = T)

ran_forest_fit_train = randomForest(Price ~.- Bedroom2,data = train_house_data)
varImpPlot(ran_forest_fit_train)
?predict()
predict_forest_test <- predict(ran_forest_fit_train,newdata = test_house_data,type = "class")

write.csv(predict_forest_test, "Hedgar_Ajakaiye_P1_part2.csv", row.names = F)



# TESTING! TESTING!! TESTING!!! PREPROCESSING
View(train_house_data)
eva_training_data <- train_house_data

# divide eva_training_data into mini_eva_train and mini_eva_test

set.seed(42)

training_index <- sample(1:nrow(eva_training_data), round(0.7*nrow(eva_training_data)))
mini_eva_train <- eva_training_data[training_index,]
mini_eva_test <- eva_training_data[-training_index,]

dim(mini_eva_train)
dim(mini_eva_test)
str(mini_eva_train)

# 1. Build LINEAR regression model:

lin_model <- lm(Price ~.-Bedroom2 -Method_S,data = mini_eva_train)
# testing and evaluating our models we would split the training data
summary(lin_model)

sort(vif(lin_model),decreasing = T)

# Predict on test data Linear
predict_test_mini <- predict(lin_model, newdata = mini_eva_test)
predict_train_mini <- predict(lin_model, newdata = mini_eva_train)

#MAPE & RMSE Evaluation of Linear Model

mini_mape_train <- mean(abs(predict_train_mini - mini_eva_train$Price)/mini_eva_train$Price)
accuracy_min_train <- 1 - mini_mape_train #69%

mini_mape_test <- mean(abs(predict_test_mini - mini_eva_test$Price)/mini_eva_test$Price)
mini_mape_train <- mean(abs(predict_train_mini - mini_eva_train$Price)/mini_eva_train$Price)

accuracy_mini_test <- 1 - mini_mape_test
accuracy_mini_train <- 1 - mini_mape_train


rmse_mini_test <- RMSE(predict_test_mini, mini_eva_test$Price)
rmse_mini_train <- RMSE(predict_train_mini, mini_eva_train$Price)

2124667/rmse_mini_test
# 2. Build Decision Tree Model 

?rpart
library(rpart)
library(rpart.plot)
tree_model <- rpart(Price ~.-Bedroom2 -Method_S,data = mini_eva_train, method = "anova")

rpart.plot(tree_model, cex = 0.8)
?rpart.plot()
printcp(tree_model)
plotcp(tree_model)
summary(tree_model)
tree_model$variable.importance # find out the ranking of variable

#check for possibility of overfitting
tree_predict_test <- predict(tree_model, newdata = mini_eva_test)
tree_predict_train <- predict(tree_model, newdata = mini_eva_train)

# carrout preprunning

#carryout post prunning
#?prune()
#tree_model_prune <- prune(tree_model, cp =0.010495)
#printcp(tree_model)
#rpart.plot(tree_model_prune, cex = 0.8)
#plotcp(tree_model)
#printcp(tree_model_prune)
#plotcp(tree_model_prune)

tree_mape_test <- mean(abs(tree_predict_test - mini_eva_test$Price)/mini_eva_test$Price)
tree_mape_train <- mean(abs(tree_predict_train - mini_eva_train$Price)/mini_eva_train$Price)

tree_accuracy_mape <- 1 - tree_mape_test # 68%
tree_accuracy_mape_train <- 1 - tree_mape_train

tree_rmse_test <- RMSE(tree_predict, mini_eva_test$Price) #480273.3


#3 model: Random forest model
library(randomForest)
?randomForest
rf_model <- randomForest(Price ~.-Bedroom2 -Method_S,data = mini_eva_train)


rf_predict <- predict(rf_model,newdata = mini_eva_test)
rf_predict_train <- predict(rf_model,newdata = mini_eva_train, type = "class")

#write.csv(rf_model, "Hedgar_Ajakaiye_P1_part2.csv", row.names = F)

 
 
 rf_mape_test <- mean(abs(rf_predict - mini_eva_test$Price)/mini_eva_test$Price)
 rf_mape_train <- mean(abs(rf_predict_train - mini_eva_train$Price)/mini_eva_train$Price)
 
 
 
 rf_accuracy_test <- 1 - rf_mape_test#79%
 rf_accuracy_train <- 1 - rf_mape_train
 
 
 dim(mini_eva_train)
 dim(mini_eva_test)
 length(rf_predict)
 
 varImpPlot(rf_model)
 
#mean(abs(rf_predict - mini_eva_train$Price)/mini_eva_train$Price)
 
rf_rmse_test <- RMSE(rf_predict, mini_eva_test$Price)

library(gbm)

# Gradient Boost Method GBM

gbm_model <- gbm(Price ~.-Bedroom2 -Method_S, 
                 data = mini_eva_train,
                 distribution = "gaussian",
                 n.trees = 2000,interaction.depth = 3)


predict_gbm_test <- predict(gbm_model, newdata = mini_eva_test, n.trees = 2000)
predict_gbm_train <- predict(gbm_model, newdata = mini_eva_train, n.trees = 1000)

gbm_mape_test <- mean(abs(predict_gbm_test - mini_eva_test$Price)/mini_eva_test$Price)
gbm_mape_train <- mean(abs(predict_gbm_train - mini_eva_train$Price)/mini_eva_train$Price)


 
accuracy_gbm_test <- 1 - gbm_mape_test

accuracy_gbm_train <- 1 - gbm_mape_train

 
 
 
 