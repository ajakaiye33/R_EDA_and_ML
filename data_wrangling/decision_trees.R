# Load necessary packages
library(dplyr)
library(caret)
library(car)

# Theory:
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
all_house_data1 <- all_house_data %>% select(-SellerG, -Address,-Postcode,-CouncilArea,-Suburb)

str(all_house_data1)
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



all_house_data2 = CreateDummies(all_house_data1, "Type",0)
all_house_data3 = CreateDummies(all_house_data2, "Method",0)


glimpse(all_house_data3)

# detect and treat missing values
all_house_data4 <- all_house_data3[!((is.na(all_house_data3$Price)) & all_house_data3$data== "train"),]

all_house_data5 <- all_house_data4

for(col in names(all_house_data5)){
  if(sum(is.na(all_house_data5[,col])) > 0 & !(col %in% c("data", "Price"))){
    all_house_data5[is.na(all_house_data5[,col]), col] = mean(all_house_data5[all_house_data5$data == "train",col],na.rm = T)
  }
  
}

lapply(all_house_data5, function(x) sum(is.na(x)))

#separate the training data set from the test data set

train_house_data <- all_house_data5 %>% filter(data == "train") %>% select(-data) 
test_house_data <- all_house_data5 %>% filter(data == "test") %>% select(-data, -Price)

View(train_house_data)
# both train and test data are free of missing values
lapply(train_house_data, function(x) sum(is.na(x)))
lapply(test_house_data, function(x) sum(is.na(x)))

# Check the VIF of both. For VIF to work training must first be fit into a linear regression
linregfit <- lm(Price ~.-Method_S -Method_VB -Bedroom2, data = train_house_data)
summary(linregfit)

sort(vif(linregfit),decreasing = T)

# Build Decision Tree 
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree_model <- rpart(Price ~.-Method_S -Method_VB -Bedroom2, data = train_house_data)

rpart.plot(tree_model)
?rpart.plot()
summary(tree_model)
tree_model$variable.importance # find out the ranking of variable

#check for possibility of overfitting
test_pred <- predict(tree_model, test_house_data)
test_house_data

# do confusion matrix

# carrout preprunning

#carryout post prunning
printcp(tree_model)
plotcp(tree_model)

#in our data, there is no issue of overfittng. if the xerro values had been fluctuating 
#and the plotcp curve depicts growth( becomming u in shape) then overfitting could have been said
# have taken place and would cut or prun accordingly

# where there is overfitting and there is need for prunning(post prunning) we would run the code:

#tree_model_prune <- prune(tree_model,cp = related CP value)
# the lower value of xerro before there was an unexpected increase, the related CP(complexity parameter) value would be used as the parameter 
#value of the cp
# the value of the prune version is used for prediction for the training and testing data


