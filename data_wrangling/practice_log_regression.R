
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

#check the structure

str(logreg_data2)

chisq.test(logreg_data2$status, as.character(logreg_data2$Revenue.Grid)) # not significant






