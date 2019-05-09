
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


#from the above, only gender categorical variable has a p-value less than 5%

# detect and treat outliers

summary(logreg_data2)
