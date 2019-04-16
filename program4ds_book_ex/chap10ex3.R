# Exercise 3: working with built-in data sets

# Load R's "USPersonalExpenditure" dataset using the `data()` function
# This will produce a data frame called `USPersonalExpenditure`
data("USPersonalExpenditure")
View(USPersonalExpenditure)
is.data.frame(USPersonalExpenditure)


# The variable `USPersonalExpenditure` is now accessible to you. Unfortunately,
# it's not a data frame (it's actually what is called a matrix)
# Test this using the `is.data.frame()` function
us_exp <- as.data.frame(USPersonalExpenditure)
View(USPersonalExpenditure)
is.data.frame(USPersonalExpenditure)

# Luckily, you can pass the USPersonalExpenditure variable as an argument to the
# `data.frame()` function to convert it a data farm. Do this, storing the
# result in a new variable
us_exp <- as.data.frame(USPersonalExpenditure)

# What are the column names of your dataframe?
colnames(us_exp) <- as.integer(colnames(us_exp))
View(us_exp)
colnames(us_exp)



## Consider: why are they so strange? Think about whether you could use a number 
## like 1940 with dollar notation!
#us_exp$1940
colnames(us_exp) <- paste0(rep("X",5),c(1940,1945,1950,1955,1960))
us_exp

# What are the row names of your dataframe?
rownames(us_exp)


# Add a column "category" to your data frame that contains the rownames
us_exp$category <- rownames(us_exp)
us_exp

# How much money was spent on personal care in 1940?
pers_care_1940 <-us_exp[us_exp$category=="Personal Care", "X1940"]



# How much money was spent on Food and Tobacco in 1960?
foo_tob_1960 <- us_exp[us_exp$category == "Food and Tobacco", "X1960"]


# What was the highest expenditure category in 1960?
max_1960 <- max(us_exp$X1960)
us_exp$category[us_exp$X1960==max_1960]
library(dplyr)
highest_exp_cat <- us_exp %>% 
  summarize(max_60 <- max(X1960)) %>% 
  filter(X1960==max_60) %>%
  select(category)
highest_exp_cat

us_exp%>% filter(category=="Food and Tobacco") %>% select(X1940)



# Define a function `lowest_category` that takes in a year as a parameter, and
# returns the lowest spending category of that year
lowest_cate <- function(yr){
  min_esp <- min(us_exp$yr)
  return(us_exp[us_exp$yr ==min_esp,"category"])
}
lowest_cate(X1940)
min(us_exp$X1940)


# Using your function, determine the lowest spending category of each year
# Hint: use the `sapply()` function to apply your function to a vector of years
sapply(us_exp$c(X1940, X1955,X1960), lowest_cate)