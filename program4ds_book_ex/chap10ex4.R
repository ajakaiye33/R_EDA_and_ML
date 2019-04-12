#Exercise 4: external data sets: Gate foundation Educational Grants

grants <- read.csv("/Users/ajakaiye/Documents/edvancer/book-exercises/chapter-10-exercises/exercise-4/data/gates_money.csv", sep = ",",stringsAsFactors = FALSE)
View(grants)
str(grants)

organization <- grants$organization
is.vector(organization)
sum(is.na(grants))

mean_value <- mean(grants$total_amount,na.rm = TRUE) #2600197

largest_grant <-max(grants$total_amount) #100000000

smallest <- min(grants$total_amount) #5000

org_with_large_grant <- grants[grants$total_amount==largest_grant, "organization"]

org_sm_grant <- grants[grants$total_amount==smallest,"organization"]

num_grant_2017 <- length(grants$total_amount[grants$start_year==2010])
num_grant_2017 <- nrow(grants[grants$start_year==2010,])

