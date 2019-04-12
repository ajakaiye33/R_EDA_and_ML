#Exercise2: woring with datafrmaes
#Create a vector of 100 employees ("Employee 1", "Employee 2", ... "Employee 100")
# Hint: use the `paste()` function and vector recycling to add a number to the word
# "Employee"

employe <- paste(1:100,rep("Employee", 100))
paste("Employee", 1:100)
paste(rep("Employee", 100), 1:100)

# Create a vector of 100 random salaries for the year 2017
# Use the `runif()` function to pick random numbers between 40000 and 50000

salary2017 <- runif(100, min=40000, max= 50000)

# Create a vector of 100 annual salary adjustments between -5000 and 10000.
# (A negative number represents a salary decrease due to corporate greed)
# Again use the `runif()` function to pick 100 random numbers in that range.

salary_adj <- runif(100, min = -5000, max = 10000)

# Create a data frame `salaries` by combining the 3 vectors you just made
# Remember to set `stringsAsFactors=FALSE`!

salaries <- data.frame(Employeeid=employe,salary2017, salary_adj, stringsAsFactors = FALSE)


# Add a column to the `salaries` data frame that represents each person's
# salary in 2018 (e.g., with the salary adjustment added in).

salaries$salary2018 <- salaries$salary2017 + salaries$salary_adj
salaries

# Add a column to the `salaries` data frame that has a value of `TRUE` if the 
# person got a raise (their salary went up)

salaries$Got_a_raise <-  salaries$salary2018 > salaries$salary2017
salaries

### Retrieve values from your data frame to answer the following questions
### Note that you should get the value as specific as possible (e.g., a single
### cell rather than the whole row!)

# What was the 2018 salary of Employee 57
salaries[salaries$Employeeid=="57 Employee", "salary2018"]
#using Dplyr
library(dplyr)
salaries %>% filter(Employeeid=="57 Employee") %>% select("salary2018")

# How many employees got a raise?
received_raise <- nrow(salaries[salaries$Got_a_raise==TRUE,])
#using Dplyr ? Find out the equivalent of length in Dplyr
salaries %>% filter(Got_a_raise==TRUE) %>% select("Employeeid")

# What was the dollar value of the highest raise?
highest_raise <- max(salaries[salaries$Got_a_raise ==TRUE , "salary_adj"])
#using Dplyr*
View(salaries)
salaries %>% filter(Got_a_raise==TRUE) %>% select("salary_adj") %>% arrange(desc(salary_adj))

# What was the "name" of the employee who received the highest raise?
salaries[salaries$Got_a_raise==TRUE & salary_adj==highest_raise,"Employeeid"]
#using Dplyr
salaries %>% filter(Got_a_raise==TRUE & salary_adj==highest_raise) %>% select("Employeeid")

# What was the largest decrease in salaries between the two years?
largest_decrease <- min(salaries[salaries$Got_a_raise==FALSE, "salary_adj"])
#using Dplyr
salaries %>% filter(Got_a_raise==FALSE) %>% select("salary_adj") %>% arrange(salary_adj)

# What was the name of the employee who recieved largest decrease in salary?
salaries[salaries$Got_a_raise == FALSE & salary_adj==largest_decrease,"Employeeid"]
#Using Dplyr
salaries %>% filter(Got_a_raise==FALSE, salary_adj==largest_decrease) %>% select("Employeeid")

# What was the average salary change?
averagechange <- mean(salaries$salary_adj)

# For people who did not get a raise, how much money did they lose on average?
mean(salaries[salaries$Got_a_raise== TRUE, "salary_adj"])
#using Dplyr
salaries %>% filter(Got_a_raise==TRUE) %>% arrange(salary_adj)



