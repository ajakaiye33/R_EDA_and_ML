#ex1: calling built-in functions
my_name <- "John"
name_length <- nchar(my_name)
print(name_length)


now_doing <- paste(my_name, "is programming")
toupper(now_doing) 

#bonus
fav1 <-  6
fav2 <- 9

#divided by the square root of 201 and reassigned to same variable
fav1 <- fav1/201^(1/2)
fav2 <-  fav2/201^(1/2)

#sum of two variable
raw_sums <- sum(fav1,fav2)

#rounded raw_sum to 1 decimal places
round_sum <- round(raw_sums, 1)

#rounded to 1 decimal places
round_1 <- round(fav1,1)
round_2 <- round(fav2,1)

#sum of rounded values
sum_round <- sum(round_1,round_2)

# which is bigger?
max(sum_round, round_sum)
