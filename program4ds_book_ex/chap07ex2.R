#Exercise 2: Indexing and filtering vectors

first_ten <- 10:20

next_ten <- seq(21,30)

all_numbers <- c(first_ten,next_ten)

eleventh <- all_numbers[11]

some_numbers <- all_numbers[2:5]

even <- which(1:100%%2==0)

all(even,even%%2==0)

phone_numbers <- c(8,6,7,5,3,0,9)

prefix <- phone_numbers[1:3]

small <- phone_numbers[phone_numbers<=5]

large <- phone_numbers[phone_numbers>5]

replace(phone_numbers,large, 5)

odd <- which(phone_numbers%%2!=0)

replace(phone_numbers,odd,0)