#Exercise: creating and accessing list

my_breakfast <- c("vegetable soup", "plantain","goatmeat")

my_lunch <- c("Beans", "Bread", "friedfish")

meals <- list(my_breakfast, my_lunch,dinner=c("Banana","vegetable soup"))

dinner <- meals$dinner

meals[[5]] <- meals[[2]]

early_meals <- list(meals[1],meals[2])

grand_meal <- list(break_fast=c("vegetablesoup","plantain"), lunch=c("Beans" ,"friedfish","goatmeat"))
lapply(grand_meal, length)

add_pizza <- function(mealz){
  mealz <- c(mealz, "pizza")
  return(mealz)
}
add_pizza('deez')
lapply(meals, add_pizza)