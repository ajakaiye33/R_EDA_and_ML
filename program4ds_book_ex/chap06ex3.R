#Excercise 3: writing and executing function

#define a function "add_three" that takes a single argument and returns a value
# 3 greater than the input
add_three <- function(val){
  return(val + 3)
}
add_three(3)

# create a variable "ten" that is the result of passing 7 to your "add_three"
ten <- add_three(7)

#define a function "imperial_to_metric that takes in two aguments: a number of feet
# and a number of inches. the function should return the equivalent length in meters
imperial_to_metric <- function(feet, inch){
  from_meter <- feet/ 3.28084
  from_inch <- inch/ 39.3701
  to_meter <- from_meter + from_inch
  return(to_meter)
  
}
#create a variable "height_in_meters" by passing height in 
#imperial to the "imperia_to_metric" function
height_meters <- imperial_to_metric(5,3)