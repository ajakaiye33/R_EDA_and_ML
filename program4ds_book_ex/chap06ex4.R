#Exercixe 4: functions and conditionals

is_twice_as_long <- function(stringy1,stringy2){
  if((nchar(stringy1) > nchar(stringy2)) & (nchar(stringy2) < nchar(stringy1))& (nchar(stringy1)>nchar(stringy2)*2)){
    res <-TRUE
  }else if((nchar(stringy2) > nchar(stringy1)) & (nchar(stringy1) < nchar(stringy2)) & (nchar(stringy2)> nchar(stringy1)*2)){
    res <- FALSE
  }else if(nchar(stringy1) == nchar(stringy2)){
    res <-"Equal length"
  }
  return(res)
}




is_twice_as_long("soccer is the most lucrative sport", "palying football")
is_twice_as_long("playing football", "soccer is the most lucrative sport")
is_twice_as_long("fool", "sool")




describe_difference <- function(stringy1,stringy2){
  diff_len <- nchar(stringy1) - nchar(stringy2)
  dif_len2 <- nchar(stringy2) - nchar(stringy1)
  if((nchar(stringy1) > nchar(stringy2)) & (nchar(stringy2) < nchar(stringy1))& (nchar(stringy1)>nchar(stringy2)*2)){
    res <-paste("Your first string is is longer by",diff_len)
  }else if((nchar(stringy2) > nchar(stringy1)) & (nchar(stringy1) < nchar(stringy2))){
    res <- paste("Your second string is longer by",dif_len2)
  }else if(nchar(stringy1) == nchar(stringy2)){
    res <-"Your string are the same lenghth"
  }
  return(res)
}


describe_difference("soccer is the most lucrative sport", "palying football")
describe_difference("playing football", "soccer is the most lucrative sport")
describe_difference("fool", "sool")
describe_difference("Buhari is dead", "Osinbajo takes over")



