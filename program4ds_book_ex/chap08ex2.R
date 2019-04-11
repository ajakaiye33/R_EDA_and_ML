#Exercise2: Using "*apply()" function
item <- as.list(runif(10))

roundy <- function(x){
  return(round(x,1))
}

lapply(item, roundy)

sentty <- 'The spate of killing in northern east must stop and as a matter of fact the waton killing of people at every slightest provocation should stop and every culprit caught should be made to face the harsh reality of the law'
slek <- strsplit(tolower( sentty),"")
vectorslek <- slek[[1]]

count_occur <- function(lety,v){
  return(sum(v== lety))
}
count_occur("e",vectorslek)
