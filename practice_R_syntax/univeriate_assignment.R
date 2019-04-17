getwd()
cens <- read.csv("census_income.csv", stringsAsFactors = FALSE)
View(cens)


#QUICK SUMMARY
#the  output was achieve with the below code
describe(cens)

#the output was achieve with the below code
cat_summary <- function(d){
  for(i in d){
    if(class(i) == 'character'){
      print(table(i))
    }
  }
  
}

cat_summary(cens)
tabl <- table(cens$education, cens$Y)

#SIMILAR CATEGORIES
# The output was achieved with the below code
round(prop.table(tabl, margin = 1),2)
#FINDING OUTLIERS
hist(cens$fnlwgt)
hist(cens$education.num)
#Catch outliers
#[q1-1.5IQR,q3 + 1.5IQR]
get_outlier <- function(x,t){
  q1 <- quantile(x)[2]
  q3 <- quantile(x)[4]
  iqtr <- IQR(x)
  frst <- q1-t*iqtr
  scd <- q3 + t*iqtr
  limit <- c(frst,scd)
  names(limit)=NULL
  return(limit)
}
d <- 1.5
get_outlier(cens$education.num,d)

