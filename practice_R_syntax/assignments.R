# Use various utility functions to create vectors containing these vales
#####################1A#####################
v1 <- 1:10
2**v1
###############ex frm the book of R########################
#question A

the_vec <- c(seq(3,6,length.out = 5), rep(c(2,-5.1,-33),2), 7/42+2)

#question B: Extract the first and last elements of your vector from A, storing them as a new object
the_vec.len <- length(x=the_vec)
scd_obj <- the_vec[c(1,the_vec.len)]

#question C: store as third obj the values returned by omitting the first and last values of your vector from A
third_ob <- the_vec[-c(1,the_vec.len)]

#use only B and C to reconstruct A
scd.len <- length(x=scd_obj)
reconstructed_vec <- c(scd_obj[scd.len-1],third_ob[c(1:10)],scd_obj[scd.len])

#question E: overwrite A with the same values sorted from smallest to largest
the_vec[(1:12)] <- sort(reconstructed_vec,decreasing = T)
#question F: Use the colon operator as an index vector to reverse the order of E and confirm this is identical to using sort on E with decreasing=TRUE
the_vec[the_vec.len:1] == sort(the_vec,decreasing = T)

#question G: Create a vector from C that repeatd the third element of C three times, the sixth element four times and the last element once
third_ob.len <- length(x=third_ob)
rep(third_ob[third_ob.len],1)
c(rep(third_ob[3],3), rep(third_ob[6],4), rep(third_ob[third_ob.len],1))

#question H: Create a new vector a a copy of E by assigning E as is to a newly named object. Using this new copy of E, overwrite the first, the fifth to the seventh(inclusive), and the last element with the values 99 to 95(inclusive), respectively
copy_e <- the_vec
copy_e.len <- length(x=copy_e)
copy_e(1,5:7,12) <- c(99:95,99:95,99:95)



###############1B###########################

vector_1to_26 <- 1:26
aphabet_26 <- letters
aphabet_26
rev_aphabet <- rev(aphabet_26)
rev_aphabet
comby <- paste(rev_aphabet,vector_1to_26,sep="")
rev_as <- rev(comby)
rev_as
#######################2########################

addtress_list <- c(frst="802/hirannandani/Mumbai",
                   scd="2A/kalka-Delhi",
                   thd="345#near adyar#Chennai",
                   fot="10-shyaam bazzar-Kolkata")
for(i in addtress_list){
  for( t in i){
    spak <- strsplit(t,"/")
    socy <- strsplit(t,"-")
    bar <- strsplit(t,"#")
  
  print(socy)
}
?substr

first <- strsplit(addtress_list, "/")
first$frst[3]
second <- strsplit(addtress_list,"#")
second$thd[3]
third <- strsplit(addtress_list,"-")
third$scd[2]
third$fot[3]
solu <- c(first$frst[3],second$thd[3],third$scd[2],third$fot[3])
for(i in solu){
  print(i)
}
############################3########################################

primes <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47)
for(i in primes){
  for(t in 48:100){
    if(t%%2!=0){
        print(t)
    
    }
  }
}

for (i in 1:47){
  if(i>1 & i %%2!=0)
    print(i)
}
for (i in 48:100){
  if(i%%2!=0){
    print(i)
  }
}
############################# 5########################################
library(dplyr)
mtcars2 = mutate(mtcars, brand = rownames(mtcars))
mtcars2
mtcars2[mtcars2$am==0 & mtcars2$gear > 3  & mtcars2$mpg > mean(mtcars2$mpg),"brand"]
#mtcars2 %>% 
  #filter(am==0,gear> 3) %>%
  #summarise(mean_miles = mean(mpg,na.rm = TRUE)) %>%
  #filter(am==0,gear >3, mpg > mean_miles) %>%
  #select(brand)

set.seed(2)
x = sample(letters[1:5],50, replace=T)
y = sample(letters[1:3],50, replace=T)
yt <- table(y)
