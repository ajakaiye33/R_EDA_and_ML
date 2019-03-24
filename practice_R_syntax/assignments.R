# Use various utility functions to create vectors containing these vales
#####################1A#####################
v1 <- 1:10
2**v1

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

