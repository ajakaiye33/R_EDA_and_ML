#Excersice 2: using built-in string functions

lyric <- "I like to eat apples and bananas"

#extract the 1st through 13th letters in lyric
intro <- substr(lyric,1,13)

#exraxt the 15th through the last letter from lyric
fruits <- substr(lyric, 15,nchar(lyric))

#substitute all the "a"s in fruits with "ee"
fruits_e <- gsub("a", "ee", fruits)

#substitute all the "a"s with "o" in fruits
fruits_o <- gsub("a","o", fruits)

# combined "intro" with "fruit_e" ending
lyric_e <- paste(intro, substr(fruits_e,14,23))

#print out intro combined with the new "fruit_o"
print(paste(intro, fruits_o))




