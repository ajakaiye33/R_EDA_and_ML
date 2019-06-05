
install.packages("ggplot2")
library(ggplot2)
getwd()
cen_data <- read.csv("census_income.csv",stringsAsFactors = FALSE)
View(cen_data)
install.packages("vcd")
library(vcd)
library(hflights)
?vcd

#Does your data follow normal distribution
#above question and associated graaphical answer/result was solved with the following code
ggplot(data=cen_data,aes(x=hours.per.week)) + geom_density(color="red") +
  stat_function(fun=dnorm,
                args=list(mean=mean(cen_data$hours.per.week),sd=sd(cen_data$hours.per.week)),
                color="green")

#correlation between two categorical variable


ggplot(data=cen_data, aes(x=race,fill= Y)) + geom_bar(position = "fill") + ylab("Percentage")
##########################################

install.packages("gapminder")
library(gapminder)
library(dplyr)
View(gapminder)
gap_2007 <- gapminder %>% filter(year==2007)
head(gap_2007)
ggplot(data= gap_2007, aes(x= gdpPercap, y=lifeExp)) + geom_point()

ggplot(data=gap_2007,aes(x=continent)) + geom_bar()

gap_2007_med <- gap_2007 %>% group_by(continent) %>% summarize(pop = mean(pop))
ggplot(data=gap_2007_med, aes(x=continent, y= pop)) + geom_bar(stat = "identity")
