library(dplyr)
library(ggplot2)
View(diamonds)
colnames(diamonds)
dim(diamonds)
str(diamonds)
diamond_sample <- sample_n(diamonds,1000)

ggplot(data=diamond_sample, mapping = aes(x=carat, y= price)) + geom_point()

ggplot(data=diamond_sample,mapping = aes(x=carat, y= price, color=clarity)) + geom_point()
ggplot(data = diamonds, mapping = aes(x=carat, y=price, color=clarity)) + geom_point()

ggplot(data = diamond_sample,mapping = aes(x=carat, y= price)) + geom_point(color = "blue")

ggplot(data = diamond_sample, mapping = aes(x= carat, y = price, shape = cut)) + geom_point()

ggplot(data = diamond_sample, mapping = aes(x = cut, y = carat, size = price)) + geom_point()

ggplot(data = diamond_sample, mapping = aes(x = cut, y = carat, size = price, color = price)) + geom_point()

ggplot(data = diamond_sample, mapping = aes(x = carat, y = price, color= cut)) + geom_line()

ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut)) + geom_smooth()

ggplot(data = diamond_sample, mapping = aes(x = cut, y = price)) + geom_col()

ggplot(data = diamond_sample, mapping = aes(x = cut, y = price, fill = clarity)) + geom_col()

ggplot(data = diamond_sample, mapping = aes(x = carat, y = price, color= cut) ) + geom_point(alpha = 0.3) + geom_smooth(se = FALSE) 

ggplot(data = diamond_sample) +
  geom_point(mapping = aes(x = carat, y = price, color = cut), alpha = 0.3) +
  geom_smooth(mapping = aes(x = carat, y = price, color = cut), se = FALSE)

# Bonus
?sqrt
?stderr()
?stderr()
clarity_summary <- diamonds %>% 
  group_by(clarity) %>% 
  summarize(mean_price = mean(price), sd = sd(price), se = sd/sqrt(length(price))) 
  
clarity_summary
ggplot(data = clarity_summary) + geom_col(mapping = aes(x=clarity, y=mean_price, fill = se))

ggplot(data = clarity_summary, mapping = aes(x=clarity, y=mean_price )) + 
  geom_bar(mapping = aes(fill = clarity), stat = "identity") +
  geom_errorbar(mapping = aes(ymin = (mean_price - se), ymax =(mean_price + se)))