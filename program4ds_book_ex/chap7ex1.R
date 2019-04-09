#Exercise 1: creating and operaing on vectors

names <- c("Hedgar", "Austine", "Doris")
print(names)

n <- 10:49
length(n)

print(n + 1)

m <- seq(10,1)

n - m

#Use the "seq" function to produce a range od numbers 
#from -5 to 10 in 0.1 increments
x_range <- seq(-5,10,by=0.1)


#create a vector "sinewave" by calling "sin()" function on each element in x_range
sine_wave <- sin(x_range)

# create a vector "cos_wave" by calling the "cos()" function on each element in x_range
cos_wave <- cos(x_range)

#Create a vector "wave" by multiplying "sin_wave" ans "cos_wave" together, then
#adding sin_wave to the product
wave <-  sine_wave * cos_wave
wave <- wave + sine_wave

#Use  the "plot()" function to plot your "wave"
plot(wave)
