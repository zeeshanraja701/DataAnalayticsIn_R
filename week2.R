
#Write a function that takes a single vector as an argument and returns the minimum and maximum values in
#an appropriate format
minMax <- function(x){
  minimum <- min(x)
  maxmimum <- max(x)
  list(minimum=minimum, maxmimum=maxmimum)
  
}
minMax(c(2,3,4,5,5,6,7,8,9))

#Think about the function is_odd() that we looked at in activity 2.5. How might we edit this function to
#create a new function called is_even() that returns TRUE if the value passed in is even and FALSE if the
#value passed in is odd

isEven <- function(n){
  value <- n%%2
  value <- as.logical(value)
  value <- !value
  return(value)
}
isEven(2)
isEven(3)
isEven(4)
isEven(5)

#A temperature in Fahrenheit (F) can be calculated from a temperature in celsius (C) via the following
#equation:

#a) Write a function that takes a vector of temperatures in celsius and converts to farenheit.

CelToFar <- function(cel){
   Far <- 9*cel/5+32
   return(Far)
 }
CelToFar(0)

#b) Write a function that takes a vector of temperatures in farenheit and converts to celsius.
 
FarToCel <- function(far){
  Cel <- 5*(far-32)/9
  return(Cel)

}
FarToCel(32)


#skewness of a variable can be calculated

Skewness <- function(x){
  n <-length(x)
  x_bar <- mean(x)
  s <- sd(x)
  Skew <- ((1/n-2)*sum(x-x_bar)^3)/s^3
  return(Skew)
  
}
x <- c(0.2128, 0.0301, 0.0922, 0.0838, 0.0831, 0.1962, 0.1001, 0.0809, 0.0678, 0.0431,
       0.0415, 0.1246, 0.4109, 0.0489, 0.0232, 0.0382, 0.0534, 0.0896, 0.0143, 0.0314)

Skewness(x)


triangle<- function(a,b,c){
  s <- 0.5*(a+b+c)
  area <- sqrt(s*(s-a)*(s-b)*(s-c))
  return(area)
}
triangle(1,2,3)

