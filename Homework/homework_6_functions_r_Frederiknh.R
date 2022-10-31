# CDS Homework 6: Practising function with Gapminder - Frederik Normann Holm
#installing and loading needed packages for the exercises
install.packages("gapminder")
library("gapminder")
library(dplyr)
gapminder
#Exercise 1 
#writing a defensive function that calculates the GDP of a given country in the gapminder dataset
calculateGDP <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
  dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  
  new <- cbind(dat, gdp=gdp)
  return(new)
}

calculateGDP(gapminder, country="Norway")

#using the same defensive function to calculate the GDP of Denmark in 1967, 1977, 1987, 1997, 2007, and 2017.
calculateGDP(gapminder, year=c(1967,1977,1987,1997,2007,2017), country="Denmark")

#Exercise 2
#creating the script that loops over every country in the dataset as well as checking for B's and life expectancy
#in order to get every country that starts with a B in the gapminder dataset i make use of the grep function
grep("^B", unique(gapminder$country), value = TRUE)

lowerThreshold <- 50
upperThreshold <- 70
Bcountries <- grep("^B", unique(gapminder$country), value = TRUE)

for(iCountry in unique(gapminder$country)){
  tmp <- mean(subset(gapminder, country==iCountry)$lifeExp)
  
  if(tmp < lowerThreshold){
    cat("Average Life Expectancy in", iCountry, "is less than", lowerThreshold, "\n")
  }
  else if(tmp > lowerThreshold && tmp < upperThreshold){
    cat("Average Life Expectancy in", iCountry, "is between", lowerThreshold, "and", upperThreshold, "\n")
  }
  else{
    cat("Average Life Expectancy in", iCountry, "is greater than", upperThreshold, "\n")
  }
  rm(tmp)
}
#for this exercise, i had trouble including the grep(^"B") function in my loop so i could never exclusively 
#print the countries that started with B and had life expectancies under 50, between 50 and 70, and over 70...


