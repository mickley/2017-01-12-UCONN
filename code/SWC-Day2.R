number <- 100

# Conditional statement
if (number > 100) {
     print("greater than 100")
} else {
     print("less than 100")
}
print("Finished checking")

# We don't need an else
if (number > 100) {
     print("greater than 100")
}

number == 100

number != 100


# We could have more than one test
number <- -3

if (number > 0) {
     print(1)
} else if (number < 0) {
     print(-1)
} else {
     print(0)
}

# Combine tests with logical operators
number1 = -15
number2 = 40

if (number1 >= 0 & number2 >= 0) {
     print("Both numbers are positive")
} else {
     print("Both number are negative")
}

# Loops

numbers <- 1:10
numbers
numbers[4]

for (number in numbers) {
     print(number)
}

for (i in 1:10) {
     print(i)
}

# The loop variable persists
letter <- "z"
print(letter)
for (letter in c("a", "b", "c")) {
     print(letter)
}

# Check what's in letter
print(letter)


# A loop to calculate the sum of a vector
numbers <- c(4, 8, 15, 16, 23, 42)
running_sum <- 0

for (number in numbers) {
     running_sum <- running_sum + number
     print(running_sum)    
}

print(running_sum)
sum(numbers)

# load gapminder data
gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv")
str(gapminder)

for (row in 1:10) {
     print(gapminder$year[row])
}


# Write a script using for loops 
# and if statements
# to loop through the first 10 rows of
# Gapminder
# Tell us which years had a life expectancy
# of less than 35 years

head(gapminder)
gapminder$year[2]

for (row in 1:10) {
     if (gapminder$lifeExp[row] < 35) {
          print(gapminder$year[row])
     }
}

# Writing functions

# Run the code first
fahr_to_kelvin <- function(temp) {
     kelvin <- ((temp - 32) * (5 / 9)) + 273.15
     return(kelvin)
}

# Run the function
fahr_to_kelvin(71)

fahr_to_kelvin(32)
fahr_to_kelvin(212)


# Variables only exist inside functions
celsius <- 10

# Another function to convert to C
kelvin_to_celsius <- function(temp) {
     celsius <- temp - 273.15
     return(celsius)
}

kelvin_to_celsius(0)
print(celsius)


# We can combine functions
fahr_to_celsius <- function(temp) {
     temp_k <- fahr_to_kelvin(temp)
     temp_c <- kelvin_to_celsius(temp_k)
     return(temp_c)
}

fahr_to_celsius(32)
fahr_to_celsius(212)

kelvin_to_celsius(fahr_to_kelvin(32))


# Write a function to convert a 
# temperature in C to F using the
# formula: F = C * 9 / 5 + 32



# Converts a temperature in C to F
# Takes temperature as an argument
# Returns the converted temperature
celsius_to_fahr <- function(temp) {
     
     # F = C * 9 / 5 + 32
     fahr <- temp * 9 / 5 + 32
     return(fahr)
}

celsius_to_fahr(100)


# Subsetting and Reshaping data
# using dplyr and tidyr

# load dplyr
library(dplyr)

head(gapminder)

# Select subsets columns
year_country_gdp <- select(gapminder, year, country, gdpPercap)

head(year_country_gdp)

# How to subset rows and columns at once
# Using pipes
year_country_gdp_euro <- gapminder %>% 
     filter(continent == "Europe", year == 2007) %>%
     select(year, country, gdpPercap)

head(year_country_gdp_euro)

Africa_2007_lifeExp <- gapminder %>%
     filter(continent == "Africa", year == 2007) %>%
     select(year, country, lifeExp)

nrow(Africa_2007_lifeExp)


# Summarizing data
mean_gdp <- gapminder %>%
     summarize(meanGDP = mean(gdpPercap))

mean_gdp
mean(gapminder$gdpPercap)

# Grouping
gdp_by_continent <- gapminder %>%
     group_by(continent) %>%
     summarize(meanGDP = mean(gdpPercap))

gdp_by_continent

head(gapminder)

# Add a new column
billion_gdp_country_2007 <- gapminder %>%
     filter(year == 2007) %>%
     mutate(billion_gdp = gdpPercap * pop / 10^9) %>%
     select(continent, country, billion_gdp)

head(billion_gdp_country_2007)


# Load tidyr
library(tidyr)


head(gapminder)

gap_wide <- read.csv("data/gapminder_wide.csv")

str(gap_wide)

gap_long <- gap_wide %>% 
     gather(obstype_year, obs_values, 
          starts_with("pop"), 
          starts_with("lifeExp"), 
          starts_with("gdpPerCap"))

head(gap_long)
str(gap_long)

gap_long <- gap_long %>%
     separate(obstype_year, 
              into = c("obs_type", "year"), 
              sep = "_")

head(gap_long)

# Long to wide
# unite is the opposite of separate
# spread is the opposite of gather

# Plotting!!

plot(x = gapminder$gdpPercap, y = gapminder$lifeExp)

# Use ggplot
library(ggplot2)

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
     geom_point()

ggplot(data = gapminder, aes(x = year, y = lifeExp, 
          by = country)) +
     geom_line(aes(color = continent)) +
     geom_point(color = "blue")

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, 
          color = continent)) + 
     geom_point(alpha = 0.5, size = 0.8) + 
     scale_x_log10() +
     geom_smooth(method = "lm")



