##### Conditional Statements: If...Else #####

# What happens if number == 100?
number <- 100
if (number > 100) {
     print("greater than 100")
} else {
     print("less than 100")
}
print("Finished checking")


# We don't need to have an else statement
if (number > 100) {
     print("greater than 100")
}

# Comparison operators (from Day 1), return TRUE or FALSE
number == 100 # Equal to
number != 100 # Not equal to
# Also > < >= <=


# We could have more than one test in an if statement
# In this case, after the first match, none of the code afterwards is even run
# Check the sign of a number
number <- -3
if (number > 0) {
     print(1)
} else if (number < 0) {
     print(-1)
} else {
     print(0)
}



# Combine tests with logical operators
# And uses '&'
# Or uses '|'

number1 = -15
number2 = 40

# If number 1 is greater than or equal to zero AND number 2 is greater than or equal to zero ...
if (number1 >= 0 & number2 >= 0) {
     print("Both numbers are positive")
} else {
     print("Both number are negative")
}

##### For Loops: Repetitive tasks in R #####

# Remember, we can get one number out of a vector with []
numbers <- 1:10
numbers
numbers[4]


# First example of for loop in R
# number is the "loop variable", changes each time through the loop
for (number in numbers) {
     print(number)
}

# we often use "i" as the loop variable
for (i in 1:10) {
     print(i)
}

# The loop variable persists after the loop is done
letter <- "z"
print(letter)
for (letter in c("a", "b", "c")) {
     print(letter)
}

# Check what's in letter, should be c (last time through the loop), not z 
print(letter)


# Exercise: A loop to calculate the sum of a vector
numbers <- c(4, 8, 15, 16, 23, 42)
running_sum <- 0
for (number in numbers) {
     running_sum <- running_sum + number
     print(running_sum)    
}

# Print out the sum after the loop is finished calculating it
print(running_sum)

# Check our work using R's built-in sum function
sum(numbers)

# Load gapminder data
gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv")

# Take a look at what we got
str(gapminder)

# Loops can be useful for looking at data
for (row in 1:10) {
     print(gapminder$year[row])
}


# Write a script using for loops and if statements to loop through the first 10 rows of Gapminder.
# Tell us which years had a life expectancy of less than 35 years

# Take a look at the first few rows
head(gapminder)

# Reminder, we could get column year, row 2 this way
gapminder$year[2]

# For each of the first 10 rows:
# First test if life expectancy for that row < 35
# If it is, print out the year for that row
for (row in 1:10) {
     if (gapminder$lifeExp[row] < 35) {
          print(gapminder$year[row])
     }
}

##### Writing Functions #####

# A function to convert fahrenheit to kelvin
# Note, this code needs to be run first before it's called
# Functions are themselves variables so we need to store them to the environment to use them
fahr_to_kelvin <- function(temp) {
     kelvin <- ((temp - 32) * (5 / 9)) + 273.15
     return(kelvin)
}

# Run the function
fahr_to_kelvin(71)

# Do some tests with expected answers to make sure it works
fahr_to_kelvin(32)
fahr_to_kelvin(212)


# Variables only exist inside functions


# Another function to convert to C
celsius <- 10
kelvin_to_celsius <- function(temp) {
     celsius <- temp - 273.15
     return(celsius)
}

# Celsius is still 10.  
# The function made it's own variable that didn't change the original one
kelvin_to_celsius(0)
print(celsius)


# We can combine functions
fahr_to_celsius <- function(temp) {
     temp_k <- fahr_to_kelvin(temp)
     temp_c <- kelvin_to_celsius(temp_k)
     return(temp_c)
}

# Again, test for expected results
fahr_to_celsius(32)
fahr_to_celsius(212)

# Or nest them inside of each other
kelvin_to_celsius(fahr_to_kelvin(32))




# Write a function to convert a 
# temperature in C to F using the
# formula: F = C * 9 / 5 + 32

# Note: It's a good idea to document your functions
# Tell us what it does, what arguments it takes, and what it returns

# Converts a temperature in C to F
# Takes temperature as an argument
# Returns the converted temperature
celsius_to_fahr <- function(temp) {
     
     # Also a good idea to explain the formula you're using
     # F = C * 9 / 5 + 32
     fahr <- temp * 9 / 5 + 32
     return(fahr)
}

# Test with an expected result
celsius_to_fahr(100)


##### Subsetting and Reshaping Data #####

# using dplyr and tidyr

# load dplyr package
library(dplyr)

# Remind ourselves what's in gapminder
head(gapminder)

# The select() function subsets columns
year_country_gdp <- select(gapminder, year, country, gdpPercap)

# Take a look at the subsetted data
head(year_country_gdp)

# How to subset rows and columns at once
# Using pipes.  Instead of a '|', the pipe in R is a '%>%'

# Take the gapminder dataset and pipe it to filter
year_country_gdp_euro <- gapminder %>%

     # First filter to only include rows in europe and 2007
     filter(continent == "Europe", year == 2007) %>%

     # Send the result along to select and select a subset of columns
     # The final result gets saved all the way back at the beginning in year_country_gdp_euro
     select(year, country, gdpPercap)

# What'd we get
head(year_country_gdp_euro)

# Challenge: Filter gapminder to only include data from 2007 in Africa, and only keep 3 columns.  
# How many rows are left?
Africa_2007_lifeExp <- gapminder %>%
     filter(continent == "Africa", year == 2007) %>%
     select(year, country, lifeExp)

nrow(Africa_2007_lifeExp)


# Summarizing data using summarize()
mean_gdp <- gapminder %>%
     summarize(meanGDP = mean(gdpPercap))

mean_gdp

# Not so useful by itself, we could have just done the same thing this way
mean(gapminder$gdpPercap)

# summarize() is much more powerful when combined with group_by()
# Now we can get a mean for each continent separately
gdp_by_continent <- gapminder %>%
     group_by(continent) %>%
     summarize(meanGDP = mean(gdpPercap))

gdp_by_continent


head(gapminder)

# Let's add a new column using mutate.  
billion_gdp_country_2007 <- gapminder %>%
     filter(year == 2007) %>%
     mutate(billion_gdp = gdpPercap * pop / 10^9) %>%
     select(continent, country, billion_gdp)

head(billion_gdp_country_2007)


# dplyr works well for most data wrangling when we need to subset or summarize.  
# But often our data will be in wide format and need to be converted to long using tidyr()

# Wide:
# Genus    Weight    Height
# Ursus    122       88

# Long: 
# Genus    Measurement    Value
# Ursus    Weight         122
# Ursus    Height         88

# Load tidyr package
library(tidyr)

# Read in the gapminder datset in wide format
gap_wide <- read.csv("data/gapminder_wide.csv")

# Take a look at what wide format looks like for this
str(gap_wide)


# We can condense columns together using gather()
# First two arguments are the two new columns: 
#  - the one that will take the old column names
#  - the one that will take the old column values
gap_long <- gap_wide %>% 
     gather(obstype_year, obs_values, 
          starts_with("pop"), 
          starts_with("lifeExp"), 
          starts_with("gdpPerCap"))

# Let's see what that did
head(gap_long)
str(gap_long)

# We have observation type and year data in the same column.  
# Better to separate them using separate
gap_long <- gap_long %>%
     separate(obstype_year, 
              into = c("obs_type", "year"), 
              sep = "_")

# What'd we get?
head(gap_long)

# Long to wide (going the other way)
# Less likely to be needed in R, but still useful in some cases
# unite() first, this is the opposite of separate()
# spread() is the opposite of gather()


##### Making Graphs #####

# R's default plotting function isn't so great.  We'll use ggplot2 instead
plot(x = gapminder$gdpPercap, y = gapminder$lifeExp)

# load ggplot2 package
library(ggplot2)


# Make a scatterplot
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
     geom_point()

# Make scatterplot with blue points, and also add lines by country colored by continent
ggplot(data = gapminder, aes(x = year, y = lifeExp, 
          by = country)) +
     geom_line(aes(color = continent)) +
     geom_point(color = "blue")


# Make a scatterplot colored by continent with partially transparent smaller points (alpha)
# Also add trendlines with confidence intervals and set the x axis to be log scale
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, 
          color = continent)) + 
     geom_point(alpha = 0.5, size = 0.8) + 
     scale_x_log10() +
     geom_smooth(method = "lm")

