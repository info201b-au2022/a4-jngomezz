library(tidyverse)
library(ggplot2)
library(dplyr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_trends <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# # Return a simple string
# test_query1 <- function() {
#   return ("Hello world")
# }
# 
# # Return a vector of numbers
# test_query2 <- function(num=6) {
#   v <- seq(1:num)
#   return(v)
# }

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# INCARCERATION TRENDS
## state with highest prison population
max_prison_pop <- incarceration_trends %>%
  filter(total_pop == max(total_pop, na.rm = T)) %>%
  select(total_pop, state)

## state with lowest prison population 
lowest_prison_pop <- incarceration_trends %>%
  filter(total_pop == min(total_pop, na.rm = T)) %>%
  select(total_pop, state)

## mean prison population
mean_prison_pop <- incarceration_trends %>%
  summarise(mean_prison_pop = mean(total_pop, na.rm =TRUE))

## mean Black population in jail 
mean_black_pop <- incarceration_trends %>%
  summarise(mean_black_pop = mean(black_jail_pop, na.rm=TRUE))

## mean Latinx population in jail
mean_latinx_pop <- incarceration_trends %>%
  summarise(mean_latinx_pop = mean(latinx_jail_pop, na.rm=TRUE))

## mean White population in jail
mean_white_pop <- incarceration_trends %>%
  summarise(mean_white_pop = mean(white_jail_pop, na.rm=TRUE))

## race population with highest mean in jail between Black, Latinx, and White population
highest_race_pop <- max(mean_white_pop, mean_latinx_pop, mean_black_pop)
View(highest_race_pop)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>

#----------------------------------------------------------------------------#

# filter data 1970-2018
bar_chart <- incarceration_trends %>%
  filter(year > "1970" & year < "2018")

# grouping year and total population
bar_chart <- incarceration_trends %>%
  group_by(year) %>%
  summarise(total_pop = n())

# plotting function
plot <- 
  ggplot(bar_chart, aes(x=year, y=total_pop)) +
  labs(title="Total Jail Population by Year", x="Year", y="Jail Population") +
  geom_bar(stat = "identity") 
plot

# get_year_jail_pop <- function() {
#   bar_chart <- sum(incarceration_trends$total_pop)
# return()   
# }
# 
# plot_data <- cbind(total_pop, year)
# # This function ... <todo:  update comment>
# plot_jail_pop_for_us <- function()  {
#   plot_data <- plot_data %>%
#     select(-year)
#   return()   
# } 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# gathering data 
line_chart <- incarceration_trends %>%
  select(state, total_pop)

# creating dataframe
states <- as.data.frame(line_chart)

# function
get_jail_pop_by_states(states) <- function() {
  as.data.frame(line_chart)
}
 
# plotting 
plot_bar <- ggplot(states, aes(x=states, y=total_pop)) +
  geom_line(stat="identity")
plot_bar

# plot_jail_pop_by_states(states) <- function() {
#  
# }

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# gatherng data 
mean_data <- c("black_pop" = mean_black_pop, "latinx_pop" = mean_latinx_pop, "white_pop" = mean_white_pop)

year <- incarceration_trends %>%
  select(year)
df <- as.data.frame(y=pop_data, x=year)
View(df)

#plotting data 
plot_line <- ggplot(df, aes(x=year)) + 
  geom_line(aes(y = pop_data), color = "darkred") + 
  geom_line(aes(y = pop_data), color="steelblue", linetype="twodash")
plot_line

# 
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


## Load data frame ---- 


