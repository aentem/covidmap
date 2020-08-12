################################################################################
###
### Data analysis in R: Week 9
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

## Research question: What is the percent increase in Covid-19 cases across states
## in the US since July 1st?

# Load packages

library(tidyverse)
library(choroplethrMaps)
library(cowplot)

# Load data

data("state.map")
states_50 <- state.map
states_48 <- map_data('state')

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Manipulate the covid data frame to make the following changes:
# 1. Convert "date" to a date formate instead of a character string
# 2. Filter to only include observations from July 1st and yesterday
# 3. Rename "state" as "region" in order to join it with the map data
# 4. Calculate the percent increase in cases for each state. Hint:
# you will first need to use "spread" to do this.
# 5. You can choose to leave the "percen increase" variable continuous,
# or make it a factor with discrete levels. If you make it a factor,
# it is up to you how you bin the values. Hint: use "case_when."

covid_increase <- covid %>%
  

# Join covid_increase the map data of the 48 or 50 states
  
map_data <- 

# Create yor map! Fill the color of each state according to its percent
# increase in cases.

