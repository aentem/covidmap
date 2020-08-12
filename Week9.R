################################################################################
###
### Data analysis in R: Week 9
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
### August 12, 2020
###
################################################################################

## Research question: What is the percent increase in Covid-19 cases across states
## in the US since July 1st?

# Load packages
#install.packages("choroplethrMaps") 
#install.packages("lubridate") 

library(tidyverse)
library(choroplethrMaps)
library(cowplot)
library(lubridate)
library(mapproj)

# Load data

data("state.map") # "state.map" comes from the choroplethrMaps package
states_50 <- state.map
states_48 <- map_data('state')

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
str(covid)

# Manipulate the covid data frame to include a "percent increase" variable for the month of July

covid_increase <- covid %>% 
  mutate(date = as.Date(date), month = month(date), day = mday(date), state = tolower(state)) %>% 
  filter(month == 7, day == 1 | day == 21) %>% 
  rename(region = state) %>% 
  select(region, day, cases) %>% # take only what need or spread will occur for all unique factors
  pivot_wider(names_from = day, values_from = cases) %>% 
  rename(July1 = "1", July21 = "21") %>% 
  mutate(pct_increase = 100*(July21 - July1)/(July1),
         pct_increase_dct = case_when(pct_increase < 20 ~ 0, 
                                      pct_increase < 40 ~ 1, 
                                      pct_increase < 60 ~ 2,
                                      pct_increase < 80 ~ 3,
                                      pct_increase < 100 ~ 4,
                                      pct_increase < 120 ~ 5, 
                                      pct_increase < 140 ~ 6,
                                      pct_increase < 160 ~ 7,
                                      pct_increase < 180 ~ 8,
                                      pct_increase < 200 ~ 9,
                                      pct_increase < 220 ~ 10,
                                      pct_increase < 240 ~ 11,
                                      pct_increase < 260 ~ 12), 
         pct_increase_dct = as.factor(pct_increase_dct))

# Join covid_increase the map data of the 48 or 50 states
  
map_data <- states_48 %>% 
  left_join(covid_increase, by = "region")

map_data_50 <- states_50 %>% 
  left_join(covid_increase, by = "region")

map_data_hi <- states_50 %>%
  filter(region == "hawaii") %>%
  inner_join(covid_increase, by="region")

map_data_ak <- states_50 %>%
  filter(region == "alaska") %>%
  inner_join(covid_increase, by="region")

#Code more generally is:
#left_join(x, y, by = c("left_key" = "right_key"))
  
# Create yor map! Fill the color of each state according to its percent
# increase in cases.
usa <- map_data("usa")

ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pct_increase), color = "black") +
  scale_fill_continuous(type = "gradient", low = "green", high = "red") +
  coord_map("polyconic") +
  geom_polygon(data = usa, color = "black", fill = NA, size = 0.5) +
  theme_bw() +
  labs(fill = "% increase", title = "Percent increase in Covid cases (July 1 to July 21)", 
        x = "Longitude", y = "Latitude")

map_data %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pct_increase), color = "black") +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red") +
  coord_map("polyconic") +
  geom_polygon(data = usa, color = "black", fill = NA, size = 0.5) +
  theme_bw() +
  labs(fill = "% increase", title = "Percent increase in Covid cases (July 1 to July 21)", 
       x = "Longitude", y = "Latitude")

plot2 <- map_data_50 %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = pct_increase), color = "black") +
  coord_fixed(ratio = 1.3) + 
  coord_map("polyconic")

# Inset with HI and Alaska

plot48 <- ggplot() + 
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = pct_increase)) + 
  coord_fixed(1.3) +
  scale_fill_gradient(low = "navy", high = "red") +
  theme_nothing() +
  theme(legend.position = "right", plot.title = element_text()) +
  labs(fill = "% increase", title = "Percent increase in July")

plotHI <- ggplot() + 
  geom_polygon(data = map_data_hi, aes(x = long, y = lat, group = group, fill = pct_increase)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "navy", high = "red", 
                      limits = c(min(covid_increase$pct_increase), 
                                 max(covid_increase$pct_increase))) +
  theme_nothing()

plotAK <- ggplot() + 
  geom_polygon(data = map_data_ak, aes(x = long, y = lat, group = group, fill = pct_increase)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "navy", high = "red", 
                      limits = c(min(covid_increase$pct_increase), 
                                 max(covid_increase$pct_increase))) +
  theme_nothing()

# Setting the min/max number for scale_fill_gradient is important so that the colors of HI/AK
# are put on the same scale as the full data frame. You will notice that if you omit these lines,
# those states will be colored incorrectly since the color scale includes only one value/color.

# Now we can use ggdraw to combine all three plots!

ggdraw() + 
  draw_plot(plot48) + 
  draw_plot(plotHI, x = 0, y = 0.02, width = 0.25, height = 0.25) + 
  draw_plot(plotAK, x = 0.27, y = 0.02, width = 0.25, height = 0.25)
