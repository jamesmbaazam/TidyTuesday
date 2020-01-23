#packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sitools)


#The data
murders <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/international_murders.csv")

gun_murders <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/gun_murders.csv")

diseases <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

nyc_regents <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/nyc_regents.csv")


# a bit of wrangling of the gun murders data

#1. convert the per 100, 000 count rate back to the original counts
gm <- gun_murders %>% 
    mutate(total_murders = count*1E5) 


#format the country names
gm$country <- tolower(gm$country) #I have to do this before the next line, which works best with lower case
gm$country <- tools::toTitleCase(gm$country) #This capitalises the first letter of the country names.

options(scipen = 5) #this is to remove the default scientific notations used for labelling the plots


gun_murders_plot <- ggplot(data = gm, aes(x = reorder(country, -total_murders), y = total_murders)) + 
    geom_bar(stat = 'identity', width = 0.5, fill = 'darkred') + 
    scale_y_continuous(breaks = seq(0, max(gm$total_murders), 50000), labels = f2si) +
    labs(x = 'Country', y = 'Count', title = 'Total gun murders per 100,000 population') +
    theme_economist() +
    theme(axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12)) +
    coord_flip() 

plot(gun_murders_plot)


#Extract the measles-like diseases
measles_like_incidence_yearly <- diseases %>% 
    filter(disease == 'Measles'|disease ==  'Mumps'| disease == 'Rubella') 


measles_like_diseases_plot <- ggplot(data = measles_like_incidence_yearly, aes(x = year, y = count, fill = disease)) + 
    geom_bar(stat = 'identity', position = 'dodge', width = 0.65, color = 'black') + 
   # facet_wrap( ~ state) +
    theme_economist() +
    scale_x_continuous(breaks = seq(min(measles_like_incidence_yearly$year), max(measles_like_incidence_yearly$year), 2), 
                       labels = seq(min(measles_like_incidence_yearly$year), max(measles_like_incidence_yearly$year), 2)
                       ) +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(fill = 'Disease', x = 'Year', y = 'Number of cases') 

plot(measles_like_diseases_plot)
