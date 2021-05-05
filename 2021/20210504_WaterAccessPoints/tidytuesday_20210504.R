# Water access points
# TidyTuesday 2021 Week 19
# Author: Nozomi Nakajima
# Date: May 4, 2021


# Set working directory ----------

setwd("~/Documents/GitHub/TidyTuesday/2021/20210504_WaterAccessPoints")


# Load libraries -----------------

library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(gganimate)

sessionInfo()

# Load data ----------------------

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
w2hr <- map_data("world") # world map


# Data exploration ---------------

  # water source frequency (addmargins to show total)
  data.frame(table(water$water_source) %>% addmargins()) 
  
  # facility type frequency 
  data.frame(table(water$facility_type) %>% addmargins()) 

  # check years of installation
  data.frame(table(water$install_year)) 
  
# Data wrangling  ---------------
  
  cleanwater <- water %>% 
    drop_na(c(facility_type, install_year)) %>% # drop if missing facility type, year 
    filter(install_year>1900, install_year<2021, # filter to years in this range
           facility_type == "Improved" | facility_type == "Unimproved") # filter to only improved vs. unimproved
  
  # check that data looks correct
  data.frame(table(cleanwater$install_year)) 
  data.frame(table(cleanwater$facility_type) %>% addmargins()) 
  
  
# Figure ------------------------

  # plot map of world
  plot <- ggplot() +
    geom_polygon(data=w2hr, 
                 aes(x=long, y=lat, group=group), 
                 fill="grey30", color="grey", size=0.1) +
  # add points of improved / unimproved water points  
    geom_point(cleanwater, 
               mapping=aes(x=lon_deg, y=lat_deg,
                             color=facility_type,
                             group=as.factor(install_year)), 
                             # add a group so the points don't animate from each other
               shape=19, alpha=1) + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    coord_fixed(1.3) + theme_minimal() + 
    labs(color="Type of water facility", 
         caption="Data from Water Point Data Exchange | Plot by @nozominaka for #TidyTuesday",
         x=NULL, y=NULL) 

  # ggsave("WaterAccessPoints_plot.png", bg = "transparent", width = 10, height = 6, dpi = 400) 
  
  # animate using installation year of water point
  plotanimate <- plot +
    transition_manual(frames = install_year, cumulative = TRUE)+
    ggtitle("Access to improved water source by year: {current_frame}")  
  
  # render animation
  animate(plot = plotanimate, 
          nframes = length(unique(cleanwater$install_year)), 
          fps = 10, end_pause = 5, height = 380, width = 600)
  
  
# Saving -------------------------
  
anim_save("WaterAccessPoints_plot.gif")  
  