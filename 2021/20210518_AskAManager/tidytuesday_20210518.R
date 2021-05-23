# load libraries
library(tidyverse)
library(scales)
library(lubridate)

# set theme
theme_set(theme_minimal())

# set working directory
setwd("~/Documents/GitHub/TidyTuesday/2021/20210518_AskAManager")

# load data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# data cleaning
salary_usd <- survey %>%
  mutate(experience_field = str_replace(years_of_experience_in_field, " - ", "-"),
         experience_field = fct_reorder(experience_field, parse_number(experience_field)),
         experience_field = fct_collapse(experience_field, "31 years or more" = c("31-40 years","41 years or more")),
         education = fct_relevel(highest_level_of_education_completed, 
                                 "High School", "Some college", "College degree", "Master's degree", "Professional degree (MD, JD, etc.)", "PhD"),
         education = recode(education, "High School" = "High school"),
         gender = fct_collapse(coalesce(gender, "Other or prefer not to answer"), "Other or prefer not to answer" = c("Other or prefer not to answer", "Prefer not to answer")),
         race = fct_lump(coalesce(race, "Other"), 4)) %>%
  filter(currency == "USD") %>%
  filter(annual_salary >= 5000,
         annual_salary <= 2e6) %>%
  filter(!is.na(education)) %>%
  filter(gender == "Man" | gender == "Woman")


# what is the difference in median salary?
salary_usd %>%
  group_by(gender) %>%
  summarize(median_salary = median(annual_salary)) 

# what is the difference in median salary by education level?
salary_usd_median <- salary_usd %>%
  group_by(education, gender) %>%
  summarize(median_salary = median(annual_salary)) %>%
  pivot_wider(names_from = gender, values_from = median_salary) %>%
  mutate(Gap = Woman/Man)

salary_usd_median

# select font for plot
library(showtext)
font_add_google(name = "Cinzel", family = "cinzel")
font_add_google(name = "Montserrat", family = "montserrat")
## find google fonts: https://fonts.google.com/
showtext_auto()
## turn on showtext to load the new font


# plot
library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(ggtext) # color words in title/subtitle

ggplot(data = salary_usd_median,
       aes(x = Man, xend = Woman, y = education)) +
  geom_dumbbell(size = 1.2, size_x = 4, size_xend = 4, 
                color="#b2b2b2", colour_x = "steelblue1", colour_xend ="hotpink1") +
  scale_x_continuous(labels = dollar_format()) + 
  labs(x = "\n Median annual salary (USD) \n",
       y = "Highest level of education \n",
       title = "Gender Wage Gap",
       subtitle = "<br> <span style='color:hotpink1;'>**Women**</span> earn less than <span style='color:steelblue1;'>**men**</span> in the *Ask A Manager* survey. <br> The gender wage gap is largest among high school graduates, <br> with women earning only 56.5% relative to men.</span>",
       caption = "Data: Ask A Manager Survey | Plot: @nozominaka") +
  # add label for "Male"
  #geom_text(data = filter(salary_usd_median, education == "PhD"),
  #            aes(x = Man, y = education, label = "Men"),
  #            color="steelblue1", size = 10, vjust = -2, fontface = "bold", family = "montserrat") +
  # add label for "Female"
  #geom_text(data = filter(salary_usd_median, education == "PhD"),
  #          aes(x = Woman, y = education, label = "Women"),
  #          color="hotpink1", size = 10, vjust = -2, fontface = "bold", family = "montserrat") +  
  # add labels for $ amounts below
  geom_text(data = salary_usd_median, aes(x = Man, y = education, label = scales::comma(Man)),
            color = "steelblue1", vjust = 2.5, size = 6, family = "montserrat") +
  geom_text(data = salary_usd_median, aes(x = Woman, y = education, label = scales::comma(Woman)),
            color = "hotpink1", vjust = 2.5, size = 6, family = "montserrat") +
  theme(plot.title = element_text(family = "cinzel", size = 80, color = "black", hjust = 0.2, 
                                  margin=margin(t=10, r=0, b=10, l=0)),
        plot.subtitle = element_markdown(family = "montserrat", size = 20, color = "black", hjust = 0.4, lineheight = 1.1),
        plot.caption = element_text(family = "montserrat", size = 14, color = "black"),
        axis.title.x = element_text(family = "montserrat", size = 22, color = "black"),
        axis.text.x = element_text(family = "montserrat", size = 18, color = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(family = "montserrat", size = 22, color = "black"),
        axis.text.y = element_text(family = "montserrat", size = 18, color = "black"),
        axis.ticks.y = element_blank()) 
  
ggsave(width = 18, height = 12, dpi = 320, filename = "AskAManager_plot.png")


  
  

