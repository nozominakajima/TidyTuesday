# load libraries
library(tidyverse)
library(skimr)
library(lubridate)
library(ggdark)
library(ggrepel)
library(ggpubr)

# set theme
theme_set(theme_minimal())


# set fonts
sysfonts::font_add_google("Press Start 2P")
sysfonts::font_add_google("Raleway")
showtext::showtext_auto()


# set working directory
setwd("~/Documents/GitHub/TidyTuesday/2021/20210523_MarioKart")


# load data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')


# clean data: only plot single lap with no shortcut
records_single <- records %>%
  mutate(track = factor(track),
         type = factor(type),
         shortcut = factor(shortcut),
         system_played = factor(system_played)) %>%
  filter(type == "Single Lap" & shortcut == "No") %>%
  group_by(track) %>%
  arrange(track, date) %>%
  mutate(date = as_date(date, origin = lubridate::origin),
         date_since_first = date - min(date),
         date_since_first = as.numeric(date_since_first/365),
         time_since_first = time - first(time),
         time_since_last = time - last(time),
         time_since_first_pct = abs(time_since_first/first(time))) 


# create label data: only keep the last record and track name 
records_single_label <- records_single %>%
  filter(date_since_first == last(date_since_first) & time == min(time)) 


# plot
records_single %>%  
  ggplot(aes(x = date_since_first, y = time_since_first_pct, color = track)) +
  geom_point(alpha = 0.5) +
  geom_line() + 
  geom_text_repel(data = records_single_label, 
                  aes(label = track, x = date_since_first, y = time_since_first_pct, color = track),
                  nudge_x       = 10000 - records_single_label$date_since_first,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "y",
                  hjust         = "left",
                  family        = "Raleway") +
  scale_x_continuous(limits = c(0, 30)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "\nYears since first world record\n",
       y = "\nRace time improved relative to first world record\n",
       title = "\n Mario Kart 64 World Records",
       subtitle = "\n How much did the world records for different races improve over time? \n The biggest improvement has been in Yoshi Valley, with the most recent world record clocking 20.9% faster than the first world record.",
       caption = "Data: Mario Kart 64 (single track, no shortcuts) | Plot: @nozominaka") +
  dark_theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        plot.title = element_text(family = "Press Start 2P", hjust = 0.5),
        plot.subtitle = element_text(family = "Raleway", hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        plot.caption = element_text(family = "Raleway"),
        axis.title.x = element_text(family = "Raleway"),
        axis.text.x = element_text(family = "Raleway"), 
        axis.title.y = element_text(family = "Raleway"),
        axis.text.y = element_text(family = "Raleway"),
        axis.ticks = element_blank())

ggsave(dpi = 320, filename = "MarioKart_plot.png")
