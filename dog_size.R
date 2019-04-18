library(tidyverse)
library(ggrepel)
library(ggthemes)

dogs_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv")

dogs_clean <- dogs_raw %>% 
  na.omit() %>% 
  set_names(nm = c("year", "avg_weight", "avg_neck"))


dogs_percent_change <- dogs_clean %>%
  arrange(year) %>%
  mutate(avg_weight_pct_change = (avg_weight/lag(avg_weight) - 1) * 100,
         avg_neck_pct_change = (avg_neck/lag(avg_neck) - 1) * 100) %>%
  na.omit()

dogs_percent_change <- dogs_percent_change %>%
  select(year,
         avg_weight_pct_change,
         avg_neck_pct_change)

percent_change_long <- gather(dogs_percent_change,
         measurement,
         value,
         avg_weight_pct_change:avg_neck_pct_change,
         factor_key=TRUE)

ggplot(percent_change_long, aes(x = year, y = value, group = measurement)) +
  geom_line(aes(color = measurement))+
  geom_point(aes(color = measurement)) + 
  ylim(-3, 2) +
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = 0,
           ymax = 2,
           fill = "skyblue4",
           alpha = 0.25) + 
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = -3,
           ymax = 0,
           fill = "skyblue2",
           alpha = 0.25) + 
  xlab('') +
  ylab("Year-over-Year Percent Change") +
  labs(title = "The butcher's dog is getting smaller",
       caption = "#TidyTuesday by Dash Wieland \nRemix of work and data by @ECONdailycharts",
       subtitle = "Year over year percent change in average weight and neck size in dogs \nregistered with the UK's Kennel Club (when fully grown)") + 
  annotate("text",
           label = "Larger Dogs",
           x = 2014,
           y = 1.5,
           size = 4,
           fontface = 2, 
           colour = "black") + 
  annotate("text",
           label = "Smaller Dogs",
           x = 2014,
           y = -2.5,
           size = 4,
           fontface = 2,
           colour = "black") + 
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('avg_weight_pct_change' = 'steelblue3',
                                 'avg_neck_pct_change' = 'darkred'), 
                      labels = c('Avg. weight',
                                 'Avg. neck size')) + 
  theme_economist() + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 10)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))


ggsave("dog_size.png", width = 5, height = 5)


