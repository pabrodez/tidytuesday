library(tidyverse)
library(janitor)
library(lubridate)
library(cowplot)

# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 

# clean up some of the factors and playtime data
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s) %>% 
  mutate(release_date = as.Date(release_date, "%b %e, %Y"))

# top 5 publishers
top_publishers <- select(clean_df, publisher) %>% 
  na.omit() %>% 
  count(publisher) %>% 
  top_n(5) %>% 
  inner_join(., clean_df, by = "publisher") 

# plot
mean_price_plot <- 
  top_publishers %>% 
  group_by(publisher, release_year = year(release_date)) %>% 
  summarise(mean_year_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(y = publisher, x = release_year, fill = mean_year_price)) +
  geom_tile(height = .15) +
  scale_fill_continuous(low = "#543f43", high = "#9e767d", 
                        name = "Mean price", 
                        guide = guide_legend(label.position = "bottom", title.position = "top", title.hjust = .5)) +
  scale_x_continuous(breaks = 2004:2018, labels = function(x) substr(x, 3, 4), limits = c(2003, 2019)) +
  coord_polar() +
  ylim(letters[1], unique(top_publishers$publisher)) +  ## create dummy levels of discrete scale to create space between center and first level
  annotate("segment", x = seq(2003.5, 2017.5, 1), y = 2, xend = seq(2003.5, 2017.5, 1), yend = 6, alpha = .1) +
  theme_void() +
  theme(text = element_text(color = "#CCCCCC", family = "Avant Garde"),
        plot.background = element_rect(fill = "transparent"), 
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom", 
        legend.key.height = unit(1, "mm"),
        legend.spacing.x = unit(4, "mm"))

legend_price <- get_legend(mean_price_plot)
mean_price_plot <- mean_price_plot + theme(legend.position = "none")

main_circle_plot <- 
  top_publishers %>% 
  group_by(publisher, release_year = year(release_date)) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = release_year, y = publisher, fill = n)) +
  geom_tile(height = .25) +
  scale_fill_continuous(low = "#3f3f54", high = "#8d8eb7",
                        breaks = c(10, 30, 60, 90), 
                        name = "Games released",
                        guide = guide_legend(label.position = "bottom", title.position = "top", title.hjust = .5)) +
  scale_x_continuous(breaks = 2004:2018, labels = function(x) substr(x, 3, 4), limits = c(2003, 2019)) +
  geom_text(aes(label = publisher, x = 2018.55), vjust = 0.5, hjust = 0, color = "#CCCCCC", family = "Avant Garde", size = 4) +
  coord_polar() +
  ylim(letters[1:10], unique(top_publishers$publisher)) +  ## create dummy levels of discrete scale to create space between center and first level
  annotate("segment", x = seq(2003.5, 2017.5, 1), y = 11, xend = seq(2003.5, 2017.5, 1), yend = 15, alpha = .1) +
  labs(title = "Games released and mean price by year by top 5 publishers",
       caption = "Source: Steam Spy | Graphic: @pabrodez") +
  theme_minimal() +
  theme(text = element_text(color = "#CCCCCC", family = "Avant Garde"),
        plot.background = element_rect(fill = "#405450"), 
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = -5, unit = "mm"), color = "#CCCCCC"),
        legend.position = "bottom", 
        legend.key.height = unit(1, "mm"),
        legend.spacing.x = unit(4, "mm"),
        plot.margin = margin(.5, 1, .5, 1, unit = "cm"),
        plot.caption = element_text(margin = margin(t = 100)),
        plot.title = element_text(hjust = .5, margin = margin(t = 25, b = 30)))

legend_games <- get_legend(main_circle_plot)
main_circle_plot <- main_circle_plot + theme(legend.position = "none")

arranged_plot <- 
  ggdraw() +
  draw_plot(main_circle_plot, 0, 0, 1, 1) +
  draw_plot(mean_price_plot, .225, .245, .55, .55) + ## adjusting position has been a source of affliction 
  draw_plot(legend_price, .2, .1, .1, .1) +
  draw_plot(legend_games, .7, .1, .1, .1)

ggsave("./tidytuesday/2019_07_30.png", plot = arranged_plot, height = 11, width = 9, dpi = "retina")
