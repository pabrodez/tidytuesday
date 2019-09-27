library(tidyverse)
library(ggforce)
library(scales)

school_diversity <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv"
  ) %>%
  set_names(tolower)

## cut prop of white into 0-25, 25-50, etc. groups in first and second school years
df <-
  school_diversity %>%
  select(leaid, school_year, white) %>% 
  group_by(leaid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 2) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "school_year", values_from = "white") %>%
  mutate(perc_diff = `2016-2017` - `1994-1995`) %>% 
  mutate(perc_group_94 = cut_width(`1994-1995`, 25, boundary = 0, closed = "left"),
         perc_group_17 = cut_width(`2016-2017`, 25, boundary = 0, closed = "left")) 

plot_schools <- 
  df %>% 
  ggplot() +
  geom_curve(aes(
    x = -10,
    y = `1994-1995`,
    xend = `2016-2017`,
    yend = -30,
    color = perc_diff
  ),
  curvature = -0.4,
  size = .1,
  ncp = 10) +
  scale_x_continuous(expand = expand_scale(mult = c(0, .01)),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = expand_scale(mult = c(0, .2))) +
  scale_color_gradient2(low = "#004B40", mid = "#F6F6F6", high = "#533600",
                        breaks = seq(-100, 100, 50),
                        limits = c(-100, 100),
                        labels = paste0(seq(-100, 100, 50), "%"),
                        guide = guide_colorbar(title = "Change in % units",
                                               title.position = "top",
                                               title.hjust = .5,
                                               barheight = unit(2, "mm"),
                                               barwidth = unit(50, "mm"))) +
  facet_col(~ perc_group_94) +
  labs(x = "2016-2017", y = "1994-1995", 
       title = "Change in proportion of white students\nin schools from 1994-95 to 2016-17",
       caption = "Graphic: @pabrodez | Source: The Washington Post") +
  theme_minimal(base_family = "Cabin") +
  theme(strip.text = element_blank(),
        panel.grid.major.x = element_line(size = rel(.5), color = "grey70"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.y = unit(.5, "cm"),
        plot.background = element_rect(fill = "grey85"),
        panel.border = element_rect(color = "transparent", fill = "transparent"),
        legend.position = "top",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.direction = "horizontal", legend.spacing.x = unit(0, units = "cm"),
        plot.margin = margin(0, 1, 0, 0.5, unit = "cm"),
        plot.caption = element_text(margin = margin(t = 1, b = .5, unit = "cm")),
        plot.title = element_text(margin = margin(t = 1, b = 1, unit = "cm"), 
                                  hjust = .5, lineheight = 1.5, face = "bold", color = "grey40", size = 16)) 

ggsave(plot = plot_schools, filename = "2019_09_24.png", height = 10, width = 5, dpi = "retina")
