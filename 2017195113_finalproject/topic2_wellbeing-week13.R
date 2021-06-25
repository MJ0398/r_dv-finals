library(gganimate)
library(magrittr)
library(tidyverse)
height <- read_csv("http://ncdrisc.org/downloads/height/NCD_RisC_eLife_2016_height_age18_countries.csv")
height %>% view
height %>% head

?names
# functions to get or set the names of an object names(x) <- value
names(height)[1] <- "Country"
names(height)[4] <- "year"
names(height)[5] <- "value"

height <- height %>% filter(Sex == "Women")

# ?mutate mutate() adds new variables and preserves existing ones
# ?min_rank min_rank(): equivalent to rank(ties.method = "min")



gap <- height %>% 
  group_by(year) %>% 
  mutate(rank = min_rank(-value) * 1,
         Value_rel= value/value[rank == 1],
         value_lbl= paste0("", value)) %>% 
  filter(rank <=8) %>% 
  ungroup()

gap %>% view

#base plot
p<- ggplot(gap, aes(rank, group = Country, 
                    fill = as.factor(Country), color = as.factor(Country))) +
  geom_tile(aes(y= value/2, height = value, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y= 0, label = paste(Country,"")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Average height of women by year of birth", x="", y= "height") +
  theme(plot.title = element_text(hjust = 0, size = 18),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        # these relate to the axes post-flip
        plot.margin = margin(1,1,1,4,"cm"))
p
# Charttype1-gganimate histogram
p1 <- p + geom_histogram() + 
  coord_flip()+
  theme_minimal() +
  labs(title = "Average Height of Women by time of birth")+
  transition_manual(as.integer(year)) +
  ease_aes("linear")
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)
animate(p1, 200, fps = 10, duration = 40, width = 700, height = 500, renderer = gifski_renderer())
anim_save("topic2_gghisto-week13.gif")

# Chart type2-gganimate scatter
install.packages("gapminder")
library(gapminder)
gapminder %>% view
library(ggplot2)
# Output: topic2_baseplot2-week13
b2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, colour = continent)) +
  labs(title = "Life expectancy based on GDP per capita", subtitle= "Source: Rstudio")
b2
gs<- b2 + 
  geom_point() +
  transition_manual(as.integer(year)) +
  ease_aes("linear")
gs
# Output: topic2_ggscatter-week13
animate(p2, 200, fps = 10, duration = 40, width = 700, height = 500, renderer = gifski_renderer())
anim_save("topic2_ggscatter-week13.gif")
