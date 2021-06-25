#Week 10 ----
#Week 10_chart type1-Google Trends ---- (my Rtweets are not authorized..)
install.packages("gtrendsR")
library(gtrendsR)
library(magrittr)
library(tidyverse)
#importing data from google
kw.wellbeing<- c("GDP", "Employment Rate", "Health", "Air Quality", "Happiness")
gt.wellbeing <- gtrends(kw.wellbeing)

gt.wellbeing %>% plot + 
  theme_minimal()

gt.wellbeing$interest_by_country %>% 
  select(hits, location, keyword) %>% 
  mutate(hits = as.numeric (hits)) %>% 
  arrange(desc(hits)) %>% head(15)

##Creating baseplot
baseplot1<- gt.wellbeing %>% plot +
            theme_minimal()
##Output:topic2_baseplot1-week10 (baseplot)
baseplot1

##Creating gtrend trendlines
gtrends1 <- baseplot1 +
  # theme_minimal() +
  # theme_clean() +
  # theme_few() +
  # theme_fivethirtyeight() +
  ggtitle("Most Searched Indicators of Well-Being", subtitle = "Soure: Google Trends 2020-06-01")

##Output:topic2_gtrends1-week10 (final output)
gtrends1

RColorBrewer::display.brewer.all()
gtrends2 <- gtrends1+ 
  scale_color_brewer(palette = "Dark2") #best of the three...
  # scale_color_brewer(palette = "Pastel2")
  # scale_color_brewer(palette = "Set1") #not good: too strong red and green

##Output:topic2_gtrends2-week10 (not color-friendly)
gtrends2 ##gtrends1 is better, the color palette of gtrends1 is better for the colorblind