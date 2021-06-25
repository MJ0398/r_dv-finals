#Week11
#Week11_chart type1_corrplot
# datacleaning
read.csv("topic2_wellbeing_data.csv") %>% view
wellbeing <- read.csv("topic2_wellbeing_data.csv")
df <- select(wellbeing, c(2,10,13,15)) 
#dplyr
df %>% view #removed
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD'))
var <- c('FRUITS_VEGGIES', 'BMI_RANGE', 'DAILY_STEPS', 'SLEEP_HOURS')
# data cleaning
df1 <- df %>% select(var)
df1 %>% glimpse
# correlation plot
cormatrix <- cor(df1) %>% print
title <- "Correlation between factors of individual well-being"
##OUtput: topic2_corrplot1-week11
corrplot(cormatrix, 
         method = "color",
         col = col(200),
         diag = FALSE,
         tl.cex = 0.65,
         type = "upper", order= "hclust",
         title = title,
         addCoef.col = "black",
         sig.level = 0.05, insig = "blank",
         mar= c(0,0,1,0))

#Week11_chart type2_trendline ---- (instead of rtweet)
HW <- read.csv("topic2_wellbeing_data3.csv")
HW %>% view
HW %>% glimpse
# OUtput: topic2_baseplot1-week11
bp <- ggplot(HW, aes(x= New.deaths, y= Deaths)) +
  ggtitle("Deaths over New deaths of COVID-19 worldwide") +
  xlab("New deaths worldwide") +
  ylab("Deaths worldwide") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
bp
# Output: topic2_trendline1-week11
tl <- bp +
  geom_point(position = position_jitter(2)) +
  geom_smooth(method = lm, se = FALSE, col = "red", lwd = 1)
tl

#week11_charttype3-interactive plot ----
library(plotly)
# ggplotly(tl) -> testing ip
# OUtput: topic2_baseplot2-week11
bp2 <- ggplot(HW, aes(x= WHO.Region, y = Confirmed, colour = Recovered)) +
  ggtitle("Deaths over New deaths of COVID-19 worldwide") +
  xlab("WHO Regions") +
  ylab("Confirmed patients in each region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))
bp2
gp <- bp2 +
  geom_point(position = position_jitter(0.25))
# OUtput: topic2_ggplotly-week11
gp
ggplotly(gp)
