#1.pre-work_1: overall ----
rm(list= ls())
library(readxl)
library(tidyverse)
library(ggplot2)
dataset_1 <- read_excel("2017195113-covid.xlsx")
dataset_1 %>% view
dataset_1 %>% dim
dataset_1 %>% glimpse #13418 rows (observations), 11columns (variables)
dataset_1 %>% class

#2.pre-work_2: ooking at the details of the data ----
#list all continent entries
dataset_1 %>% 
  group_by(continentExp) %>% 
  summarize() #Africa America Asia Europe Oceania Other
#calculate average cases per continent
dataset_1 %>% 
  group_by(continentExp) %>% 
  summarize(ave.cases = mean(cases)) 
  #Africa 13.8 America 489. Asia 131. Europe 266. Oceania 17.9 Other 10.9
#round to full number
dataset_1 %>% 
  group_by(continentExp) %>% 
  summarize(ave.cases = round(mean(cases), digits =0)) 
  #Africa 14 America 489 Asia 131 Europe 266 Oceania 18 Other 11
#arrange by descending order
dataset_1 %>% 
  group_by(continentExp) %>% 
  summarize(ave.cases = round(mean(cases), digits =0)) %>% 
  arrange(desc(ave.cases))

#3.pre-work_3: data wrangling ----
##Data transformation: looking only at USA data
USdata <-data.frame (filter(dataset_1, countryterritoryCode == "USA"))
USdata %>% glimpse #119 rows (observations), 11 columns (variables)

#4.ggworkflow_1: creating base plot ----
##base plot 1: Cases per Continent
base1 <- ggplot(dataset_1 %>% 
                  group_by(continentExp) %>% 
                  filter(year == 2020),
                  aes(x= reorder(continentExp, cases), y= cases)) +
                  xlab("Continent") + ylab("Cases") +
                  ggtitle("Cases per Continent", subtitle = "date:") +
                  theme_minimal() +
                  theme(plot.title = element_text(size = 16, hjust = 0.5))
                  ##cases per continent (all countries)
base1
base2 <- ggplot(USdata %>% 
                  group_by(month),
                  aes(x= reorder(month, cases), y= cases)) +
                  xlab("Month") + ylab("Cases") +
                  ggtitle("Cases per each month in USA") +
                  theme_minimal() +
                  theme(plot.title = element_text(size = 16, hjust = 0.5))
                  ##cases per each month (USA)
base2
##using base plot
base1+ geom_boxplot(aes(fill = continentExp))
  #bad; the frequency of COVID19 varies too much in each country within continents
  ##should find a different x variable
base2+ geom_boxplot(aes(fill = month))
USdata %>% 
  group_by(month) %>% 
  summarize(med.cases = round(median(cases), digits =0)) 
  #12=0, 1=0, 2=0, 3=823, 4=28819
  #12,1,2: almost no cases
  #3: median low, skewed distribution, outlier exists
   ##can be inferred no cases most of the days in March, but sudden increase of cases
  #4: cases increased, relatively normal distribution, outliers on both sides
base2+ geom_point(position = "jitter", col = USdata$month)
  #x=time graph might be the best way to visualize

#5.ggworkflow_2: creating final plot----
base2 + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position= "jitter", size = 0.9)

