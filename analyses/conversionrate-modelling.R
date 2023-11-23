#NOTE: Ctrl-F "YOURPATH" to find all paths you need to change to your own.

#################################################################
##                         PREPARATION                         ##
#################################################################

# clean workspace
rm(list = ls())

# load libraries
library(librarian)
librarian::shelf(stringr,lubridate,dplyr,mice,car,ggplot2,  cran_repo = 'https://cran.r-project.org')

# set working directory
setwd("YOURPATH")


#################################################################
##                          DATA WORK                          ##
#################################################################
# DATA WORK ----

# read in knmi weather data and Wehkamp data
weatherdataknmi_detailed <- read.csv("YOURPATH")

wehkamp_data <- data.frame(read.csv("YOURPATH"))
# Download COVID data from the web and adjust it to our needs ----
Covid_cases <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

Covid_cases_reduced <- Covid_cases[Covid_cases$Country.Region == "Netherlands" & Covid_cases$Province.State == "",]

# Transpose the dataframe and exclude the first four entries (province, country, lat, lon)
Neth_covidcases <- data.frame(t(Covid_cases_reduced[,-c(1:4)]))

# Extract a date variable from the row.names and set names 
Neth_covidcases$date <- as.Date(row.names(Neth_covidcases),"X%m.%d.%y")
colnames(Neth_covidcases) <- c("covidcases", "date")
rownames(Neth_covidcases) <- c()

# make difference column
Neth_covidcases$NewCovidCases <- c(NA, diff(Neth_covidcases$covidcases,1))
plot(Neth_covidcases$date, Neth_covidcases$NewCovidCases)
boxplot(Neth_covidcases$NewCovidCases)

# adjust the 1 extreme outlier
Neth_covidcases$NewCovidCases[Neth_covidcases$NewCovidCases > 150000] <- max(Neth_covidcases$NewCovidCases[Neth_covidcases$NewCovidCases < 150000],na.rm = TRUE)
plot(Neth_covidcases$date, Neth_covidcases$NewCovidCases)
boxplot(Neth_covidcases$NewCovidCases)

# add a week + weekly average column for 2022
Neth_covidcases <- Neth_covidcases[Neth_covidcases$date > "2021-12-31", ]
Neth_covidcases$Week <- week(Neth_covidcases$date)

Weeks <- unique(Neth_covidcases$Week)
for (iWeek in Weeks) {
  Neth_covidcases$WeekAverageCovid[Neth_covidcases$Week == iWeek] <- mean(Neth_covidcases$NewCovidCases[Neth_covidcases$Week == iWeek],na.rm = TRUE)  
}

# check if makes sense
plot(Neth_covidcases$date, Neth_covidcases$NewCovidCases)
lines(Neth_covidcases$date, Neth_covidcases$WeekAverage, type = "l", col = "red")

# make a weekend+week dummy, (1 = monday, 7 = sunday) ----
Neth_covidcases$wday <- wday(Neth_covidcases$date, week_start = getOption("lubridate.week.start", 1))
Neth_covidcases$weekend_dummy <- ifelse(Neth_covidcases$wday == 6 | Neth_covidcases$wday == 7, 1, 0)
Neth_covidcases$week_dummy <- ifelse(Neth_covidcases$weekend_dummy == 1, 0, 1)

# select for columns to be used
Neth_covidcases <- Neth_covidcases[, c("date", "weekend_dummy", "week_dummy", "NewCovidCases", "WeekAverageCovid")]


# adjust weather data to needs ----
# got a few different sources for weather data, using the detailed weather data here
weatherdataknmi_detailed$date <- as.Date(as.character(weatherdataknmi_detailed$YYYYMMDD), "%Y%m%d")

# function to get aggregate summaries, remove the NA's from missing data weather stations
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x), na.rm = TRUE))}

# take average from all measurement stations and select desired columns
weather_data <- seg.summ(weatherdataknmi_detailed, weatherdataknmi_detailed$date)
weather_data$date <- as.Date(as.character(weather_data$YYYYMMDD), "%Y%m%d")

# FG        : Daily mean windspeed (in 0.1 m/s)
# TG        : Daily mean temperature in (0.1 degrees Celsius)
# SQ        : Sunshine duration (in 0.1 hour) calculated from global radiation (-1 for <0.05 hour)
# SP        : Percentage of maximum potential sunshine duration
# DR        : Precipitation duration (in 0.1 hour)
# RH        : Daily precipitation amount (in 0.1 mm) (-1 for <0.05 mm)
# NG        : Mean daily cloud cover (in octants; 9=sky invisible)
weather_data_selected <- data.frame(weather_data[, c("date", "FG", "TG", "SQ", "SP", "DR", "RH", "NG")])
colnames(weather_data_selected) <- c("date", "windspd", "temp", "sun_duration", "perc_max_sunduration", "rain_duration", "rain_amount", "cloud_cover")

# merge the data into 1 external dataset ----
external_data <- data.frame(merge(Neth_covidcases, weather_data_selected))
str(external_data); summary(external_data)

# merging wehkamp and the external dataset ----
# preparing wehkamp data
wehkamp_data$session_date <- as.Date(wehkamp_data$session_date)
wehkamp_data$cust_last_order_date <- as.Date(wehkamp_data$cust_last_order_date)
wehkamp_data <- wehkamp_data %>% mutate_at(c("cust_age", "cust_relationship_length", "male_u", "female", "cust_conversion_rate", "cust_min_price", "cust_max_price", 
                                             "session_conversion_rate", "session_sale_dummy", "n_session_views", "n_session_sales", "loyal_sale_cust", "loyal_session_cust"), as.numeric)
str(wehkamp_data); summary(wehkamp_data)

#the actual merge
session_data <- data.frame(merge(wehkamp_data, external_data, by.x = "session_date", by.y = "date"))

# session data descriptives ----
#structure, summary, standard deviation of session data
str(session_data); summary(session_data)
options(scipen=999)
sapply(session_data, sd, na.rm = TRUE)

#tests to see if merge lost nothing
min(wehkamp_data$session_date) == min(session_data$session_date)
max(wehkamp_data$session_date) == max(session_data$session_date)
nrow(wehkamp_data) == nrow(session_data)

# the default customer
default_cust <- "E790F5B013B583670687E2F6694687F5"
summary(session_data[session_data$customer_id == default_cust, ])

# last row of md pattern is about the default customer probably
md.pattern(session_data)

# to do : look at missing observations and see if can be remedied, do the tests and visualizations

#write function for generating boxplots repeatedly to save time
session_data_num <- Filter(is.numeric, session_data)
boxplot_all <- function(data) {
  for (i in 1:ncol(data)) {
    boxplot(data[,i], xlab=names(data)[i])
  }
}
# test for function: boxplot(session_data_num$cust_max_price,xlab=names(session_data_num)[7])

# generate boxplots for all numeric variables to find outliers
boxplot_all(session_data_num)

session_data$session_conversion_rate <- session_data$session_conversion_rate/100



# INFLUENCES ON CONVERSION RATE ----
attach(session_data)

##################################################################
##                       INTERNAL DRIVERS                       ##
##################################################################

## INTERNAL DRIVERS ----

### GENDER ----
# male has slightly higher conversion rate and siginificant
# seem to be not normal, maybe wilcoxon rank test is better, also gives that difference is significant
ks.test(session_conversion_rate, "pnorm")

#Wilcoxon Rank Test - Gender - significant difference!
wilcox.test(session_conversion_rate~female)


### LOYALTY ----
# when using loyalty by session views over 3 months, result shows those view less have higher CR;

#Wilcoxon Rank Test - Two loyalty measures - significant diference!
wilcox.test(session_conversion_rate~loyal_session_cust)
wilcox.test(session_conversion_rate~loyal_sale_cust)


### RELATIONSHIP LENGTH ----
# significant and model is better than base model, positive coefficent too, longer relationship = better
rl_cr_cus <- lm(cust_conversion_rate~cust_relationship_length, data=session_data)
summary(rl_cr_cus)
rl_cr_se <- lm(session_conversion_rate~cust_relationship_length, data=session_data)
summary(rl_cr_se)


### FAVORITE CHANNEL ----
#As session conversion rate is not normal, ANOVA cannot be used here, therefore we apply Kruskal-Wallis test.
kruskal.test(session_conversion_rate~factor(cust_fav_channel, ordered = FALSE))
#Kruskal-Wallis showed fav channels have significantly different session/customer conversion rates.

#### MEAN CONVERSION RATE PER CHANNEL ----
# Website has highest conversion
seg.summ(session_data[, c("cust_conversion_rate", "session_conversion_rate")], session_data$cust_fav_channel)


### FAVORITE ARTICLE GROUPS ----
# have significantly different session/customer conversion rates
kruskal.test(cust_conversion_rate~factor(cust_fav_art_group, ordered = FALSE))
kruskal.test(session_conversion_rate~factor(cust_fav_art_group, ordered = FALSE))

#### MEAN CONVERSION RATE PER ARTICLE GROUP ----
# girls fashion brands probably has insufficient data, below it is Telstar and Lds Fashion Brands Tops
seg.summ(session_data[, c("cust_conversion_rate", "session_conversion_rate")], session_data$cust_fav_art_group)


### PRICE ----
# price is significant and the lower min price, the higher conversion rate, the higher max price, the higher conversion rate as well.
price_cr_cus <- lm(cust_conversion_rate~cust_min_price + cust_max_price)
summary(price_cr_cus)
# same is not true for session conversion rate, max price is insignificant, min price is the same
price_cr_se <- lm(session_conversion_rate~cust_min_price + cust_max_price)
summary(price_cr_se)


### NUMBER OF VIEWS - NUMBER OF SALES ----
# both significant, but as views go up, cr goes down, as sales go up so does the conversion rate as expected.
views_sales_cr_se <- lm(session_conversion_rate~n_session_views + n_session_sales)
summary(views_sales_cr_se)
# again we see it is sales not views that help conversion rates, we want customers to buy not linger


### WEEKDAY/WEEKEND ----
# we see that during the week the session conversion rate is significantly higher
wilcox.test(session_conversion_rate~weekend_dummy)

library(bannerCommenter)


##################################################################
##                       EXTERNAL DRIVERS                       ##
##################################################################
## EXTERNAL DRIVERS ----

### COVID CASES ----
# the higher new covid cases, the lower session conversion rate was, significantly
covid_cr_se <- lm(session_conversion_rate~WeekAverageCovid)
summary(covid_cr_se)

### WEATHER FACTORS ----
# only temperature was somewhat significant with a positive coeficient, the model was better than base overall
weather_cr_se <- lm(session_conversion_rate~windspd + temp + sun_duration + rain_duration)
summary(weather_cr_se)


##################################################################
##                        VISUALIZATIONS                        ##
##################################################################
# VISUALIZATIONS ----


## GENDER ----
session_data1 <- session_data %>%
  filter(female %in% c(0,1))
ggplot(data=session_data1,
       aes(x=as.factor(female), y=cust_conversion_rate)) +
  geom_boxplot()

session_data2 <- session_data %>%
  filter(female %in% c(0,1))
ggplot(data=session_data2,
       aes(x=as.factor(female), y=session_conversion_rate)) +
  geom_boxplot()

## LOYALTY ----
session_data3 <- session_data %>%
  filter(loyal_session_cust %in% c(0,1))
ggplot(data=session_data3,
       aes(x=as.factor(loyal_session_cust), y=cust_conversion_rate)) +
  geom_boxplot()

session_data4 <- session_data %>%
  filter(loyal_session_cust %in% c(0,1))
ggplot(data=session_data4,
       aes(x=as.factor(loyal_session_cust), y=session_conversion_rate)) +
  geom_boxplot()

session_data5 <- session_data %>%
  filter(loyal_sale_cust %in% c(0,1))
ggplot(data=session_data5,
       aes(x=as.factor(loyal_sale_cust), y=cust_conversion_rate)) +
  geom_boxplot()

session_data6 <- session_data %>%
  filter(loyal_sale_cust %in% c(0,1))
ggplot(data=session_data6,
       aes(x=as.factor(loyal_sale_cust), y=session_conversion_rate)) +
  geom_boxplot()

## WEEK/WEEKEND ----
ggplot(data=session_data,
       aes(x=as.factor(weekend_dummy), y=session_conversion_rate)) +
  geom_boxplot()

ggplot(data=session_data,
       aes(x=as.factor(weekend_dummy), y=cust_conversion_rate)) +
  geom_boxplot()

## LINEAR MODELs ----
session_data %>%
  select(cust_conversion_rate, cust_relationship_length, cust_min_price, cust_max_price) %>%
  pairs
#Careful! figure below takes long time to generate
session_data %>%
  select(session_conversion_rate, cust_relationship_length, cust_min_price, cust_max_price,
         n_session_views, n_session_sales, WeekAverageCovid,
         windspd, temp, sun_duration, rain_duration) %>%
  pairs

## BAR ----
channel_avg_cr <- seg.summ(session_data[, c("cust_conversion_rate", "session_conversion_rate")], session_data$cust_fav_channel)
channel_avg_cr <- channel_avg_cr[-2,]
channel_avg_cr <- channel_avg_cr[-1,]
colnames(channel_avg_cr)[1] <- 'Channels'

detach(session_data)
attach(channel_avg_cr)
ggplot(data=channel_avg_cr)+
  geom_bar(aes(y=session_conversion_rate, x=reorder(Channels,session_conversion_rate)),
           fill=4,
           stat="identity", show.legend=FALSE)+
  ggtitle("Mean session conversion rate by channels")+
  theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=14))+
  theme(plot.title = element_text(size=14))+
  xlab("")+ylab("Session Conversion Rate")+
  coord_flip()

article_avg_cr <- seg.summ(session_data[, c("cust_conversion_rate", "session_conversion_rate")], session_data$cust_fav_art_group)
article_avg_cr <- article_avg_cr[-c(14,15,8,5),]
colnames(article_avg_cr)[1] <- "article_groups"
detach(channel_avg_cr)
attach(article_avg_cr)
ggplot(data=article_avg_cr)+
  geom_bar(aes(y=session_conversion_rate, x=reorder(article_groups,session_conversion_rate)),
           fill='#f15c80',
           stat="identity", show.legend=FALSE)+
  ggtitle("Mean session conversion rate by article groups")+
  theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.text = element_text(size=10))+
  theme(axis.title = element_text(size=14))+
  theme(plot.title = element_text(size=14))+
  xlab("")+ylab("Session Conversion Rate")+
  coord_flip()