#NOTE: Ctrl-F "YOURPATH" to find all paths you need to change to your own.

### Digital Marketing Intelligence - Assignment 1 R-script
#Group 3-10:
# Carasatorre Parra, Teresa
# Dreemer, George
# Hadjipieri, Katerina
# Oikonomou, Asimina

rm(list = ls()) #clean workspace

# Preliminary work: packages, data and checks ----
## Install & Load libraries (librarian) ----
install.packages("librarian")
library(librarian)
librarian::shelf(dplyr,readxl,bannerCommenter,lubridate,ggplot2,car,nortest,janitor, quiet = TRUE)

## Load datasets ----
financial.df <- read_xlsx(file.choose()) #Dataset 1 for Financial related questions
behavior.df <- read_xlsx(file.choose()) #Dataset 2 for Customer/Behavior related questions

### In Financial df: Creating a new date variable (date object) and a week variable ----
# useful for time plots
financial.df$Date_new <- as.Date(as.character(financial.df$Date), "%Y%m%d") #convert the Date column into a date object
financial.df <- relocate(financial.df, Date_new, .after = Date) #for tidyness relocate the new variable next to the old one
financial.df$Week_iso <- isoweek(financial.df$Date_new)
financial.df <- relocate(financial.df, Week_iso, .after = Date_new)

### In Behavior df: Creating a new date variable (date object) and a week variable ----
# useful for time plots
behavior.df$Date_new <- as.Date(as.character(behavior.df$Date), "%Y%m%d") #convert the Date column into a date object
behavior.df <- relocate(behavior.df, Date_new, .after = Date) #for tidyness relocate the new variable next to the old one
behavior.df$Week_iso <- isoweek(behavior.df$Date_new)
behavior.df <- relocate(behavior.df, Week_iso, .after = Date_new)

#We change the format of AnalysisWeekNr as numeric
behavior.df$AnalysisWeekNr <- as.numeric(behavior.df$AnalysisWeekNr)

## Data checks ----

### Financial df ----
#### General ----
summary(financial.df); str(financial.df)
# if relevant / if not delete
#      Quantity         Revenue
#  Min.   :   4.0   Min.   :   41.12
#  1st Qu.:  76.0   1st Qu.: 1131.01
#  Median : 255.5   Median : 4509.11
#  Mean   : 475.5   Mean   : 8551.17
#  3rd Qu.: 639.2   3rd Qu.:10229.77
#  Max.   :4822.0   Max.   :97176.81

#### NA check ----
apply(financial.df, 2, function(x) any(is.na(x)))
#    Fiscal Year        Platform             Day Category Level1 Category Level2
#           FALSE           FALSE           FALSE           FALSE           FALSE
#            Date    Detail Views        Quantity         Revenue          Period
#           FALSE           FALSE           FALSE           FALSE           FALSE
#        Launched
#           FALSE


#### Logic check ----
##### Nominal variables check ----
# First, we check if all nominal variables make sense,
# by looking if the unique values of these variables match our expectations.
apply(financial.df[,-c(3,6,7,8,9)],2, function(x) unique(x))  #note: we excluded the non-nominal variables
# $`Fiscal Year`
# [1] "FY2021" "FY2022"
#
# $Platform
# [1] "App" "Web"
#
# $`Category Level1`
# [1] "BEAUTY"
#
# $`Category Level2`
# [1] "Beauty Accessories" "Care" "Fragances" "Makeup"
#
# $Period
# [1] "Before" "Intro"  "After"
#
# $Launched
# [1] "No"  "Yes"

##### Numeric variables check ----
# Next, we check if there are any values with a negative value or 0
apply(financial.df[,-c(1,2,4,5,10,11)],2, function(x) length(which(x <= 0)))  #note: we excluded the non-nominal variables
#          Day         Date Detail Views     Quantity      Revenue
#            0            0            0            0            0

#### Outlier checks ----

boxplot(financial.df$Revenue, xlab = "Revenue")
outRev <- boxplot.stats(financial.df$Revenue)$out
outrowRev <- which(financial.df$Revenue %in% c(outRev))
outrowRev
# Revenue Outliers are at rows:
#  [1] 220 259 260 261 262 263 264 265 266 267 268 424 425 427 428 429 430 431 432 433 434 435
# [23] 453 549 550 553 554 555 556 557 558 559 560 561 575 589 590 591 592 593 594 595 596 597
# [45] 598 599 600 601 602 603 604 605

financial.df[outrowRev,] # to see the rows


### Behavior df ----

#### General ----
summary(behavior.df); str(behavior.df)
# if relevant / if not delete
#      Users         Weekly Users
#  Min.   : 344.0   Min.   :  858
#  1st Qu.: 473.2   1st Qu.: 1072
#  Median :2164.0   Median : 6152
#  Mean   :2725.1   Mean   : 7277
#  3rd Qu.:4799.0   3rd Qu.:12987
#  Max.   :8580.0   Max.   :16412
#
#    Sessions      Transactions       Revenue          Quantity
#  Min.   :  456   Min.   :  13.0   Min.   :  1481   Min.   :  27.0
#  1st Qu.:  731   1st Qu.:  43.0   1st Qu.:  5261   1st Qu.: 131.0
#  Median : 3034   Median : 151.0   Median : 19257   Median : 469.0
#  Mean   : 4795   Mean   : 294.2   Mean   : 33504   Mean   : 884.7
#  3rd Qu.: 8393   3rd Qu.: 520.0   3rd Qu.: 56625   3rd Qu.:1544.5
#  Max.   :17024   Max.   :1110.0   Max.   :120479   Max.   :3431.0
#
#  Beauty Revenue    Beauty Quantity
#  Min.   :   0.00   Min.   :  0.00
#  1st Qu.:  87.53   1st Qu.:  7.00
#  Median : 504.90   Median : 36.00
#  Mean   :1176.31   Mean   : 73.04
#  3rd Qu.:1828.63   3rd Qu.:102.25
#  Max.   :8384.11   Max.   :588.00

#### NA check ----
apply(behavior.df, 2, function(x) any(is.na(x)))
#   AnalysisWeekNr            Date Device Category           Users    Weekly Users
#           FALSE           FALSE           FALSE           FALSE           FALSE
#        Sessions    Transactions         Revenue        Quantity Category Level1
#           FALSE           FALSE           FALSE           FALSE           FALSE
#  Beauty Revenue Beauty Quantity
#           FALSE           FALSE


#### Logic check ----
##### Nominal variables check ----
# First, we check if all nominal variables make sense,
# by looking if the unique values of these variables match our expectations.
apply(behavior.df[,-c(2,4,5,6,7,8,9,11,12)],2, function(x) unique(x))  #note: we excluded the non-nominal variables
# $AnalysisWeekNr
#  [1] " 1" " 2" " 3" " 4" " 5" " 6" " 7" " 8" " 9" "10" "11" "12" "13"
#
# $`Device Category`
# [1] "mobile" "tablet"
#
# $`Category Level1`
# [1] "BEAUTY"

##### Numeric variables check ----
# Next, we check if there are any values with a negative value or 0
apply(behavior.df[,c(2,4,5,6,7,8,9,11,12)],2, function(x) length(which(x <= 0)))  #note: we excluded the non-nominal variables
#           Date           Users    Weekly Users        Sessions    Transactions
#               0               0               0               0               0
#         Revenue        Quantity  Beauty Revenue Beauty Quantity
#               0               0               3               3

# it seems there are cases, let's clarify:
apply(behavior.df[,c(2,4,5,6,7,8,9,11,12)],2, function(x) which(x == 0))
# $`Beauty Revenue`
# [1] 117 126 151
#
# $`Beauty Quantity`
# [1] 117 126 151
# 3 observations where Beauty Revenue and Beauty Quantity are 0: row 117, 126 and 151.

nonbeauty_observations.df <- as.data.frame(behavior.df[c(117,126,151),]) # just saving them here for now
nonbeauty_observations.df
# We can see that these observations do have revenue and have somehow been labeled BEAUTY but have no beauty spending.
#   AnalysisWeekNr     Date Device Category Users Weekly Users Sessions Transactions Revenue
# 1              4 20221126          tablet   489         1036      776           51 5676.25
# 2              5 20221205          tablet   353          902      534           13 1481.38
# 3              9 20221230          tablet   463         1274      743           41 5488.04
#   Quantity Category Level1 Beauty Revenue Beauty Quantity
# 1      118          BEAUTY              0               0
# 2       27          BEAUTY              0               0
# 3      107          BEAUTY              0               0


#### Outlier checks ----

boxplot(behavior.df$Revenue, xlab = "Revenue")
outRev2 <- boxplot.stats(behavior.df$Revenue)$out
outrowRev2 <- which(behavior.df$Revenue %in% c(outRev2))
outrowRev2
# Revenue Outliers are at rows: - (no outliers)
behavior.df[outrowRev2,] # to see the rows (no outliers)


# A. Financial Questions ----

## 1) How much extra revenue did the magic mirror deliver? (Revenue) ----
# Here we want to look if the total revenue significantly differs between non-launch and launch observations in each of the periods
# Furthermore we want to see exactly how much extra revenue was generated in the launched observations compared to non-launched

### Extra revenue in "Intro" period of Launch vs. Non-launch observations ----
extrarevenue_intro <- sum(financial.df[financial.df$Launched == "Yes" & financial.df$Period == "Intro" & financial.df$`Category Level2` == "Makeup",c("Revenue")]) - sum(financial.df[financial.df$Launched == "No" & financial.df$Period == "Intro" & financial.df$`Category Level2` == "Makeup",c("Revenue")])
# €105,490.60 - €62,023.42 = €43,467.18
# During the same time frame (Intro period) in the non-launch era, we see that the revenue was 2.05x smaller


### Extra revenue in "After" period of Launch vs. Non-launch observations ----
extrarevenue_after <- sum(financial.df[financial.df$Launched == "Yes" & financial.df$Period == "After" & financial.df$`Category Level2` == "Makeup",c("Revenue")]) - sum(financial.df[financial.df$Launched == "No" & financial.df$Period == "After" & financial.df$`Category Level2` == "Makeup",c("Revenue")])
# €204,983.70 - €93,415.77 = €111,567.90
# During the same time frame (Intro period) in the non-launch era, we see that the revenue was 2.05x smaller


### Is this a significant difference? ----

# Wilcoxon signed rank exact test
wilcox.test(Revenue~as.factor(Launched), data = financial.df[financial.df$Period != "Before" & financial.df$`Category Level2` == "Makeup",], paired = TRUE)
# V = 80, p-value = 1.969e-09
# Results: Significant difference between the distributions of the two groups for Revenue (NoLaunch vs. Launch in the "Intro" or "After" period)


### Compute total extra revenue generated ----
total_extrarevenue <- extrarevenue_intro + extrarevenue_after
total_extrarevenue
# €155,035.10

### Create a boxplot to show the distribution ----
# Note: Exclude the Before period, as that is only present in the Non-launched and skews the scales
ggplot(financial.df[financial.df$Platform == "App" & financial.df$Period != "Before" & financial.df$'Category Level2' == "Makeup",], aes(x = Launched, y = Revenue, fill = Launched)) +
  geom_boxplot() +
  ggtitle('Revenue in App: Pre- and Post-launch \n Category: Makeup') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Launched",
       y = "Revenue (\u20ac)")



## 2) How many extra items were sold? (Quantity) ----
# Here we apply the same logic as to the revenue, but we use the "Quantity" variable

### Extra items in "Intro" period of Launch vs. Non-launch observations ----
extraitems_intro <- sum(financial.df[financial.df$Launched == "Yes" & financial.df$Period == "Intro" & financial.df$`Category Level2` == "Makeup",c("Quantity")]) - sum(financial.df[financial.df$Launched == "No" & financial.df$Period == "Intro" & financial.df$`Category Level2` == "Makeup",c("Quantity")])
# 9458 items - 6065 items = 3393 extra items sold in the intro period of when the app was launched vs. non-launched

### Extra revenue in "After" period of Launch vs. Non-launch observations ----
extraitems_after <- sum(financial.df[financial.df$Launched == "Yes" & financial.df$Period == "After" & financial.df$`Category Level2` == "Makeup",c("Quantity")]) - sum(financial.df[financial.df$Launched == "No" & financial.df$Period == "After" & financial.df$`Category Level2` == "Makeup",c("Quantity")])
# 17819 items - 7294 items = 10525 extra items sold in the after period of when the app was launched vs. non-launched

### Is this a significant difference? ----

#Before testing we need to check the assumptions of Normality on Quantity

# Normality: Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(financial.df$Quantity)
#D = 0.21797, p-value < 2.2e-16
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better

# Wilcoxon signed rank exact test
wilcox.test(Quantity ~ Launched, data = financial.df[financial.df$Period != "Before" & financial.df$`Category Level2` == "Makeup",], paired = TRUE)
# V = 107, p-value = 6.984e-09
# Results: Significant difference between the distributions of the two groups when it comes to Quantity (NoLaunch vs. Launch in the "Intro" or "After" periods)


### Compute total extra items sold ----
total_extraitems <- extraitems_intro + extraitems_after
total_extraitems
# 13918 extra items


### Create a boxplot to show the distribution ----
# Note: Exclude the Before period, as that is only present in the Non-launched and skews the scales
ggplot(financial.df[financial.df$Platform == "App" & financial.df$Period != "Before" & financial.df$'Category Level2' == "Makeup",], aes(x = Launched, y = Quantity, fill = Launched)) +
  geom_boxplot() +
  ggtitle('Quantity in App: Pre- and Post-launch \n Category: Makeup') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Launched",
       y = "Quantity")


## 3) What is the strangest thing you see in the data and can you maybe find/give an explanation? ----

# IDEA 1:
# To be honest I'm not entirely sure, my logic checks didn't yield any strangeness (IF WE CAN USE THE ODDITY IN DATABASE 2, then it would be cool)
# The thing I find strange is that Fiscal year has two values FY2021 and FY2022, when there are clearly 2023 observations
# I guess the explanation for that is that the fiscal year 2022 ends on January 17 2023? As that is the last 2023 observation in our data set

# IDEA 2:
# Perhaps that there is no "Before" period for the Launched phase, so we cannot compare the Before period.

# IDEA 3 - Best?:
# We see there are outliers in the Revenue.
outrowRev
# # Revenue Outliers are at rows:
# #  [1] 220 259 260 261 262 263 264 265 266 267 268 424 425 427 428 429 430 431 432 433 434 435
# # [23] 453 549 550 553 554 555 556 557 558 559 560 561 575 589 590 591 592 593 594 595 596 597
# # [45] 598 599 600 601 602 603 604 605

financial.df[outrowRev,c("Date_new")] # to see the rows

# Visualize outliers
ggplot(financial.df, aes(x = Platform, y = Revenue, fill = Platform)) +
  geom_boxplot() +
  ggtitle('Revenue Outliers in App & Web') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Platform",
       y = "Daily Beauty Revenue (\u20ac)",
       color = "Legend")




# B. Behavior Questions ----

#Find the launch date from the first dataset - we use this to illustrate the before-after launch periods in our time plots
launch_date <- financial.df$Date_new[financial.df$Launched == 'Yes' & financial.df$Period == 'Intro'][1]
#Define launch week based on where the above date is situated - we use this is Question 4, where our X-axis is AnalysisWeekNr (not date)
launch_week <- behavior.df$AnalysisWeekNr[behavior.df$Date_new == launch_date][1]

## 4) Did the magic mirror succeed in its goal to stimulate more repeat purchases for beauty products. Please explain your findings. ----

#We change the format of AnalysisWeekNr as numeric
behavior.df$AnalysisWeekNr <- as.numeric(behavior.df$AnalysisWeekNr)

# We  find the mean  of weekly users that made repeat purchases for beauty products
repeat_purchases_Beauty <- aggregate(behavior.df$AnalysisWeekNr ~ behavior.df$'Weekly Users' + behavior.df$'Beauty Quantity', FUN = function(x) mean(x))
names(repeat_purchases_Beauty) <- c('AnalysisWeekNr','WeeklyUsers','BeautyQuantity')

#We make a bar plot to illustrate how many customers came back and shop from the beauty category (yes it was successful)

# create the plot
ggplot(repeat_purchases_Beauty, aes(x = AnalysisWeekNr, y = WeeklyUsers, fill = BeautyQuantity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Week of Year", y = "Number of unique users") +
  scale_x_continuous(breaks = 1:52, labels = 1:52) + theme_bw()


## 5) Did it increase the frequency of app use for the visitors that interacted with the magic mirror? What might be possible drawbacks you are missing by using this dataset? ----

unique(behavior.df$AnalysisWeekNr) #there are 13 weeks

# NOTE: With all ifelse/loops we are doing below, we aggregate tablet and mobile users together, because the app is both on mobile and tablet by definition.

#TrueWeeklyUsers/TotalWeeklyUsers: a variable the aggregates mobile & tablet users and signifies the total users (not unique users)

behavior.df$TrueWeeklyUsers <- ifelse(behavior.df$AnalysisWeekNr == 1, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 1]),
                                      ifelse(behavior.df$AnalysisWeekNr == 2, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 2]),
                                             ifelse(behavior.df$AnalysisWeekNr == 3, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 3]),
                                                    ifelse(behavior.df$AnalysisWeekNr == 4, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 4]),
                                                           ifelse(behavior.df$AnalysisWeekNr == 5, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 5]),
                                                                  ifelse(behavior.df$AnalysisWeekNr == 6, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 6]),
                                                                         ifelse(behavior.df$AnalysisWeekNr == 7, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 7]),
                                                                                ifelse(behavior.df$AnalysisWeekNr == 8, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 8]),
                                                                                       ifelse(behavior.df$AnalysisWeekNr == 9, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 9]),
                                                                                              ifelse(behavior.df$AnalysisWeekNr == 10, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 10]),
                                                                                                     ifelse(behavior.df$AnalysisWeekNr == 11, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 11]),
                                                                                                            ifelse(behavior.df$AnalysisWeekNr == 12, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 12]),
                                                                                                                   ifelse(behavior.df$AnalysisWeekNr == 13, sum(behavior.df$Users[behavior.df$AnalysisWeekNr == 13]), NA)))))))))))))

behavior.df$WeeklySessions <- ifelse(behavior.df$AnalysisWeekNr == 1, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 1]),
                                      ifelse(behavior.df$AnalysisWeekNr == 2, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 2]),
                                             ifelse(behavior.df$AnalysisWeekNr == 3, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 3]),
                                                    ifelse(behavior.df$AnalysisWeekNr == 4, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 4]),
                                                           ifelse(behavior.df$AnalysisWeekNr == 5, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 5]),
                                                                  ifelse(behavior.df$AnalysisWeekNr == 6, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 6]),
                                                                         ifelse(behavior.df$AnalysisWeekNr == 7, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 7]),
                                                                                ifelse(behavior.df$AnalysisWeekNr == 8, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 8]),
                                                                                       ifelse(behavior.df$AnalysisWeekNr == 9, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 9]),
                                                                                              ifelse(behavior.df$AnalysisWeekNr == 10, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 10]),
                                                                                                     ifelse(behavior.df$AnalysisWeekNr == 11, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 11]),
                                                                                                            ifelse(behavior.df$AnalysisWeekNr == 12, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 12]),
                                                                                                                   ifelse(behavior.df$AnalysisWeekNr == 13, sum(behavior.df$Sessions[behavior.df$AnalysisWeekNr == 13]), NA)))))))))))))

behavior.df$WeeklySessionsPerUser <- ifelse(behavior.df$AnalysisWeekNr == 1, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 1]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 1],
                                            ifelse(behavior.df$AnalysisWeekNr == 2, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 2]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 2],
                                                   ifelse(behavior.df$AnalysisWeekNr == 3, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 3]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 3],
                                                          ifelse(behavior.df$AnalysisWeekNr == 4, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 4]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 4],
                                                                 ifelse(behavior.df$AnalysisWeekNr == 5, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 5]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 5],
                                                                        ifelse(behavior.df$AnalysisWeekNr == 6, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 6]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 6],
                                                                               ifelse(behavior.df$AnalysisWeekNr == 7, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 7]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 7],
                                                                                      ifelse(behavior.df$AnalysisWeekNr == 8, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 8]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 8],
                                                                                             ifelse(behavior.df$AnalysisWeekNr == 9, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 9]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 9],
                                                                                                    ifelse(behavior.df$AnalysisWeekNr == 10, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 10]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 10],
                                                                                                           ifelse(behavior.df$AnalysisWeekNr == 11, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 11]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 11],
                                                                                                                  ifelse(behavior.df$AnalysisWeekNr == 12, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 12]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 12],
                                                                                                                         ifelse(behavior.df$AnalysisWeekNr == 13, behavior.df$WeeklySessions[behavior.df$AnalysisWeekNr == 13]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 13], NA)))))))))))))



#### PLOT of Weekly Sessions per User over time ----
#color_plots contains the colors for the plots in question 5 and 6
colors_plots <- c("Weekly Sessions per User" = "dark green", "Weekly Beauty Revenue per User" = "purple", "Weekly Non-Beauty Revenue per User" = "orange", "Weekly Total Revenue per User" = "blue", "Launch of Magic Mirror" = "red")

# Create Launched variable that says "1" if the date is on and beyond 12-20-2022 & "0" if the date is before this date.
# We chose this date because that is the first day in the "Intro" period in the Financial dataset
behavior.df$Launched <- ifelse(behavior.df$Date_new >= as.Date(as.character("20221220"), "%Y%m%d"), 1, 0)


ggplot(behavior.df, aes(x = Date_new)) +
  geom_line(aes(y = WeeklySessionsPerUser, color = "Weekly Sessions per User"), size = 2) +
  ggtitle('Weekly Sessions per User Over Time \n Mobile & Tablet Combined') + geom_vline(aes(xintercept = launch_date, color = "Launch of Magic Mirror"), linetype = "dashed", size = 1.5) +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Time",
       y = "Weekly Sessions/User",
       color = "Legend") +
  scale_color_manual(values = colors_plots)

# Now we look if this difference is statistically significant thanks to the newly created Launched variable

# Normality: Lilliefors (Kolmogorov-Smirnov) normality test
# The null hypothesis (H0) for the test is the data comes from a normal distribution.
# The alternate hypothesis (H1) is that the data doesn’t come from a normal distribution.
lillie.test(behavior.df$WeeklySessionsPerUser)
# D = 0.19082, p-value < 2.2e-16
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better

# Wilcoxon signed rank exact test -
# NOTE: Due to Launched (84) and Non-launched observations (98) having different # of observations,
# we need to remove the first 7 days (based on the date), which will remove 7 days from mobile and 7 from tablet.
# Which will reduce the total "No" observations by a total of 14.
wilcox.test(WeeklySessionsPerUser ~ as.factor(Launched), data = behavior.df[behavior.df$Date_new > as.Date(as.character("20221107"), "%Y%m%d"),], paired = TRUE)
# V = 1883, p-value = 0.6626
# Results: No significant difference between the distributions of WeeklySessionsPerUser in the Launch vs. Non-launch


#### Relocations of the new variables to keep the df tidy
behavior.df <- relocate(behavior.df, TrueWeeklyUsers, .after = "Weekly Users") #for tidyness relocate the new variable next to the old one
behavior.df <- relocate(behavior.df, WeeklySessions, .after = "Sessions") #for tidyness relocate the new variable next to the old one
behavior.df <- relocate(behavior.df, WeeklySessionsPerUser, .after = WeeklySessions) #for tidyness relocate the new variable next to the old one


## 6) Did it increase the value of the customers that used the magic mirror? ----

# In the next three code sections, we calculate the weekly beauty-only revenue, non-beauty revenue (total - beauty) and total revenue.
# We distinguish between these three to see not only the effects of the app on beauty revenue per user (value),
# but also the spillover effects illustrated through non-beauty revenue and total revenue per user.


### BEAUTY VALUE ONLY ----
behavior.df$WeeklyBeautyRevenue <- ifelse(behavior.df$AnalysisWeekNr == 1, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 1]),
                                     ifelse(behavior.df$AnalysisWeekNr == 2, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 2]),
                                            ifelse(behavior.df$AnalysisWeekNr == 3, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 3]),
                                                   ifelse(behavior.df$AnalysisWeekNr == 4, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 4]),
                                                          ifelse(behavior.df$AnalysisWeekNr == 5, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 5]),
                                                                 ifelse(behavior.df$AnalysisWeekNr == 6, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 6]),
                                                                        ifelse(behavior.df$AnalysisWeekNr == 7, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 7]),
                                                                               ifelse(behavior.df$AnalysisWeekNr == 8, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 8]),
                                                                                      ifelse(behavior.df$AnalysisWeekNr == 9, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 9]),
                                                                                             ifelse(behavior.df$AnalysisWeekNr == 10, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 10]),
                                                                                                    ifelse(behavior.df$AnalysisWeekNr == 11, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 11]),
                                                                                                           ifelse(behavior.df$AnalysisWeekNr == 12, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 12]),
                                                                                                                  ifelse(behavior.df$AnalysisWeekNr == 13, sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 13]), NA)))))))))))))





behavior.df$WeeklyBeautyRevenuePerUser <- ifelse(behavior.df$AnalysisWeekNr == 1, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 1]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 1],
                                            ifelse(behavior.df$AnalysisWeekNr == 2, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 2]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 2],
                                                   ifelse(behavior.df$AnalysisWeekNr == 3, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 3]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 3],
                                                          ifelse(behavior.df$AnalysisWeekNr == 4, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 4]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 4],
                                                                 ifelse(behavior.df$AnalysisWeekNr == 5, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 5]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 5],
                                                                        ifelse(behavior.df$AnalysisWeekNr == 6, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 6]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 6],
                                                                               ifelse(behavior.df$AnalysisWeekNr == 7, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 7]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 7],
                                                                                      ifelse(behavior.df$AnalysisWeekNr == 8, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 8]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 8],
                                                                                             ifelse(behavior.df$AnalysisWeekNr == 9, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 9]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 9],
                                                                                                    ifelse(behavior.df$AnalysisWeekNr == 10, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 10]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 10],
                                                                                                           ifelse(behavior.df$AnalysisWeekNr == 11, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 11]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 11],
                                                                                                                  ifelse(behavior.df$AnalysisWeekNr == 12, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 12]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 12],
                                                                                                                         ifelse(behavior.df$AnalysisWeekNr == 13, behavior.df$WeeklyBeautyRevenue[behavior.df$AnalysisWeekNr == 13]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 13], NA)))))))))))))


#### PLOT ----
ggplot(behavior.df, aes(x = Date_new)) +
  geom_line(aes(y = WeeklyBeautyRevenuePerUser, color = "Weekly Beauty Revenue per User"), size = 2) +
  ggtitle('Weekly Beauty Revenue per User Over Time \n Mobile & Tablet Combined') + geom_vline(aes(xintercept = launch_date, color = "Launch of Magic Mirror"), linetype = "dashed", size = 1.5) +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Time",
       y = "Weekly Beauty Revenue/User (\u20ac)",
       color = "Legend") +
  scale_color_manual(values = colors_plots)


#### Relocations of the new variables to keep the df tidy
behavior.df <- relocate(behavior.df, WeeklyBeautyRevenue, .after = "Beauty Revenue") #for tidyness relocate the new variable next to the old one
behavior.df <- relocate(behavior.df, WeeklyBeautyRevenuePerUser, .after = WeeklyBeautyRevenue) #for tidyness relocate the new variable next to the old one


### NON-BEAUTY VALUE ONLY (TOTAL - BEAUTY) ----
behavior.df$WeeklyNonBeautyRevenue <- ifelse(behavior.df$AnalysisWeekNr == 1, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 1]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 1])),
                                          ifelse(behavior.df$AnalysisWeekNr == 2, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 2]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 2])),
                                                 ifelse(behavior.df$AnalysisWeekNr == 3, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 3]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 3])),
                                                        ifelse(behavior.df$AnalysisWeekNr == 4, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 4]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 4])),
                                                               ifelse(behavior.df$AnalysisWeekNr == 5, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 5]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 5])),
                                                                      ifelse(behavior.df$AnalysisWeekNr == 6, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 6]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 6])),
                                                                             ifelse(behavior.df$AnalysisWeekNr == 7, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 7]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 7])),
                                                                                    ifelse(behavior.df$AnalysisWeekNr == 8, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 8]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 8])),
                                                                                           ifelse(behavior.df$AnalysisWeekNr == 9, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 9]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 9])),
                                                                                                  ifelse(behavior.df$AnalysisWeekNr == 10, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 10]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 10])),
                                                                                                         ifelse(behavior.df$AnalysisWeekNr == 11, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 11]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 11])),
                                                                                                                ifelse(behavior.df$AnalysisWeekNr == 12, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 12]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 12])),
                                                                                                                       ifelse(behavior.df$AnalysisWeekNr == 13, (sum(behavior.df$Revenue[behavior.df$AnalysisWeekNr == 13]) - sum(behavior.df$`Beauty Revenue`[behavior.df$AnalysisWeekNr == 13])), NA)))))))))))))





behavior.df$WeeklyNonBeautyRevenuePerUser <- ifelse(behavior.df$AnalysisWeekNr == 1, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 1]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 1],
                                                 ifelse(behavior.df$AnalysisWeekNr == 2, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 2]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 2],
                                                        ifelse(behavior.df$AnalysisWeekNr == 3, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 3]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 3],
                                                               ifelse(behavior.df$AnalysisWeekNr == 4, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 4]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 4],
                                                                      ifelse(behavior.df$AnalysisWeekNr == 5, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 5]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 5],
                                                                             ifelse(behavior.df$AnalysisWeekNr == 6, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 6]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 6],
                                                                                    ifelse(behavior.df$AnalysisWeekNr == 7, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 7]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 7],
                                                                                           ifelse(behavior.df$AnalysisWeekNr == 8, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 8]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 8],
                                                                                                  ifelse(behavior.df$AnalysisWeekNr == 9, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 9]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 9],
                                                                                                         ifelse(behavior.df$AnalysisWeekNr == 10, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 10]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 10],
                                                                                                                ifelse(behavior.df$AnalysisWeekNr == 11, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 11]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 11],
                                                                                                                       ifelse(behavior.df$AnalysisWeekNr == 12, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 12]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 12],
                                                                                                                              ifelse(behavior.df$AnalysisWeekNr == 13, behavior.df$WeeklyNonBeautyRevenue[behavior.df$AnalysisWeekNr == 13]/behavior.df$TrueWeeklyUsers[behavior.df$AnalysisWeekNr == 13], NA)))))))))))))


#### PLOT ----
ggplot(behavior.df, aes(x = Date_new)) +
  geom_line(aes(y = WeeklyNonBeautyRevenuePerUser, color = "Weekly Non-Beauty Revenue per User"), size = 2) +
  ggtitle('Weekly Non-Beauty Revenue per User Over Time \n Mobile & Tablet Combined') + geom_vline(aes(xintercept = launch_date, color = "Launch of Magic Mirror"), linetype = "dashed", size = 1.5) +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Time",
       y = "Weekly Non-Beauty Revenue/User (\u20ac)",
       color = "Legend") +
  scale_color_manual(values = colors_plots)

#### Relocations of the new variables to keep the df tidy
behavior.df <- relocate(behavior.df, WeeklyNonBeautyRevenue, .after = WeeklyBeautyRevenuePerUser) #for tidyness relocate the new variable next to the old one
behavior.df <- relocate(behavior.df, WeeklyNonBeautyRevenuePerUser, .after = WeeklyNonBeautyRevenue) #for tidyness relocate the new variable next to the old one


### Is it significant? ----
# Here we test both for WeeklyNonBeautyRevenuePerUser and WeeklyBeautyRevenuePerUser

# Normality: Lilliefors (Kolmogorov-Smirnov) normality test
# The null hypothesis (H0) for the test is the data comes from a normal distribution.
# The alternate hypothesis (H1) is that the data doesn’t come from a normal distribution.
lillie.test(behavior.df$WeeklyNonBeautyRevenuePerUser)
# D = 0.1734, p-value = 1.378e-14
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better
lillie.test(behavior.df$WeeklyBeautyRevenuePerUser)
# D = 0.14481, p-value = 5.669e-10
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better


# Wilcoxon signed rank exact test -
# NOTE: Due to Launched (84) and Non-launched observations (98) having different # of observations,
# we need to remove the first 7 days (based on the date), which will remove 7 days from mobile and 7 from tablet.
# Which will reduce the total "No" observations by a total of 14.
wilcox.test(WeeklyNonBeautyRevenuePerUser ~ as.factor(Launched), data = behavior.df[behavior.df$Date_new > as.Date(as.character("20221107"), "%Y%m%d"),], paired = TRUE)
# V = 3570, p-value = 1.398e-15
# Results: Significant difference between the distributions of WeeklyNonBeautyRevenuePerUser in the Launch vs. Non-launch

wilcox.test(WeeklyBeautyRevenuePerUser ~ as.factor(Launched), data = behavior.df[behavior.df$Date_new > as.Date(as.character("20221107"), "%Y%m%d"),], paired = TRUE)
# V = 1792, p-value = 0.9768
# Results: No significant difference between the distributions of WeeklyBeautyRevenuePerUser in the Launch vs. Non-launch

# Mean of WeeklyBeautyRevenuePerUser Before and After Launch
mean(behavior.df$WeeklyBeautyRevenuePerUser[behavior.df$Date_new > as.Date(as.character("20221107"), "%Y%m%d") & behavior.df$Launched == 0])
mean(behavior.df$WeeklyBeautyRevenuePerUser[behavior.df$Date_new > as.Date(as.character("20221107"), "%Y%m%d") & behavior.df$Launched == 1])

# Non-coding questions
## 7) Using all information and answers so far, what could you do to elaborate on the magic mirror and integrate app and web around this feature? An important question, because we know that multi device and multi platform customers are higher value customers. ----

### App vs. Web on Revenue: Where do people convert? ----

ggplot(financial.df[financial.df$Launched == "Yes" & financial.df$Period != "Before",], aes(x = Platform, y = Revenue, fill = Platform)) +
  geom_boxplot() +
  ggtitle('Daily Beauty Revenue: Which platform converts? \n App vs. Web - Launched: Yes') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Platform",
       y = "Daily Beauty Revenue (\u20ac)",
       color = "Legend")

ggplot(financial.df[financial.df$Launched == "No",], aes(x = Platform, y = Revenue, fill = Platform)) +
  geom_boxplot() +
  ggtitle('Daily Beauty Revenue: Which platform converts? \n App vs. Web - Launched: No') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Platform",
       y = "Daily Beauty Revenue (\u20ac)",
       color = "Legend")


# Normality: Lilliefors (Kolmogorov-Smirnov) normality test
# The null hypothesis (H0) for the test is the data comes from a normal distribution.
# The alternate hypothesis (H1) is that the data doesn’t come from a normal distribution.
lillie.test(financial.df$Revenue)
# D = 0.2599, p-value < 2.2e-16
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better

# Wilcoxon signed rank exact test
wilcox.test(Revenue ~ as.factor(Platform), data = financial.df[financial.df$Period != "Before",], paired = TRUE)
# V = 168, p-value < 2.2e-16
# Results: Significant difference between the distributions of Revenue in the App vs. Web



### App vs. Web on Browsing: Where do people browse? ----
financial.df$DetailViews <- financial.df$'Detail Views'
ggplot(financial.df[financial.df$Launched == "Yes" & financial.df$Period != "Before",], aes(x = Platform, y = DetailViews, fill = Platform)) +
  geom_boxplot() +
  ggtitle('Daily Product Page Views: Where do people browse? \n App vs. Web - Launched: Yes') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Platform",
       y = "Daily Product Page Views",
       color = "Legend")

ggplot(financial.df[financial.df$Launched == "No",], aes(x = Platform, y = DetailViews, fill = Platform)) +
  geom_boxplot() +
  ggtitle('Daily Product Page Views: Where do people browse? \n App vs. Web - Launched: No') +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5), axis.text = element_text(size = 15)) +
  labs(x = "Platform",
       y = "Daily Product Page Views",
       color = "Legend")

# Normality: Lilliefors (Kolmogorov-Smirnov) normality test
# The null hypothesis (H0) for the test is the data comes from a normal distribution.
# The alternate hypothesis (H1) is that the data doesn’t come from a normal distribution.
lillie.test(financial.df$`Detail Views`)
# D = 0.16304, p-value < 2.2e-16
# Conclusion: the data doesn't come from a normal distribution therefore a Wilcoxon Rank Test is better
financial.df$DetailViews <- financial.df$'Detail Views'
  # Wilcoxon signed rank exact test
wilcox.test(DetailViews ~ as.factor(Platform), data = financial.df[financial.df$Period != "Before",], paired = TRUE)
# V = 1958.5, p-value < 2.2e-16
# Results: Significant difference between the distribution of the Detail page views generated in the App vs. Web



## 8) Summarize the limitations (and potential solutions) on the last slide. Be clear on why you think these are limitations.
# non-coding question