#NOTE: Ctrl-F "YOURPATH" to find all paths you need to change to your own.

##################################################################
##                         Preparation                          ##
##################################################################

#clear workspace
rm(list = ls())

##:::::::::::::::::##
##  Installations  ##
##:::::::::::::::::##
# INSTALLATIONS ----

#Handy package that Installs, Updates and Loads
# packages from CRAN, Github & Bioconductor
install.packages("librarian")
library("librarian")

#install & load packages with shelf function
librarian::shelf(nFactors,gplots,RColorBrewer,semPlot, lmtest, aTSA, ggplot2, ggcorrplot, stringr, plyr, formattable, fpc, psych, testthat, rpart, partykit, neuralnet, rstudioapi,
                 car,corrplot,dplyr,stats,bannerCommenter, vars, urca, clipr, cluster, mclust, r-lib/rlang, ROCR, MASS, Hmisc, ipred, caret, gbm, randomForest, e1071, update_all = TRUE)

install.packages("ggpubr", repos = "https://cloud.r-project.org/", dependencies = TRUE)
library("ggpubr")

install.packages("rpart", repos = "https://cloud.r-project.org/", dependencies = TRUE)
library("rpart")

##::::::::::::::::::::::
##  Custom Functions  ::
##::::::::::::::::::::::
# CUSTOM FUNCTIONS ----

## bannerCommenter Simplifier # <Code Formatting> ----
#Functionality: (1) banner() and boxup() in one
# (2) Copies the result to your clipboard
#How to use: txt is just the text we use as input | ftype is the function type 1 for banner and 2 for boxup() | bChar (bandChar) and s (snug) are boxup specific arguments
b <- function(txt, ftype, bChar, s) {

  #if bchar was not specified it's a default value I am using in this doc
  if(missing(bChar)) {
    bChar <- ":"
  }

  #if type of function is missing, assume it's banner() (for ease)
  if(missing(ftype)) {
    ftype <- 1
  }

  #main if statement
  if (ftype == 1) {
    #if snug arguemnt is missing just assume it's FALSE
    if(missing(s)) {
      s <- FALSE
    }
    result <- banner(txt, snug = s)
    cat(result)
    write_clip(result,"character","")
  } else if (ftype == 2) {
    #if snug arguemnt is missing just assume it's TRUE
    if(missing(s)) {
      s <- TRUE
    }
    result <- boxup(txt, snug = s, bandChar = bChar, rightSideHashes=2)
    cat(result)
    write_clip(result,"character","")
  }
}

#Fit criteria calculator
#Custom function that calculates: hit rate information, tdl, lift curve and gini
fitcriteria_calculator <- function(predict,IV){
  # USER GUIDE ------------------------------------------------------------------------------
  ## predict: this is your predict() ran based on your lr-model
  ## (e.g., predict(churn_basemodel, type = "response", newdata= validation_sample)

  ## IV_from_validationsample: here specify your IV from the validation sample
  ## (e.g., validation_sample$Churn)
  # -----------------------------------------------------------------------------------------
  tablebase <- ifelse(predict>.5,1,0) #basis for hit rate table & decile ntile()

  # HIT RATE CALCULATIONS
  hitrate_table <- table(IV, tablebase, dnn= c("Observed", "Predicted"))
  hit_rate <- ((hitrate_table[1,1]+hitrate_table[2,2])/sum(hitrate_table))*100
  sensitivity <- (hitrate_table[2,2]/(hitrate_table[2,2]+hitrate_table[1,2]))*100
  specificity <- (hitrate_table[1,1]/(hitrate_table[1,1]+hitrate_table[2,1]))*100

  # TDL CALCULATIONS (10 levels)
  decile_ntile <- ntile(tablebase, 10)
  decile_table <- table(IV, decile_ntile, dnn= c("Observed", "Decile"))
  tdl <- (decile_table[2,10] / (decile_table[1,10]+ decile_table[2,10])) / mean(IV)

  # LIFT CURVE PLOT
  decile_pred <- prediction(predict, IV)
  decile_perf <- performance(decile_pred,"tpr","fpr")
  plot(decile_perf,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i", main="Lift Curve")+abline(0,1, col="red")

  # GINI CALCULATIONS
  auc_perf <- performance(decile_pred,"auc")
  #The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
  #So to get the Gini we do:
  gini <- as.numeric(auc_perf@y.values)*2-1


  #Output
  cat('HIT RATE INFORMATION:','\n','True Negatives:', hitrate_table[1,1], '\n','False Negatives:', hitrate_table[1,2], '\n','True Positives:', hitrate_table[2,2], '\n','False Positives:', hitrate_table[2,1], '\n',
      '\n', 'Hit rate (%): ', hit_rate, '\n', 'Sensitivity (%): ', sensitivity, '\n', 'Specificity (%): ', specificity,'\n','\n',
      'TOP DECILE LIFT INFORMATION:','\n','TDL value:',tdl,'\n','\n',
      'GINI INFORMATION:','\n','GINI value:',gini)
}

#Odds ratio
odds_ratio <- function (estimate_value){
  print((exp(estimate_value)-1)*100)
}

##::::::::::
##  Data  ::
##::::::::::
# DATA ----

# IMPORT - opens popup to select your CSV file
customerdata.df <- read.csv(rstudioapi::selectFile("Select File"))


## DATA CHECKS ----
summary(customerdata.df)
str(customerdata.df)

### NA Check ----
# Conclusion: No NA values.
any(is.na(customerdata.df))
# What columns?
apply(customerdata.df, 2, function(x) any(is.na(x)))
#       Customer_ID            Gender               Age            Income   Relation_length
#             FALSE             FALSE             FALSE             FALSE             FALSE
#   Contract_length     Start_channel        Email_list          Home_age        Home_label
#             FALSE             FALSE             FALSE             FALSE             FALSE
# Electricity_usage         Gas_usage          Province             Churn        HL_ordinal
#             FALSE             FALSE             FALSE             FALSE             FALSE
#     startch_dummy
#             FALSE


### Logic Checks ----

#checking for logical unique values
unique(customerdata.df$Start_channel) #only the right cats
unique(customerdata.df$Home_label_rank) #only the right labels
unique(customerdata.df$Gender) #only 2 genders in the data
unique(customerdata.df$Province) #only the 12 correct provinces appear in the data
unique(customerdata.df$Churn) #only churn (1) or no churn (0) in the data


#amount 0-values per variable
length(which(customerdata.df$Relation_length == 0)) # 101 new customers
length(which(customerdata.df$Contract_length == 0)) # 7961 customers with flexible contract
length(which(customerdata.df$Email_list == 0)) # 4503 customer with no email visible to the firm -> more likely to churn
length(which(customerdata.df$Start_channel == "Online")) # 16401 customers who started Online (3599 who started on Phone)
length(which(customerdata.df$Gas_usage == 0)) # 442 households that only use electricity
length(which(customerdata.df$Home_age == 0)) # 162 people with a brand new home


# Age:
# Is there anyone older than 100 and younger than 18?
# https://nltimes.nl/2022/06/09/2600-nl-residents-100-years-old
# Remove people younger than 17 and older than 100
rows2remove.age <- which(customerdata.df$Age < 18)
customerdata.df <- customerdata.df[-c(rows2remove.age),]
str(rows2remove.age) # sanity check: 2 total rows to remove -> new df has 19,976

# Relationship length vs. Age:
# Are there people with lower age than their rel. length?
# make sure relationship length is in years - https://www.inchcalculator.com/convert/month-to-year/
which((customerdata.df$Relation_length * 0.0833334) > customerdata.df$Age)
# Answer: There are no such abnormalities.

# Start channel vs. Email_list: <- ? - It is 1650 observations
# People who signed up online, but of whom there is no email on record
# Why? Illogical, any signup process online requires an email - maybe something is wrong with this?
which(customerdata.df$Email_list == 0 & customerdata.df$Start_channel == "Online")

# Note: We also manually checked other variables and found no logical inconsistencies.


## NEW VARIABLES ----
#HOME LABEL RANK
unique(customerdata.df$Home_label)
#A=7 -> B -> C -> D -> E -> F -> G=1
customerdata.df[customerdata.df$Home_label == "A", "Home_label_rank"] <- 7
customerdata.df[customerdata.df$Home_label == "B", "Home_label_rank"] <- 6
customerdata.df[customerdata.df$Home_label == "C", "Home_label_rank"] <- 5
customerdata.df[customerdata.df$Home_label == "D", "Home_label_rank"] <- 4
customerdata.df[customerdata.df$Home_label == "E", "Home_label_rank"] <- 3
customerdata.df[customerdata.df$Home_label == "F", "Home_label_rank"] <- 2
customerdata.df[customerdata.df$Home_label == "G", "Home_label_rank"] <- 1

#START CHANNEL DUMMY
#Online = 1; Phone = 0
customerdata.df$startchannel_dummy <- ifelse(customerdata.df$Start_channel == "Online", 1, 0)

#RELATIONSHIP & CONTRACT LENGTH in YEARS
customerdata.df$Relation_length_yrs <- (customerdata.df$Relation_length * 0.0833334)
customerdata.df$Contract_length_yrs <- (customerdata.df$Contract_length * 0.0833334)

#AGE CATEGORIES + AGE CATEGORY DUMMIES
customerdata.df$Age_group <- cut(customerdata.df$Age, c(0, 22, 44, 68, Inf), c("0-22", "23-44", "45-68", ">68"), include.lowest=TRUE)
customerdata.df$AgeGroup_022 <- ifelse(customerdata.df$Age_group == "0-22", 1, 0)
customerdata.df$AgeGroup_2344 <- ifelse(customerdata.df$Age_group == "23-44", 1, 0)
customerdata.df$AgeGroup_4568 <- ifelse(customerdata.df$Age_group == "45-68", 1, 0)
customerdata.df$AgeGroup_68up <- ifelse(customerdata.df$Age_group == ">68", 1, 0)

#FLEXIBLE CONTRACT DUMMY
customerdata.df$Flexibel_contract <- ifelse(customerdata.df$Contract_length == 0, 1, 0)

#Income, Electricity_usage, Gas_usage
#For these variables' outliers we hypothesize that they were measured on a yearly level, so we divide by 12
#Income above 100000 we divide the value by 12 and log it; furthermore we create a "high income" dummy
customerdata.df$AdjLogIncome <- log(ifelse(customerdata.df$Income > 100000,(customerdata.df$Income/12),customerdata.df$Income))
customerdata.df$HighIncome_dummy <- ifelse(customerdata.df$Income > 100000,1,0)
#Electricity_usage & Gas_usage values above 10000, we divide the value by 12
customerdata.df$AdjGas_usage <- ifelse(customerdata.df$Gas_usage > 10000,(customerdata.df$Gas_usage/12),customerdata.df$Gas_usage)
customerdata.df$AdjElectricity_usage <- ifelse(customerdata.df$Electricity_usage > 10000,(customerdata.df$Electricity_usage/12),customerdata.df$Electricity_usage)


#Rearange to make dataframe tidier
library(dplyr)
customerdata.df <- relocate(customerdata.df, startchannel_dummy, .after = Start_channel)
customerdata.df <- relocate(customerdata.df, Home_label_rank, .after = Home_label)
customerdata.df <- relocate(customerdata.df, Relation_length_yrs, .after = Relation_length)
customerdata.df <- relocate(customerdata.df, Contract_length_yrs, .after = Contract_length)
customerdata.df <- relocate(customerdata.df, Age_group, .after = Age)
customerdata.df <- relocate(customerdata.df, AgeGroup_022, .after = Age_group)
customerdata.df <- relocate(customerdata.df, AgeGroup_2344, .after = AgeGroup_022)
customerdata.df <- relocate(customerdata.df, AgeGroup_4568, .after = AgeGroup_2344)
customerdata.df <- relocate(customerdata.df, AgeGroup_68up, .after = AgeGroup_4568)
customerdata.df <- relocate(customerdata.df, Flexibel_contract, .after = Contract_length)
customerdata.df <- relocate(customerdata.df, AdjLogIncome, .after = Income)
customerdata.df <- relocate(customerdata.df, HighIncome_dummy, .after = AdjLogIncome)
customerdata.df <- relocate(customerdata.df, AdjGas_usage, .after = Gas_usage)
customerdata.df <- relocate(customerdata.df, AdjElectricity_usage, .after = Electricity_usage)



## DESCRIPTIVE STATISTICS ----
### Means, Medians, etc. ----
summary(customerdata.df)


### Correlations /w Churn ----
# Conclusion (based on Churn):
# Top 3 Negative correlations: Contract_length (-0.24), homelabel_ordinal (-0.21), Relation_length (-0.17)
# Top 2 Positive correlations: Email_list (0.14), startchannel_dummy (0.14)
# We will confirm these also through literature review

#Conclusions (general):
# Age is positively correlated to Relationship_length: The older a customer is the higher the relationship length with the firm
# Start channel is positively correlated to Email_list: be aware
str(customerdata.df[,-c(4,8,9,13,17)])
#CORR MATRIX 1
library(corrplot)
cp.cor <- cor(customerdata.df[,-c(4,9,12,13,14,17,21,23,25,27)])
corrplot(cp.cor, title = "Churn Correlations", method = 'pie')
#CORR MATRIX 2
corrtest <- cor.mtest(customerdata.df[,-c(4,9,12,13,14,17,21,23,25,27)], conf.level = 0.95)
corrplot(cor(customerdata.df[,-c(4,9,12,13,14,17,21,23,25,27)]), p.mat = corrtest$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

### Mean Churn values per Province ----
# Conclusion: No province stands out too much. Top 3: Zeeland (0.51), Groningen (0.50), Drenthe (0.49)
meanchurn.province <- aggregate(Churn ~ Province, data=customerdata.df, mean)
meanchurn.province
#         Province     Churn
# 1        Drenthe 0.4879808 <- #3
# 2      Flevoland 0.4743590
# 3      Friesland 0.4736196
# 4     Gelderland 0.4591716
# 5      Groningen 0.5015723 <- #2
# 6        Limburg 0.4472403
# 7  Noord-Brabant 0.4775868
# 8  Noord-Holland 0.4828409
# 9     Overijssel 0.4772727
# 10       Utrecht 0.4668335
# 11       Zeeland 0.5105882 <- #1

### Mean Energy (Gas and Electricity) consumption per Label ----
# Conclusion: Seems that electricity usage increases as homelabel becomes worse.
# As electricity usage may be connected to churn, it is likely that energy label also is given its relationship with usage.
meanelectricityusage.homelabel <- aggregate(Electricity_usage ~ Home_label, data=customerdata.df, mean)
meanelectricityusage.homelabel
#   Home_label Electricity_usage
# 1          A          1863.833
# 2          B          2148.861
# 3          C          2270.974
# 4          D          2466.679
# 5          E          2553.570
# 6          F          2587.880
# 7          G          2631.987 <- ~41% increase from A
meangasusage.homelabel <- aggregate(Gas_usage ~ Home_label, data=customerdata.df, mean)
meangasusage.homelabel
#    Home_label Gas_usage
# 1          A  503.2077
# 2          B  916.4159
# 3          C 1198.4370
# 4          D 1368.7069
# 5          E 1514.0679
# 6          F 1709.1505
# 7          G 1729.3636 <- ~244% increase from A


# Mean churn per province
print(group_by(customerdata.df, Province) %>%
        summarise(count = n(), mean = mean(Churn, na.rm = TRUE),)) # there are some differences among provinces which could be interesting for prediction
#mean churn per gender
print(group_by(customerdata.df, Gender) %>%
        summarise(
          count = n(),
          mean = mean(Churn, na.rm = TRUE),
        )) # Small difference among genders
#mean churn per start_channel
print(group_by(customerdata.df, Start_channel) %>%
        summarise(
          count = n(),
          mean = mean(Churn, na.rm = TRUE),
        )) # Quite a big difference among start channel phone and website
#mean churn per home label
print(group_by(customerdata.df, Home_label) %>%
        summarise(
          count = n(),
          mean = 100*mean(Churn, na.rm = TRUE),
        )) # Big differences among home labels (churn goes up when home labels get worse)

#mean churn per start channel over home label ranks
Churn_averaged1 <- customerdata.df %>%
  group_by(Home_label_rank, Start_channel) %>%
  summarise(Churn_per_channel = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged1, aes(x=Home_label_rank, y=Churn_per_channel)) +
  geom_line(aes(color = Start_channel), linewidth = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#mean churn for being on email list or not over contract_length
Churn_averaged2 <- customerdata.df %>%
  group_by(Contract_length, Email_list) %>%
  summarise(Churn_for_email_list = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged2, aes(x=Contract_length, y=Churn_for_email_list)) +
  geom_line(aes(color = factor(Email_list)), linewidth = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

#mean churn over relationship length
Churn_averaged3 <- customerdata.df %>%
  group_by(Relation_length) %>%
  summarise(Churn_over_relationship = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged3, aes(x=Relation_length, y=Churn_over_relationship)) +
  geom_line(color = "#00AFBB")

#mean churn over contract length
Churn_averaged4 <- customerdata.df %>%
  group_by(Contract_length) %>%
  summarise(Churn_over_contract = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged4, aes(x=Contract_length, y=Churn_over_contract)) +
  geom_line(color = "#00AFBB")

#plotting gas and energy usage
library(Hmisc)
#divide usage into groups(higher is more usage)
customerdata4.df <- customerdata.df
customerdata4.df$Electricity_usageG <- as.numeric(cut2(customerdata4.df$Electricity_usage, g=100))
customerdata4.df$Gas_usageG <- as.numeric(cut2(customerdata4.df$Gas_usage, g=100))
#plot the mean churn per energy usage group 
Churn_averaged4 <- customerdata4.df %>%
  group_by(Electricity_usageG) %>%
  summarise(Churn_per_electro = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged4, aes(x=Electricity_usageG, y=Churn_per_electro)) +
  geom_line(color = "#00AFBB")
#plot the mean churn per gas usage group
Churn_averaged4 <- customerdata4.df %>%
  group_by(Gas_usageG) %>%
  summarise(Churn_per_gas = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged4, aes(x=Gas_usageG, y=Churn_per_gas)) +
  geom_line(color = "#E7B800")

#plot the mean churn for email list and not on list over home label rank
Churn_averaged5 <- customerdata.df %>%
  group_by(Home_label_rank, Email_list) %>%
  summarise(Churn_per_emaillist = mean(Churn, na.rm = TRUE))
ggplot(Churn_averaged5, aes(x=Home_label_rank, y=Churn_per_emaillist)) +
  geom_line(aes(color = factor(Email_list)), linewidth = 1) +
  scale_color_manual(values = c("red", "green"))


### Box Plots and histograms: Checking for outliers ----

# CHURN X INCOME
ggboxplot(customerdata.df, x = "Churn", y = "Age", 
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Income", xlab = "Churn") 
gghistogram(customerdata.df, x = "Age", y = "count", 
            color = "#E7B800",
            ylab = "Count", xlab = "Age", bins = 100) 
sum(customerdata.df$Age > 99)

ggboxplot(customerdata.df, x = "Churn", y = "Income",
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Income", xlab = "Churn") #some very high incomes (millions a year)
gghistogram(customerdata.df, x = "Income", y = "count", 
            color = "#E7B800",
            ylab = "Count", xlab = "Income", bins = 100)
sum(customerdata.df$Income < 500)
#After adjustment & log
gghistogram(customerdata.df, x = "AdjLogIncome", y = "count",
            color = "#E7B800",
            ylab = "Count", xlab = "Income", bins = 100)

ggboxplot(customerdata.df, x = "Churn", y = "Relation_length", 
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Relation_length", xlab = "Churn") #no outliers
gghistogram(customerdata.df, x = "Relation_length", y = "count", 
            color = "#00AFBB",
            ylab = "Count", xlab = "Relation_length", bins = 100) 
sum(customerdata.df$Relation_length == 0) #few relationship lengths being 0

ggboxplot(customerdata.df, x = "Churn", y = "Contract_length", 
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Contract_length", xlab = "Churn") #no outliers
gghistogram(customerdata.df, x = "Contract_length", y = "count", 
            color = "#E7B800",
            ylab = "Count", xlab = "Contract_length", bins = 100) 

ggboxplot(customerdata.df, x = "Churn", y = "Home_age", 
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Home_age", xlab = "Churn") #some old homes, but not impossible
gghistogram(customerdata.df, x = "Home_age", y = "count", 
            color = "#E7B800",
            ylab = "Count", xlab = "Home_age", bins = 100) 

ggboxplot(customerdata.df, x = "Churn", y = "Electricity_usage",
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Electricity_usage", xlab = "Churn") #there are usages of almost 20 times the average
gghistogram(customerdata.df, x = "Electricity_usage", y = "count",
            color = "#00AFBB",
            ylab = "Count", xlab = "Electricity_usage", bins = 100)

ggboxplot(customerdata.df, x = "Churn", y = "Gas_usage", 
          color = "Churn", palette =  c("#00AFBB", "#E7B800"),
          ylab = "Gas_usage", xlab = "Churn") #there are usages of >10 times the average
gghistogram(customerdata.df, x = "Gas_usage", y = "count", 
            color = "#E7B800",
            ylab = "Count", xlab = "Gas usage", bins = 100)

## REGRESSIONS ----

#Gender
#(Test:Chi-Square test & Regression model)
glm.gender <- glm(Churn ~ as.factor(Gender), family=binomial, data=customerdata.df)
summary(glm.gender)

avg.churn.gender <- customerdata.df %>%
  group_by(Gender = customerdata.df$Gender) %>%
  summarise(AverageChurn = mean(customerdata.df$Churn))

chisq.gender <- table(customerdata.df$Gender, customerdata.df$Churn)
chisq.test(chisq.gender)
ggplot(data=avg.churn.gender, aes(x=Gender, y=AverageChurn))+ geom_bar(stat="identity", position=position_dodge())+ theme(axis.title=element_text(size=12))+ ggtitle("Average Churn per gender")+ labs(x="Gender", y="Average Churn")


## Creating the Base model ----------------------------------------------------

### Test Model: Log-regression 100 ----
# Estimation on 100% of sample - just testing - not used further
churn_logtestmodel <- glm(Churn ~  Relation_length_yrs + Contract_length_yrs + Email_list + startchannel_dummy + as.factor(Home_label_rank) + AdjElectricity_usage + AdjGas_usage, family=binomial, data=customerdata.df)
summary(churn_logtestmodel)
#Findings: All but Home Label are significant (< 0.001)

#Estimate odds ratio
odds_ratio <- function (estimate_value){
  print((exp(estimate_value)-1)*100)
}
odds_ratio(as.vector(churn_logtestmodel$coefficients[-1]))
#  [1] -10.92925588 -49.77532093  70.71936673  62.99331212  -7.67315800  -6.48637407
#  [7]  -6.24666682  -4.29862433  -3.70448846 -43.00113159   0.11423921   0.08568111

#Predictions of Churn
churn_predictionmodel1 <- predict(churn_logtestmodel, type = "response", newdata=customerdata.df)


### Base Model: Log-regression 75/25 ----
# Estimation on 75% of sample (25% validation)

#Get a 75% estimation sample and 25% validation sample
set.seed(1234)
customerdata.df$estimation_sample <-rbinom(nrow(customerdata.df), 1, 0.75)

#Estimate the model using only the estimation sample
churn_basemodel <- glm(Churn ~  Relation_length_yrs + Contract_length_yrs + Email_list + startchannel_dummy + as.factor(Home_label_rank) + AdjElectricity_usage + AdjGas_usage, family=binomial, data=customerdata.df, subset=estimation_sample==1)
summary(churn_basemodel)
odds_ratio(-5.898e-01)
#Estimate odds ratio again
odds_ratio(as.vector(churn_basemodel$coefficients[-1]))
#   [1] -10.93853850 -50.47142037  65.57987024  64.55886724 -11.59460940  -5.07355825
#  [7]  -7.65264558   0.33237289  -5.04142228 -40.35776908   0.11870373   0.08684698

#Create a new dataframe with the validation sample and estimation sample
validation_sample <- customerdata.df[customerdata.df$estimation_sample==0,]
estimation_sample <- customerdata.df[customerdata.df$estimation_sample==1,]

#Get predictions for all observations
churn_predictionmodel2 <- predict(churn_basemodel, type = "response", newdata= validation_sample)


### Fit criteria of Base Model ----
library(ROCR)
fitcriteria_calculator(churn_predictionmodel2,validation_sample$Churn) #test if function works well
#### Hit Rate
#Make the basis for the hit rate table
predicted_basemodel <- ifelse(churn_predictionmodel2>.5,1,0)
hit_rate_basemodel <- table(validation_sample$Churn, predicted_basemodel, dnn= c("Observed", "Predicted"))
hit_rate_basemodel #Output is the other way around! [compared to lecture]
#         Predicted
# Observed    0    1
#        0 1768  747
#        1  743 1692
#Findings: 1692 true positives; 743 false negatives; 747 false positives; 1768 true negatives

#Get the hit rate, sensitivity and specificity
((hit_rate_basemodel[1,1]+hit_rate_basemodel[2,2])/sum(hit_rate_basemodel))*100 #hit rate [a+d/(a+b+c+d)]% | hit rate = 69.9%
(hit_rate_basemodel[2,2]/(hit_rate_basemodel[2,2]+hit_rate_basemodel[1,2]))*100 #sensitivity [a/(a+c)]% | positive correctly speicified = 69.37%
(hit_rate_basemodel[1,1]/(hit_rate_basemodel[1,1]+hit_rate_basemodel[2,1]))*100 #specificity [d/(b+d)]% | negative correctly specified = 70.41%

#### Top decile lift (TDL) & GINI
decile_predicted_basemodel <- ntile(predicted_basemodel, 10)
decile_basemodel <- table(validation_sample$Churn, decile_predicted_basemodel, dnn= c("Observed", "Decile"))
decile_basemodel
#         Decile
# Observed   1   2   3   4   5   6   7   8   9  10
#        0 364 352 340 349 333 161 164 138 159 155
#        1 131 143 155 146 162 334 331 357 336 340

#Calculate the TDL
(decile_basemodel[2,10] / (decile_basemodel[1,10]+ decile_basemodel[2,10])) / mean(validation_sample$Churn)
# TDL = 1.396304
# Model is ~40% better than randomly predicting churners

#Make lift curve (ROCR)
pred_basemodel <- prediction(churn_predictionmodel2, validation_sample$Churn)
perf_basemodel <- performance(pred_basemodel,"tpr","fpr")
plot(perf_basemodel,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i", main="Lift Curve: Churn")
abline(0,1, col="red")

auc_basemodel <- performance(pred_basemodel,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_basemodel@y.values)*2-1 # GINI = 0.54


## Creating Stepwise Log-Regression (SLR) Models ----------------------------------------------------
library(MASS) 
#Create new df cleaned from unsuitable variables
# All variables except: Customer_ID (1); Age_group (4); Income (9); Relation_length (12); Contract_length (14); Start_channel (17); Home_label (21); Electricity_usage (23); Gas_usage (25); Province (27); estimation_sample (29)
estimation_model_dataset <- estimation_sample[,-c(1,4,9,12,14,17,21,23,25,27,29)]
validation_model_dataset <- validation_sample[,-c(1,4,9,12,14,17,21,23,25,27,29)]

# make home label as factor so it analyses each label's importance seperately
estimation_model_dataset$Home_label_rank <- as.factor(estimation_model_dataset$Home_label_rank)
validation_model_dataset$Home_label_rank <- as.factor(validation_model_dataset$Home_label_rank)

#Estimate full and null model
lr_full <- glm(Churn ~ ., data = estimation_model_dataset, family = binomial)
lr_null <- glm(Churn ~ 0, data = estimation_model_dataset, family = binomial)

# AIC version: Fitting backward (from full), forward (from null) and both (from full)
slr_backward_AIC <- stepAIC(lr_full, direction="backward", trace = TRUE)
slr_forward_AIC <- stepAIC(lr_null, direction="forward", scope=list(lower=lr_null, upper=lr_full), trace = TRUE)
slr_both_AIC <- stepAIC(lr_full, direction="both", trace = TRUE)
summary(slr_forward_AIC)
# BIC version: Fitting backward (from full), forward (from null) and both (from full)
## To do step-wise regression with the BIC you can add "k = log(n)"
## (where n is the amount of observations on which the model is estimated) to the stepAIC function
# in our case both: "sum(customerdata.df$estimation_sample)" & "count(estimation_model_dataset)" can be used = 15048 observations
slr_backward_BIC <- stepAIC(lr_full, direction="backward", trace = TRUE, k = log(sum(customerdata.df$estimation_sample)))
slr_forward_BIC <- stepAIC(lr_null, direction="forward", scope=list(lower=lr_null, upper=lr_full), trace = TRUE, k = log(sum(customerdata.df$estimation_sample)))
slr_both_BIC <- stepAIC(lr_full, direction="both", trace = TRUE, k = log(sum(customerdata.df$estimation_sample)))

# Predictions from all stepwise models
#AIC MODELS - Backward, Forward, Both
predictions_backward_AIC <- predict(slr_backward_AIC, type = "response", newdata=validation_model_dataset)
predictions_forward_AIC <- predict(slr_forward_AIC, type = "response", newdata=validation_model_dataset)
predictions_both_AIC <- predict(slr_both_AIC, type = "response", newdata=validation_model_dataset)
#BIC MODELS - Backward, Forward, Both
predictions_backward_BIC <- predict(slr_backward_BIC, type = "response", newdata=validation_model_dataset)
predictions_forward_BIC <- predict(slr_forward_BIC, type = "response", newdata=validation_model_dataset)
predictions_both_BIC <- predict(slr_both_BIC, type = "response", newdata=validation_model_dataset)

# Calculate Fit Criteria from all stepwise models
fitcriteria_calculator <- function(predict,IV){
  # USER GUIDE ------------------------------------------------------------------------------
  ## predict: this is your predict() ran based on your lr-model
  ## (e.g., predict(churn_basemodel, type = "response", newdata= validation_sample)

  ## IV_from_validationsample: here specify your IV from the validation sample
  ## (e.g., validation_sample$Churn)
  # -----------------------------------------------------------------------------------------
  tablebase <- ifelse(predict>.5,1,0) #basis for hit rate table & decile ntile()

  # HIT RATE CALCULATIONS
  hitrate_table <- table(IV, tablebase, dnn= c("Observed", "Predicted"))
  hit_rate <- ((hitrate_table[1,1]+hitrate_table[2,2])/sum(hitrate_table))*100
  sensitivity <- (hitrate_table[2,2]/(hitrate_table[2,2]+hitrate_table[1,2]))*100
  specificity <- (hitrate_table[1,1]/(hitrate_table[1,1]+hitrate_table[2,1]))*100

  # TDL CALCULATIONS (10 levels)
  decile_ntile <- ntile(tablebase, 10)
  decile_table <- table(IV, decile_ntile, dnn= c("Observed", "Decile"))
  tdl <- (decile_table[2,10] / (decile_table[1,10]+ decile_table[2,10])) / mean(IV)

  # LIFT CURVE PLOT
  decile_pred <- prediction(predict, IV)
  decile_perf <- performance(decile_pred,"tpr","fpr")
  plot(decile_perf,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i", main="Lift Curve")+abline(0,1, col="red")

  # GINI CALCULATIONS
  auc_perf <- performance(decile_pred,"auc")
  #The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
  #So to get the Gini we do:
  gini <- as.numeric(auc_perf@y.values)*2-1


  #Output
  cat('HIT RATE INFORMATION:','\n','True Negatives:', hitrate_table[1,1], '\n','False Negatives:', hitrate_table[1,2], '\n','True Positives:', hitrate_table[2,2], '\n','False Positives:', hitrate_table[2,1], '\n',
      '\n', 'Hit rate (%): ', hit_rate, '\n', 'Sensitivity (%): ', sensitivity, '\n', 'Specificity (%): ', specificity,'\n','\n',
      'TOP DECILE LIFT INFORMATION:','\n','TDL value:',tdl,'\n','\n',
      'GINI INFORMATION:','\n','GINI value:',gini)
}

#AIC MODELS - Backward, Forward, Both
fitcriteria_calculator(predictions_backward_AIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1904
#  False Negatives: 611
#  True Positives: 1819
#  False Positives: 616
#
#  Hit rate (%):  75.21212
#  Sensitivity (%):  74.85597
#  Specificity (%):  75.55556
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.577002
#
#  GINI INFORMATION:
#  GINI value: 0.6590892

fitcriteria_calculator(predictions_forward_AIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1910
#  False Negatives: 605
#  True Positives: 1817
#  False Positives: 618
#
#  Hit rate (%):  75.29293
#  Sensitivity (%):  75.02064
#  Specificity (%):  75.5538
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.589322
#
#  GINI INFORMATION:
#  GINI value: 0.6580151

fitcriteria_calculator(predictions_both_AIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1904
#  False Negatives: 611
#  True Positives: 1819
#  False Positives: 616
#
#  Hit rate (%):  75.21212
#  Sensitivity (%):  74.85597
#  Specificity (%):  75.55556
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.577002
#
#  GINI INFORMATION:
#  GINI value: 0.6590892

#BIC MODELS - Backward, Forward, Both
fitcriteria_calculator(predictions_backward_BIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1899
#  False Negatives: 616
#  True Positives: 1806
#  False Positives: 629
#
#  Hit rate (%):  74.84848
#  Sensitivity (%):  74.56647
#  Specificity (%):  75.11867
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.572895
#
#  GINI INFORMATION:
#  GINI value: 0.6593642

fitcriteria_calculator(predictions_forward_BIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1898
#  False Negatives: 617
#  True Positives: 1802
#  False Positives: 633
#
#  Hit rate (%):  74.74747
#  Sensitivity (%):  74.49359
#  Specificity (%):  74.99012
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.572895
#
#  GINI INFORMATION:
#  GINI value: 0.6575553

fitcriteria_calculator(predictions_both_BIC,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1899
#  False Negatives: 616
#  True Positives: 1806
#  False Positives: 629
#
#  Hit rate (%):  74.84848
#  Sensitivity (%):  74.56647
#  Specificity (%):  75.11867
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.572895
#
#  GINI INFORMATION:
#  GINI value: 0.6593642

# Summary of best SLR (AIC forward):
summary(slr_forward_AIC)
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)
# Flexibel_contract     2.006e+00  4.408e-02  45.501  < 2e-16 ***
# Home_label_rank1      7.669e-01  3.811e-01   2.013  0.04416 *
# Home_label_rank2      6.632e-01  3.829e-01   1.732  0.08326 .
# Home_label_rank3      7.364e-01  3.788e-01   1.944  0.05192 .
# Home_label_rank4      7.128e-01  3.763e-01   1.894  0.05819 .
# Home_label_rank5      7.969e-01  3.736e-01   2.133  0.03293 *
# Home_label_rank6      7.500e-01  3.725e-01   2.013  0.04410 *
# Home_label_rank7      1.641e-01  3.757e-01   0.437  0.66231
# AdjElectricity_usage  1.356e-03  4.261e-05  31.814  < 2e-16 ***
# Relation_length_yrs  -1.345e-01  6.490e-03 -20.729  < 2e-16 ***
# AdjGas_usage          9.877e-04  4.851e-05  20.363  < 2e-16 ***
# AdjLogIncome         -7.661e-01  3.996e-02 -19.170  < 2e-16 ***
# Email_list            5.626e-01  6.167e-02   9.123  < 2e-16 ***
# startchannel_dummy    5.983e-01  6.920e-02   8.646  < 2e-16 ***
# HighIncome_dummy      1.053e+00  3.424e-01   3.075  0.00210 **
# Age                   6.450e-03  2.462e-03   2.620  0.00878 **
# AgeGroup_2344         9.458e-02  6.074e-02   1.557  0.11945



## Estimate a CART tree ----------------------------------------------------
library(rpart)
library(partykit)
# Tree model 1
CART_tree_churn1 <- rpart(Churn ~ ., data=estimation_model_dataset, method="class")
CART_tree_churn1_visual <- as.party(CART_tree_churn1)
plot(CART_tree_churn1_visual , type="simple", gp = gpar(fontsize = 10))
#Save predictions
predictions_cart1 <- predict(CART_tree_churn1, newdata=validation_model_dataset, type ="prob")[,2]
#Fit criteria for tree 1
fitcriteria_calculator(predictions_cart1,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1886
#  False Negatives: 629
#  True Positives: 1647
#  False Positives: 788
#
#  Hit rate (%):  71.37374
#  Sensitivity (%):  72.3638
#  Specificity (%):  70.53104
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.498973
#
#  GINI INFORMATION:
#  GINI value: 0.4957948

# Tree model 2
newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)
CART_tree_churn2 <- rpart(Churn ~ ., data=estimation_model_dataset, method="class", control=newsettings1)
CART_tree_churn2_visual <- as.party(CART_tree_churn2)
plot(CART_tree_churn2_visual , type="simple")
#Save predictions
predictions_cart2 <- predict(CART_tree_churn2, newdata=validation_model_dataset, type ="prob")[,2]
#Fit criteria for tree 2
fitcriteria_calculator(predictions_cart2,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1791
#  False Negatives: 724
#  True Positives: 1698
#  False Positives: 737
#
#  Hit rate (%):  70.48485
#  Sensitivity (%):  70.10735
#  Specificity (%):  70.84652
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.462012
#
#  GINI INFORMATION:
#  GINI value: 0.492142


## Bagging -----------------------------------------------------------------
library(ipred)
library(caret)

newsettings2 <- rpart.control(minsplit = 2, cp = 0.0)
#Essentially these two arguments allow the individual trees to grow extremely deep, which leads to trees with high variance but low bias. Then when we apply bagging we're able to reduce the variance of the final model while keeping the bias low.

#estimate model with bagging - WARNING: TAKES A LONG TIME WITH nbagg=100
Bagging_tree100 <- train(as.factor(Churn) ~ ., data=estimation_model_dataset, method="treebag", nbagg=100, trControl = trainControl(method = "cv", number = 10), control=newsettings2)

#Save predictions
predictions_bagging100 <- predict(Bagging_tree1, newdata=validation_model_dataset, type ="prob")[,2]
#Fit criteria for bagging model - nbags 100
fitcriteria_calculator(predictions_bagging100,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1889
#  False Negatives: 626
#  True Positives: 1787
#  False Positives: 648
#
#  Hit rate (%):  74.26263
#  Sensitivity (%):  74.05719
#  Specificity (%):  74.45802
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.548255
#
#  GINI INFORMATION:
#  GINI value: 0.6322807

#calculate variable importance
pred.imp1 <- varImp(Bagging_tree100)
pred.imp1
#   only 20 most important variables shown (out of 22)
#
#                      Overall
# AdjElectricity_usage 100.000
# AdjGas_usage          93.197
# AdjLogIncome          79.376
# Relation_length_yrs   78.586
# Home_age              58.621
# Age                   57.215
# AgeGroup_4568          7.917
# AgeGroup_2344          7.567
# Home_label_rank4       7.034
# Home_label_rank5       6.764
# Home_label_rank3       6.316
# Contract_length_yrs   55.718
# Flexibel_contract     24.748
# Email_list            18.286
# startchannel_dummy    17.206
# Gender                11.401
# Home_label_rank7      10.942
# Home_label_rank6       5.367
# Home_label_rank2       4.552
# AgeGroup_68up          3.016

#You can also plot the results
barplot(pred.imp1$importance$Overall, names.arg = row.names(pred.imp1$importance))



## Boosting -----------------------------------------------------------------
library(gbm)
#Estimate the model - WARNING: n.trees = 10000 may take some time
boost_tree1 <- gbm(Churn ~ ., data=estimation_model_dataset, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)

#Get model output (summary also provides a graph)
boost_tree1
# gbm(formula = Churn ~ ., distribution = "bernoulli", data = estimation_model_dataset,
#     n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with bernoulli loss function.
# 10000 iterations were performed.
# There were 17 predictors of which 17 had non-zero influence.
summary(boost_tree1)
#                                      var     rel.inf
# AdjElectricity_usage AdjElectricity_usage 20.50774911
# AdjGas_usage                 AdjGas_usage 16.67818215
# AdjLogIncome                 AdjLogIncome 12.27450411
# Contract_length_yrs   Contract_length_yrs 12.12663925
# Relation_length_yrs   Relation_length_yrs 11.08802990
# Flexibel_contract       Flexibel_contract  9.06336962
# Home_age                         Home_age  5.07124901
# Age                                   Age  4.08447464
# Home_label_rank           Home_label_rank  4.01368802
# startchannel_dummy     startchannel_dummy  2.23337241
# Email_list                     Email_list  1.91399049
# Gender                             Gender  0.40138553
# HighIncome_dummy         HighIncome_dummy  0.24714687
# AgeGroup_4568               AgeGroup_4568  0.19304552
# AgeGroup_2344               AgeGroup_2344  0.06402358
# AgeGroup_022                 AgeGroup_022  0.02398789
# AgeGroup_68up               AgeGroup_68up  0.01516191

best.iter1 <- gbm.perf(boost_tree1, method = "OOB")
summary(boost_tree1, n.trees = best.iter1)
#                                       var      rel.inf
# AdjElectricity_usage AdjElectricity_usage 24.419484172
# AdjGas_usage                 AdjGas_usage 17.058078288
# Contract_length_yrs   Contract_length_yrs 16.584320859
# Flexibel_contract       Flexibel_contract 16.304495686
# Relation_length_yrs   Relation_length_yrs 11.298571875
# AdjLogIncome                 AdjLogIncome  6.760916613
# startchannel_dummy     startchannel_dummy  3.622906753
# Email_list                     Email_list  2.997717002
# Home_label_rank           Home_label_rank  0.465852047
# Age                                   Age  0.209115808
# Home_age                         Home_age  0.179095288
# HighIncome_dummy         HighIncome_dummy  0.077906141
# AgeGroup_4568               AgeGroup_4568  0.009343477
# Gender                             Gender  0.007249882
# AgeGroup_2344               AgeGroup_2344  0.004946109
# AgeGroup_022                 AgeGroup_022  0.000000000
# AgeGroup_68up               AgeGroup_68up  0.000000000

#Save predictions
predictions_boost1 <- predict(boost_tree1, newdata=validation_model_dataset, n.trees = best.iter1, type ="response")
#Fit criteria for boosting model 1
fitcriteria_calculator(predictions_boost1,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1921
#  False Negatives: 594
#  True Positives: 1824
#  False Positives: 611
#
#  Hit rate (%):  75.65657
#  Sensitivity (%):  75.43424
#  Specificity (%):  75.86888
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.577002
#
#  GINI INFORMATION:
#  GINI value: 0.6744207



## Random forest -----------------------------------------------------------
library(randomForest)
Random_forest1 <- randomForest(as.factor(Churn) ~ ., data=estimation_model_dataset, importance=TRUE)
Random_forest1000 <- randomForest(as.factor(Churn) ~ ., data=estimation_model_dataset,
                                                       ntree=1000, mtry=3, nodesize=1, maxnodes=100, importance=TRUE)

predictions_forest1 <- predict(Random_forest1, newdata=validation_model_dataset, type ="prob")[,2]
predictions_forest10000 <- predict(Random_forest10000, newdata=validation_model_dataset, type ="prob")[,2]



#Fit criteria for forest model 1
fitcriteria_calculator(predictions_forest1,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1893
#  False Negatives: 622
#  True Positives: 1827
#  False Positives: 608
#
#  Hit rate (%):  75.15152
#  Sensitivity (%):  74.60188
#  Specificity (%):  75.68972
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.572895
#
#  GINI INFORMATION:
#  GINI value: 0.6530623


#Fit criteria for forest model 1000
fitcriteria_calculator(predictions_forest1000,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1966
#  False Negatives: 549
#  True Positives: 1715
#  False Positives: 720
#
#  Hit rate (%):  74.36364
#  Sensitivity (%):  75.75088
#  Specificity (%):  73.19434
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.544148
#
#  GINI INFORMATION:
#  GINI value: 0.6420478

#Most important variables (based on MDAccuracy and MDGINI):
varImpPlot(Random_forest1)
varImpPlot(Random_forest10000) #





## Support Vector Machine (SVM) --------------------------------------------------
library(e1071)
### SVM - Linear Kernel ----
# WARNING: may take a while to run this
svm_linear <- svm(Churn ~ AdjElectricity_usage + AdjGas_usage + Relation_length_yrs + AdjLogIncome + startchannel_dummy + Flexibel_contract + Contract_length_yrs, data = estimation_model_dataset,
             type = 'C-classification', probability = TRUE,
             kernel = 'linear')

# Couldn't find a plot that made sense
plot(svm_linear, estimation_model_dataset, AdjElectricity_usage~Contract_length_yrs)

#Get predictions
predictions_svm_linear <- predict(svm_linear, newdata=validation_model_dataset, probability=TRUE)
predictions_svm_linear <- attr(predictions_svm_linear,"probabilities")[,1]

#Fit criteria for SVM Linear Kernel model
fitcriteria_calculator(predictions_svm_linear,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1906
#  False Negatives: 609
#  True Positives: 1797
#  False Positives: 638
#
#  Hit rate (%):  74.80808
#  Sensitivity (%):  74.68828
#  Specificity (%):  74.92138
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.577002
#
#  GINI INFORMATION:
#  GINI value: 0.6526043


### SVM - RBF (Gaussian Radial Basis Function) Kernel ----
# WARNING: may take a while to run this
svm_rbf <- svm(Churn ~ AdjElectricity_usage + AdjGas_usage + Relation_length_yrs + AdjLogIncome + startchannel_dummy + Flexibel_contract + Contract_length_yrs, data = estimation_model_dataset,
               type = 'C-classification', probability = TRUE,
               kernel = 'radial')

plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~AdjGas_usage)
plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~Relation_length_yrs)
plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~AdjLogIncome)# <--- People with high electricity usage and low to medium income are more likely to churn
plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~startchannel_dummy)
plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~Flexibel_contract)
plot(svm_rbf, estimation_model_dataset, AdjElectricity_usage~Contract_length_yrs)

plot(svm_rbf, estimation_model_dataset, AdjGas_usage~Relation_length_yrs)
plot(svm_rbf, estimation_model_dataset, AdjGas_usage~AdjLogIncome)
plot(svm_rbf, estimation_model_dataset, AdjGas_usage~startchannel_dummy)
plot(svm_rbf, estimation_model_dataset, AdjGas_usage~Flexibel_contract)
plot(svm_rbf, estimation_model_dataset, AdjGas_usage~Contract_length_yrs)

plot(svm_rbf, estimation_model_dataset, Relation_length_yrs~AdjLogIncome)
plot(svm_rbf, estimation_model_dataset, Relation_length_yrs~startchannel_dummy)
plot(svm_rbf, estimation_model_dataset, Relation_length_yrs~Flexibel_contract)
plot(svm_rbf, estimation_model_dataset, Relation_length_yrs~Contract_length_yrs)

plot(svm_rbf, estimation_model_dataset, AdjLogIncome~startchannel_dummy)
plot(svm_rbf, estimation_model_dataset, AdjLogIncome~Flexibel_contract)
plot(svm_rbf, estimation_model_dataset, AdjLogIncome~Contract_length_yrs)

plot(svm_rbf, estimation_model_dataset, startchannel_dummy~Flexibel_contract)
plot(svm_rbf, estimation_model_dataset, startchannel_dummy~Contract_length_yrs)

plot(svm_rbf, estimation_model_dataset, Flexibel_contract~Contract_length_yrs)

summary(svm_rbf)
#Get predictions
predictions_svm_rbf <- predict(svm_rbf, newdata=validation_model_dataset, probability=TRUE)
predictions_svm_rbf <- attr(predictions_svm_rbf,"probabilities")[,1]

#Fit criteria for SVM RBF Kernel model
fitcriteria_calculator(predictions_svm_rbf,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1942
#  False Negatives: 573
#  True Positives: 1794
#  False Positives: 641
#
#  Hit rate (%):  75.47475
#  Sensitivity (%):  75.79214
#  Specificity (%):  75.18389
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.572895
#
#  GINI INFORMATION:
#  GINI value: 0.6403832



### SVM - Polynomial Kernel ----
# WARNING: may take a while to run this
svm_polynomial <- svm(Churn ~ AdjElectricity_usage + AdjGas_usage + Relation_length_yrs + AdjLogIncome + startchannel_dummy + Flexibel_contract + Contract_length_yrs, data = estimation_model_dataset,
             type = 'C-classification', probability = TRUE,
             kernel = 'polynomial')

plot(svm_polynomial, estimation_model_dataset, AdjElectricity_usage~Contract_length_yrs)

#Get predictions
predictions_svm_poly <- predict(svm_polynomial, newdata=validation_model_dataset, probability=TRUE)
predictions_svm_poly <- attr(predictions_svm_poly,"probabilities")[,1]

#Fit criteria for SVM Polynomial Kernel model
fitcriteria_calculator(predictions_svm_poly,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 2015
#  False Negatives: 500
#  True Positives: 1656
#  False Positives: 779
#
#  Hit rate (%):  74.16162
#  Sensitivity (%):  76.80891
#  Specificity (%):  72.11883
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.577002
#
#  GINI INFORMATION:
#  GINI value: 0.6249382


### SVM - Sigmoid Kernel ----
# WARNING: may take a while to run this
svm_sigmoid <- svm(Churn ~ AdjElectricity_usage + AdjGas_usage + Relation_length_yrs + AdjLogIncome + startchannel_dummy + Flexibel_contract + Contract_length_yrs, data = estimation_model_dataset,
               type = 'C-classification', probability = TRUE,
               kernel = 'sigmoid')

plot(svm_sigmoid, estimation_model_dataset, AdjElectricity_usage~Contract_length_yrs)

#Get predictions
predictions_svm_sigmoid <- predict(svm_sigmoid, newdata=validation_model_dataset, probability=TRUE)
predictions_svm_sigmoid <- attr(predictions_svm_sigmoid,"probabilities")[,1]

#Fit criteria for SVM Sigmoid Kernel model
fitcriteria_calculator(predictions_svm_sigmoid,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 1702
#  False Negatives: 813
#  True Positives: 1594
#  False Positives: 841
#
#  Hit rate (%):  66.58586
#  Sensitivity (%):  66.22351
#  Specificity (%):  66.92882
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.416838
#
#  GINI INFORMATION:
#  GINI value: 0.3747619



## Artificial Neural Networks ----------------------------------------------
library(neuralnet)
# Fit neural network 1
# WARNING: Running with these 3 variables takes a long time
# I ran the top 3 variables based on all the previous ML techniques
ANN_1_ElecGasIncome <- neuralnet(Churn ~ AdjElectricity_usage + AdjGas_usage + AdjLogIncome, data = estimation_model_dataset, hidden=c(3,2), stepmax=1e+06, act.fct = "logistic",
                   linear.output = FALSE)

plot(ANN_1_ElecGasIncome)

# get model predictions
temp_test <- subset(validation_model_dataset, select = c("Churn", "AdjElectricity_usage", "AdjGas_usage", "AdjLogIncome"))
head(temp_test)
#    Churn AdjElectricity_usage AdjGas_usage AdjLogIncome
# 5      1                 2983         1120     7.670429
# 14     0                 1976         1714     9.044758
# 16     1                 2034         2180     8.679482
# 26     1                 3118         2111     8.935640
# 28     1                 2151         1580     8.236421
# 29     1                 2172         1509     9.041685

nn.results <- neuralnet::compute(ANN_1_ElecGasIncome, temp_test)
nn.results <- nn.results$net.result[,1]

#Fit criteria for ANN 1
fitcriteria_calculator(nn.results,validation_model_dataset$Churn)
# HIT RATE INFORMATION:
#  True Negatives: 671
#  False Negatives: 1844
#  True Positives: 2092
#  False Positives: 343
#
#  Hit rate (%):  55.81818
#  Sensitivity (%):  53.15041
#  Specificity (%):  66.17357
#
#  TOP DECILE LIFT INFORMATION:
#  TDL value: 1.043121
#
#  GINI INFORMATION:
#  GINI value: 0.1272472