### Master Thesis: Willingness to Share Health Data in Italy: Investigation under a Multi-context Paradigm with DCE data.
### George Dreemer
### MSc Marketing Analytics & Data Science
# NOTE: All outputs are provided in comments after each code snippet for ease of audit.

rm(list = ls()) #clean workspace

# Preliminary work: packages, data and checks ----
## Install & Load libraries (librarian) ----
chooseCRANmirror(ind = 29) #set CRAN mirror to Netherlands for this session
install.packages("librarian")

# Main Installations & Loads (shelf)
librarian::shelf(data.table,factoextra, poLCA, FLXMRbinary,flexmix,lcmm,mlogit,gmnl,psych, corrplot, rstatix,dplyr,ggplot2,RColorBrewer,car,nortest,tibble,stargazer,tidyr,stats,officer,bannerCommenter,janitor,crayon, quiet = TRUE)



## Load data: dce + survey ----
setwd('')
dce.df <- read.csv('#DATA/clean-dce_data.csv', sep=';', na.strings = c('-999'))
survey.df <- read.csv('#DATA/clean-survey_data.csv', sep=';', na.strings = c('-999'))

### Categorical variable value logic deciphering ----

## Age category ranges
aggregate(AGE ~ AGE_cat, data = survey.df, FUN = function(x) c(min(x), max(x)))
#   AGE_cat AGE.1 AGE.2
# 1       1    19    39
# 2       2    40    59
# 3       3    60    91
# so 1 = 19-39; 2 = 40-59; 3 = 60-91

## E_HL_cat deciphering
aggregate(E_HL_cat ~ E_HEALTH_1, data = survey.df, FUN = function(x) c(min(x), max(x)))
#   E_HEALTH_1 E_HL_cat.1
# 1          1          3          <- 3: Inadequate eHL
# 2          2          3
# 3          3          2          <- 2: Problematic eHL
# 4          4          1
# 5          5          1          <- 1: Sufficient eHL

# Note: I used the Johansson et al. paper on page 4 right in the beginning of Desc. Stat
# to create these names for 1-3, also to understand the logic.

## Type information / Scenarios (1-4)
str(as.factor(dce.df$typ_information))
#Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 4 4 ...
# 1 =  lifestyle information
# 2 = physical health
# 3 = mental health
# 4 = genetic information


###########################################################################
###########################################################################
###                                                                     ###
###                             DATA CHECKS                             ###
###                                                                     ###
###########################################################################
###########################################################################
## Data checks ----


##################################################################
##                        General checks                        ##
##################################################################
### Structure and vartype check ----
str(dce.df); str(survey.df) # ✅

### Vartype fixes ----

# Convert "DURATION" variable from character to numeric
survey.df$DURATION <- as.numeric(survey.df$DURATION); dce.df$DURATION <- as.numeric(dce.df$DURATION)

# Convert "DURATION_min" variable from character to numeric
survey.df$DURATION_min <- as.numeric(survey.df$DURATION_min); dce.df$DURATION_min <- as.numeric(dce.df$DURATION_min)

# Convert "GENDER" variable from int to factor
survey.df$GENDER <- as.factor(survey.df$GENDER); dce.df$GENDER <- as.factor(dce.df$GENDER);

### Values check ----
summary(dce.df); summary(survey.df) # ✅
# notes:
# choice | mean: 68.5%
# there are NAs

# Fix: recalculate DURATION_min for any rows with NAs
survey.df$DURATION_min[which(is.na(survey.df$DURATION_min))] <- survey.df$DURATION[which(is.na(survey.df$DURATION_min))] / 60
dce.df$DURATION_min[which(is.na(dce.df$DURATION_min))] <- dce.df$DURATION[which(is.na(dce.df$DURATION_min))] / 60



##################################################################
##                           NA check                           ##
##################################################################
### NA check ----

#### in dce.df ----
# 1: Explore where the NAs are (TRUE = there are NAs)
which(apply(dce.df, 2, function(x) any(is.na(x))))
# There are NAs in: GEN_dum, CHR_dum, TRUST_HELPFUL, TRUST_FAIR, TRUST_TRUSTED

# 2: Calculate the percentage of missing data per column
100 * colMeans(is.na(dce.df))
#  GEN_dum: 0.61%
#  CHR_dum: 3.04%
#  TRUST_HELPFUL: 3.34%
#  TRUST_FAIR: 3.95%
#  TRUST_TRUSTED: 2.74%

# 3: Visualize
# create a dataframe with variable names and values
dce_na <- data.frame(variable = c("GEN_dum", "CHR_dum", "TRUST_HELPFUL", "TRUST_FAIR", "TRUST_TRUSTED"),
                     value = c(0.61, 3.04, 3.34, 3.95, 2.74))

# plot the % of NAs per variable
ggplot(dce_na, aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = rainbow(count(dce_na))) +
  geom_text(aes(label = paste0(value)), vjust = -0.5) + # add value labels
  xlab("Variable Name") +
  ylab("% of NAs") +
  labs(
    title = "Percentage of NAs in Survey data",
    subtitle = 'Displaying only the columns with NAs present.',
    #caption = "Source: Gapminder dataset",
    x = "Variable Name",
    y = "% of NAs"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
rm(dce_na) # cleanup

#### in survey.df ----
# 1: Explore where the NAs are
which(apply(survey.df, 2, function(x) any(is.na(x))))
# There are NAs in:
# TRUST_TRUSTED, TRUST_FAIR, TRUST_HELPFUL,
# health_info_2aDCE, AGE_check, AGE_cat, AGE_dum,
# MALE, E_HL_cat, E_HL_dum, EDUCATION_check,EDU_cat,
# EDU_dum, GEN_dum, CHR_dum, DI_cat, DI_dum, DURATION_min

# 2: Calculate the percentage of missing data per column
100 * colMeans(is.na(survey.df))
# TRUST_TRUSTED: 2.74%
# TRUST_FAIR: 3.95%
# TRUST_HELPFUL: 3.34%
# health_info_2aDCE: 0.30%
# AGE_check: 0.30%
# AGE_cat: 0.30%
# AGE_dum: 0.30%
# MALE: 0.30%
# E_HL_cat: 0.30%
# E_HL_dum: 0.30%
# EDUCATION_check: 0.30%
# EDU_cat: 0.30%
# EDU_dum: 0.30%
# GEN_dum: 0.91%
# CHR_dum: 3.34%
# DI_cat: 0.30%
# DI_dum: 0.30%
# DURATION_min: 0.30%

# 3: Visualize
# create a dataframe with variable names and values
survey_na <- data.frame(variable = c("TRUST_TRUSTED", "TRUST_FAIR", "TRUST_HELPFUL", "health_info_2aDCE", "AGE_check",
                              "AGE_cat", "AGE_dum", "MALE", "E_HL_cat", "E_HL_dum", "EDUCATION_check", "EDU_cat",
                              "EDU_dum", "GEN_dum", "CHR_dum", "DI_cat", "DI_dum", "DURATION_min"),
                 value = c(2.7355623, 3.9513678, 3.3434650, 0.3039514, 0.3039514, 0.3039514, 0.3039514, 0.3039514,
                           0.3039514, 0.3039514, 0.3039514, 0.3039514, 0.3039514, 0.9118541, 3.3434650, 0.3039514,
                           0.3039514, 0.3039514))

# plot the % of NAs per variable
ggplot(survey_na, aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat = "identity", fill = rainbow(count(survey_na))) +
  geom_text(aes(label = round(value, 2)), vjust = -0.5) + # add value labels
  xlab("Variable Name") +
  ylab("% of NAs") +
  labs(
    title = "Percentage of NAs in Sample",
    subtitle = 'Displaying as percentage of total sample.',
    #caption = "Source: Gapminder dataset",
    x = "Variable Name",
    y = "% of NAs"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
rm(survey_na) # cleanup


##################################################################
##                        Outlier checks                        ##
##################################################################
### Outlier checks ----
{
#### for dce.df ----
n <- 0 #<-run everytime with loop (counter)
for (var in dce.df) {
  strexcluder <- is.character(var) # checks if the variable is a chr type/string
  factorexcluder <- is.factor(var) # checks if the variable is a factor
  dummyexcluder <- length(unique(var)) # counts the unique values of the variable (if it's 2 it assumes it's a dummy)
  n <- n + 1 # we account for each itteration, then:
  varname <- colnames(dce.df[n]) # use it here to create the variable names

  # filters out string, factor and dummy variables
  if (strexcluder == FALSE && factorexcluder == FALSE && dummyexcluder > 2) {

    outliers <- boxplot.stats(var)$out # the outlier values are stored here
    outliercounter <- length(boxplot.stats(var)$out) # counts how many outliers there are
    outlierrows <- which(var %in% c(outliers)) # says which rows in the original dataframe the outliers are in

    # checks for outliers
    if (outliercounter == 0) {
      # console announcement:
      cat('>  \u2705  <',"No outliers found in variable ","\u25B8",varname,"\u25C2",'\n')
    } else {
      # creates the vectors with outlier values, named in the format: original varname + .outliers
      assign(paste0(varname,'.outliers'), outliers) #alt. to paste0: paste(varname, 'outliers', sep='.')
      # console announcement:
      cat('>  \u26D4  <',outliercounter,"outliers found in variable ","\u25B8",varname,"\u25C2",'\n')
      #cat(outliercounter,"outliers found, in rows:",outlierrows,'\n','With values: ',outliers,'\n','\n')
      #boxplot(var)
    }

  } else { # if the variable is a string, factor or dummy:
    # console announcement:
    cat('>  \u274E  <','Skipped because variable ',"\u25B8",varname,"\u25C2",' is a factor, boolean or a string.','\n')
  }
  # cleanup - surpressing warnings since they are not applicable
  suppressWarnings({rm(dummyexcluder); rm(strexcluder); rm(factorexcluder); rm(outliers); rm(outliercounter); rm(outlierrows); rm(var)})
}
#cleanup after loop
rm(varname); rm(n)
# Output:
# >  ✅  < No outliers found in variable  ▸ ID ◂
# >  ✅  < No outliers found in variable  ▸ DESIGN_ROW ◂
# >  ✅  < No outliers found in variable  ▸ SEQ ◂
# >  ✅  < No outliers found in variable  ▸ typ_information ◂
# >  ❎  < Skipped because variable  ▸ DCEno ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ choice ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ C_Tec ◂
# >  ✅  < No outliers found in variable  ▸ C_Res ◂
# >  ✅  < No outliers found in variable  ▸ U_Tec ◂
# >  ✅  < No outliers found in variable  ▸ U_Pha ◂
# >  ✅  < No outliers found in variable  ▸ U_Res ◂
# >  ⛔  < 2616 outliers found in variable  ▸ R_Dev ◂
# >  ⛔  < 2602 outliers found in variable  ▸ R_Pro ◂
# >  ✅  < No outliers found in variable  ▸ R_Pol ◂
# >  ✅  < No outliers found in variable  ▸ I_Not ◂
# >  ✅  < No outliers found in variable  ▸ I_Inf ◂
# >  ✅  < No outliers found in variable  ▸ I_Opt ◂
# >  ✅  < No outliers found in variable  ▸ R_No ◂
# >  ✅  < No outliers found in variable  ▸ R_Tra ◂
# >  ❎  < Skipped because variable  ▸ GENDER ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ MALE ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ AGE ◂
# >  ✅  < No outliers found in variable  ▸ AGE_cat ◂
# >  ❎  < Skipped because variable  ▸ AGE_dum ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ EDUCATION ◂
# >  ✅  < No outliers found in variable  ▸ EDU_cat ◂
# >  ❎  < Skipped because variable  ▸ EDU_dum ◂  is a factor, boolean or a string.
# >  ⛔  < 192 outliers found in variable  ▸ GENERAL_HEALTH ◂
# >  ✅  < No outliers found in variable  ▸ GEN_dum ◂
# >  ⛔  < 16 outliers found in variable  ▸ CHRONIC_CONDITION ◂
# >  ✅  < No outliers found in variable  ▸ CHR_dum ◂
# >  ✅  < No outliers found in variable  ▸ DI_cat ◂
# >  ❎  < Skipped because variable  ▸ DI_dum ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ E_HL_cat ◂
# >  ❎  < Skipped because variable  ▸ E_HL_dum ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ TRUST_HELPFUL ◂
# >  ✅  < No outliers found in variable  ▸ TRUST_FAIR ◂
# >  ✅  < No outliers found in variable  ▸ TRUST_TRUSTED ◂
# >  ⛔  < 544 outliers found in variable  ▸ DURATION_min ◂
# >  ⛔  < 96 outliers found in variable  ▸ FEEDBACK_UNDERSTAND ◂


#### for survey.df ----
n <- 0 #<-run everytime with loop (counter)
for (var in survey.df) {
  strexcluder <- is.character(var) # checks if the variable is a chr type/string
  factorexcluder <- is.factor(var) # checks if the variable is a factor
  dummyexcluder <- length(unique(var)) # counts the unique values of the variable (if it's 2 it assumes it's a dummy)
  n <- n + 1 # we account for each itteration, then:
  varname <- colnames(survey.df[n]) # use it here to create the variable names

  # filters out string, factor and dummy variables
  if (strexcluder == FALSE && factorexcluder == FALSE && dummyexcluder > 2) {

    outliers <- boxplot.stats(var)$out # the outlier values are stored here
    outliercounter <- length(boxplot.stats(var)$out) # counts how many outliers there are
    outlierrows <- which(var %in% c(outliers)) # says which rows in the original dataframe the outliers are in

    # checks for outliers
    if (outliercounter == 0) {
      # console announcement:
      cat('>  \u2705  <',"No outliers found in variable ","\u25B8",varname,"\u25C2",'\n')
    } else {
      # creates the vectors with outlier values, named in the format: original varname + .outliers
      assign(paste0(varname,'.outliers'), outliers) #alt. to paste0: paste(varname, 'outliers', sep='.')
      # console announcement:
      cat('>  \u26D4  <',outliercounter,"outliers found in variable ","\u25B8",varname,"\u25C2",'\n')
      #cat(outliercounter,"outliers found, in rows:",outlierrows,'\n','With values: ',outliers,'\n','\n')
      #boxplot(var)
    }

  } else { # if the variable is a string, factor or dummy:
    # console announcement:
    cat('>  \u274E  <','Skipped because variable ',"\u25B8",varname,"\u25C2",' is a factor, boolean or a string.','\n')
  }
  # cleanup - surpressing warnings since they are not applicable
  suppressWarnings({rm(dummyexcluder); rm(strexcluder); rm(factorexcluder); rm(outliers); rm(outliercounter); rm(outlierrows); rm(var)})
}
#cleanup after loop
rm(varname); rm(n)
# Output:
# >  ❎  < Skipped because variable  ▸ DEVICE_Tablet ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ DEVICE_non ◂  is a factor, boolean or a string.
# >  ⛔  < 1 outliers found in variable  ▸ APP_USE ◂
# >  ✅  < No outliers found in variable  ▸ HOW_OFTEN_APP ◂
# >  ✅  < No outliers found in variable  ▸ health_info ◂
# >  ⛔  < 12 outliers found in variable  ▸ GENERAL_HEALTH ◂
# >  ⛔  < 1 outliers found in variable  ▸ CHRONIC_CONDITION ◂
# >  ⛔  < 16 outliers found in variable  ▸ TRUST_TECH ◂
# >  ⛔  < 11 outliers found in variable  ▸ TRUST_PHA ◂
# >  ⛔  < 11 outliers found in variable  ▸ TRUST_RES ◂
# >  ⛔  < 8 outliers found in variable  ▸ TRUST_AUT ◂
# >  ⛔  < 16 outliers found in variable  ▸ TRUST_LEG ◂
# >  ✅  < No outliers found in variable  ▸ TRUST_TRUSTED ◂
# >  ✅  < No outliers found in variable  ▸ TRUST_FAIR ◂
# >  ✅  < No outliers found in variable  ▸ TRUST_HELPFUL ◂
# >  ❎  < Skipped because variable  ▸ TRACK_COVID ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ TRACK_GOV ◂
# >  ⛔  < 10 outliers found in variable  ▸ NEW_experiment ◂
# >  ✅  < No outliers found in variable  ▸ NEW_first ◂
# >  ⛔  < 13 outliers found in variable  ▸ NEW_hesitant ◂
# >  ⛔  < 15 outliers found in variable  ▸ NEW_like ◂
# >  ⛔  < 4 outliers found in variable  ▸ USEFUL_INTERNET ◂
# >  ⛔  < 4 outliers found in variable  ▸ IMPORTANT_INTERNET ◂
# >  ⛔  < 9 outliers found in variable  ▸ E_HEALTH_1 ◂
# >  ⛔  < 7 outliers found in variable  ▸ E_HEALTH_2 ◂
# >  ⛔  < 7 outliers found in variable  ▸ E_HEALTH_3 ◂
# >  ⛔  < 7 outliers found in variable  ▸ E_HEALTH_4 ◂
# >  ⛔  < 7 outliers found in variable  ▸ E_HEALTH_5 ◂
# >  ⛔  < 12 outliers found in variable  ▸ E_HEALTH_6 ◂
# >  ⛔  < 8 outliers found in variable  ▸ E_HEALTH_7 ◂
# >  ⛔  < 18 outliers found in variable  ▸ E_HEALTH_8 ◂
# >  ⛔  < 6 outliers found in variable  ▸ FEEDBACK_UNDERSTAND ◂
# >  ⛔  < 6 outliers found in variable  ▸ FEEDBACK_DIFFICULT ◂
# >  ❎  < Skipped because variable  ▸ FEEDBACK_TEXT ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ health_info_2aDCE ◂
# >  ✅  < No outliers found in variable  ▸ AGE_check ◂
# >  ✅  < No outliers found in variable  ▸ AGE_cat ◂
# >  ✅  < No outliers found in variable  ▸ AGE_dum ◂
# >  ✅  < No outliers found in variable  ▸ MALE ◂
# >  ✅  < No outliers found in variable  ▸ E_HL_cat ◂
# >  ✅  < No outliers found in variable  ▸ E_HL_dum ◂
# >  ✅  < No outliers found in variable  ▸ EDUCATION_check ◂
# >  ✅  < No outliers found in variable  ▸ EDU_cat ◂
# >  ✅  < No outliers found in variable  ▸ EDU_dum ◂
# >  ✅  < No outliers found in variable  ▸ GEN_dum ◂
# >  ✅  < No outliers found in variable  ▸ CHR_dum ◂
# >  ✅  < No outliers found in variable  ▸ DI_cat ◂
# >  ⛔  < 59 outliers found in variable  ▸ DI_dum ◂
# >  ⛔  < 34 outliers found in variable  ▸ DURATION_min ◂
}



#### Explore Outliers in Duration ----
# Explore the outlier values to determine possible reason or decide for treatment.

summary(survey.df$DURATION_min)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    5.05    8.06   11.27   21.41   16.62 2022.94
summary(DURATION_min.outliers)
#       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   30.37   33.13   39.34  104.01   51.48 2022.94
str(DURATION_min.outliers)

# Visualize
ggplot(data.frame(DURATION_min.outliers), aes(DURATION_min.outliers)) +
  geom_histogram(color = "#000000", fill = "orange") +
  labs(
    title = "Histogram of Outliers: Duration in Minutes",
    subtitle = paste("\u2022", "Min: ", round(min(DURATION_min.outliers), 2), "    \u2022", "Mean: ",  round(mean(DURATION_min.outliers), 2), "    \u2022", "Max: ", round(max(DURATION_min.outliers), 2)),
    #caption = "Source:",
    x = "Duration (min)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


# Limited x-axis between 0 -> 150
ggplot(data.frame(DURATION_min.outliers), aes(DURATION_min.outliers)) +
  geom_histogram(color = "#000000", fill = "orange") +
  labs(
    title = "Histogram of Outliers: Duration in Minutes",
    subtitle = paste("\u2022", "Min: ", round(min(DURATION_min.outliers), 2), "    \u2022", "Mean: ",  round(mean(DURATION_min.outliers), 2), "    \u2022", "Max: ", round(max(DURATION_min.outliers), 2)),
    #caption = "Source:",
    x = "Duration (min)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  ) +
  xlim(0, 150)

# Conclusion:
# After consultation with Prof. Calmasini it was determined these outliers are not an issue for two reasons:
# (1) The duration may be inflated due to the survey software allowing for pausing the survey and coming back to it
# in which case the duration counter keeps going.
# (2) After checking the content of the feedback in the outlier observations, it has been determined the responses are within reason
# and do not indicate any harmful or questionable behavior.



############################################################################
############################################################################
###                                                                      ###
###                        DESCRIPTIVE STATISTICS                        ###
###                                                                      ###
############################################################################
############################################################################

## Descriptive Statistics ----
summary(survey.df$GENDER)
### Exploring: GENDER & EDUCATION ----
education_gender_df <- as.data.frame(table(survey.df$EDUCATION, survey.df$GENDER))
names(education_gender_df) <- c("Education", "Gender", "Count")

# plot bar chart with grouped bars
ggplot(education_gender_df, aes(x = Education, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(100 * Count / sum(Count), 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  xlab("Education Level") +
  ylab("Number of People") +
  labs(
    title = "Respondents' Education level per Gender",
    subtitle = "% of the whole sample displayed above the bars.",
    x = "Education Level",
    y = "Number of Respondents"
  ) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6), labels = c("Compulsory School", "High School", "Vocational School", "Bachelor's Degree", "Master's Degree", "Doctorate")) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = c("dark blue", "purple")) +
  theme_classic() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, lineheight = 1.5, hjust = 0),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
rm(education_gender_df) # cleanup

### Exploring: GENDER & AGE CATEGORIES ----

age_gender_df <- as.data.frame(table(survey.df$AGE_cat, survey.df$GENDER))
names(age_gender_df) <- c("Age_Category", "Gender", "Count")

# plot bar chart with grouped bars
mean_age <- round(mean(survey.df$AGE, na.rm = TRUE), 2) # for the subtitle
sd_age <- round(sd(survey.df$AGE, na.rm = TRUE), 2) # for the subtitle

ggplot(age_gender_df, aes(x = Age_Category, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(100 * Count / sum(Count), 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  xlab("Age Category") +
  ylab("Number of People") +
  labs(
    title = "Respondents' Age Category per Gender",
    subtitle = paste0('Mean Age in Sample: ', mean_age,
                      ' (SD:', sd_age,')'),
    x = "Age Category",
    y = "Number of Respondents"
  ) +
  scale_x_discrete(breaks = c(1, 2, 3), labels = c("19-39", "40-59", "60-91")) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = c("dark blue", "purple")) +
  theme_classic() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, lineheight = 1.5, hjust = 0),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
rm(age_gender_df, mean_age, sd_age) # cleanup



### Exploring: GENERAL_HEALTH ----
general_health_df <- data.frame(table(survey.df$GENERAL_HEALTH))

ggplot(general_health_df, aes(x = "", y = Freq, fill = factor(Var1), label = ifelse((100 * Freq / sum(Freq)) > 20, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: General Health",
    subtitle = 'In general, how is your health?',
    fill = "General Health"
  ) +
  scale_fill_manual(values = c("dark green", "green", "light green", "yellow", "orange", "grey"),
                    labels = c("Excellent", "Very Good", "Good", "Fair", "Not Good", "Prefer not to say")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 1.3),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = 0.777),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(general_health_df) # cleanup

### Exploring: CHRONIC_CONDITION ----
chronic_condition_df <- data.frame(table(survey.df$CHRONIC_CONDITION))

ggplot(chronic_condition_df, aes(x = "", y = Freq, fill = factor(Var1),
                                 label = ifelse((100 * Freq / sum(Freq)) > 20, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: Chronic Condition",
    subtitle = 'Do you suffer from any chronic illness for which you need medical visits/take medicine?',
    fill = "Chronic Condition"
  ) +
  scale_fill_manual(values = c("dark red", "dark green", "dark grey", "black"),
                    labels = c("Yes", "No", "Don't Know", "Prefer not to say")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 2),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = -0.8),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(chronic_condition_df) # cleanup

### Exploring: E_HL_cat ----
e_hl_cat_df <- data.frame(table(survey.df$E_HL_cat))

ggplot(e_hl_cat_df, aes(x = "", y = Freq, fill = factor(Var1),
                        label = ifelse((100 * Freq / sum(Freq)) > 20, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: eHealth Literacy",
    fill = "E_HL_cat"
  ) +
  scale_fill_manual(values = c("dark green", "orange", "dark red"),
                    labels = c("Sufficient eHL", "Problematic eHL", "Inadequate eHL")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 1.7),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = -0.6),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(e_hl_cat_df) # cleanup

### Exploring: HOW_OFTEN_APP ----
how_often_app_df <- data.frame(table(survey.df$HOW_OFTEN_APP))

ggplot(how_often_app_df, aes(x = "", y = Freq, fill = factor(Var1),
                             label = ifelse((100 * Freq / sum(Freq)) > 10, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: Frequency of using Health Apps",
    fill = "Frequency of using Health Apps",
    subtitle = "What is the frequency with which you provide health information through apps?"
  ) +
  scale_fill_manual(values = c("#ff0000", "#ff8000", "#ffff00", "#80ff00", "#00ff00", "#00ff80", "#00ffff", "#0080ff", "#0000ff", "#8000ff", "#ff00ff"),
                    labels = c("Every hour", "Many times per day", "Few times per day", "Many times per week", "Few times per week",
                               "Many times per month", "Few times per month", "Rarely", "Never", "I don't know", "I prefer not to answer")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = -0.13),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = -0.6),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(how_often_app_df) # cleanup

### Exploring: APP_USE ----
app_use_df <- data.frame(table(survey.df$APP_USE))

ggplot(app_use_df, aes(x = "", y = Freq, fill = factor(Var1),
                       label = ifelse((100 * Freq / sum(Freq)) > 10, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: Use of Health Apps",
    subtitle = 'Have you ever used applications to monitor your health?',
    fill = "Use of Health Apps"
  ) +
  scale_fill_manual(values = c("dark green", "orange", "dark red", "grey"),
                    labels = c("Yes", "No", "Don't know", "Prefer not to say")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 3.1),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = 1.3),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(app_use_df) # cleanup


### Exploring: HEALTH_RELATED_JOB ----
health_related_job_df <- data.frame(table(survey.df$HEALTH_RELATED_JOB))

ggplot(health_related_job_df, aes(x = "", y = Freq, fill = factor(Var1),
                                  label = ifelse((100 * Freq / sum(Freq)) > 5, paste0(round(100 * Freq / sum(Freq), 1), "%"), paste('')))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), size = 7, fontface = 'bold') +
  coord_polar("y", start = 0) +
  labs(
    title = "Percent of Sample: Health-related job",
    subtitle = 'Have you ever held a job in a healthcare setting?',
    fill = "Health-related job"
  ) +
  scale_fill_manual(values = c("#00ff00", "#ff0000", "#808080"),
                    labels = c("Yes", "No", "Prefer not to say")) +
  theme_void() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 1.6),
    plot.subtitle = element_text(color='#777777', size = 11, lineheight = 1.5, hjust = 0.9),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
rm(health_related_job_df) # cleanup



############################################################################
############################################################################
###                                                                      ###
###                               ANALYSIS                               ###
###                                                                      ###
############################################################################
############################################################################
## Analysis (continued in Stata) ----

## Visualiation of RIA (class-specific) ----
# Create the dataframe with the calculated relative importance scores
rel_imp_df <- data.frame(
  Attribute = c(rep(c('Original Data Collector', 'New Data User', 'Reason', 'Information', 'Review'),2)),
  Class = c(rep('Class 1', 5), rep('Class 2', 5)),
  Relative_importance = c(0.17, 0.60, 0.39, 0.22, 0.22, 0.15, 0.54, 0.44, 0.30, 0.23)
)


# Generate the plot (
ggplot(rel_imp_df, aes(x = Class, y = Relative_importance, fill = Attribute)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(100 * Relative_importance, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  xlab("Class") +
  ylab("Relative Importance Score") +
  labs(
    title = "Relative Importance of Attributes per Class",
    subtitle = "% of the maximum importance displayed above the bars."
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, lineheight = 1.5, hjust = 0),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Visualiation of ARIA (average RIA for both classes) ----
# Calculate average RIA for both classes
rel_imp_df$Class <- as.factor(rel_imp_df$Class)
avg_rel_imp_df <- aggregate(Relative_importance ~ Attribute, data = rel_imp_df, FUN = mean)

# Generate the plot
# Generate the plot
ggplot(avg_rel_imp_df, aes(x = Relative_importance, y = reorder(Attribute, Relative_importance), fill = Attribute)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(100 * Relative_importance, 1), "%")), hjust = -0.1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  xlab("Average Relative Importance Score") +
  ylab("") +
  labs(
    title = "Average Relative Importance of Attributes",
    subtitle = "% of the maximum importance displayed next to the bars."
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  theme(
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, lineheight = 1.5, hjust = 0),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold")
  )



# Code Graveyard ----
# This section is not used in the main thesis and
# has accumulated during the process of trial and error.

## Exploring lcmm+dfidx: hlme() = WRONG ----
{
  library(lcmm)
  library(dfidx)

  # A data frame with 1678 observations over 300 different subjects and 22 variables.
  # ID subject identification number
  # Ydep1 longitudinal continuous outcome
  # Ydep2 longitudinal ordinal outcome with 31 levels
  # Ydep3 longitudinal ordinal outcome with 11 levels
  # Tentry delayed entry for the time-to-event
  # Tevent observed time-to-event: either censoring time or time of event
  # Event indicator that Tevent is the time of event
  # Time time of measurement
  # X1 binary covariate
  # X2 binary covariate
  # X3 continuous covariate
  # X4 categorical covariate

  # Convert the choice variable to a binary factor
  dce_df_test$choice <- factor(dce_df_test$choice, levels = c(0, 1))

  # Create a unique row identifier
  dce_df_test$cid <- as.numeric(interaction(dce_df_test$id, seq_along(dce_df_test$id)))

  # Define the formula
  formula <- choice ~ F1_1 + F1_2 + F1_3 + F1_4 + F2_1 + F2_2 + F2_3 + F2_4 + F3_1 + F3_2 + F3_3 + F3_4 + F4_1 + F4_2 + F4_3 + F4_4 + F5_1 + F5_2 + F5_3 + F5_4

  # Fit the latent class model
  numClasses <- 4

  hlme_object <- hlme(fixed = formula, random = ~1, subject = "id", data = dce_df_test)
  model_lcmm <- hlme(fixed = formula, random = ~1, subject = "id", mixture = ~1, ng = numClasses, data = dce_df_test, B = hlme_object)

  # Display model results
  summary(model_lcmm)

  # Heterogenous linear mixed model
  #      fitted by maximum likelihood method
  #
  # hlme(fixed = formula, mixture = ~1, random = ~1, subject = "id",
  #     ng = numClasses, data = dce_df_test)
  #
  # Statistical Model:
  #      Dataset: dce_df_test
  #      Number of subjects: 900
  #      Number of observations: 14400
  #      Number of latent classes: 4
  #      Number of parameters: 29
  #
  # Iteration process:
  #      Convergence criteria satisfied
  #      Number of iterations:  172
  #      Convergence criteria: parameters= 1.8e-11
  #                          : likelihood= 2.5e-09
  #                          : second derivatives= 2.4e-11
  #
  # Goodness-of-fit statistics:
  #      maximum log-likelihood: -10343.9
  #      AIC: 20745.79
  #      BIC: 20885.06
  #
  #
  # Maximum Likelihood Estimates:
  #
  # Fixed effects in the class-membership model:
  # (the class of reference is the last class)
  #
  #                      coef      Se    Wald p-value
  # intercept class1 -0.29521 1.19395  -0.247 0.80471
  # intercept class2 -0.10767 2.62508  -0.041 0.96728
  # intercept class3  0.08954 2.88120   0.031 0.97521
  #
  # Fixed effects in the longitudinal model:
  #
  #                      coef      Se    Wald p-value
  # intercept class1  1.32179 0.02199  60.115 0.00000
  # intercept class2  1.52878 0.02663  57.412 0.00000
  # intercept class3  1.52878 0.02334  65.508 0.00000
  # intercept class4  1.52878 0.02476  61.744 0.00000
  # F1_1             -0.01584 0.00509  -3.110 0.00187
  # F1_2             -0.00888 0.00516  -1.720 0.08535
  # F1_3              0.00339 0.00513   0.661 0.50835
  # F1_4              0.00212 0.00541   0.392 0.69510
  # F2_1              0.02884 0.00509   5.663 0.00000
  # F2_2             -0.00172 0.00546  -0.314 0.75320
  # F2_3             -0.00949 0.00512  -1.853 0.06395
  # F2_4             -0.00037 0.00529  -0.071 0.94372
  # F3_1             -0.01842 0.00507  -3.636 0.00028
  # F3_2             -0.01290 0.00515  -2.506 0.01220
  # F3_3              0.00372 0.00512   0.727 0.46731
  # F3_4              0.01049 0.00506   2.072 0.03828
  # F4_1              0.01874 0.00500   3.746 0.00018
  # F4_2             -0.00742 0.00512  -1.448 0.14760
  # F4_3             -0.00855 0.00517  -1.655 0.09799
  # F4_4              0.00331 0.00515   0.644 0.51965
  # F5_1             -0.02575 0.00510  -5.046 0.00000
  # F5_2              0.00763 0.00509   1.499 0.13381
  # F5_3             -0.02319 0.00507  -4.571 0.00000
  # F5_4              0.00040 0.00501   0.081 0.93582
  #
  #
  # Variance-covariance matrix of the random-effects:
  #           intercept
  # intercept         0
  #
  #                              coef      Se
  # Residual standard error:  0.49057 0.00297

  # Model fit statistics: AIC and log-likelihood
  AIC(model_lcmm)
  logLik(model_lcmm)
}
# lcmm is out


## Exploring gmnl+mlogit ----
{
  install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
  library(mlogit)
  librarian::shelf(gmnl)
  ### Package author's example code:
  ## Examples using the Electricity data set from the mlogit package
  data("Electricity", package = "mlogit")
  Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice",
                        varying = 3:26, shape = "wide", sep = "")
  ## Estimate a LC model with 2 classes
  Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1,
                  data = Electr,
                  subset = 1:3000,
                  model = 'lc',
                  panel = TRUE,
                  Q = 2)
  summary(Elec.lc)
  # Call:
  # gmnl(formula = choice ~ pf + cl + loc + wk + tod + seas | 0 |
  #     0 | 0 | 1, data = Electr, subset = 1:3000, model = "lc",
  #     Q = 2, panel = TRUE, method = "bfgs")
  #
  # Frequencies of categories:
  #
  #       1       2       3       4
  # 0.21467 0.30267 0.21733 0.26533
  #
  # The estimation took: 0h:0m:1s
  #
  # Coefficients:
  #               Estimate Std. Error  z-value  Pr(>|z|)
  # class.1.pf   -0.445822   0.087567  -5.0912 3.557e-07 ***
  # class.1.cl   -0.184654   0.030077  -6.1393 8.288e-10 ***
  # class.1.loc   1.214376   0.161829   7.5041 6.195e-14 ***
  # class.1.wk    0.964074   0.142875   6.7477 1.502e-11 ***
  # class.1.tod  -3.218417   0.687990  -4.6780 2.897e-06 ***
  # class.1.seas -3.486496   0.692898  -5.0318 4.860e-07 ***
  # class.2.pf   -0.843079   0.096787  -8.7107 < 2.2e-16 ***
  # class.2.cl   -0.124183   0.045291  -2.7419  0.006109 **
  # class.2.loc   1.644477   0.268852   6.1167 9.555e-10 ***
  # class.2.wk    1.413870   0.211971   6.6701 2.556e-11 ***
  # class.2.tod  -9.373176   0.867618 -10.8033 < 2.2e-16 ***
  # class.2.seas -9.264694   0.884694 -10.4722 < 2.2e-16 ***
  # (class)2     -0.220005   0.078803  -2.7918  0.005241 **
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  #
  # Optimization of log-likelihood by BFGS maximization
  # Log Likelihood: -792.86
  # Number of observations: 750
  # Number of iterations: 77
  # Exit of MLE: successful convergence

  ## End(Not run)

  # other
{
  # cid: choice identifier
  # Unique identifier for each choice situation, combining ID and DESIGN_ROW
  # It is necessary for mlogit.data
  # Create a unique row identifier
  dce.df$cid <- as.numeric(interaction(dce.df$ID, dce.df$SEQ))

  # Prepare data for gmnl
  dce.df$INDEX <- as.numeric(interaction(dce.df$ID, dce.df$DESIGN_ROW))

  # Prepare data for gmnl
  data_gmnl <- mlogit.data(dce.df,
                           id.var = "ID",
                           choice = "choice",
                           varying = c("C_Tec", "C_Res", "U_Tec", "U_Pha", "U_Res", "R_Dev", "R_Pro", "R_Pol", "I_Not", "I_Inf", "I_Opt", "R_No", "R_Tra"),
                           shape = "wide",
                           sep = "",
                           chid.var = 'INDEX',
                           alt.levels = c("-1", "0", "1"))

  # Define the utility function
  formula <- choice ~ C_Tec + C_Res + U_Tec + U_Pha + U_Res + R_Dev + R_Pro + R_Pol + I_Not + I_Inf + I_Opt + R_No + R_Tra

  # Fit the latent class model
  # Adjust the number of classes (numClasses) as desired
  numClasses <- 2
  model <- gmnl(formula, data_gmnl, model = "lc", Q = numClasses, R = 50)

  # Display model results
  summary(model)

  # Model fit statistics: AIC and log-likelihood
  AIC(model)
  logLik(model)

}

{
  # Install required packages
  install.packages(c("gmnl", "dfidx", "Formula"))
  library(gmnl)
  library(dfidx)
  library(Formula)



  # Create a unique row identifier using the "ID" and "DESIGN_ROW" variables
  dce.df$INDEX <- as.numeric(interaction(dce.df$ID, dce.df$DESIGN_ROW))

  # Convert choice variable to logical
  dce.df$choice <- dce.df$choice == 1

  # Prepare data for gmnl using dfidx package
  data_gmnl <- dfidx(
    dce.df,
    choice = "choice",
    idx = list(id = "ID", chid = "DESIGN_ROW")
  )

  # Define the utility function using the Formula package
  formula <- Formula::Formula(
    choice ~ C_Tec + C_Res + U_Tec + U_Pha + U_Res + R_Dev + R_Pro + R_Pol + I_Not + I_Inf + I_Opt + R_No + R_Tra
  )

  # Fit the latent class model
  # Adjust the number of classes (numClasses) as desired
  numClasses <- 2
  model <- gmnl(formula, data_gmnl, model = "lc", Q = numClasses, R = 50)

  # Display model results
  summary(model)

  # Model fit statistics: AIC and log-likelihood
  AIC(model)
  logLik(model)


  # Fit the model
  library(lcmm)
  model_lcmm <- lcmm(formula, random=~1|class, data=data_lcmm)

  # Display model results
  summary(model_lcmm)

  # Model fit statistics: AIC and log-likelihood
  AIC(model_lcmm)
  logLik(model_lcmm)
}

}


## Experiment with mlogit & gmnl ----
{
  library(mlogit)
  library(gmnl)

  # Create example data frame
  set.seed(123) # set seed for reproducibility
  n_respondents <- 100
  n_tasks <- 16
  n_factors <- 5
  n_levels <- 4
  dce_df_test <- data.frame(id = rep(seq(100, 999), each = n_tasks),
                            choice = sample(0:1, n_respondents * n_tasks, replace = TRUE),
                            matrix(sample(-1:1, n_respondents * n_tasks * n_factors * n_levels, replace = TRUE),
                                   nrow = n_respondents * n_tasks, ncol = n_factors * n_levels,
                                   dimnames = list(NULL, paste0(rep(paste0("F", 1:n_factors), each = n_levels),
                                                                "_", rep(1:n_levels, times = n_factors)))))



  # Create a unique row identifier
  dce_df_test$cid <- as.numeric(interaction(dce_df_test$id, 1:n_tasks))

  # Prepare data for gmnl
  data_gmnl <- dfidx(
    dce_df_test,
    choice = "choice",
    idx = list(id = "cid"),
    sep = "",
    varying = colnames(dce_df_test)[4:ncol(dce_df_test)]
  )

  # Define the utility function
  formula <- choice ~ 0 + F1_1 + F1_2 + F1_3 + F1_4 + F2_1 + F2_2 + F2_3 + F2_4 + F3_1 + F3_2 + F3_3 + F3_4 + F4_1 + F4_2 + F4_3 + F4_4 + F5_1 + F5_2 + F5_3 + F5_4

  # Fit the latent class model
  numClasses <- 4
  model <- gmnl(formula, data_gmnl, model = "lc", Q = numClasses, R = 50)

  # Display model results
  summary(model)

  # Model fit statistics: AIC and log-likelihood
  AIC(model)
  logLik(model)
}


{


  ## Explore: GENDER & EDUCATION ----
  ggplot(education_gender_df, aes(x = Education, y = Count, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Education Level") +
    ylab("Number of People") +
    labs(
      title = "Respondents' Education level per Gender",
      subtitle = sprintf("Compulsory School [%.1f%%] | High School [%.1f%%] | Vocational School [%.1f%%] \nBachelor's Degree [%.1f%%] | Master's Degree [%.1f%%] | Doctorate [%.1f%%]",
                         100 * sum(education_gender_df[education_gender_df$Education == 1, "Count"]) / sum(education_gender_df$Count),
                         100 * sum(education_gender_df[education_gender_df$Education == 2, "Count"]) / sum(education_gender_df$Count),
                         100 * sum(education_gender_df[education_gender_df$Education == 3, "Count"]) / sum(education_gender_df$Count),
                         100 * sum(education_gender_df[education_gender_df$Education == 4, "Count"]) / sum(education_gender_df$Count),
                         100 * sum(education_gender_df[education_gender_df$Education == 5, "Count"]) / sum(education_gender_df$Count),
                         100 * sum(education_gender_df[education_gender_df$Education == 6, "Count"]) / sum(education_gender_df$Count)),
      x = "Education Level",
      y = "Number of Respondents"
    ) +
    scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6), labels = c("Compulsory School", "High School", "Vocational School", "Bachelor's Degree", "Master's Degree", "Doctorate")) +
    scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = c("dark blue", "purple")) +
    theme_classic() +
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12, lineheight = 1.5, hjust = 0),
      plot.caption = element_text(face = "italic"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

}



{
  library(flexmix)

  # Prepare data
  dce.long <- reshape(dce.df,
                      varying = c("C_Tec", "C_Res", "U_Tec", "U_Pha", "U_Res", "R_Dev", "R_Pro", "R_Pol", "I_Not", "I_Inf", "I_Opt", "R_No", "R_Tra"),
                      v.names = "utility",
                      timevar = "attribute",
                      idvar = c("ID", "DESIGN_ROW"),
                      direction = "long")

  # Define the formula
  formula <- choice ~ utility

  # Fit the latent class model with 4 classes
  numClasses <- 4
  model_flexmix <- flexmix(formula, data = dce.long, k = numClasses, model = FLXMRbinary())

  # Display model results
  summary(model_flexmix)

  # Model fit statistics: AIC and log-likelihood
  AIC(model_flexmix)
  logLik(model_flexmix)
}





{
  library(nnet)

  # Define the formula
  formula <- choice ~ C_Tec + C_Res + U_Tec + U_Pha + U_Res + R_Dev + R_Pro + R_Pol + I_Not + I_Inf + I_Opt + R_No + R_Tra

  # Fit the latent class model with 4 classes
  numClasses <- 4

  # Create the ID variable
  dce.df$ID <- as.factor(dce.df$ID)

  # Split the dataset into training and testing sets
  set.seed(42)
  train_idx <- sample(seq_len(nrow(dce.df)), size = floor(0.75 * nrow(dce.df)))
  train_data <- dce.df[train_idx, ]
  test_data <- dce.df[-train_idx, ]

  # Fit the multinomial logistic regression model
  model_multinom <- multinom(formula, data = train_data)

  # Display model results
  summary(model_multinom)

  # Model fit statistics: AIC and log-likelihood
  AIC(model_multinom)
  logLik(model_multinom)

  # Predict the class membership probabilities for the test data
  class_probs <- predict(model_multinom, newdata = test_data, type = "probs")

  # Determine the class membership based on the highest probability
  class_membership <- apply(class_probs, 1, which.max)

  # Calculate the accuracy of the model
  accuracy <- mean(class_membership == test_data$choice)
  print(paste("Accuracy:", accuracy))

}


{
  # Recode the variables
  recode_vars <- c("C_Tec", "C_Res", "U_Tec", "U_Pha", "U_Res", "R_Dev", "R_Pro", "R_Pol", "I_Not", "I_Inf", "I_Opt", "R_No", "R_Tra")
  dce.df[recode_vars] <- dce.df[recode_vars] + 1
  dce.df[, recode_vars] <- lapply(dce.df[, recode_vars], function(x) { as.integer(as.character(x)) })


  # Fit the Latent Class Model using poLCA
  library(poLCA)

  # Define the formula
  formula <- as.formula(paste("choice ~", paste(recode_vars, collapse = " + ")))

  # Fit the latent class model with 4 classes
  numClasses <- 4
  model_polca <- poLCA(choice ~ C_Tec + C_Res, data = dce.df, nclass = 2)

  # Display model results
  summary(model_polca)

  # Model fit statistics: AIC and log-likelihood
  AIC(model_polca)
  #logLik(model_polca)

  library(mclust)

  # Recode the variables
  recode_vars <- c("C_Tec", "C_Res", "U_Tec", "U_Pha", "U_Res", "R_Dev", "R_Pro", "R_Pol", "I_Not", "I_Inf", "I_Opt", "R_No", "R_Tra")
  dce.df[recode_vars] <- dce.df[recode_vars] + 1

  # Fit the latent class model with 4 classes
  numClasses <- 4
  model_mclust <- Mclust(dce.df[recode_vars], G = numClasses)

  # Display model results
  summary(model_mclust)

  # Model fit statistics: AIC and BIC
  AIC(model_mclust)
  BIC(model_mclust)

  plot(model_mclust, data = dce.df, what = "classification")


  library(factoextra)

  # Add cluster labels to the data frame
  dce.df$class <- model_mclust$classification

  # Visualize the clusters
  fviz_cluster(model_mclust, data = dce.df, geom = "point", stand = FALSE)
}