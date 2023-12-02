### Translation, Text Mining, Sentiment Analysis
### On Survey Feedback data
# George Dreemer

# NOTE 1: All outputs are provided in comments after each code snippet for ease of audit.
# NOTE 2: Ctrl-F "YOURPATH" to see where you need to change paths

rm(list = ls()) #clean workspace

# Preliminary work: packages, data and checks ----
## Install & Load libraries (librarian) ----
chooseCRANmirror(ind = 29) #set CRAN mirror to Netherlands for this session
install.packages("librarian")

# Main Installations & Loads (shelf)
librarian::shelf(htmlwidgets,googleLanguageR,sentimentr, syuzhet, tm, rio, ngram, tm,dplyr,
                 ggplot2,RColorBrewer,car,nortest,tibble,"lchiffon/wordcloud2",
                 tidyr,stats,tidytext,officer,bannerCommenter,janitor,crayon, quiet = TRUE)

## Load data ----

# [ REDACTED ]: Limited access until further notice. #
