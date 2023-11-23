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

# main data: ID, DURATION, DURATION_min, GENDER, FEEDBACK_TEXT
feedback.df <- read.csv2(file.choose(), header = TRUE, stringsAsFactors = FALSE)[,c("ID","DURATION", "DURATION_min","GENDER","FEEDBACK_TEXT")]

# accessory data
morestopwords <- read.delim(url("https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/terrier.txt"))

## Data checks ----

### Variable types check and structure ----
str(feedback.df) # Finding: DURATION variables are strings/chr; GENDER is int
# Convert "DURATION" variable from character to numeric
feedback.df$DURATION <- as.numeric(feedback.df$DURATION)

# Convert "DURATION_min" variable from character to numeric
feedback.df$DURATION_min <- as.numeric(feedback.df$DURATION_min)

# Convert "GENDER" variable from int to factor
feedback.df$GENDER <- as.factor(feedback.df$GENDER) #make group categorical

str(feedback.df)
# 'data.frame':	329 obs. of  5 variables:
#  $ ID           : int  698 705 706 709 710 711 707 704 701 714 ...
#  $ DURATION     : num  366 506 538 500 321 ...
#  $ DURATION_min : num  6.1 8.43 8.96 8.33 5.36 ...
#  $ GENDER       : Factor w/ 2 levels "1","2": 2 2 1 2 2 2 1 2 2 1 ...
#  $ FEEDBACK_TEXT: chr  "grafica semplice e accattivante. ...


### Values check ----
summary(feedback.df) # use for duration and gender descriptives
#       ID          DURATION         DURATION_min      GENDER
#  Min.   : 698   Min.   :   303.3   Min.   :   5.050   1:175
#  1st Qu.: 823   1st Qu.:   483.5   1st Qu.:   8.057   2:154
#  Median : 941   Median :   676.1   Median :  11.260
#  Mean   : 997   Mean   :  1284.5   Mean   :  21.433
#  3rd Qu.:1177   3rd Qu.:   997.4   3rd Qu.:  16.628
#  Max.   :1557   Max.   :121376.2   Max.   :2022.940
#                                    NA's   :1
#  FEEDBACK_TEXT
#  Length:329
#  Class :character
#  Mode  :character

### NA check ----
apply(feedback.df, 2, function(x) any(is.na(x))) # Finding: interestingly there is an NA in min but not DURATION
#            ID      DURATION  DURATION_min        GENDER
#         FALSE         FALSE          TRUE         FALSE
# FEEDBACK_TEXT
#         FALSE

# Fix: recalculate DURATION_min for any rows with NAs
feedback.df$DURATION_min[which(is.na(feedback.df$DURATION_min))] <- feedback.df$DURATION[which(is.na(feedback.df$DURATION_min))] / 60


### Outlier check ----
n <- 0 #<-run everytime with loop (counter)
for (var in feedback.df) {
  strexcluder <- is.character(var) # checks if the variable is a chr type/string
  factorexcluder <- is.factor(var) # checks if the variable is a factor
  dummyexcluder <- length(unique(var)) # counts the unique values of the variable (if it's 2 it assumes it's a dummy)
  n <- n + 1 # we account for each itteration, then:
  varname <- colnames(feedback.df[n]) # use it here to create the variable names

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
      cat('>  \u26D4  <',outliercounter,"outliers found in variable ","\u25B8",varname,"\u25C2","| SAVED OUTLIERS IN: ",paste0(varname,'.outliers'),'\n')
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

# >  ✅  < No outliers found in variable  ▸ ID ◂
# >  ⛔  < 34 outliers found in variable  ▸ DURATION ◂ | SAVED OUTLIERS IN:  DURATION.outliers
# >  ⛔  < 34 outliers found in variable  ▸ DURATION_min ◂ | SAVED OUTLIERS IN:  DURATION_min.outliers
# >  ❎  < Skipped because variable  ▸ GENDER ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ FEEDBACK_TEXT ◂  is a factor, boolean or a string.

feedback.df$FEEDBACK_TEXT[which(feedback.df$DURATION_min %in% DURATION_min.outliers)]

#### Explore Outliers ----
# Explore the outlier values to determine possible reason or decide for treatment.

##### DURATION ----
summary(feedback.df$DURATION_min)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    5.05    8.06   11.27   21.41   16.62 2022.94
summary(DURATION_min.outliers)
#       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   30.37   33.13   39.34  104.01   51.48 2022.94

# Visualize
ggplot(data.frame(DURATION_min.outliers), aes(DURATION_min.outliers)) +
  geom_histogram(color = "#000000", fill = "orange") +
  labs(
    title = "Histogram of Outliers: Duration in Minutes",
    subtitle = paste("\u2022", "Min: \u20ac", round(min(DURATION_min.outliers), 2), "    \u2022", "Mean: \u20ac",  round(mean(DURATION_min.outliers), 2), "    \u2022", "Max: \u20ac", round(max(DURATION_min.outliers), 2)),
    #caption = "Source:",
    x = "Duration (min)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "dark green", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


# Limited x-axis between 0 -> 150
ggplot(data.frame(DURATION_min.outliers), aes(DURATION_min.outliers)) +
  geom_histogram(color = "#000000", fill = "orange") +
  labs(
    title = "Histogram of Outliers: Duration in Minutes",
    subtitle = paste("\u2022", "Min: \u20ac", round(min(DURATION_min.outliers), 2), "    \u2022", "Mean: \u20ac",  round(mean(DURATION_min.outliers), 2), "    \u2022", "Max: \u20ac", round(max(DURATION_min.outliers), 2)),
    #caption = "Source:",
    x = "Duration (min)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "dark green", size = 18, face = "bold"),
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


### Logic Checks ----

# Remove rows where "FEEDBACK_TEXT" is empty, contains only whitespace characters, or contains only symbols
feedback.df <- feedback.df[!(nchar(trimws(feedback.df$FEEDBACK_TEXT)) == 0 #
                              | grepl("^[^[:alnum:]]+$", feedback.df$FEEDBACK_TEXT)), ]

# there are still ones that are gibberish, we remove them manually
# rows: 108 "bmhvbvc"; 160 "nn"; 253 "nd"; 312 "Ghj"
# Remove rows where "FEEDBACK_TEXT" is "bmhvbvc", "nn", "nd", or "Ghj"
feedback.df <- feedback.df[!(feedback.df$FEEDBACK_TEXT %in% c("bmhvbvc", "nn", "nd", "Ghj")), ]



#################################################################
##                         Translation                         ##
#################################################################

## Translation ----
# Now that we have treated the data we can move on to translating it

# Authenticate with Googel API for Translate (json file method) / OAuth 2.0
googleAuthR::gar_set_client("PATH TO YOUR JSON FILE")

# Authenticate service account
Sys.setenv("GCS_AUTH_FILE" = "PATH TO YOUR JSON FILE")
googleCloudStorageR::gcs_auth()

# TEST
# Sentence: "grafica semplice e accattivante. Chiarezza dei contenuti e della disposizione degli stessi. Domande mirate"
original_sentence <- feedback.df[1, "FEEDBACK_TEXT"]
# Translation: "simple and attractive graphics. Clarity of content and arrangement of the same. Targeted questions"
translated_sentence <- gl_translate(t_string = feedback.df[1, "FEEDBACK_TEXT"], target = "en", source = "it")
# NOTE: IT RETURNS A TIBBLE WITH BOTH ORIGINAL ("text") AND TRANSLATION ("translatedText")
rm(original_sentence, translated_sentence)

# Function to translate a string from Italian to English
translate_italian_to_english <- function(text) {
  translation <- gl_translate(t_string = paste(text), target = "en", source = "it")
  return(translation$translatedText)
}

# Loop through each original text row -> translate and save to "FEEDBACK_TEXT.EN"
for (i in seq_len(nrow(feedback.df))) {
  feedback.df[i, "FEEDBACK_TEXT.EN"] <- translate_italian_to_english(feedback.df[i, "FEEDBACK_TEXT"])
}
rm(i) # cleanup after loop

# In order to not need to run all this every time: save new .csv for this analysis
write.csv2(feedback.df, file = "YOURPATH/feedback_translated.csv", row.names = FALSE)

feedback_translated.df <- read.csv2("YOURPATH/feedback_translated.csv", header = TRUE, stringsAsFactors = FALSE)



#################################################################
##                        Text Analysis                        ##
#################################################################

## Text Analysis ----

### Word count variable ----

# count_words(): function that removes the punctuation,
# splits into words and counts the words per observation
count_words <- function(text) {
  # Remove punctuation marks
  text <- gsub("[[:punct:]]", " ", text)
  # Split string into words
  words <- strsplit(text, "\\s+")
  # Count number of words
  return(length(words[[1]]))
}

# create the word count column: "word_count"
feedback_translated.df$word_count <- sapply(feedback_translated.df$FEEDBACK_TEXT.EN, count_words)

# check out our new variable
summary(feedback_translated.df$word_count)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   1.000   4.000   6.000   7.489  10.000  55.000


### Create Corpus, Cleaning, Tokenization ----

#### Creating Corpus: feedback.corpus ----
feedback.precorpus <- Corpus(VectorSource(feedback_translated.df$FEEDBACK_TEXT.EN))

#### Cleaning & Tokenization ----
# Create a list of text cleaning transformations
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleaning_transformations <- list(
  content_transformer(tolower),
  removePunctuation,
  removeNumbers,
  content_transformer(removeURL),
  stripWhitespace
)


# Chained cleaning
for (transformation in cleaning_transformations) {
  feedback.corpus <- tm_map(feedback.precorpus, transformation)
  cat("\u2705 Operation is done.", "\n")
}

# Stop words
feedback.corpus <- tm_map(feedback.corpus, removeWords, stopwords("english"))
feedback.corpus <- tm_map(feedback.corpus, removeWords, morestopwords[,1])
feedback.corpus <- tm_map(feedback.corpus, removeWords, "the")
feedback.corpus <- tm_map(feedback.corpus, removePunctuation)
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 231

# Tokenization - Creating TDM
feedback.tdm <- TermDocumentMatrix(feedback.corpus)
# <<TermDocumentMatrix (terms: 359, documents: 231)>>
# Non-/sparse entries: 927/82002
# Sparsity           : 99%
# Maximal term length: 34
# Weighting          : term frequency (tf)

feedback.tdm <- as.matrix(feedback.tdm)
feedback.tdm[1:10, 1:10]
#                 Docs
# Terms            1 2 3 4 5 6 7 8 9 10
#   arrangement    1 0 0 0 0 0 0 0 0  0
#   attractive     1 0 0 0 0 0 0 0 0  0
#   clarity        1 0 0 0 0 0 0 0 0  0
#   content        1 0 0 0 0 0 0 0 0  0
#   graphics.      1 0 0 0 0 0 0 0 0  0
#   questions      1 0 1 0 0 1 0 1 0  0
#   simple         1 0 1 0 0 0 1 0 0  0
#   targeted       1 0 0 0 0 0 0 0 0  0
#   clear          0 1 0 0 0 1 0 0 0  0
#   understandable 0 1 0 0 0 0 1 0 0  0

#makes it a dataframe where each column is a different term and rows are documents
feedback.tdm <- as.data.frame(t(feedback.tdm))

# add totals occurence of a term in all docs row (NOTE: It is the last row in the df)
feedback.tdm <- rbind(feedback.tdm, "totals" = colSums(feedback.tdm)) # make a totals row


#banner

### Text mining ----

# create a new df for this part of the analysis: totals.df
totals.df <- as.data.frame(t(feedback.tdm['totals',])) # create df from the totals row
# the rows are the words, so we move them to their own column "word"
totals.df <- rownames_to_column(totals.df, var = "word")


#### Top 10 words: looking at top 10 used words ----

# get the name of the most frequently used word: "questions"
totals.df$word[which(totals.df[,'totals'] == max(totals.df[,'totals']))]

# get a dataframe of the frequencies sorted in descending order
top10 <- totals.df %>%
  arrange(desc(totals)) %>%
  top_n(10, totals)

# TOP 10
#                word totals
# 1       questions     95
# 2           clear     80
# 3            easy     36
# 4      understand     33
# 5            well     29
# 6          simple     27
# 7  understandable     26
# 8             the     23
# 9          answer     15
# 10        clearly     13


# Visualize: Top 10
top10_plot <- ggplot(top10, aes(x=reorder(as.factor(word), totals), y=totals, label=totals)) +
  geom_point(stat='identity', aes(col=word), size=12)  +
  scale_color_manual(name="Top 10 Words",
                     labels = paste0(top10$word, ' (', top10$totals,' times)'),
                     values = brewer.pal(10, "Set3")) +
  geom_text(color="black", size=4.4) +
  labs(title="Top 10 Most Frequently Used Words",
       #subtitle="",
       x = "",
       y = "Frequency") +
  coord_flip() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic"),
    axis.text = element_text(size = 14, face = "bold")
  )

# save the plot as a PNG file
ggsave("YOURPATH/top10_plot.png",
       plot = top10_plot, width = 10, height = 8, dpi = 300)


#### Wordclouds ----

# Wordcloud of words only occuring more than 5 times
wordcloud_morethan5 <- wordcloud2(totals.df[which(totals.df$totals > 5),],
           size = 0.7,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize = 1,
           backgroundColor = "white",
           color = "random-light")

# Save the wordcloud as an HTML file (htmlwidgets package)
saveWidget(wordcloud_morethan5, "YOURPATH/wordcloud_morethan5.html", selfcontained = FALSE)


# Wordcloud of ALL words - NOTE: We excluded the word "questions"
wordcloud_all <- wordcloud2(totals.df[which(totals.df$totals < 95),],
                                  size = 0.7,
                                  shape = 'circle',
                                  rotateRatio = 0.5,
                                  minSize = 1,
                                  backgroundColor = "white",
                                  color = "random-light")
wordcloud_all
# Save the wordcloud as an HTML file (htmlwidgets package)
saveWidget(wordcloud_all, "YOURPATH/wordcloud_all.html", selfcontained = FALSE)



### Sentiment Analysis ----

# create new datadrame
sentiment.df <- feedback_translated.df  %>%
  mutate(syuzhet = get_sentiment(FEEDBACK_TEXT.EN)) %>% #syuzhet default method
  mutate(sentimentr = sentiment_by(FEEDBACK_TEXT.EN)$ave_sentiment) %>% #sentimentr method; note: we only save column 4 (avg_sentiment)
  mutate(afinn = get_sentiment(FEEDBACK_TEXT.EN, method = "afinn")) %>% #afinn method
  mutate(bing = get_sentiment(FEEDBACK_TEXT.EN, method = "bing")) %>% #bing method
  mutate(get_nrc_sentiment(FEEDBACK_TEXT.EN)) #nrc method


#### Distribution of various sentiment variables ----

# Create a function to generate sentiment distribution plots
plot_sentiment_distribution <- function(sentiment_var, title, subtitle, color) {
  ggplot(sentiment.df, aes_string(x = sentiment_var)) +
    geom_density(fill = color, alpha = 0.5) +
    labs(
      title = title,
      subtitle = subtitle,
      #caption = "Source: Gapminder dataset",
      x = paste0(sentiment_var," score"),
      y = "Density"
    ) +
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(face = "italic")
    )
}

# Generate the plots for each sentiment variable
syuzhet_plot <- plot_sentiment_distribution("syuzhet", "Syuzhet Sentiment Distribution", "~ 77% of respondents had a positive sentiment.", "#ADD8E6")
sentimentr_plot <- plot_sentiment_distribution("sentimentr", "Sentimentr Sentiment Distribution", "~81% of respondents had a positive sentiment.", "#98FB98")
# save
ggsave("YOURPATH/syuzhet_plot.png",
       plot = syuzhet_plot, width = 10, height = 8, dpi = 300)
ggsave("YOURPATH/sentimentr_plot.png",
       plot = sentimentr_plot, width = 10, height = 8, dpi = 300)


#### NRC Visualization: Distribution of Emotions throughout Campaigns ----

emo_bar<- colSums(sentiment.df[,c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")])
emo_sum<- data.frame(count=emo_bar,emotions=names(emo_bar))

#create a barplot showing the counts for each of eight different emotions and positive/negative rating
nrc_plot <- ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count, fill = emotions)) +
  geom_bar(stat = 'identity') +
  labs(
    title = "Distribution of Emotions in the Feedback",
    subtitle = "Using NRC Lexicon to quantify emotions.",
    #caption = "Source: Gapminder dataset",
    x = "Emotion",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = 'Set3')

# save
ggsave("YOURPATH/nrc_plot.png",
       plot = nrc_plot, width = 10, height = 8, dpi = 300)




#### Syuzhet & Sentimentr sentiments by Duration ----

# Create a categorical variable from duration
# 1 = up to incl 10 minutes
# 2 = more than 10 up to and incl. 20 minutes
# 3 = more than 20 up to and incl 35 minutes
# 4 = more than 35 minutes

sentiment.df$DURATIONmin_CAT <- ifelse(sentiment.df$DURATION_min <= 10, 1,
                                       ifelse(sentiment.df$DURATION_min > 10 & sentiment.df$DURATION_min <= 20, 2,
                                               ifelse(sentiment.df$DURATION_min > 20 & sentiment.df$DURATION_min <= 35, 3, 4)))

# plotting function: "plot_duration_sentiment_scatter" - WIP
plot_duration_sentiment_scatter <- function(sentiment_var_x, sentiment_var_y) {
  ggplot(sentiment.df, aes_string(x = sentiment_var_x, y = sentiment_var_y, color = "as.factor(DURATIONmin_CAT)")) +
    geom_point(size=3) +
    scale_color_manual(values = c("#DCDCDC", "#A9A9A9", "#696969", "#000000")) +
    labs(
      title = paste(sentiment_var_x, "vs.", sentiment_var_y, "by Duration categories"),
      subtitle = "1 = up to 10 min; 2 = 10-20 min; 3 = 20-35 min; 4 = more than 35 min",
      #caption = "Source: Gapminder dataset",
      x = sentiment_var_x,
      y = sentiment_var_y,
      color = "Duration"
    ) +
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(face = "italic")
    ) +
    # Add horizontal and vertical lines through the origin / point where it become positive
    geom_vline(xintercept = 0, color = "dark green", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "dark green", linetype = "dashed") +
    geom_text(x = 0.2, y = -0.2, label = "X=0", color = "dark green", size = 4) +
    geom_text(x = -0.2, y = 0.2, label = "Y=0", color = "dark green", size = 4) +
    geom_text(x = 2.5, y = 1.5, label = "Positive Sentiment Quadrant \n According to Both", color = "dark green", size = 5)
}

# Scatter plot of syuzhet vs sentimentr, colored by duration
sentimentvsduration_plot <- plot_duration_sentiment_scatter("syuzhet", "sentimentr")
# save
ggsave("YOURPATH/sentimentvsduration_plot.png",
       plot = sentimentvsduration_plot, width = 10, height = 8, dpi = 300)




#### Syuzhet & Sentimentr sentiments by Gender ----

# plotting function: "plot_gender_sentiment_scatter" - WIP
plot_gender_sentiment_scatter <- function(sentiment_var_x, sentiment_var_y) {
  ggplot(sentiment.df, aes_string(x = sentiment_var_x, y = sentiment_var_y, color = "as.factor(GENDER)")) +
    geom_point(size=3) +
    scale_color_manual(values = c("pink", "purple")) +
    labs(
      title = paste(sentiment_var_x, "vs.", sentiment_var_y, "by Gender"),
      subtitle = "1 = Female; 2 = Male",
      #caption = "Source: Gapminder dataset",
      x = sentiment_var_x,
      y = sentiment_var_y,
      color = "Gender"
    ) +
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(face = "italic")
    ) +
    # Add horizontal and vertical lines through the origin / point where it become positive
    geom_vline(xintercept = 0, color = "dark green", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "dark green", linetype = "dashed") +
    geom_text(x = 0.2, y = -0.2, label = "X=0", color = "dark green", size = 4) +
    geom_text(x = -0.2, y = 0.2, label = "Y=0", color = "dark green", size = 4) +
    geom_text(x = 2.5, y = 1.5, label = "Positive Sentiment Quadrant \n According to Both", color = "dark green", size = 5)
}

# Example usage: Scatter plot of syuzhet vs sentimentr, colored by gender
sentimentvsgender_plot <- plot_gender_sentiment_scatter("syuzhet", "sentimentr")
# save
ggsave("YOURPATH/sentimentvsgender_plot.png",
       plot = sentimentvsgender_plot, width = 10, height = 8, dpi = 300)




### Additional information ----
summary(sentiment.df)
#       ID            DURATION       DURATION_min        GENDER
#  Min.   : 698.0   Min.   : 309.5   Min.   :  5.16   Min.   :1.000
#  1st Qu.: 813.5   1st Qu.: 488.6   1st Qu.:  8.14   1st Qu.:1.000
#  Median : 937.0   Median : 680.0   Median : 11.33   Median :1.000
#  Mean   : 998.0   Mean   : 894.0   Mean   : 14.90   Mean   :1.476
#  3rd Qu.:1189.5   3rd Qu.: 994.1   3rd Qu.: 16.57   3rd Qu.:2.000
#  Max.   :1552.0   Max.   :6211.3   Max.   :103.52   Max.   :2.000
#  FEEDBACK_TEXT      FEEDBACK_TEXT.EN     word_count        syuzhet
#  Length:231         Length:231         Min.   : 1.000   Min.   :-0.6000
#  Class :character   Class :character   1st Qu.: 4.000   1st Qu.: 0.2500
#  Mode  :character   Mode  :character   Median : 6.000   Median : 0.5500
#                                        Mean   : 7.489   Mean   : 0.6628
#                                        3rd Qu.:10.000   3rd Qu.: 1.0000
#                                        Max.   :55.000   Max.   : 3.2500
#    sentimentr          afinn              bing             anger
#  Min.   :-0.3536   Min.   :-4.0000   Min.   :-1.0000   Min.   :0.00000
#  1st Qu.: 0.1894   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:0.00000
#  Median : 0.3578   Median : 1.0000   Median : 1.0000   Median :0.00000
#  Mean   : 0.3831   Mean   : 0.7403   Mean   : 0.9048   Mean   :0.02597
#  3rd Qu.: 0.5657   3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.:0.00000
#  Max.   : 1.8739   Max.   : 8.0000   Max.   : 4.0000   Max.   :1.00000
#   anticipation        disgust              fear              joy
#  Min.   :0.00000   Min.   :0.000000   Min.   :0.00000   Min.   :0.00000
#  1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000
#  Median :0.00000   Median :0.000000   Median :0.00000   Median :0.00000
#  Mean   :0.07359   Mean   :0.008658   Mean   :0.05628   Mean   :0.06926
#  3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000
#  Max.   :2.00000   Max.   :1.000000   Max.   :1.00000   Max.   :2.00000
#     sadness           surprise           trust           negative
#  Min.   :0.00000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000
#  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000
#  Median :0.00000   Median :0.00000   Median :0.0000   Median :0.00000
#  Mean   :0.05628   Mean   :0.02597   Mean   :0.1818   Mean   :0.08658
#  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000
#  Max.   :1.00000   Max.   :1.00000   Max.   :3.0000   Max.   :1.00000
#     positive      DURATIONmin_CAT
#  Min.   :0.0000   Min.   :1.000
#  1st Qu.:0.0000   1st Qu.:1.000
#  Median :0.0000   Median :2.000
#  Mean   :0.3203   Mean   :1.814
#  3rd Qu.:0.0000   3rd Qu.:2.000
#  Max.   :4.0000   Max.   :4.000


length(sentiment.df$syuzhet[which(sentiment.df$syuzhet > 0)]) #177 positive
length(sentiment.df$syuzhet[which(sentiment.df$syuzhet < 0)]) #16 negative
length(sentiment.df$syuzhet[which(sentiment.df$syuzhet == 0)]) #38 neutral

length(sentiment.df$sentimentr[which(sentiment.df$sentimentr > 0)]) #186 positive
length(sentiment.df$sentimentr[which(sentiment.df$sentimentr < 0)]) #17 negative
length(sentiment.df$sentimentr[which(sentiment.df$sentimentr == 0)]) #28 neutral

# Average of two methods:
# Positive: 182 (79%)
# Negative: 16 (7%)
# Neutral: 33 (14%)


# most positive Syuzhet sentence:
# "The information was very clear and interesting, thanks and good work"
sentiment.df$FEEDBACK_TEXT.EN[which(sentiment.df$syuzhet == max(sentiment.df$syuzhet))]

# most positive Sentimentr sentence:
# "Very interesting and easy to understand"
sentiment.df$FEEDBACK_TEXT.EN[which(sentiment.df$sentimentr == max(sentiment.df$sentimentr))]
sentiment.df$FEEDBACK_TEXT.EN[which(sentiment.df$sentimentr < 0)]

sentiment.df$FEEDBACK_TEXT.EN[which(sentiment.df$word_count > 18)]

# most positive Bing sentence:
# "The questions were clear and comprehensive, and the platform worked very well."
sentiment.df$FEEDBACK_TEXT.EN[which(sentiment.df$bing == max(sentiment.df$bing))]


# Some Negative Sentences from Bing:
# "I'm not an expert in the medical field and I have many doubts"
# "too elaborate and repetitive!"

# "I had doubts about some questions, such as using the internet to find information about greetings. If I have symptoms, I hardly go on the internet to find an explanation, I prefer to ask a doctor. Instead, the Internet is useful for finding structures or organizations that can help me manage health problems or cures."

# "BECAUSE I TRIED TO EVALUATE WITH ALL THE NOTIONS I HAD, IN REALITY IT'S DEFINITELY MORE COMPLICATED"

# "IN SOME QUESTIONS IN DOUBT I HAVE CHOSEN THE MIDDLE WAY"
sentiment.df$FEEDBACK_TEXT[which(sentiment.df$bing == min(sentiment.df$bing))]


