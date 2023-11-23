### Digital Marketing Intelligence - Assignment 2 R-script
#Group 3-10:
# Carasatorre Parra, Teresa
# Dreemer, George
# Hadjipieri, Katerina
# Oikonomou, Asimina

rm(list = ls()) #clean workspace

# Preliminary work: packages, data and checks ----
## Install & Load libraries (librarian) ----
chooseCRANmirror(ind = 29) #set CRAN mirror to Netherlands for this session
install.packages("librarian")

# Main Installations & Loads (shelf)
librarian::shelf(MatchIt,sentimentr, syuzhet, tm, rio, ngram, tm, rpart, rpart.plot, caret, topicmodels,ldatuning,LDAvis,text2vec,
                 ipred, partykit, MASS,car,corrplot, rstatix,dplyr,ggplot2,RColorBrewer,car,nortest,tibble,"lchiffon/wordcloud2",
                 stargazer,tidyr,stats,tidytext,officer,bannerCommenter,janitor,crayon, quiet = TRUE)



## Load data ----
onlinemarketing.df <- read.csv(file.choose()) #Dataset 1 for Part 1: Online Marketing Campaigns
email.df <- read.delim(file.choose()) #Dataset 2 for Part 2: Email Campaigns
morestopwords <- read.delim(url("https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/terrier.txt"))
# only 'not' is applicable to stopwords('english') but save words for later use
stopwords_contextsaving <- stopwords('english')[which(stopwords('english')!= c("not", "no", "never", "none", "neither", "nor", "but", "however", "although", "even", "though", "yet", "still", "rather", "quite", "very", "extremely", "too", "enough", "all", "every", "each", "any", "either", "neither", "some", "many", "few", "several", "most", "almost", "just", "about", "nearly", "around", "perhaps", "maybe", "might", "could", "can", "should", "would", "will", "shall", "must"))]


## Data checks ----

### Variable types check and structure ----
str(onlinemarketing.df); str(email.df)
email.df$group <- as.factor(email.df$group) #make group categorical

### Values check ----
summary(onlinemarketing.df); summary(email.df)

### NA check ----
apply(onlinemarketing.df, 2, function(x) any(is.na(x)))
# Answer     ID  words clicks
#  FALSE  FALSE  FALSE  FALSE
apply(email.df, 2, function(x) any(is.na(x)))
# id          group         header          image           open          click
#          FALSE          FALSE          FALSE          FALSE          FALSE          FALSE
#         donate past_donations     days_since    past_visits            age         gender
#          FALSE          FALSE          FALSE          FALSE          FALSE          FALSE

### Outlier checks ----

#________loop for email.df ___________
n <- 0 #<-run everytime with loop (counter)
for (var in email.df) {
  strexcluder <- is.character(var) # checks if the variable is a chr type/string
  factorexcluder <- is.factor(var) # checks if the variable is a factor
  dummyexcluder <- length(unique(var)) # counts the unique values of the variable (if it's 2 it assumes it's a dummy)
  n <- n + 1 # we account for each itteration, then:
  varname <- colnames(email.df[n]) # use it here to create the variable names

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
# >  ✅  < No outliers found in variable  ▸ id ◂
# >  ❎  < Skipped because variable  ▸ group ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ header ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ image ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ open ◂  is a factor, boolean or a string.
# >  ❎  < Skipped because variable  ▸ click ◂  is a factor, boolean or a string.
# >  ⛔  < 15282 outliers found in variable  ▸ donate ◂
# >  ⛔  < 14280 outliers found in variable  ▸ past_donations ◂
# >  ⛔  < 5849 outliers found in variable  ▸ days_since ◂
# >  ⛔  < 4621 outliers found in variable  ▸ past_visits ◂
# >  ✅  < No outliers found in variable  ▸ age ◂
# >  ❎  < Skipped because variable  ▸ gender ◂  is a factor, boolean or a string.

#________loop for onlinemarketing.df ___________
n <- 0 #<-run everytime with loop (counter)
for (var in onlinemarketing.df) {
  strexcluder <- is.character(var) # checks if the variable is a chr type/string
  factorexcluder <- is.factor(var) # checks if the variable is a factor
  dummyexcluder <- length(unique(var)) # counts the unique values of the variable (if it's 2 it assumes it's a dummy)
  n <- n + 1 # we account for each itteration, then:
  varname <- colnames(onlinemarketing.df[n]) # use it here to create the variable names

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
# >  ❎  < Skipped because variable  ▸ Answer ◂  is a factor, boolean or a string.
# >  ✅  < No outliers found in variable  ▸ ID ◂
# >  ⛔  < 21 outliers found in variable  ▸ words ◂
# >  ⛔  < 1 outliers found in variable  ▸ clicks ◂


#### Explore Outliers ----
# Explore the outlier values to determine possible reason or decide for treatment.

##### for email.df ----

###### Donations ----
# Conclusion: Most people donate low amounts, therefore any amounts higher than 100 are seen as outliers
# the donations go up to 3k+, which is not unheard of. Therefore we choose to keep these observations.

summary(email.df$donate)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    0.00    0.00    0.00   42.61   43.72 3214.80
summary(donate.outliers)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   109.3   140.3   189.4   255.5   289.1  3214.8

# Visualize
ggplot(data.frame(donate.outliers), aes(donate.outliers)) +
  geom_histogram(color = "#000000", fill = "dark green") +
  labs(
    title = "Histogram of Outliers: Donations",
    subtitle = paste("\u2022", "Min: \u20ac", round(min(donate.outliers), 2), "    \u2022", "Mean: \u20ac",  round(mean(donate.outliers), 2), "    \u2022", "Max: \u20ac", round(max(donate.outliers), 2)),
    #caption = "Source: Gapminder dataset",
    x = "Donation Amount (\u20ac)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "dark green", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )



###### Past Donations ----
# Conclusion: Most people donate low amounts, therefore any amounts higher than 459 are seen as outliers
# the donations go up to 9k+, which is not unheard of. Therefore we choose to keep these observations.
summary(email.df$past_donations)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    58.0    94.0   130.0   235.3   240.1  9789.4
summary(past_donations.outliers)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   459.3   554.3   702.0   905.5  1007.9  9789.4

# Visualize
ggplot(data.frame(past_donations.outliers), aes(past_donations.outliers)) +
  geom_histogram(color = "#000000", fill = "#301934") +
  labs(
    title = "Histogram of Outliers: Past Donations",
    subtitle = paste("\u2022", "Min: \u20ac", round(min(past_donations.outliers), 2), "    \u2022", "Mean: \u20ac",  round(mean(past_donations.outliers), 2), "    \u2022", "Max: \u20ac", round(max(past_donations.outliers), 2)),
    #caption = "Source: Gapminder dataset",
    x = "Donation Amount (\u20ac)",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#301934", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


###### Days Since ----
# Conclusion: Need to ask teacher what even is days since? Is 1488 not a little too much?
# -
summary(email.df$days_since)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#     0.0    39.0    95.0   135.2   188.0  1488.0
summary(days_since.outliers)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   413.0   453.0   509.0   547.8   599.0  1488.0

# Visualize
ggplot(data.frame(days_since.outliers), aes(days_since.outliers)) +
  geom_histogram(color = "#000000", fill = "#8B4000") +
  labs(
    title = "Histogram of Outliers: Days Since",
    subtitle = paste("\u2022", "Min: ", round(min(days_since.outliers), 0), 'days', "    \u2022", "Mean: ",  round(mean(days_since.outliers), 0),'days', "    \u2022", "Max: ", round(max(days_since.outliers), 0),'days'),
    #caption = "Source: Gapminder dataset",
    x = "Days",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#8B4000", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


###### Past Visits ----
# Conclusion: The outliers here start at 12 visits, which is not unheard of depending on the timeframe.
# We see relatively very few people above 20 visits, all those above may be considered crawlers or bots?
# Therefore maybe we should remove observations with above 20 visits?

summary(email.df$past_visits)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   0.000   4.000   6.000   5.946   7.000  51.000
summary(past_visits.outliers)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   12.00   12.00   13.00   14.04   15.00   51.00

# Visualize
ggplot(data.frame(past_visits.outliers), aes(past_visits.outliers)) +
  geom_histogram(color = "#000000", fill = "#008b8b") +
  labs(
    title = "Histogram of Outliers: Past Visits",
    subtitle = paste("\u2022", "Min: ", round(min(past_visits.outliers), 0), 'visits', "    \u2022", "Mean: ",  round(mean(past_visits.outliers), 0),'visits', "    \u2022", "Max: ", round(max(past_visits.outliers), 0),'visits'),
    #caption = "Source: Gapminder dataset",
    x = "Visits",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#008b8b", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )

##### for onlinemarketing.df ----

###### Words ----
# Conclusion: We see that outliers here range between 35 and 57 words, which is a realistic amount of words.
summary(onlinemarketing.df$words)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    5.00   14.00   19.00   19.06   22.00   57.00
summary(words.outliers)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   35.00   37.00   40.00   41.81   43.00   57.00

# Visualize
ggplot(data.frame(words.outliers), aes(words.outliers)) +
  geom_histogram(color = "#000000", fill = "#BCA89F") +
  labs(
    title = "Histogram of Outliers: Words",
    subtitle = paste("\u2022", "Min: ", round(min(words.outliers), 0), 'words', "    \u2022", "Mean: ",  round(mean(words.outliers), 0),'words', "    \u2022", "Max: ", round(max(words.outliers), 0),'words'),
    #caption = "Source: Gapminder dataset",
    x = "Words",
    y = "# of Observations"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#4C3228", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic")
  )

###### Clicks ----
# Conlusion: There is only one outlier here, but it is 2x higher than all the others.
# Maybe this is a exceptional ad or it is riddled with botted clicks.
summary(onlinemarketing.df$clicks)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   24.69  171.85  233.80  231.70  289.75  498.39
summary(clicks.outliers)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  498.4   498.4   498.4   498.4   498.4   498.4


# Assignment Questions ----

## PART 1: ONLINE MARKETING CAMPAIGNS ----
# onlinemarketing.df

### Q1: median number of clicks generated ----
# Answer: 233.80 clicks
summary(onlinemarketing.df$clicks)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   24.69  171.85  233.80  231.70  289.75  498.39

### Q2: mean number of words, most freq. used words ----
# Answer: Around 19 words,

# Q2.1: first part of the question (mean):
summary(onlinemarketing.df$words)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    5.00   14.00   19.00   19.06   22.00   57.00

# Q2.2: second part of the question (most freq. used words):
# TOP 10
#          word freq
# 1        help  353
# 2     support  312
# 3     refugee  229
# 4       child  143
# 5  assistance  104
# 6        life   86
# 7        give   79
# 8   christmas   78
# 9       today   76
# 10 difference   75

# Build corpus (lexicon)

corpus_lex <- Corpus(VectorSource(onlinemarketing.df$Answer))
inspect(corpus_lex[1:5])

# Clean text: lowercase, punctuation, numbers, stopwords, URLs (http), whitespace and specific words

# LOWERCASE:
corpus_lex <- tm_map(corpus_lex, tolower)
inspect(corpus_lex[1:5])

# PUNCTUATION:
corpus_lex <- tm_map(corpus_lex, removePunctuation)
inspect(corpus_lex[1:5])

# NUMBERS:
corpus_lex <- tm_map(corpus_lex, removeNumbers)
inspect(corpus_lex[1:5])

# STOPWORDS:
cleanset <- tm_map(corpus_lex, removeWords, stopwords('english'))

# we remove additional stop words because we noticed the above code does not remove all stop words, e.g. "for" and "the"
# source: https://raw.githubusercontent.com/igorbrigadir/stopwords/master/en/terrier.txt
cleanset <- tm_map(cleanset, removeWords, morestopwords[,1])
# due to the nature of the dataset we remove these words too, as instructed by the DMI team
cleanset <- tm_map(cleanset, removeWords, c('cbm', 'wwf', 'scotland', 'quarrier', 'franke', 'cross', 'red cross', 'switzerland', 'dansk', 'danish', 'juventute', 'caritas', 'swiss', 'denmark', 'keyword', 'keywords', 'scottish', 'bible', 'council'))
# after inspecting the top words in line 369, we decide to remove additional words:
cleanset <- tm_map(cleanset, removeWords, c('quarriers', 'christoffel')) # <^- confirm with teacher if this is good
inspect(cleanset[1:5])

# URLS:
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

# WHITESPACES:
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


# Tokenization: Create Term document matrix (Bag of words)
tdm <- TermDocumentMatrix(cleanset)
tdm
# <<TermDocumentMatrix (terms: 1044, documents: 565)>>
# Non-/sparse entries: 7483/582377
# Sparsity           : 99%
# Maximal term length: 14
# Weighting          : term frequency (tf)
tdm <- as.matrix(tdm)
tdm[1:10, 1:10]
tdm <- as.data.frame(t(tdm)) #makes it a dataframe where each column is a different term and rows are documents

# add totals occurence of a term in all docs row
tdm <- rbind(tdm, colSums(tdm)) # make a totals row
rownames(tdm)[566] <- "freq"
summary(tdm['freq',])

# get the name of the most frequently used word: "help" (top 1)
colnames(tdm)[which(tdm['freq',] == max(tdm['freq',]))]

# get a dataframe of the frequencies sorted in descending order
sort(tdm['freq',],decreasing = TRUE)

# TOP 10
#          word freq
# 1        help  353
# 2     support  312
# 3     refugee  229
# 4       child  143
# 5  assistance  104
# 6        life   86
# 7        give   79
# 8   christmas   78
# 9       today   76
# 10 difference   75

# Visualize: Top 10
word.freq.sorted <- as.data.frame(t(tdm['freq',])) %>% arrange(desc(freq))
word.freq.sorted <- tibble::rownames_to_column(word.freq.sorted, 'word')
top10 <- subset(word.freq.sorted, word.freq.sorted$freq >= 75)

ggplot(top10, aes(x=reorder(as.factor(word), freq), y=freq, label=freq)) +
  geom_point(stat='identity', aes(col=word), size=12)  +
  scale_color_manual(name="Top 10 Words",
                     labels = paste0(top10$word, ' (', top10$freq,' times)'),
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

### Q3: visualize the overall distribution of words in the data in a compelling way ----
# Visualize: Overall
# Word cloud: Words occuring more than 5 times
wordcloud2(word.freq.sorted[which(word.freq.sorted$freq > 5),],
           size = 0.7,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize = 1)

# Barplot: Words occuring more than 50 times (Top 23)
par(mar=c(5,6,4,2)) #adjust left marging to accomodate longer words
barplot(word.freq.sorted[which(word.freq.sorted$freq > 50),c("freq")],
        las = 2,
        names.arg = word.freq.sorted[which(word.freq.sorted$freq > 50),c("word")],
        col = rainbow(length(word.freq.sorted[which(word.freq.sorted$freq > 50),c("word")])),
        #font.axis = 2,
        horiz = T,
        xlab = "Frequency")
barplot.title <- expression(paste(bold("Top 23 Words"))) # custom add the title to fit the ggplot2 style
barplot.subtitle <- "Words occuring more than 50 times."
mtext(side=3, line=1.5, at=-0.07, adj=0, cex=1.6, barplot.title) # placing the titles
mtext(side=3, line=0.7, at=-0.07, adj=0, cex=0.9, barplot.subtitle)
rm(barplot.title); rm(barplot.subtitle) # cleanup

### Q5:  textual features based on off-the-shelf lexicons ----
#### Testing ----
#syu
get_sentiment("this is the best awesome answer to question 5") #1.1
get_sentiment("this is the best answer to question 5") #0.5
get_sentiment("this is the answer to question 5") #0
#syu - afinn
get_sentiment("this is the best awesome answer to question 5", method = "afinn") #7
get_sentiment("this is the best answer to question 5", method = "afinn") #3
get_sentiment("this is the answer to question 5", method = "afinn") #0
#syu - bing
get_sentiment("this is the best awesome answer to question 5", method = "bing") #2
get_sentiment("this is the best answer to question 5", method = "bing") #1
get_sentiment("this is the answer to question 5", method = "bing") #0
#nrc
get_nrc_sentiment("this is the best awesome answer to question 5")
#   anger anticipation disgust fear joy sadness surprise trust negative positive
# 1     0            0       0    0   0       0        0     0        0        1
get_nrc_sentiment("this is the best answer to question 5")
#   anger anticipation disgust fear joy sadness surprise trust negative positive
# 1     0            0       0    0   0       0        0     0        0        1
get_nrc_sentiment("this is the answer to question 5")
#   anger anticipation disgust fear joy sadness surprise trust negative positive
# 1     0            0       0    0   0       0        0     0        0        1
#sentimentr
sentiment(c("this is the best awesome answer to question 5",
            "this is the best answer to question 5",
            "this is the answer to question 5"))
#    element_id sentence_id word_count sentiment
# 1:          1           1          8 0.3889087
# 2:          2           1          7 0.1889822
# 3:          3           1          6 0.0000000


#### Lexicon-based methods operalization ----

# fresh df for this analysis: sentiment.df
# relocations & janitor var name treatment (lowercasing) / for fun /
sentiment.df <- feedback_translated.df %>%
  relocate(ID) %>%
    relocate(words, .after = ID) %>%
     relocate(clicks, .after = words) %>%
      rename('text' = Answer) %>%
        clean_names()

##### Compute Sentiment: Various methods ----
#note on sentimentr: we use sentiment_by() to get avg. sentiment, not sentiment() as some rows have multiple sentences,
# which sentiment() would split into multiple rows (per sentence). This is counterproductive as we can't split address 'clicks' in such a situation

sentiment.df <- sentiment.df %>%
  mutate(syuzhet = get_sentiment(text)) %>% #syuzhet default method
    mutate(sentimentr = sentiment_by(text)$ave_sentiment) %>% #sentimentr method; note: we only save column 4 (avg_sentiment)
      mutate(afinn = get_sentiment(text, method = "afinn")) %>% #afinn method
        mutate(bing = get_sentiment(text, method = "bing")) %>% #bing method
          mutate(get_nrc_sentiment(text)) #nrc method



##### NRC Visualization: Distribution of Emotions throughout Campaigns ----

emo_bar<- colSums(sentiment.df[,c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")])
emo_sum<- data.frame(count=emo_bar,emotions=names(emo_bar))
#create a barplot showing the counts for each of eight different emotions and positive/negative rating

ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count, fill = emotions)) +
  geom_bar(stat = 'identity') +
  labs(
    title = "Distribution of Emotions throughout Campaigns",
    subtitle = "Using NRC Lexicon to quantify emotions.",
    #caption = "Source: Gapminder dataset",
    x = "Emotion",
    y = "Campaigns"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = 'Set3')




##### Compare Methods: syuzhet & sentimentr ----

###### Consensus Visualization ----
# 'consensus' dummy column logic:
# 1 if both methods have value above, below or equal to 0 (consensus = 1)
# 0 if values deviate from each other (no consensus = 0)
sentiment.df$consensus <- as.factor(ifelse(sentiment.df$syuzhet > 0 & sentiment.df$sentimentr > 0, 1, # pos
                                           ifelse(sentiment.df$syuzhet < 0 & sentiment.df$sentimentr < 0, 1, #neg
                                                  ifelse(sentiment.df$syuzhet == 0 & sentiment.df$sentimentr == 0, 1, 0)))) #zero, else...
sentiment.df <- relocate(sentiment.df, consensus, .after = text)


ggplot(sentiment.df, aes(x = syuzhet, y = sentimentr)) + geom_point(aes(color = consensus)) +
  scale_color_manual(name = "Consensus", values = c("0" = "red", "1" = "#5BB450"), labels=c('No', 'Yes')) +
  guides(color = guide_legend(reverse = TRUE)) + # reverses order in legend items..
  labs(
    title = "Sentiment consensus: Syuzhet & Sentimentr",
    subtitle = paste0("The two methods agree in ", round((nrow(sentiment.df[sentiment.df$consensus == 1,])/nrow(sentiment.df))*100, 0), "% of cases. *"),
    caption = paste0("* Agree: ", nrow(sentiment.df[sentiment.df$consensus == 1,]), "; Disagree: ", nrow(sentiment.df[sentiment.df$consensus == 0,])),
    x = "Syuzhet",
    y = "Sentimentr"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(color = "#276221" ,size = 14, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


###### Misclassifications: Quantify disagreement through absolute distance ----

# let's look at the misclassified texts/text where there is no consensus
misclassified.df <- sentiment.df[which(sentiment.df$consensus == 0),c('id','text','syuzhet','sentimentr')]

# Compute the Absolute Distancde: "Misclassification magnitude" = mismag
# logic: if syuzhet says -0.1 while sentimentr says 0.5, then the absolute distance is 0.6
# the bigger the number the higher the disagreement
misclassified.df <- mutate(misclassified.df, mismag = (abs(syuzhet) + abs(sentimentr)))

# the 14 sentences with highest magnitude
mis14 <- arrange(misclassified.df,desc(mismag))[1:14,c(1,2)]
print(mis14) # they seem to be unintelligible

# visualize magnitude
par(mar=c(4,5,3,1)) #adjust left marging to accomodate longer words
barplot(sort(misclassified.df[,c("mismag")],decreasing = TRUE),
        las = 2,
        names.arg = paste0("#",misclassified.df[,c("id")]),
        col = c("#800000", "#990000", "#B30000", "#CC0000", "#E60000", "#FF0000", "#FF1A1A", "#FF3333", "#FF4D4D", "#FF6666", "#FF7F7F", "#FF9999", "#FFB2B2", "#FFCCCC", "#FFE6E6", "#FFEBEB", "#FFEDED", "#FFEFEF", "#FFF1F1", "#FFF3F3", "#FFF5F5", "#FFF7F7"),
        #font.axis = 2,
        horiz = T,
        xlab = "Magnitute",
        ylab = "Sentence #ID",
        cex.names = 0.7)
barplot.title <- expression(paste(bold("Misclassification Magnitude: Syuzhet & Sentimentr"))) # custom add the title to fit the ggplot2 style
barplot.subtitle <- "The absolute level of disagreement between the two methods."
mtext(side=3, line=1.3, at=-0.07, adj=0, cex=1.4, barplot.title) # placing the titles
mtext(side=3, line=0.5, at=-0.07, adj=0, cex=1.1, barplot.subtitle)
rm(barplot.title); rm(barplot.subtitle) # cleanup



##### Bing Visualization: Top 10 Words by Sentiment ----

# Create fresh df for this visualization: bing.df
bing.df <- onlinemarketing.df
bing.df$cleanset<- sapply(cleanset, as.character)

bing_word_count <- bing.df %>%
  unnest_tokens(output = word, input = cleanset) %>%
  inner_join(get_sentiments('bing'), by = c('word' = 'word')) %>%
  count(word, sentiment, sort = TRUE)

#select top 10 words by sentiment
bing_top_10 <- bing_word_count %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

#create a bar plot
ggplot(bing_top_10, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Words by Sentiment (Bing Lexicon)",
       x = "Word",
       y = "Count") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red", "neutral" = "gray")) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )





### Q6. Conduct topic modeling ----
# Determine the optimal number of topics, explore and interpret the
# topics and generate the topic probabilities for each campaign.

# fresh df for this analysis: topics.df
topic.df <- sentiment.df[,1:4]

# fresh corpus for this analysis: corpus_topics
corpus_topics <- Corpus(VectorSource(topic.df$text))

# pre-processing corpus
corpus_topics <- corpus_topics %>%
  tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
        tm_map(removeWords, stopwords_contextsaving) %>%
          tm_map(removeWords, c('quarriers', 'christoffel')) %>%
            tm_map(removeWords, c('cbm', 'wwf', 'scotland', 'quarrier', 'franke', 'cross', 'red cross', 'switzerland', 'dansk', 'danish', 'juventute', 'caritas', 'swiss', 'denmark', 'keyword', 'keywords', 'scottish', 'bible', 'council'))
class(corpus_topics) # sanity check

# Set up document-term matrix & clean sparse terms (only ones the occur less than 1%)
dtm_topics <- DocumentTermMatrix(corpus_topics)
dtm_topics <- removeSparseTerms(dtm_topics, 0.99) # with this we go from 1000 terms to 274
inspect(dtm_topics)

#### Visualize: Optimal Topic Number ----
# Griffiths2004: based on the coherence of the most probable words in the topics.
# CaoJuan2009: based on the expected log-likelihood of the model.
# Deveaud2014: based on the exclusivity and prevalence of the top words in the topics.
# Arun2010: based on the expected log-likelihood and the coherence of the topics.
find_topics <- FindTopicsNumber(dtm_topics,
                 topics = seq(from = 2, to = 8, by = 1),
                 metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                 method = "Gibbs",
                 control = list(seed = 77),
                 mc.cores = 2L,
                 verbose = TRUE)
FindTopicsNumber_plot(find_topics)
# Conclusion: Based on this 5 topics seems optimal


#### LDA models: Fitting for 5 topics ----
set.seed(444)

# Create the model
lda_model5 <- LDA(dtm_topics, k = 5, method = "Gibbs", control = list(seed = 77, verbose = 50))
# See top 10 terms to determine titles
terms(lda_model5,10)
#       Topic 1       Topic 2     Topic 3      Topic 4      Topic 5
#  [1,] "child"       "support"   "refugee"    "services"   "help"
#  [2,] "per"         "christmas" "assistance" "get"        "difference"
#  [3,] "jobs"        "today"     "life"       "find"       "world"
#  [4,] "parents"     "give"      "human"      "now"        "save"
#  [5,] "experienced" "family"    "shelter"    "asylum"     "nature"
#  [6,] "care"        "children"  "drc"        "counseling" "protect"
#  [7,] "know"        "contact"   "country"    "people"     "join"
#  [8,] "time"        "buy"       "medical"    "homeless"   "member"
#  [9,] "holiday"     "need"      "care"       "charity"    "preserve"
# [10,] "job"         "adult"     "occupation" "vulnerable" "day"

# Naming:
# Topic 1: Childcare and Parenting
# Topic 2: Christmas and Gift-Giving
# Topic 3: Refugee Assistance and Support
# Topic 4: Community Services and Resources
# Topic 5: Environmental Conservation and Sustainability


##### LDA Visualization ----

# Using Gamma (Theta): generate the topic probabilities for each campaign

# Probabilities per topic per term: Create prob.df
prob.df <-  as.data.frame(lda_model5@gamma) #the probabilities
colnames(prob.df) <- paste0("prob_t", 1:5) #change colnames


# Important Note on GAMMA! (in LDA() vs. in FitLdaModel()):
# In FitLdaModel() function from the topicmodels package,
# the theta matrix contains the probabilities of topics for each document in the corpus.
# The gamma matrix in LDA() function from the same package also contains the probabilities
# of topics for each document, so they serve the same purpose.

# reshape for visualization
prob.df_reshaped <- reshape2::melt(prob.df)

# Define custom topic labels
lda_model5.topics <- c("Topic 1: Childcare and Parenting", "Topic 2: Christmas and Gift-Giving", "Topic 3: Refugee Assistance and Support",
                       "Topic 4: Community Services and Resources", "Topic 5: Environmental Conservation and Sustainability")

lda_model5.topics <- list(
  'prob_t1'= "T1: Childcare and Parenting",
  'prob_t2'="T2: Christmas and Gift-Giving",
  'prob_t3'="T3: Refugee Assistance and Support",
  'prob_t4'="T4: Community Services and Resources",
  'prob_t5'="T5: Environmental Conservation and Sustainability"
)

# Create a function to use the custom labels
topiclabelfun <- function(variable,value){
  return(lda_model5.topics[value])
}

# Use the custom label function in facet_wrap
ggplot(prob.df_reshaped, aes(x = value, fill = variable)) +
  geom_histogram(bins= 50) +
  scale_fill_manual(name = "Topic", values = c("prob_t1" = "#e74c3c", "prob_t2" = "#8e44ad", "prob_t3" = "#2ecc71", "prob_t4" = "#f1c40f", "prob_t5" = "#3498db"), labels=c('T1', 'T2', 'T3', 'T4', 'T5')) +
  facet_wrap(~ variable, ncol = 2, labeller = labeller(variable = topiclabelfun)) +
  labs(
    title = "Probability Distribution across Campaigns",
    subtitle = "Showcase campaign association to each topic based on probability.",
    #caption = "Source: Gapminder dataset",
    x = "Probability (gamma)",
    y = "Number of Documents"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(face = "italic")
  )


### Q8. Modelling ----
# Goal: Include your text-based variables in different regression models.

# Step 0: In this preliminary step we (1) create our analysis.df, which combines all of our variables into one df.
#         (2) We check for correlations. (3) We test linear regression model and check its merit.
#         (4) We expect multicollinearity due to the sheer number of variables and anticipate using
#             regularization techniques to reduce model compexity in Step 1.

# Step 1: (1) We are running a Lasso regression and an Elastic net for ML-based variable selection.
#         > [Topic probabilities]: (prob_t1.._t5)
#         > [Sentiment variables]: (sentimentr, syuzhet, nrc, nfinn, bing)
#         > (Click Outlier dummy): as seen in our outlier checks there is an outlier for clicks.
#                                This may be either a highly successfull ad from which we can learn,
#                                or an ad with bot clicks. Either way we treat for it by including it.
#         (2) We choose the model of the two with better results on RMSE out of sample and use it for Step 2.
#             If their RMSE values are very close we will prioritize the model with less variables.

# Step 2: (1) We expect "words" (num of words) to have a nonlinear relationship with clicks. We test this.
#         (2) We build a second linear model including only the selected variables from Step 1.
#         (3) We test linear model assumptions and fit a final model.
#         (4) We interpret the model and visualize some relationships.

# Step 3: We perform Bagging and create a tree model to gain further perspective into
#         the importance of each variable in predicting clicks.


#### Step 0 ----

##### Create Dataframe: analysis.df ----
# Here we are utilizing our prob.df
prob.df <- cbind(prob.df,id=as.integer(lda_model5@documents))
prob.df <- relocate(prob.df, id) #add id to be able to merge

keepcol <- c("id", "words", "clicks", "syuzhet", "sentimentr", "afinn", "bing", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
analysis.df <- merge(prob.df, subset(sentiment.df, select = keepcol), by = "id", all = TRUE)
analysis.df <- relocate(analysis.df, clicks, .after = id)

# add the click outlier dummy
analysis.df$highclick.dummy <- ifelse(analysis.df$clicks == clicks.outliers,1,0)

str(analysis.df[,-1]) #what we use for the regressions (excluding 'id' var basically)
rm(keepcol) # cleanup

##### Correlation Matrix ----
corrtest <- cor.mtest(analysis.df[,-1], conf.level = 0.95)
corrplot(cor(analysis.df[,-1]), p.mat = corrtest$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
# Findings: We see that clicks are significantly correalted to the majority of the variables.
#           > Clicks are POSITIVELY correlated to: NCR (disgust, sadness, anger, fear), number of words and Topic 2 and 5.
#           > Clicks are NEGATIVELY correlated to: Syuzhet, Sentimentr, Bing, Afinn, NCR (anticipation, trust, joy), Topic 1.
#
# Conclusion: This is already painting a picture that negative sentiment and emotions are related with more cliks.
#             We will investigate this further in our models.

##### Test Model (Full) ----
testmodel <- lm(clicks ~ highclick.dummy + prob_t1 + prob_t2 + prob_t3 + prob_t4 + prob_t5 +
  words + syuzhet + sentimentr + afinn + bing +
  anger + anticipation + disgust + fear + joy + sadness + surprise + trust,
                     data = analysis.df)

summary(testmodel) # we do NOT interpret this model, it has problems. Which we address next.
# Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)      76.5963    54.2910   1.411  0.15886
# highclick.dummy 127.5570    65.9230   1.935  0.05351 .
# prob_t1          17.6769    75.9821   0.233  0.81612
# prob_t2         209.2316    80.1315   2.611  0.00927 **
# prob_t3         -39.6515    90.4835  -0.438  0.66140
# prob_t4         192.7940    73.3219   2.629  0.00879 **
# prob_t5               NA         NA      NA       NA
# words             4.4175     0.4670   9.460  < 2e-16 ***
# syuzhet         -10.3052     5.0934  -2.023  0.04354 *
# sentimentr        8.8732    15.4415   0.575  0.56577
# afinn            -0.1764     1.4252  -0.124  0.90151
# bing             -5.8608     3.1292  -1.873  0.06161 .
# anger            17.3217     7.3390   2.360  0.01861 *
# anticipation     -1.6821     4.2788  -0.393  0.69438
# disgust          -2.6953    10.6463  -0.253  0.80023
# fear              1.4289     4.1978   0.340  0.73370
# joy              -8.6007     4.9348  -1.743  0.08192 .
# sadness          18.0708     5.2062   3.471  0.00056 ***
# surprise         -1.1756     5.5199  -0.213  0.83143
# trust            -0.3049     3.2686  -0.093  0.92572
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 63.36 on 546 degrees of freedom
# Multiple R-squared:  0.3965,	Adjusted R-squared:  0.3766
# F-statistic: 19.93 on 18 and 546 DF,  p-value: < 2.2e-16

# > Prelim. Conclusion: The high estimates may be an indication that our model has a high complexity, as well as possible multicollinearity.

##### Check for Multicollinearity: VIF ----
#vif(fullmodel_base)

# We get an error: there are aliased coefficients in the model
# This means there is multicollinearity in this model.

# > Final conclusion from base model: This definitively means we need to drop coefficients from the model.


#### Step 1 ----
# goal: determine important variables predicting clicks and build a reduced model
# note: we determine best model based on RMSE predominantly, although MAE is also taken into account
# if you need to learn more about the function, please refer to the Documentation:

ffr.rr.lr.enr_auto <- function (df,y,cv,lambda,alpha,model_id = NULL,mode = NULL,split = NULL) {
  ##################################################################
  ##                          User Guide                          ##
  ##################################################################

  # Mandatory Input #
  #   df: unsplit in terms of train/test & subsetted, meaning remove
  #       any unwanted columns you don't want in the model.
  #
  #   y: specify the name of your desired DV as it is called in df
  #      for example 'clicks'
  #
  #   cv: Cross-validation folds (trainControl object)
  #
  #   lambda: lambda (for ridge and lasso)
  #
  #   alpha: alpha (for elnet)

  # Optional Input #
  #   mode: mode is used to specify which regressions you want to run, the codes for the regression types
  #         are in the name, e.g.: ffr = fast-forward regression, lr = lasso regression...
  #         [default: all]
  #
  #   split: this is to specify your train vs. test df split.
  #          [default = 0.80 or an 80% train, 20% test]
  #
  #   model_id: for naming purposes when used in the loop/iteratively, so that model object strings generated
  #             by the function will remain unique.
  #----------------------------------------------------------------

  #################################################################
  ##                       Function Starts                       ##
  #################################################################
  # (Specify default values)["mode","split","model_id"]
  mode <- ifelse(is.null(mode),"all",mode) #if "mode" is not specified assign "all"
  split <- ifelse(is.null(split), 0.80,split) #if "split" is not speficied assign 80%-20% split (0.80)
  model_id <- ifelse(is.null(model_id), 1,model_id) #if "model_id" is not specified assign 1

  # (Create train and test) + (Define y)
  df_train <- df[1:((round(nrow(df) * split))),] #we use this to train our models in-sample
  df_test <- df[(((round(nrow(df) * split)))+1):(nrow(df)),] #we use this to test our models out-sample


  # (Fast Forward Regression flow)
  if (mode == "ffr" | mode == "all") { #if mode is (all) or (ffr) => run fast forward regression
    ffr <- train(as.formula(paste(y,'~ .')), data = df_train, method = "leapForward", trControl = cv, preProc = c("center","scale"), tuneGrid = expand.grid(nvmax = seq(2, 30, 1)))
    ffr.coef <- coef(ffr$finalModel, unlist(ffr$bestTune)) ## COEFFICIENTS IN FINAL MODEL
    ## (PREDICTIONS) & (RMSE,MAE) [IN- AND OUT-OF-SAMPLE]
    ffr.pred_insample <- predict(ffr,df_train) #IN-SAMPLE
    ffr.rmse_insample <- RMSE(ffr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    ffr.mae_insample <- MAE(ffr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    ffr.pred_outsample <- predict(ffr,df_test) #OUT-OF-SAMPLE
    ffr.rmse_outsample <- RMSE(ffr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    ffr.mae_outsample <- MAE(ffr.pred_outsample, unlist(subset(df_test, select = paste0(y)))) #finally, cat prints the results in a readable way + we plot the model:
    #ffr.plot <- plot(ffr) ## PLOT FF REGRESSION
    cat('\n','FAST FORWARD REGRESSION RESULTS:','\n','  FFR-RMSE (INSAMPLE): ',ffr.rmse_insample,'\n','  FFR-MAE (INSAMPLE): ',ffr.mae_insample,'\n','  FFR-RMSE (OUTSAMPLE): ',ffr.rmse_outsample,'\n','  FFR-MAE (OUTSAMPLE): ',ffr.mae_outsample,'\n','\n','FFR COEFFICIENTS:','\n')
    print(ffr.coef)
    #ffr.plot

    # Save objects to global env
    modelnamegenerator <- as.character(paste('ffr.model_id',as.character(model_id),as.character(split),sep = "_"))
    assign(modelnamegenerator,ffr,envir = .GlobalEnv) #save the model object for later testing

    coefnamegenerator <- as.character(paste('ffr.coef_id',as.character(model_id),as.character(split),sep = "_"))
    assign(coefnamegenerator,ffr.coef,envir = .GlobalEnv) #save the coef object for later testing

    cat("Your model was saved! It's called: ",modelnamegenerator,'\n','The coefficients of this model have been saved here: ',coefnamegenerator)
  }

  # (Ridge Regression flow)
  if (mode == "rr" | mode == "all") { #if mode is (all) or (rr) => run ridge regression
    rr <- train(as.formula(paste(y,'~ .')), data = df_train, method = "glmnet", trControl = cv, preProc = c("center","scale"), tuneGrid = expand.grid(alpha = 0, lambda = lambda))
    rr.coef <- coef(rr$finalModel, rr$finalModel$lambdaOpt)  ## COEFFICIENTS IN FINAL MODEL
    ## (PREDICTIONS) & (RMSE,MAE) [IN- AND OUT-OF-SAMPLE]
    rr.pred_insample <- predict(rr,df_train) #IN-SAMPLE
    rr.rmse_insample <- RMSE(rr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    rr.mae_insample <- MAE(rr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    rr.pred_outsample <- predict(rr,df_test) #OUT-OF-SAMPLE
    rr.rmse_outsample <- RMSE(rr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    rr.mae_outsample <- MAE(rr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    #rr.plot <- plot(rr)  ## PLOT R REGRESSION
    cat('\n','\n','<-------------------------------------->','\n','RIDGE REGRESSION RESULTS:','\n','  RR-RMSE (INSAMPLE): ',rr.rmse_insample,'\n','  RR-MAE (INSAMPLE): ',rr.mae_insample,'\n','  RR-RMSE (OUTSAMPLE): ',rr.rmse_outsample,'\n','  RR-MAE (OUTSAMPLE): ',rr.mae_outsample,'\n','\n','RR COEFFICIENTS:','\n')
    print(rr.coef)
    #rr.plot

    modelnamegenerator <- as.character(paste('rr.model_id',as.character(model_id),as.character(split),sep = "_"))
    assign(modelnamegenerator,rr,envir = .GlobalEnv) #save the model object for later testing

    coefnamegenerator <- as.character(paste('rr.coef_id',as.character(model_id),as.character(split),sep = "_"))
    assign(coefnamegenerator,rr.coef,envir = .GlobalEnv) #save the coef object for later testing

    cat("Your model was saved! It's called: ",modelnamegenerator,'\n','The coefficients of this model have been saved here: ',coefnamegenerator)
  }

  # (Lasso Regression flow)
  if (mode == "lr" | mode == "all") { #if mode is (all) or (lr) => run lasso regression
    #y <- df_train[,y_colnum]
    lr <- train(as.formula(paste(y,'~ .')), data = df_train, method = "glmnet", trControl = cv, preProc = c("center","scale"), tuneGrid = expand.grid(alpha = 1, lambda = lambda))
    lr.coef <- coef(lr$finalModel, lr$finalModel$lambdaOpt)  ## COEFFICIENTS IN FINAL MODEL
    ## (PREDICTIONS) & (RMSE,MAE) [IN- AND OUT-OF-SAMPLE]
    lr.pred_insample <- predict(lr,df_train) #IN-SAMPLE
    lr.rmse_insample <- RMSE(lr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    lr.mae_insample <- MAE(lr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    lr.pred_outsample <- predict(lr,df_test) #OUT-OF-SAMPLE
    lr.rmse_outsample <- RMSE(lr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    lr.mae_outsample <- MAE(lr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    #lr.plot <- plot(lr)  ## PLOT L REGRESSION
    cat('\n','\n','<-------------------------------------->','\n','LASSO REGRESSION RESULTS:','\n','  LR-RMSE (INSAMPLE): ',lr.rmse_insample,'\n','  LR-MAE (INSAMPLE): ',lr.mae_insample,'\n','  LR-RMSE (OUTSAMPLE): ',lr.rmse_outsample,'\n','  LR-MAE (OUTSAMPLE): ',lr.mae_outsample,'\n','\n','LR COEFFICIENTS:','\n')
    print(lr.coef)
    #lr.plot

    modelnamegenerator <- as.character(paste('lr.model_id',as.character(model_id),as.character(split),sep = "_"))
    assign(modelnamegenerator,lr,envir = .GlobalEnv) #save the model object for later testing

    coefnamegenerator <- as.character(paste('lr.coef_id',as.character(model_id),as.character(split),sep = "_"))
    assign(coefnamegenerator,lr.coef,envir = .GlobalEnv) #save the coef object for later testing

    cat("Your model was saved! It's called: ",modelnamegenerator,'\n','The coefficients of this model have been saved here: ',coefnamegenerator)
  }

  # (Elastic Net Regression flow)
  if (mode == "enr" | mode == "all") {
    enr <- train(as.formula(paste(y,'~ .')), data = df_train, method = "glmnet", trControl = cv, preProc = c("center","scale"), tuneGrid = expand.grid(alpha = alpha, lambda = lambda))
    enr.coef <- coef(enr$finalModel, enr$finalModel$lambdaOpt)  ## COEFFICIENTS IN FINAL MODEL
    ## (PREDICTIONS) & (RMSE,MAE) [IN- AND OUT-OF-SAMPLE]
    enr.pred_insample <- predict(enr,df_train) #IN-SAMPLE
    enr.rmse_insample <- RMSE(enr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    enr.mae_insample <- MAE(enr.pred_insample, unlist(subset(df_train, select = paste0(y))))
    enr.pred_outsample <- predict(enr,df_test) #OUT-OF-SAMPLE
    enr.rmse_outsample <- RMSE(enr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    enr.mae_outsample <- MAE(enr.pred_outsample, unlist(subset(df_test, select = paste0(y))))
    #enr.plot <- plot(enr)  ## PLOT EN REGRESSION
    cat('\n','\n','<-------------------------------------->','\n','ELASTIC NET REGRESSION RESULTS:','\n','  ENR-RMSE (INSAMPLE): ',enr.rmse_insample,'\n','  ENR-MAE (INSAMPLE): ',enr.mae_insample,'\n','  ENR-RMSE (OUTSAMPLE): ',enr.rmse_outsample,'\n','  ENR-MAE (OUTSAMPLE): ',enr.mae_outsample,'\n','\n','ENR COEFFICIENTS:','\n')
    print(enr.coef)
    #enr.plot

    modelnamegenerator <- as.character(paste('enr.model_id',as.character(model_id),as.character(split),sep = "_"))
    assign(modelnamegenerator,enr,envir = .GlobalEnv) #save the model object for later testing

    coefnamegenerator <- as.character(paste('enr.coef_id',as.character(model_id),as.character(split),sep = "_"))
    assign(coefnamegenerator,enr.coef,envir = .GlobalEnv) #save the coef object for later testing

    cat("Your model was saved! It's called: ",modelnamegenerator,'\n','The coefficients of this model have been saved here: ',coefnamegenerator)
  }

  # (RMSE & MAE Comparisons flow - 4 STEPS)
  # Note: We prioritize high score for Out-sample RMSE, therefore we only save the best model
  #       in terms of Out-sample RMSE, which should result in the best model for new data.
  #       However, we still check all other metrics and print the information.

  if (mode == "all") { #ONLY make comparisons in RMSE and MAE values among models if we ran ALL models
    cat('\n','\n','<-------------------------------------->','\n','WHICH MODEL HAS LOWEST RMSE & MAE? (IN-SAMPLE):','\n')

    # > STEP 1: Which model has lowest RMSE? (In-sample)
    if (ffr.rmse_insample < rr.rmse_insample & ffr.rmse_insample < lr.rmse_insample & ffr.rmse_insample < enr.rmse_insample) { #FFR RMSE then MAE VS ALL
      cat("RMSE values (In-sample) of Fast Forward Regression model are lowest!",'\n')
    } else if (rr.rmse_insample < ffr.rmse_insample & rr.rmse_insample < lr.rmse_insample & rr.rmse_insample < enr.rmse_insample) { #RR RMSE then MAE VS ALL
      cat("RMSE values (In-sample) of Ridge Regression model are lowest!",'\n')
    } else if (lr.rmse_insample < ffr.rmse_insample & lr.rmse_insample < rr.rmse_insample & lr.rmse_insample < enr.rmse_insample) { #LR RMSE then MAE VS ALL
      cat("RMSE values (In-sample) of Lasso Regression model are lowest!",'\n')
    } else if (enr.rmse_insample < ffr.rmse_insample & enr.rmse_insample < rr.rmse_insample & enr.rmse_insample < lr.rmse_insample) { #ENR RMSE then MAE VS ALL
      cat("RMSE values (In-sample) of Elastic Net Regression model are lowest!",'\n')
    } else { #ACCOUNT FOR SCENARIO WHERE SOME VALUES ARE EQUAL, SAY WHICH VALUES (TRUE/FALSE)
      cat('There are RMSE values that are equal','\n','FF and RR?:',(ffr.rmse_insample == rr.rmse_insample),'\n','FF and LR?:',(ffr.rmse_insample == lr.rmse_insample),'\n','FF and ENR?:',(ffr.rmse_insample == enr.rmse_insample),'\n',
          '\n','RR and LR?:',(rr.rmse_insample == lr.rmse_insample),'\n','RR and ENR?:',(rr.rmse_insample == enr.rmse_insample),'\n','FF and ENR?:',(ffr.rmse_insample == enr.rmse_insample),'\n',
          '\n','LR and ENR?:',(lr.rmse_insample == enr.rmse_insample),'\n')
    }

    # > STEP 2: Which model has lowest MAE? (In-sample)
    if (ffr.mae_insample < rr.mae_insample & ffr.mae_insample < lr.mae_insample & ffr.mae_insample < enr.mae_insample) {
      cat("MAE values (In-sample) of Fast Forward Regression model are lowest!")
    } else if (rr.mae_insample < ffr.mae_insample & rr.mae_insample < lr.mae_insample & rr.mae_insample < enr.mae_insample) {
      cat("MAE values (In-sample) of Ridge Regression model are lowest!")
    } else if (lr.mae_insample < ffr.mae_insample & lr.mae_insample < rr.mae_insample & lr.mae_insample < enr.mae_insample) {
      cat("MAE values (In-sample) of Lasso Regression model are lowest!")
    } else if (enr.mae_insample < ffr.mae_insample & enr.mae_insample < rr.mae_insample & enr.mae_insample < lr.mae_insample) {
      cat("MAE values (In-sample) of Elastic Net Regression model are lowest!")
    } else { #ACCOUNT FOR SCENARIO WHERE SOME VALUES ARE EQUAL, SAY WHICH VALUES (TRUE/FALSE)
      cat('There are MAE values that are equal','\n','FF and RR?:',(ffr.mae_insample == rr.mae_insample),'\n','FF and LR?:',(ffr.mae_insample == lr.mae_insample),'\n','FF and ENR?:',(ffr.mae_insample == enr.mae_insample),'\n',
          '\n','RR and LR?:',(rr.mae_insample == lr.mae_insample),'\n','RR and ENR?:',(rr.mae_insample == enr.mae_insample),'\n','FF and ENR?:',(ffr.mae_insample == enr.mae_insample),'\n',
          '\n','LR and ENR?:',(lr.mae_insample == enr.mae_insample),'\n')
    }

    cat('\n','\n','<-------------------------------------->','\n','WHICH MODEL HAS LOWEST RMSE & MAE? (OUT-SAMPLE):','\n')

    # > STEP 3: Which model has lowest RMSE? (Out-sample) < we save the best model based on this metric
    if (ffr.rmse_outsample < rr.rmse_outsample & ffr.rmse_outsample < lr.rmse_outsample & ffr.rmse_outsample < enr.rmse_outsample) { #FFR RMSE then MAE VS ALL
      cat("RMSE values (Out-sample) of Fast Forward Regression model are lowest!",'\n')
      modelnamegenerator <- as.character(paste('ffr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator,ffr,envir = .GlobalEnv) #save the model object for later testing
    } else if (rr.rmse_outsample < ffr.rmse_outsample & rr.rmse_outsample < lr.rmse_outsample & rr.rmse_outsample < enr.rmse_outsample) { #RR RMSE then MAE VS ALL
      cat("RMSE values (Out-sample) of Ridge Regression model are lowest!",'\n')
      modelnamegenerator <- as.character(paste('rr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator,rr,envir = .GlobalEnv) #save the model object for later testing
    } else if (lr.rmse_outsample < ffr.rmse_outsample & lr.rmse_outsample < rr.rmse_outsample & lr.rmse_outsample < enr.rmse_outsample) { #LR RMSE then MAE VS ALL
      cat("RMSE values (Out-sample) of Lasso Regression model are lowest!",'\n')
      modelnamegenerator <- as.character(paste('lr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator,lr,envir = .GlobalEnv) #save the model object for later testing
    } else if (enr.rmse_outsample < ffr.rmse_outsample & enr.rmse_outsample < rr.rmse_outsample & enr.rmse_outsample < lr.rmse_outsample) { #ENR RMSE then MAE VS ALL
      cat("RMSE values (Out-sample) of Elastic Net Regression model are lowest!",'\n')
      modelnamegenerator <- as.character(paste('enr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator,enr,envir = .GlobalEnv) #save the model object for later testing
    } else { #ACCOUNT FOR SCENARIO WHERE SOME VALUES ARE EQUAL, SAY WHICH VALUES (TRUE/FALSE)
      cat('There are RMSE values that are equal','\n','FF and RR?:',(ffr.rmse_outsample == rr.rmse_outsample),ffr.rmse_outsample,rr.rmse_outsample,'\n','FF and LR?:',(ffr.rmse_outsample == lr.rmse_outsample),ffr.rmse_outsample,lr.rmse_outsample,'\n','FF and ENR?:',(ffr.rmse_outsample == enr.rmse_outsample),ffr.rmse_outsample,enr.rmse_outsample,'\n',
          '\n','RR and LR?:',(rr.rmse_outsample == lr.rmse_outsample),rr.rmse_outsample,lr.rmse_outsample,'\n','RR and ENR?:',(rr.rmse_outsample == enr.rmse_outsample),rr.rmse_outsample,enr.rmse_outsample,'\n',
          '\n','LR and ENR:',(lr.rmse_outsample == enr.rmse_outsample),lr.rmse_outsample,enr.rmse_outsample,'\n')
      modelnamegenerator.ffr <- as.character(paste('ffr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator.ffr,ffr,envir = .GlobalEnv) #save the model object for later testing
      modelnamegenerator.rr <- as.character(paste('rr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator.rr,rr,envir = .GlobalEnv) #save the model object for later testing
      modelnamegenerator.lr <- as.character(paste('lr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator.lr,lr,envir = .GlobalEnv) #save the model object for later testing
      modelnamegenerator.enr <- as.character(paste('enr.rmseout.bestmodel_id',as.character(model_id),as.character(split),sep = "_"))
      assign(modelnamegenerator.enr,enr,envir = .GlobalEnv) #save the model object for later testing
    }

    # > STEP 4: Which model has lowest MAE? (Out-sample)
    if (ffr.mae_outsample < rr.mae_outsample & ffr.mae_outsample < lr.mae_outsample & ffr.mae_outsample < enr.mae_outsample) {
      cat("MAE values (Out-sample) of Fast Forward Regression model are lowest!")
    } else if (rr.mae_outsample < ffr.mae_outsample & rr.mae_outsample < lr.mae_outsample & rr.mae_outsample < enr.mae_outsample) {
      cat("MAE values (Out-sample) of Ridge Regression model are lowest!")
    } else if (lr.mae_outsample < ffr.mae_outsample & lr.mae_outsample < rr.mae_outsample & lr.mae_outsample < enr.mae_outsample) {
      cat("MAE values (Out-sample) of Lasso Regression model are lowest!")
    } else if (enr.mae_outsample < ffr.mae_outsample & enr.mae_outsample < rr.mae_outsample & enr.mae_outsample < lr.mae_outsample) {
      cat("MAE values (Out-sample) of Elastic Net Regression model are lowest!")
    } else { #ACCOUNT FOR SCENARIO WHERE SOME VALUES ARE EQUAL, SAY WHICH VALUES (TRUE/FALSE)
      cat('There are MAE values that are equal','\n','FF and RR?:',(ffr.mae_outsample == rr.mae_outsample),'\n','FF and LR?:',(ffr.mae_outsample == lr.mae_outsample),'\n','FF and ENR?:',(ffr.mae_outsample == enr.mae_outsample),'\n',
          '\n','RR and LR?:',(rr.mae_outsample == lr.mae_outsample),'\n','RR and ENR?:',(rr.mae_outsample == enr.mae_outsample),'\n','FF and ENR?:',(ffr.mae_outsample == enr.mae_outsample),'\n',
          '\n','LR and ENR?:',(lr.mae_outsample == enr.mae_outsample))
    }
  }
}


##### Lasso Regression ----
# Conclusion: Lasso outputs these variables as important:
#             prob_t2, prob_t3, words, syuzhet, afinn, bing, anger, anticipation, fear, joy, sadness, highclick dummy
#             Before we build a new linear regression with these, we will consider an elastic net.
#             To choose between the two we will look at which has the lowest RMSE out-sample; Lasso's is 67.30382.
set.seed(123)
librarian::shelf("ETA444/dreemstat")
?testspace444::ezbanner()
devtools::install_github("ETA444/dreemstat")
dreemstat::varselector(df = select(analysis.df, -id),
            y = 'clicks',
            cv = trainControl(method = "cv", number = 5),
            lambda = c(seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 5)),
            alpha = seq(0.00, 1, 0.1),
            mode = 'lr')

ffr.rr.lr.enr_auto(df = select(analysis.df, -id),
                   y = 'clicks',
                   cv = trainControl(method = "cv", number = 5),
                   lambda = c(seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 5)),
                   alpha = seq(0.00, 1, 0.1),
                   mode = 'lr')
#  <-------------------------------------->
#  LASSO REGRESSION RESULTS:
#    LR-RMSE (INSAMPLE):  62.56731
#    LR-MAE (INSAMPLE):  52.89616
#    LR-RMSE (OUTSAMPLE):  67.30382
#    LR-MAE (OUTSAMPLE):  55.03817
#
#  LR COEFFICIENTS:
# 20 x 1 sparse Matrix of class "dgCMatrix"
#                         s1
# (Intercept)     227.030703
# prob_t1           .
# prob_t2           9.099687
# prob_t3          -1.570109
# prob_t4           .
# prob_t5           .
# words            29.533939
# syuzhet         -13.809460
# sentimentr        .
# afinn            -1.003589
# bing             -6.169993
# anger             7.038691
# anticipation     -3.028159
# disgust           .
# fear              3.786343
# joy              -4.027655
# sadness          15.962335
# surprise          .
# trust             .
# highclick.dummy   5.021016
# Your model was saved! It's called:  lr.model_id_1_0.8
#  The coefficients of this model have been saved here:  lr.coef_id_1_0.8


##### Elastic Net Regression ----
# Conclusion: Elastic Net outputs these variables as important:
#             prob_t2, prob_t3, words, syuzhet, bing, anger, anticipation, fear, joy, sadness, highclick dummy
#             Elnet's RMSE is 67.71822. It is a bit higher than Lasso, but fairly similar.
#             We prioritize Elnet because it has 1 less variable.
ffr.rr.lr.enr_auto(df = select(analysis.df, -id),
                   y = 'clicks',
                   cv = trainControl(method = "cv", number = 5),
                   lambda = c(seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 5)),
                   alpha = seq(0.00, 1, 0.1),
                   mode = 'enr')
#   <-------------------------------------->
#  ELASTIC NET REGRESSION RESULTS:
#    ENR-RMSE (INSAMPLE):  62.8142
#    ENR-MAE (INSAMPLE):  53.04313
#    ENR-RMSE (OUTSAMPLE):  67.71822
#    ENR-MAE (OUTSAMPLE):  55.45801
#
#  ENR COEFFICIENTS:
# 20 x 1 sparse Matrix of class "dgCMatrix"
#                            s1
# (Intercept)     227.030703439
# prob_t1           .
# prob_t2           8.380506692
# prob_t3          -0.003389709
# prob_t4           .
# prob_t5           .
# words            28.612962753
# syuzhet         -15.630107203
# sentimentr        .
# afinn             .
# bing             -4.654980422
# anger             6.379461602
# anticipation     -2.346606446
# disgust           .
# fear              3.405404070
# joy              -3.043983569
# sadness          15.418984874
# surprise          .
# trust             .
# highclick.dummy   4.248370275
# Your model was saved! It's called:  enr.model_id_1_0.8
#  The coefficients of this model have been saved here:  enr.coef_id_1_0.8



#### Step 2 ----

##### Test for Non-linear relationships

###### Words & clicks: manual check and visualization ----

# Fit linear regression model with quadratic term for words
test_lm <- lm(clicks ~ words + I(words^2), data = analysis.df)

# Test for significance of quadratic term using ANOVA
anova(test_lm, test = "Chisq")
# Findings: It is significant, meaning there is a non-linear relationship.

# Conclusion: We visualize the realtionship and do not include the variable in the final model.

# Visualization:
ggplot(analysis.df, aes(x = words, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Words",
    subtitle = "After 45 words the number of clicks start going down.",
    #caption = "Source: Gapminder dataset",
    x = "Words",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

# Findings: After ~40 words the clicks start going down. Due to lack of more data
#           we cannot see the line further but assume it goes down furhter.
#           We also see that the most clicks are achieved with less than 30 words.


###### All variables at once: for loop ----
varstocheck2 <- names(analysis.df[,c(-1)])
suppressWarnings({ # surpressing warnings since they are not applicable
  for (var in varstocheck2) {
    if (var != "clicks" && var != "id" && var != "highclick.dummy") {
      cat("Analyzing:",var,'\n')
      lmnonlintest <- lm(as.formula(paste(analysis.df['clicks'],'~',analysis.df[var],'+',I(analysis.df[var]^2))), data = analysis.df)
      # Test for significance of quadratic term using ANOVA
      quadraticterm_pvalue <- anova(lmnonlintest, test = "Chisq")[2,5]
      print(quadraticterm_pvalue)
    } else {
      cat("Cant analyze clicks or id, or dummies.")
    }
  }
})
rm(lmnonlintest); rm(quadraticterm_pvalue) # cleanup
# Analyzing: prob_t1
# [1] 0.1437343
# Analyzing: prob_t2
# [1] 0.638089
# Analyzing: prob_t3
# [1] 0.06621326
# Analyzing: prob_t4 <-
# [1] 0.002239691    <--- significant, but not in our final model
# Analyzing: prob_t5
# [1] 0.4029339
# Analyzing: words
# [1] 0.0005157804   <--- significant
# Analyzing: syuzhet
# [1] 0.1027142
# Analyzing: sentimentr
# [1] 0.8178041
# Analyzing: afinn   <--- significant, but not in our final model
# [1] 0.02162893  <-
# Analyzing: bing     <--- significant
# [1] 0.0004999138
# Analyzing: anger
# [1] 0.04359017
# Analyzing: anticipation
# [1] 0.2148217
# Analyzing: disgust
# [1] 0.6653362
# Analyzing: fear
# [1] 0.9874847
# Analyzing: joy
# [1] 0.3526293
# Analyzing: sadness
# [1] 0.02041584    <--- significant
# Analyzing: surprise
# [1] 0.8202845
# Analyzing: trust
# [1] 0.697966


# Visualize: bing, anger, sadness


# Visualization: bing
ggplot(analysis.df, aes(x = bing, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Bing",
    subtitle = "As sentiment goes up clicks fall up to 3.0, then they start rising.",
    #caption = "Source: Gapminder dataset",
    x = "Bing Sentiment Score",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

# Findings: As sentiment goes up clicks fall up to 3.0, then they start rising.

# Visualization: anger
ggplot(analysis.df, aes(x = anger, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Anger",
    subtitle = "Angry sentiment's clicks peak at 3 then fall.",
    #caption = "Source: Gapminder dataset",
    x = "Anger NRC Score",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

# Findings: Angry sentiment's clicks peak at 3 then fall.

# Visualization: anger
ggplot(analysis.df, aes(x = sadness, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Sadness",
    subtitle = "Clicks rise exponentially as sad sentiment rises.",
    #caption = "Source: Gapminder dataset",
    x = "Sadness NRC Score",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

# Findings: Clicks rise exponentially as sad sentiment rises.


##### Fit LM and test assumptions ----
reducedmodel_base <- lm(clicks ~ prob_t2 + prob_t3 + words + words^2 + syuzhet + bing + anger + anticipation + fear + joy + sadness + highclick.dummy,
                     data = analysis.df)

# NOTE: We only do visual testing, being less strict.

###### Multicollinearity ----
# Conclusion: Great! All VIF values are normal (< 5), seems that our regularization techniques helped
vif(reducedmodel_base)
#         prob_t2         prob_t3           words         syuzhet            bing
#        1.272324        1.728493        1.279225        2.624289        2.044013
#           anger    anticipation            fear             joy         sadness
#        2.663190        2.591093        2.515061        2.918022        2.110596
# highclick.dummy
#        1.051173

###### Linearity & Homoscedasticity ----
# Conclusion: The plot does not show any significant patterns
plot(fitted(reducedmodel_base), residuals(reducedmodel_base))

###### Normality ----
# Preliminary Conclusion: There seems to be a normality issue.
qqnorm(resid(reducedmodel_base))
qqline(resid(reducedmodel_base))

# Further investigation: We will look at histograms of each.

varstocheck <- c("prob_t2", "prob_t3", "syuzhet", "bing", "anger", "anticipation", "fear", "joy", "sadness")
for (var in varstocheck) {
  hist(analysis.df[[var]], main = paste("Histogram of", var))
}
rm(varstocheck)
# Finding: This reveals the nature of some variables such as the NRC emotions, they are not normal.
#          Furthermore, since they have zero's they cannot be treated simply by logging them (we tried).
#          Finally, we tried logging the prob_t2 and _t3, as well as the words.
#          We reran the model with the logged variables and found there still to be a non-normality.

# Final Conclusion: Due to the nature of some variables and the fact the sample size is big enough,
#                   we do not pursue any further remedies.


##### Final LM model ----
final_lm <- lm(clicks ~ prob_t2 + prob_t3 + words + words^2 + syuzhet + bing + bing^2 + anger + anger^2 + anticipation + fear + joy + sadness + sadness^2 + highclick.dummy,
                        data = analysis.df)

summary(final_lm)
# Findings: Topic 2 (Christmas & Gift-Giving) has a significant and positive effect on clicks. [Holiday-themed text results in more clicks]
#           Topic 3 (Refugee Assistance and Support) has a significant and negative effect on clicks. [Refugee-themed text results in less clicks]
#           Syuzhet & Bing Sentiment has a significant negative effect on clicks. [Lower/negative sentiment more clicks]
#           NRC Anger has a significant positive effect on clicks. [Angry text sentiment = more clicks]
#           NRC Sadness has a significant positive effect on clicks. [Sad text sentiment = more clicks]
#           Joy and our dummy variable are almsot significant.
#
# Results:  Our model is overall significant and has an Adj-RSquared of 0.37

# Conclusion: This makes sense as negative sentiment in the context of a charity makes people sad.
#             When people are sad they are more likely to click and donate. Furthermore, when we consider the
#             significant topics we also see that when posts are about holidays people click more. This may be
#             due to people being more generous around holidays and click to donate.


#  Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)      150.6732    20.2981   7.423 4.35e-13 ***
# prob_t2          163.6861    62.4834   2.620  0.00904 **
# prob_t3         -125.4864    63.5266  -1.975  0.04873 *
# words              4.1191     0.4284   9.615  < 2e-16 *** <- we do NOT interpret this
# syuzhet           -7.7511     3.3735  -2.298  0.02195 *
# bing              -6.6386     2.9897  -2.220  0.02679 *
# anger             14.3386     5.9721   2.401  0.01668 *
# anticipation      -3.3698     3.9729  -0.848  0.39669
# fear               2.5305     3.9956   0.633  0.52678
# joy               -8.7579     4.4944  -1.949  0.05185 .
# sadness           18.8320     4.7876   3.933 9.44e-05 ***
# highclick.dummy  125.7583    65.2173   1.928  0.05433 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 63.55 on 553 degrees of freedom
# Multiple R-squared:  0.3851,	Adjusted R-squared:  0.3728
# F-statistic: 31.48 on 11 and 553 DF,  p-value: < 2.2e-16


###### Plot Some Relationships ----

####### Syuzhet Sentiment ----
# note: Perhaps the most interesting regression plot to include in the report.
ggplot(analysis.df, aes(x = syuzhet, y = clicks)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "orange") +
  labs(
    title = "Syuzhet Sentiment to Clicks",
    subtitle = "Negative sentiment brings to more clicks.",
    #caption = "Source: Gapminder dataset",
    x = "Syuzhet Sentiment",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )


####### Topic 2: Holiday-themed ----
ggplot(analysis.df, aes(x = prob_t2, y = clicks)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(
    title = "Holiday-themed Text to Clicks",
    subtitle = "Holiday-themed text brings to more clicks.",
    #caption = "Source: Gapminder dataset",
    x = "Holiday-theme probability",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

####### Topic 3: Refugee-themed ----
# note: the line is nearly flat.
ggplot(analysis.df, aes(x = prob_t3, y = clicks)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(
    title = "Refugee-themed Text to Clicks",
    subtitle = "Refugee-themed text brings to more clicks.",
    #caption = "Source: Gapminder dataset",
    x = "Refugee-theme probability",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )


#### Step 3 ----

##### Bagging ----

#estimate model with bagging - WARNING: TAKES A LONG TIME WITH nbagg=500
Bagging_tree500 <- train(clicks ~ .,
                         data=analysis.df[,-1],
                         method="treebag",
                         nbagg=500,
                         trControl = trainControl(method = "cv", number = 10),
                         control=rpart.control(minsplit = 2, cp = 0.0))

#calculate variable importance
varimp <- varImp(Bagging_tree500)
row.names(varimp$importance) <- c("AFINN", "Anger", "Anticipation", "BING", "Disgust", "Fear", "highclick.dummy", "Joy", "Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Sadness", "SentimentR", "Surprise", "Syuzhet", "Trust", "Words")
varimp_sorted <- arrange(as.data.frame(varimp$importance), Overall)
#                 Overall  # We plot the results below
# Topic 2         100.000
# Topic 1          99.230
# Topic 3          95.642
# Topic 4          94.145
# Topic 5          91.676
# SentimentR       61.348
# Syuzhet          60.133
# Words            53.655
# AFINN            31.471
# BING             21.961
# Anticipation     17.290
# Trust            13.502
# Fear             12.914
# Joy              11.639
# Sadness           9.603
# Anger             6.623
# Surprise          6.216
# Disgust           3.500
# highclick.dummy   0.000

# Plot results
par(mar=c(5,6,4,2)) #adjust left marging to accomodate longer words
barplot(varimp_sorted$Overall[which(varimp_sorted$Overall > 0)],
        las = 2,
        names.arg = row.names(varimp_sorted)[which(varimp_sorted$Overall > 0)],
        col = c("#fcfffc", "#f9fff9", "#f5fff5", "#f0fff0", "#e6ffe6", "#ccffcc", "#b2ffb2", "#99ff99", "#7fff7f", "#66ff66", "#4dff4d", "#33ff33", "#1aff1a", "#00ff00", "#00e600", "#00cc00", "#00b300", "#009900", "#008000", "#006600", "#004d00"),
        #font.axis = 2,
        horiz = T,
        xlab = "Importance")
barplot.title <- expression(paste(bold("Variable Importance in Predicting Clicks"))) # custom add the title to fit the ggplot2 style
barplot.subtitle <- "Based on a 500 bag model."
mtext(side=3, line=1, at=-0.07, adj=0, cex=1.6, barplot.title) # placing the titles
mtext(side=3, line=0.2, at=-0.07, adj=0, cex=0.9, barplot.subtitle)
rm(barplot.title); rm(barplot.subtitle) # cleanup



##### CART Tree Model ----
# Findings: The observations that achieved the most clicks (363.512) have these characteristics:
#           | Anger > 0.5 (Angry sentiment)
#           | Syuzhet < 2.425 (indecisive yet on the sentiment)
#           | Sadness >= 1.5 (Sad sentiment)
#           | Anger > 0.5 (Angry sentiment)
#           | Syuzhet < -0.1 (Only negative sentiment)

# Conclusion: This shows us an overall negative sentiment does lead to more clicks.

# We run our tree only with the top 10 most important variables
CART_tree <- rpart(clicks ~ ., data=analysis.df[,-1], method="anova")

# Visualize
names(CART_tree$variable.importance) <- c("Anger", "Syuzhet", "Words", "Disgust", "Sentimentr", "Sadness", "Prob_t3", "Fear", "AFINN", "Prob_t2", "Anticipation", "Prob_t5", "Prob_t4", "Trust", "Bing", "Prob_t1", "Surprise", "Joy")
colors <- c("#fcfffc", "#f9fff9", "#f5fff5", "#f0fff0", "#e6ffe6", "#ccffcc", "#b2ffb2", "#99ff99", "#7fff7f", "#66ff66", "#4dff4d", "#33ff33", "#1aff1a", "#00ff00", "#00e600", "#00cc00", "#00b300", "#009900", "#008000", "#006600", "#004d00")
# convert to party object and plot
CART_tree_visual <- as.party.rpart(CART_tree)


plot(CART_tree_visual,
     main = "CART Tree: Variables Predicting Clicks",
     type= "simple",
     gp = gpar(fontsize = 16, col = "dark green", lwd = 2))


##### Stargazer table: report results
stargazer(final_lm,
          title = "Final Linear Model Results",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")



## PART 2: EMAIL MARKETING CAMPAIGNS ----
# email.df

### Preliminary steps ----
head(email.df)
#Treatment indicator (x)
table(email.df$group)
#Response (y's)
summary(email.df[,c("open", "click", "donate")])
## Baseline variables (z)
summary(email.df[,c("days_since", "past_donations","past_visits")])
summary(email.df[, c("age","gender")])

### Analysis of A/B tests

### Q1: Randomization checks ----

email.df %>% group_by(group) %>% summarize(mean(days_since), mean(past_donations), mean(past_visits))
email.df %>% group_by(group) %>% summarize(mean(age), mean(gender)) #similar accros groups
#it worked !!!
#WHY:This is typically done by comparing the baseline characteristics of the two groups to ensure that they are
# well-balanced and there are no significant differences that could affect the results of the experiment.
# If the randomization worked correctly, the groups should be similar in terms of their characteristics, and
# any differences observed between them can be attributed to the intervention

email.df %>% group_by(group) %>% summarize(mean(past_donations> 0))
email.df %>% group_by(group) %>% summarize(mean(past_visits> 0)) # i think it means that a high number of customers of all the groups came back

# calculates the mean of a logical expression past_donations > 0 for each group. The expression past_donations > 0
# returns a logical vector indicating whether each element in the variable past_donations is greater than zero.
# Since TRUE is treated as 1 and FALSE as 0 in R, taking the mean of this vector calculates the proportion of
# observations in each group where past_donations is greater than zero.

#SO RESULTS : #about all of the email list has purchased in the past and this is similar for both groups

#visualisation
email.df%>% filter(past_donations > 0) %>%
  ggplot(aes(x=past_donations, fill=group)) +
  geom_histogram(binwidth = 25, alpha=0.2, position="identity") +
  scale_fill_manual(values=c("blue", "red","green")) +
  xlim(0, 2000) +
  labs(
    title = "Distribution of Past Donations by Treatment Group",
    #subtitle = "Refugee-themed text brings to more clicks.",
    #caption = "Source: Gapminder dataset",
    x = "Past Donations ($)",
    y = "Customers"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

email.df%>% filter( past_visits> 0) %>%
  ggplot(aes(x=past_visits, fill=group)) +
  geom_histogram(binwidth = 1, alpha=0.2, position="identity") +
  scale_fill_manual(values=c("blue", "red","green")) +
  xlim(0, 50) +
  xlab("Days Since Last Activity") + ylab("Customers") +
  labs(title="Distribution of Website Visits by Treatment Group") +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

### Q2: Check the descriptive statistics ----
# what is average number of opened emails, clicks and donations per group?

email.df %>% group_by(group) %>% summarize(mean(open), mean(click), mean(donate))
#   group           `mean(open)` `mean(click)` `mean(donate)`
#   <fct>                  <dbl>         <dbl>          <dbl>
# 1 image_landscape        0.652        0.148            51.7
# 2 image_person           0.718        0.185            51.2
# 3 no_image               0.229        0.0586           24.8
# the emails without image they did not open them or cleck or donate almost half as the other
# two that are very similar and with really high numbers

### Q3: Does including images in emails influence open rates, click rates and donations? ----
email.df %>% group_by(group,image) %>% summarize(mean(open), mean(click), mean(donate))
#   group           image `mean(open)` `mean(click)` `mean(donate)`
#   <fct>           <int>        <dbl>         <dbl>          <dbl>
# 1 image_landscape     1        0.652        0.148            51.7
# 2 image_person        1        0.718        0.185            51.2
# 3 no_image            0        0.229        0.0586           24.8

t.test(open ~ image, data=email.df)
# 95 percent confidence interval:
#  -0.4611950 -0.4509117
# sample estimates:
# mean in group 0 mean in group 1
#       0.2289378       0.6849912

t.test(click ~ image, data=email.df)
# 95 percent confidence interval:
#  -0.1115746 -0.1047654
# sample estimates:
# mean in group 0 mean in group 1
#      0.05864989      0.16681991

t.test(donate ~ image, data=email.df)
# 95 percent confidence interval:
#  -27.81381 -25.48329
# sample estimates:
# mean in group 0 mean in group 1
#        24.84057        51.48912

# ANSWER: yes a lot as we can see from the avarage

### Q4: Does including a header in emails influence the open rates, click rates and donations? ----
email.df %>% group_by(group,header) %>% summarize(mean(open), mean(click), mean(donate))
#   group           header `mean(open)` `mean(click)` `mean(donate)`
#   <fct>            <int>        <dbl>         <dbl>          <dbl>
# 1 image_landscape      0        0.648        0.147            50.8
# 2 image_landscape      1        0.655        0.149            52.7
# 3 image_person         0        0.723        0.185            51.0
# 4 image_person         1        0.713        0.186            51.5
# 5 no_image             0        0.230        0.0602           24.2
# 6 no_image             1        0.228        0.0571           25.5

#Does including a header in emails influence the open rates, click rates and donations?----
t.test(open ~ header, data=email.df)
# 95 percent confidence interval:
#  -0.005326610  0.005781777
# sample estimates:
# mean in group 0 mean in group 1
#       0.5330844       0.5328569

t.test(click ~ header, data=email.df)
# 95 percent confidence interval:
#  -0.00420862  0.00329792
# sample estimates:
# mean in group 0 mean in group 1
#       0.1305355       0.1309909

t.test(donate ~ header, data=email.df)
# 95 percent confidence interval:
#  -2.5673045 -0.1256835
# sample estimates:
# mean in group 0 mean in group 1
#        41.93451        43.28100


#### Does email A have higher open rate than B ----
d_treat <- email.df[email.df$group != "ctrl",]
d_treat$group <- droplevels(as.factor(d_treat$group))
xtabs(~ group + open, data=d_treat)
#                  open
# group                 0     1
#   image_landscape 14395 26934
#   image_person    11643 29686
#   no_image        31868  9462
#yes the emails with the image open more  especially the image with the person

# Confirm significance with proportions test: opens
prop.test(xtabs(~ group + open, data=d_treat)[,2:1])
# X-squared = 23391, df = 2, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#    prop 1    prop 2    prop 3
# 0.6516974 0.7182850 0.2289378
#we have evidence to suggest that the proportions of the two groups are different for opens

# Visualization: open rates for emails A & B
mosaicplot(xtabs(~ group + open, data=d_treat),
           main="Email Campaign: Email Opens",
           color=c(control="pink", treatment="purple"))

#### Does email A have a higher click rate than B? ----
xtabs(~ group + click, data=d_treat)
#                  click
# group                 0     1
#   image_landscape 35206  6123
#   image_person    33663  7666
#   no_image        38906  2424
#the image email  had more clicks

# Confirm significance with proportions test: clicks
prop.test(xtabs(~ group + click, data=d_treat)[,2:1])
# X-squared = 3089.8, df = 2, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
#     prop 1     prop 2     prop 3
# 0.14815263 0.18548719 0.05864989
##we have evidence to suggest that the proportions of the two groups are different for clicks

# Visualization: barplot of clicks and opens for emails A & B

email.df%>% filter(group != "ctrl") %>% group_by(group) %>%
  summarize(open=mean(open), click=mean(click)) %>%
  gather(response, mean, -group) %>%
  ggplot(aes(fill=response, y=mean, x=group)) +
  geom_bar(position="dodge", stat="identity") +
  ylab("Response Rate") + xlab("") +
  labs(title="Response Rate: Click & Open by Treatment Group") +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

#### Do any groups have higher average donate? ----
email.df %>% group_by(group) %>% summarize(mean(donate))
#  group           `mean(donate)`
#   <chr>                    <dbl>
# 1 image_landscape           51.7
# 2 image_person              51.2
# 3 no_image                  24.8
#yes the image has the higher donations the one without the image no

#### Do any groups have higher average donation? ----
email.df %>%
  ggplot(aes(y = donate, x = group, fill = group)) +
  geom_boxplot() +
  ylab("30-Day Donations ($)") + xlab("") +
  scale_y_log10() +
  scale_fill_brewer(palette = "Set2")  +
  labs(title="Distribution of Donations by Treatment Group") +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )

# Confirm significance with proportions test: donations
prop.test(xtabs(~ group + donate, data=d_treat)[,2:1])
# X-squared = 5.6867, df = 2, p-value = 0.05823
# alternative hypothesis: two.sided
# sample estimates:
#       prop 1       prop 2       prop 3
# 3.855050e-05 1.939112e-04 2.953599e-05
# we have evidence to suggest that the proportions of the two groups are different for donations

#Sample size calculator
sd(email.df$donate)
#[1] 109.6523

power.t.test(sd=sd(email.df$donate), delta=1, sig.level=0.95, power=0.80)
#      Two-sample t test power calculation
#
#               n = 19666.05
#           delta = 1
#              sd = 109.6523
#       sig.level = 0.95
#           power = 0.8
#     alternative = two.sided
#

# In this code, I have specified the effect size as delta=1, the standard deviation as sd=sd(d$purch), the significance
# level as sig.level=0.05, the desired power as power=0.80, and the type of t-test as type="two.sample" (assuming you are comparing two groups).
# The output of this function will give you the required sample size for each group to achieve the desired power.
# You can adjust the sample size or the other parameters as needed to meet your specific research needs.

### Q5: Use regression models to analyze whether there are any interaction between headers and images. ----
# What do you learn? Make a recommendation about the best email configuration.
lm_model <- lm(open ~ header * image , data = email.df)
summary(lm_model)
# Conclusion: No, they dont have interaction, but image has so is the best one nobody noticed he
# headers people notice images, but not so much letters.
#
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.2300898  0.0031194  73.761   <2e-16 ***
# header       -0.0023226  0.0044293  -0.524    0.600
# image         0.4557718  0.0038258 119.130   <2e-16 ***
# header:image  0.0005824  0.0054248   0.107    0.915
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.4502 on 123984 degrees of freedom
# Multiple R-squared:  0.1857,	Adjusted R-squared:  0.1857
# F-statistic:  9424 on 3 and 123984 DF,  p-value: < 2.2e-16
stargazer(lm_model,
          title = "Investigating interraction between headers and images.",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "There is no significant interraction effect.",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")



### Q6: Do the effect vary depending on donor characteristics? ----
lm_model2 <- lm(donate ~ age * gender , data = email.df)
summary(lm_model2)
# Conclusion: No again no significance
#
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 44.27427    1.43870  30.774   <2e-16 ***
# age         -0.02507    0.03044  -0.823   0.4102
# gender      -3.53670    2.04052  -1.733   0.0831 .
# age:gender   0.05447    0.04312   1.263   0.2066
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 109.7 on 123984 degrees of freedom
# Multiple R-squared:  3.729e-05,	Adjusted R-squared:  1.309e-05
# F-statistic: 1.541 on 3 and 123984 DF,  p-value: 0.2016
stargazer(lm_model2,
          title = "Investigating effect variance based on Donor characteristic",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "There is no significant variance in the effect based on donor characteristics.",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")




### Q7: Check whether individuals who did a donation have different profiles than those that did not make a donation. ----
# Conduct a propensity score matching to correct for potential differences,and interpret the results. How does the effect of images/headers on donation amount change post-matching?


###check data pre-matching
#check for potential differences pre-matching - randomization checks
email.df$donate_people<- ifelse(email.df$donate > 0, 1,0) #dummy for donate yes no

email.df %>%  group_by(donate_people) %>% summarise(mean_math = mean((age)), mean(gender), mean(past_donations), mean(past_visits), mean(days_since))

# estimate a binomial logit model to predict probability that someone will donate or not
ps_model <- lm(donate_people ~ age + gender + past_donations + days_since + past_visits, data=email.df)
summary(ps_model)
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)     8.548e-02  4.302e-03  19.872   <2e-16 ***
# age            -4.684e-06  7.110e-05  -0.066    0.947
# gender         -1.232e-03  2.054e-03  -0.600    0.548
# past_donations  7.436e-04  3.172e-06 234.402   <2e-16 ***
# days_since     -6.508e-04  7.725e-06 -84.251   <2e-16 ***
# past_visits     2.325e-02  3.615e-04  64.299   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.3616 on 123982 degrees of freedom
# Multiple R-squared:  0.3885,	Adjusted R-squared:  0.3884
# F-statistic: 1.575e+04 on 5 and 123982 DF,  p-value: < 2.2e-16

##### Stargazer table: report results
stargazer(ps_model,
          title = "Binomial Logit Model: Pre-matching",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")

#add predicted probabilities to data frame
email.df$pscore1 <- predict(ps_model, type="response")


# conduct propensity score matching
ps_match <- matchit(donate_people ~  age + gender + past_donations + days_since + past_visits, data=email.df, method="nearest")
summary(ps_match)
#                Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# distance              0.9831        0.0076          8.6537    13.2101    0.5413   0.9857
# age                  45.0515       45.0678         -0.0011     0.9985    0.0009   0.0027
# gender                0.4962        0.5005         -0.0086          .    0.0043   0.0043
# past_donations      520.0357      107.4519          0.8556   279.3478    0.4220   0.9883
# days_since           79.9341      160.0459         -1.0311     0.2796    0.0761   0.2763
# past_visits           6.8220        5.5524          0.3889     1.6226    0.0325   0.1520
#
# Summary of Balance for Matched Data:
#                Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# distance              0.9831        0.0169          8.5708     6.4021    0.2958   0.9850
# age                  45.0515       45.1102         -0.0041     1.0021    0.0016   0.0045
# gender                0.4962        0.5044         -0.0163          .    0.0081   0.0081
# past_donations      520.0357      134.8189          0.7988  1332.8846    0.4191   0.9883
# days_since           79.9341      147.7485         -0.8728     0.3297    0.0644   0.2469
# past_visits           6.8220        5.6471          0.3599     1.5850    0.0301   0.1393
#                Std. Pair Dist.
# distance                8.5708
# age                     1.1573
# gender                  0.9939
# past_donations          0.7988
# days_since              1.5491
# past_visits             0.9967
#
# Sample Sizes:
#           Control Treated
# All         85575   38413
# Matched     38413   38413
# Unmatched   47162       0
# Discarded       0       0
plot(summary(ps_match))

#plot kernel density pre- and post-matching
plot(ps_match, type <- "density", which.xs = ~ age + gender + past_donations + past_visits+ days_since)

# match cases
match_cases = match.data(ps_match,distance ="pscore")

# in case you wan to see which cases of the control group are matched to the treatment?
match_cases <- ps_match$match.matrix
#create data with only matched cases
dta_m <- match.data(ps_match)
#How does the effect of images/headers on donation amount change post-matching----
# replace the dataframe with predictive score
lm4 <- lm(donate ~ header + pscore1, data = dta_m)
summary(lm4)
# Coefficients:
#             Estimate Std. Error  t value Pr(>|t|)
# (Intercept) -86.7686     0.2724 -318.536   <2e-16 ***
# header        0.1366     0.2916    0.469    0.639
# pscore1     389.6573     0.4494  866.994   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 40.4 on 76823 degrees of freedom
# Multiple R-squared:  0.9073,	Adjusted R-squared:  0.9073
# F-statistic: 3.759e+05 on 2 and 76823 DF,  p-value: < 2.2e-16

# Startgazer table
stargazer(lm4,
          title = "Linear Model for Header: Post-matching",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")


lm5 <- lm(donate ~ image + pscore1, data = dta_m)
summary(lm5)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) -89.9814     0.3094 -290.86   <2e-16 ***
# image         5.0950     0.3201   15.92   <2e-16 ***
# pscore1     388.9089     0.4512  862.04   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 40.34 on 76823 degrees of freedom
# Multiple R-squared:  0.9076,	Adjusted R-squared:  0.9076
# F-statistic: 3.772e+05 on 2 and 76823 DF,  p-value: < 2.2e-16

# Startgazer table
stargazer(lm5,
          title = "Linear Model for Image: Post-matching",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "latex")
