#NOTE: Ctrl-F "YOURPATH" to find all paths you need to change to your own.

##################################################################
##                         Preparation                          ##
##################################################################

#clear workspace
rm(list = ls())

##:::::::::::::::::##
##  Installations  ##
##:::::::::::::::::##
# Installations ----

#Handy package that Installs, Updates and Loads
# packages from CRAN, Github & Bioconductor
install.packages("librarian")
library("librarian")

#install & load packages with shelf function
librarian::shelf(nFactors,gplots,RColorBrewer,semPlot, lmtest, aTSA, ggplot2, ggcorrplot, stringr,
                 car,corrplot,dplyr,stats,bannerCommenter, vars, urca, clipr)



##::::::::::::::::::::::
##  Custom Functions  ::
##::::::::::::::::::::::
# Custom Functions ----

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


## My Custom Granger Causality Test # <Data Analysis> ----
#Functionality: (1) It checks the relationship both ways
# (2) You can do as many lags as you wish
#Outputs: (1) neat df with all your pvalues with corresponding lags
# (2) An output in the console telling you at which lag the p-value is lowest
#How to use (example): gct_autolag(redstar.df$LnAdvertising,redstar.df$LnTotalCompAdvertising,13)
gct_autolag <- function(v1,v2,nlag){
  #outputdf.name takes the v1 v2 names, which include the input df name and $, removes that and leaves only the var names + at the end GCT.DF (note: paste function combines them and adds _ in between for usability)
  outputdf.name <- as.character(paste(str_extract(deparse(substitute(v1)),'\\b\\w+$'),str_extract(deparse(substitute(v2)),'\\b\\w+$'),'GCT.DF',sep = "_"))

  outputdf <- data.frame()
  for (lag in 1:nlag) { #loop runs granger in both directions up to and including specified nlag
    pval.v1v2 <- grangertest(v1~v2, order = lag)[2,4] #this saves ONLY the p-value result of GCT = [2,4]
    pval.v2v1 <- grangertest(v2~v1, order = lag)[2,4] #does the same for other direction
    outputdf[lag, 1] <- lag #assigns nth lag to a columns so we know which pval is for which lag
    outputdf[lag,2] <- pval.v1v2 #save the pvalues of first relationship into c2
    outputdf[lag,3] <- pval.v2v1 #save the pvalues of vice versa relationship into c3
  }
  colnames(outputdf) <- c("n.lag", "pval.v1v2", "pval.v2v1") #names the columns

  assign(outputdf.name,outputdf,envir = .GlobalEnv) #save the results in a df in the global environment incase we need the data

  #Also print the most meaningful part right away, so we don't have to lose our eyesight looking at p-values (+reduce human error)
  cat('Lowest p-values (per relationship) /w Lag:', '\n',
      str_extract(deparse(substitute(v1)),'\\b\\w+$'),'vs.',str_extract(deparse(substitute(v2)),'\\b\\w+$'),'- p-value:',min(outputdf$pval.v1v2), 'at Lag:', outputdf[outputdf$pval.v1v2==min(outputdf$pval.v1v2),][,1], '\n',
      str_extract(deparse(substitute(v2)),'\\b\\w+$'),'vs.',str_extract(deparse(substitute(v1)),'\\b\\w+$'),'- p-value:',min(outputdf$pval.v2v1), 'at Lag:', outputdf[outputdf$pval.v2v1==min(outputdf$pval.v2v1),][,1]
  )
}


##::::::::::
##  Data  ::
##::::::::::
# Data ----

## Import ----
redstar.df <-read.csv("YOURPATH")

## Examine ----
str(redstar.df)
summary(redstar.df)
head(redstar.df)


##################################################################
##                    Descriptive Statistics                    ##
##################################################################
# Descriptive Statistics ----

##:::::::::::::::::::::::
##  Time Series Plots  ::
##:::::::::::::::::::::::
## Time Series Plots ----

### Red Star's | Sales, Advertising, Price ----
plot(redstar.df[,c(3)],redstar.df[,c(4)], type="l", col="green", lwd=2, xlab="Week", ylab="Own Sales", main="Own Sales over time")
plot(redstar.df[,c(3)],redstar.df[,c(5)], type="l", col="purple", lwd=2, xlab="Week", ylab="Own Advertising", main="Own Advertising over time")
plot(redstar.df[,c(3)],redstar.df[,c(6)], type="l", col="red", lwd=2, xlab="Week", ylab="Own Price", main="Own Price over time")

### Competitor | Advertising, Price ----
plot(redstar.df[,c(3)],redstar.df[,c(7)], type="l", col="blue", lwd=2, xlab="Week", ylab="Competitor Advertising", main="Competitor Price over time")
plot(redstar.df[,c(3)],redstar.df[,c(8)], type="l", col="orange", lwd=2, xlab="Week", ylab="Competitor Price", main="Competitor Price over time")


##::::::::::::::::::::::::
##  Correlation Matrix  ::
##::::::::::::::::::::::::
## Correlation Matrix ----
rs.cor <- cor(redstar.df[,c(4,5,6,7,8)])
ggcorrplot(rs.cor,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("red", "white", "green"))
# Some Findings:
# Own Price has a positive (0.37) correlation with Average Comp. Price,
# higher our price goes, so does comp. prices (Just Industry price copying effect?)

# Own Sales has a weak negative (-0.14) correlation with Total Comp. Advertising,
# our Sales decrease when competitors advertise more (more eyes on our competitors => more sales for them?)

# Own Sales has a weak negative (-0.16) correlation with Own Price,
# when we price is higher our sales are lower (consumers are a little price-sensitive?)

# Own Advertising has a weak positive (0.16) correlation with Avg. Competitor Price,
# we seem to advertise more, when our competitors increase their price (seizing the opportunity?)


##################################################################
##                     Time Series Analysis                     ##
##################################################################
# Time Series Analysis ----


##:::::::::::::::::::::::::::::
##  Granger Causality Tests  ::
##:::::::::::::::::::::::::::::
## Granger Causality Tests (step 1) ----
# Variable x is granger causing a variable y if knowing the past of x
# improves our forecast of y based on the past of y.

##::::::::::::::::::::::
##  Significant GCTs  ::
##::::::::::::::::::::::
### Significant GCTs ----

#### Total Comp. Advertising vs. Own Sales ----
gct_autolag(redstar.df$LnTotalCompAdvertising,redstar.df$LnSales,13)
#Results: lowest pvalue for v1~v2 is 0.27 (at 1 lag), lowest pvalue for v2~v1 is 0.014 (at 2 lags); for up to 13 lags.
#Interpretation: Total Comp. Advertising is Granger-causing Own Sales.

#### Own Advertising vs Total Comp. Advertising ----
gct_autolag(redstar.df$LnAdvertising,redstar.df$LnTotalCompAdvertising,13)
#Results: lowest pvalue for v1~v2 is 0.22 (at 13 lags), lowest pvalue for v2~v1 is 0.004 (at 6 lags); for up to 13 lags.
#Interpretation: Own Advertising is Granger-causing Total Comp. Advertising.

#### Own Advertising vs Avg. Comp. Price ----
gct_autolag(redstar.df$LnAdvertising,redstar.df$LnAvgCompPrice,13)
#Results: lowest pvalue for v1~v2 is 0.02 (at 4 lags), lowest pvalue for v2~v1 is 0.14 (at 2 lags); for up to 13 lags.
#Interpretation: Avg. Comp. Price is Granger-causing Own Advertising.

#### Own Price vs Avg. Comp. Price ----
gct_autolag(redstar.df$LnPrice,redstar.df$LnAvgCompPrice,13)
#Results: lowest pvalue for v1~v2 is 0.005 (at 1 lag), lowest pvalue for v2~v1 is 0.038 (at 2 lags); for up to 13 lags.
#Interpretation: There is a significant mutual Granger causality, this is logical since
# usually firms in one industry determine their price externally.

#### Own Price vs Own Sales ----
gct_autolag(redstar.df$LnPrice,redstar.df$LnSales,13)
#Results: lowest pvalue for v1~v2 is 0.037 (at 1 lag), lowest pvalue for v2~v1 is 0.014 (at 3 lags); for up to 13 lags.
#Interpretation: There is a significant mutual Granger causality, this is again logical because price-sales
# are in a similar relationship as supply-demand. Both influence each other.


##::::::::::::::::::::::::
##  Insignificant GCTs  ::
##::::::::::::::::::::::::
### Insignificant GCTs ----

#### Own Advertising vs Own Sales ----
gct_autolag(redstar.df$LnAdvertising,redstar.df$LnSales,13)
#Results: lowest pvalue for v1~v2 is 0.43 (at 3 lags), lowest pvalue for v2~v1 is 0.19 (at 2 lags); for up to 13 lags.
#Interpretation: There is no significant Granger causality present for up to 13 lags.

#### Own Price vs Total Comp. Advertising ----
gct_autolag(redstar.df$LnPrice,redstar.df$LnTotalCompAdvertising,13)
#Results: lowest pvalue for v1~v2 is 0.07 (at 1 lag), lowest pvalue for v2~v1 is 0.499 (at 3 lags); for up to 13 lags.
#Interpretation: There is no significant Granger causality present for up to 13 lags.

#### Avg. Comp. Price vs. Own Sales ----
gct_autolag(redstar.df$LnAvgCompPrice,redstar.df$LnSales,13)
#Results: lowest pvalue for v1~v2 is 0.087 (at 1 lag), lowest pvalue for v2~v1 is 0.414 (at 1 lag); for up to 13 lags.
#Interpretation: There is no significant Granger causality present for up to 13 lags.


##:::::::::::::::::::::::::::::
##  Stationary vs. Evolving  ::
##:::::::::::::::::::::::::::::
## Stationary vs. Evolving (step 2) ----

### Own Sales ----
adf.test(redstar.df$LnSales, nlag = 4, output = TRUE)
#Result: Type 1 (cannot reject), Type 2 and 3 (can reject!) = likely mean-stationary (4 lags)
pp.test(redstar.df$LnSales, output = TRUE)
#Result: Type 1 (cannot reject), Type 2 and 3 (can reject!) = probably mean-stationary (4 lags)
# CONCLUSION: ADF and PP sufficiently showcase we can reject unit root.  Own Sales is mean-stationary.

### Own Price ----
adf.test(redstar.df$LnPrice, nlag = 4, output = TRUE)
#Result: Type 1 (cannot reject), Type 2 and 3 (can reject!) = likely mean-stationary (4 lags)
pp.test(redstar.df$LnPrice, output = TRUE)
#Result: Type 1 (cannot reject), Type 2 and 3 (can reject!) = probably mean-stationary (4 lags)
# CONCLUSION: ADF and PP sufficiently showcase we can reject unit root. Own Price is mean-stationary.

### Own Advertising ----
adf.test(redstar.df$LnAdvertising, nlag = 4, output = TRUE)
#Result: Type 1, Type 2 and 3 (can reject!) = zero-mean, stationarity (4 lags)
pp.test(redstar.df$LnAdvertising, output = TRUE)
#Result: Type 1, Type 2 and 3 (can reject!) = zero-mean, stationarity (4 lags)
# CONCLUSION: Seems there is overwhelming evidence already from ADF and did PP just in case.
# Own Advertising has a zero mean (no drift, no trend)

### Total Comp. Advertising ----
adf.test(redstar.df$LnTotalCompAdvertising, nlag = 4, output = TRUE)
#Result: Type 1 (can reject at 1 lag, cannot 3-4), Type 2 and 3 (can reject!) = likely mean-stationarity (4 lags)
pp.test(redstar.df$LnTotalCompAdvertising, output = TRUE)
#Result: Type 1 (cannot reject), Type 2 and 3 (can reject!) = very likely mean-stationarity (4 lags)
# CONCLUSION: ADF and PP sufficiently showcase we can reject unit root. Total Comp. Advertising is mean-stationary

### Avg. Comp. Price ----
adf.test(redstar.df$LnAvgCompPrice, nlag = 4, output = TRUE)
#Result: Type 1 and Type 2 (cannot reject) and 3 (can reject only at lag 1, 3-4 cannot reject) = unclear
pp.test(redstar.df$LnAvgCompPrice, output = TRUE)
#Result: Type 1 and Type 2 (cannot reject) and 3 (can reject!) = it seems adding the trend made it significant, unclear
kpss.test(redstar.df$LnAvgCompPrice, output = TRUE)
#Result: Type 1 (cannot reject!), Type 2 & 3 (can reject) => unclear
# CONCLUSION: It's not very clear if it's stationary, therefore it is perhaps safer to choose a unit root.

# We will use the first difference for Avg.Comp.Price in the VAR model, others as levels
redstar.df$LnAvgCompPrice.diff <- c(0, diff(redstar.df$LnAvgCompPrice, lag = 1, differences=1))
#Note: We do not test for cointegration, as we have one evolving variable.

### 4-Scenarios ----
# Stationary Own Advertising -> Stationary Sales ('Business as Usual': effects of Advertising on Sales are temporary)
# Stationary Total Comp. Advertising -> Stationary Sales ('Business as Usual': effects of Total Comp. Advertising on Sales are temporary)
# Stationary Own Price -> Stationary Sales ('Business as Usual: effects of Own Price on Sales are temporary)
# Evolving Avg. Comp. Price -> Stationary Sales ('Escalation': continued Avg. Comp. Price changes have no permanent effect on our Sales)


##::::::::::::::::
##  VARx Model  ::
##::::::::::::::::
## VARx Model (Step 3) ----
#Exogenous: Qrtr1, Qrtr2, Qrtr3, Qrtr4 (we exclude one - "dummy trap")
#Endogenous (levels): LnSales, LnAdvertising, LnPrice, LnTotalCompAdvertising
#Endogeneous (first difference): LnAvgCompPrice.diff
#Test for: Linear trend (can be included through type = "both")

#Endogenous: LnSales, LnAdvertising, LnPrice, LnTotalCompAdvertising, LnAvgCompPrice.diff
rs.endogenous <- redstar.df[,c(4,5,6,7,13)]
#Exogenous: Qrtr2, Qrtr3, Qrtr4 (excl. Qrtr1 to avoid dummy-trap) - we interpret relative to the first quarter.
rs.exogenous <- redstar.df[,c(10,11,12)]

#Lag-length: 4 (based on business knowledge that after the end of 4 weeks effects are not there)
VARselect(rs.endogenous,lag.max = 4, type = "both", exogen = rs.exogenous)
#AIC and FPE say 3; HQ and BIC say 1
# CONCLUSION: BIC is more reliable, also previous MR shows 1 lag as optimal, so we go for 1 lag (p=1 in our VAR() model).

#VARx Model (we use type "both" as it includes both the intercept and linear trend):
rs.varx <- VAR(rs.endogenous, p=1, type = "both", exogen = rs.exogenous)

#Note: We can only interpret the exogenous variables, as endogenous are not clear.
#Own Sales:
summary(rs.varx, "LnSales")
#Sales had a significant immediate marketing effect during Quarter 2 and Quarter 3 (p<0.001) & Quarter 4 (p~0.008)

#Own Price:
summary(rs.varx,"LnPrice")
#Price had no significant immediate marketing effect these 4 quarters and a significant linear trend.

#Own Advertising:
summary(rs.varx, "LnAdvertising")
#Advertising had a significant immediate marketing effect during Quarter 2 (p~0.003)

#Total Comp. Advertising
summary(rs.varx, "LnTotalCompAdvertising")
#Total Competitor Advertising had a significant immediate marketing effect during Quarter 3 (p~0.046) and Quarter 4 (p<0.001)

#Average Competitor Price
summary(rs.varx, "LnAvgCompPrice.diff")
#Average Competitor Price had no significant marketing effect these 4 quarters.



##:::::::::::::::::::::
##  Generating IRFs  ::
##:::::::::::::::::::::
## Generating IRFs (Step 4) ----
#Notes: (1) Redstar works in a 13-week time perspective, which is important for our IRF and FIVD analyses. (n.ahead=13)
# (2) We judge the significance at 68% confidence interval (ci=0.68)

### IRF: Immediate (1) & Cumulative (2) for Sales (stationary) ----
rs.irf_sales1 <- irf(rs.varx, impulse = NULL, response = "LnSales", n.ahead = 13,
                ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                runs = 500)
plot(rs.irf_sales1)
#Results (1): All Immediate Effects on Sales die out/return back to 0
rs.irf_sales2 <- irf(rs.varx, impulse = NULL, response = "LnSales", n.ahead = 13,
                     ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                     runs = 500)
plot(rs.irf_sales2)
#Results (2): Sales on Sales is positive and significant (2a), Advertising on Sales is positive and significant (2b),
#Price on Sales is insignificant (2c), Total Comp. Advertising on Sales is negative and significant (2d),
#Avg.Comp.Price on Sales is insignificant (2e)


### IRF: Immediate (1) & Cumulative (2) for Advertising (stationary) ----
rs.irf_adv1 <- irf(rs.varx, impulse = NULL, response = "LnAdvertising", n.ahead = 13,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                     runs = 500)
plot(rs.irf_adv1)
#Results (1): All Immediate Effects on Advertising die out/return back to 0
rs.irf_adv2 <- irf(rs.varx, impulse = NULL, response = "LnAdvertising", n.ahead = 13,
                     ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                     runs = 500)
plot(rs.irf_adv2)
#Results (2): Sales on Advertising is insignificant (2a), Advertising on Advertising is positive and significant (2b),
#Price on Advertising is positive and significant (2c), Total Comp. Advertising on Own Advertising is insignificant (2d),
#Avg. Comp. Price on Advertising is positive and significant (2e)


### IRF: Immediate (1) & Cumulative (2) for Price (stationary) ----
rs.irf_price1 <- irf(rs.varx, impulse = NULL, response = "LnPrice", n.ahead = 13,
                   ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                   runs = 500)
plot(rs.irf_price1)
#Results (1): All Immediate Effects on Price die out/return back to 0
rs.irf_price2 <- irf(rs.varx, impulse = NULL, response = "LnPrice", n.ahead = 13,
                   ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                   runs = 500)
plot(rs.irf_price2)
#Results (2): Sales on Price is negative and significant (2a), Advertising on Price is negative and significant (2b),
#Price on Price is positive and significant (2c), Total Comp. Advertising on Price is positive and significant (2d),
#Avg. Comp. Price on Price insignificant (2e)


### IRF: Immediate (1) & Cumulative (2) for Total Comp. Advertising (stationary) ----
rs.irf_TCA1 <- irf(rs.varx, impulse = NULL, response = "LnTotalCompAdvertising", n.ahead = 13,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                     runs = 500)
plot(rs.irf_TCA1)
#Results (1): All Immediate Effects on Total Comp. Advertising die out/return back to 0
rs.irf_TCA2 <- irf(rs.varx, impulse = NULL, response = "LnTotalCompAdvertising", n.ahead = 13,
                     ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                     runs = 500)
plot(rs.irf_TCA2)
#Results (2): Sales on Total Comp. Advertising is negative and significant (2a), Advertising on Total Comp. Advertising is insignificant (2b),
#Price on Total Comp. Advertising insignificant (2c), Total Comp. Advertising on Total Comp. Advertising is positive and significant (2d),
#Avg. Comp. Price on Total Comp. Advertising insignificant (2e)



### IRF: Immediate (1) & Cumulative (2) for Avg. Comp. Price (evolving) ----
rs.irf_ACP1 <- irf(rs.varx, impulse = NULL, response = "LnAvgCompPrice.diff", n.ahead = 13,
                   ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68,
                   runs = 500)
plot(rs.irf_ACP1)
#Results (1): All Immediate Effects on Avg. Comp. Price (diff) die out/return back to 0
rs.irf_ACP2 <- irf(rs.varx, impulse = NULL, response = "LnAvgCompPrice.diff", n.ahead = 13,
                   ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68,
                   runs = 500)
plot(rs.irf_ACP2)
#Results (2): Sales on Avg. Comp. Price is positive and significant (2a), Advertising on Avg. Comp. Price is insignificant (2b),
#Price on Avg. Comp. Price negativa and significant (2c), Total Comp. Advertising on Avg. Comp. Price is insignificant (2d),
#Avg. Comp. Price on Avg. Comp. Price is positive and significant (2e)




##::::::::::::::::::::::
##  Generating FEVD  ::
##::::::::::::::::::::::
## Generating FEVD (Step 5) ----
# Notes: Quarter perspective - 13week periods


rs.fevd <-fevd(rs.varx,n.ahead = 13)


barbase1 <- rs.fevd[1]
barbase2 <- as.matrix(unlist(barbase1),ncol =5, byrow = TRUE)

bt1 <- Reduce(rbind,rs.fevd)
bt2 <- t(bt1)
bt2 <- bt2[,c(13,26,39,52,65)]
colnames(bt2) <- c("Sales", "Advertising", "Price", "Competitor Advertising (TCA)", "Competitor Price (ACP)")

#Look at % figures
bt2percent <- bt2 * 100
bt2percent
#                          Sales        Advertising      Price          Competitor Advertising (TCA)    Competitor Price (ACP)
#LnSales                 94.43140074     0.5513635      7.4029357              3.106185898                    2.0938992
#LnAdvertising           2.01284178     96.1540349      5.2218000              1.254972571                    4.5821679
#LnPrice                 0.29149036      2.2177624      83.7388025             0.998105220                    3.6209289
#LnTotalCompAdvertising  3.17119314      0.4602686      3.4619734              94.633836358                   0.2114916
#LnAvgCompPrice.diff     0.09307399      0.6165705      0.1744884              0.006899953                    89.4915124

#Visualize
barplot(bt2,  col = c("Green", "Purple", "Red", "Blue", "Orange"),
        main="Importance of Endogenous Drivers",
        names.arg = c("Sales","Advertising","Price","TCA","ACP"))
legend("topright",
       legend = c("Sales", "Advertising", "Price", "Competitor Advertising (TCA)", "Competitor Price (ACP)"),
       fill = c("Green", "Purple", "Red", "Blue", "Orange"))


