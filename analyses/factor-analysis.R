#NOTE: Ctrl-F "YOURPATH" to find all paths you need to change to your own.

##################################################################
##                         Preparation                          ##
##################################################################


#clear workspace
rm(list = ls())

#Handy package that Installs, Updates and Loads
# packages from CRAN, Github & Bioconductor
install.packages("librarian")
library("librarian")

#install & load all necessary packages with shelf function
librarian::shelf(nFactors,gplots,RColorBrewer,semPlot,
                 car,corrplot,dplyr,stats,bannerCommenter)

#external functions
#KMO function
kmo <- function(x) {
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
#BARTLETT function
bartlett.sphere<-function(data) {chi.square=-( (dim(data)[1]-1) -
  (2*dim(data)[2]-5)/6 )*
  log(det(cor(data,use='pairwise.complete.obs')));cat('chi.square value ',
                                                      chi.square , ' on ', (dim(data)[2]^2-dim(data)[2])/2,' degrees of freedom.'
  , ' p-value: ', 1-pchisq(chi.square,(dim(data)[2]^2-dim(data)[2])/2))}
#AIC & BIC combined function
a_bic <- function(model_name) {
  result <- c(AIC(model_name),BIC(model_name))
  cat('AIC: ', result[1], ' | ', 'BIC: ', result[2])
}
#Mean-center scale function
center_scale <- function(x){scale(x,scale = FALSE)}


#load the data
fashion <- read.csv('YOURPATH')

#check out data
head(fashion)
summary(fashion)
str(fashion)


##################################################################
##                          Question 1                          ##
##################################################################
#Question 1 ----


##::::::::::::::::##
##  Question 1.1  ##
##::::::::::::::::##

#Rename the Question columns to more descriptive names,
# based on the nature of the question.
#Lifestyle Questions 11-23 & Sustainable Questions 24-34 (seperated by new line)
colnames(fashion)[11:34] <- c("Taste_Q11", "Expensive_Q12", "NoPI_Q13", "Inspiration_Q14", "Functionality_Q15", "Eye_Q16", "Quality_Q17", "Comfort_Q18", "Necessity_Q19", "Shopping_Q20", "Fashion_Q21", "Read_Q22", "Coordination_Q23",
                              "Knowledge1_Q24", "Knowledge2_Q25", "Unaware_Q26", "Expert_Q27", "Unaware2_Q28", "Unware2_Q29", "BuyOrganic_Q30", "BuyOrganic2_Q31", "Influence_Q32", "Expensive_Q33", "Control_Q34")


## Q1.1 Lifestyle ----

#standardizing the data & create df for lifestyle containing only standardized lifestyle Qs
fashion.lifestyle <- data.frame(scale(fashion[, 11:23]))


##--------------------
##  Visualizations   -
##--------------------

#Plot Correlations for Lifestyle Questions
corrplot(cor(fashion[11:23]), order="hclust")

# Aggregate lifestyle attributes
fashion.lifestyle_mean <- aggregate(. ~ fashion$AgeCategory, data=fashion.lifestyle, mean)
fashion.lifestyle_mean <- fashion.lifestyle_mean[,-1]

## Heatmap of attribute by brand
heatmap(as.matrix(fashion.lifestyle_mean),
        col=brewer.pal(6, "YlOrRd"), Colv=NA, Rowv=NA,
        main="Lifestyle attributes")

heatmap(as.matrix(fashion.lifestyle_mean),
        col=brewer.pal(9, "YlOrRd"),
        main="Lifestyle attributes")



##--------------------
##  FA1: Lifestyle   -
##--------------------

#Is Factor Analysis suitable? (KMO & Bartlett)
kmo(fashion.lifestyle)
#KMO value: ~0.805 (range 0-1, closer to 1 the beter)
bartlett.sphere(fashion.lifestyle)
#Bartlett: p-value is significant, we can reject the H0.
#Conclusion: We can do FA!

#EFA
#How many factors to retain? (nscree and eigen values)
nScree(fashion.lifestyle)
eigen(cor(fashion.lifestyle))
#Conclusion: Explore 3 and 4 factors

# 3 factors vs. 4 factors

factanal(fashion.lifestyle, factors=3)
#### Shopping and Fashion load very strongly on Factor 1 and might be interpreted as Shopping factor
#### Taste and Eye load very strongly on Factor 2
#### No Purchase Intention load very strongly on Factor 3 as well as expensive and quality
#### p-value = 0.002277

factanal(fashion.lifestyle, factors=4)
#### Shopping and Fashion load very strongly on Factor 1 and might be interpreted as Shopping factor
#### Taste load very strongly on Factor 2
#### No Purchase Intention load very strongly on Factor 3
#### Necessity load very strongly on Factor 4
#### p-value = 0.219


#Run FA /w Varimax rotation for 3 factors + Visualize
lifestyle.fa3_vm <- factanal(fashion.lifestyle, factors=3, rotation="varimax")

heatmap.2(lifestyle.fa3_vm$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for lifestyle adjectives")
#### With 3 factors there are still show some correlations between other variables


#Run FA /w Varimax rotation for 4 factors + Visualize
lifestyle.fa4_vm <- factanal(fashion.lifestyle, factors=4, rotation="varimax")

heatmap.2(lifestyle.fa4_vm$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for lifestyle adjectives")
#### With 4 factors the heat map is more distributed among the other factors.
#### Thus, 4 factors should be retained for lifestyle factors.

#Plot the structure
semPaths(lifestyle.fa4_vm, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "darkred"),
         edge.label.cex=0.75, nCharNodes=7)

#Create a new scores data frame with age categories addded
lifestyle.fa4_vm <- factanal(fashion.lifestyle, factors=4, rotation="varimax",
                                    scores="Bartlett")
fashion.lifestyle.scores <- data.frame(lifestyle.fa4_vm$scores)
fashion.lifestyle.scores$AgeCategory <- fashion$AgeCategory

#Check how the age categories score on the different factors
fashion.lifestyle.scores_mean <- aggregate(. ~ AgeCategory, data=fashion.lifestyle.scores, mean)
rownames(fashion.lifestyle.scores_mean) <- fashion.lifestyle.scores_mean[, 1]
fashion.lifestyle.scores_mean <- fashion.lifestyle.scores_mean[, -1]
names(fashion.lifestyle.scores_mean) <- c("Trendy_Q20", "Fashionista_Q11", "Image_Q13", "Casual_Q19")
#Conclusion: Trendy and Fashionista - age groups below 20 years old (1), while Image for 31-40 (3) & Casual for 51-60 (5)


#Visualize above results in a heatmap
heatmap.2(as.matrix(fashion.lifestyle.scores_mean),
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          cexCol=1.2, main="Mean Factor Score\nby AgeCategory")


##::::::::::::::::##
##  Question 1.2  ##
##::::::::::::::::##

## Q1.2 Sustainability ----

#standardizing the data & create df for sustainability containing only standardized sustainability-related Qs
fashion.sustainable <- data.frame(scale(fashion[,24:34]))


##--------------------
##  Visualizations   -
##--------------------

#Plot Correlations for Sustainability Questions
corrplot(cor(fashion[24:34]), order="hclust")

# Aggregate sustainability attributes
fashion.sustainable_mean <- aggregate(. ~ fashion$AgeCategory, data=fashion.sustainable, mean)
fashion.sustainable_mean <- fashion.sustainable_mean[,-1]

## Heatmap of attribute by brand
heatmap(as.matrix(fashion.sustainable_mean),
        col=brewer.pal(6, "YlOrRd"), Colv=NA, Rowv=NA,
        main="Sustainability attributes")

heatmap(as.matrix(fashion.sustainable_mean),
        col=brewer.pal(9, "YlOrRd"),
        main="Sustainability attributes")



##-------------------------
##  FA2: Sustainability   -
##-------------------------


#Is Factor Analysis suitable? (KMO & Bartlett)
kmo(fashion.sustainable)
#KMO value: ~0.819 (range 0-1, closer to 1 the beter)
bartlett.sphere(fashion.sustainable)
#Bartlett: p-value is significant, we can reject the H0.
#Conclusion: We can do FA!

#EFA
#How many factors to retain? (nscree and eigen values)
nScree(fashion.sustainable)
eigen(cor(fashion.sustainable))
#Conclusion: Explore 2 or 3 factors



# 2 factors vs. 3 factors

factanal(fashion.sustainable, factors = 2)
#### Knowledge2 and Unaware load very strongly on Factor 1
#### BuyOrganic and BuyOrganic2 load very strongly on Factor 2
#### p-value = < 0.05

factanal(fashion.sustainable, factors=3)
#### Knowledge1 and Knowledge2 load very strongly on Factor 1
#### Unaware3 load very strongly on Factor 2
#### BuyOrganic and BuyOrganic2 load very strongly on Factor 3
#### p-value = 0.343


#Run FA /w Varimax rotation for 2 factors + Visualize
sustainable.fa2_vm <- factanal(fashion.sustainable, factors=2, rotation="varimax")

heatmap.2(sustainable.fa2_vm$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for sustainability adjectives")
#### With 2 factors the heat map has a lot of variables marked red


#Run FA /w Varimax rotation for 3 factors + Visualize
sustainable.fa3_vm <- factanal(fashion.sustainable, factors=3, rotation="varimax")

heatmap.2(sustainable.fa3_vm$loadings,
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for sustainability adjectives")
#### With 3 factors the heat map is more distributed among the other factors.
#### Thus, 3 factors should be retained for sustainable factors.

#Plot the structure
semPaths(sustainable.fa3_vm, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "darkred"),
         edge.label.cex=0.75, nCharNodes=7)

#Create a new scores data frame with age categories addded
sustainable.fa3_vm <- factanal(fashion.sustainable, factors=3, rotation="varimax",
                             scores="Bartlett")
fashion.sustainable.scores <- data.frame(sustainable.fa3_vm$scores)
fashion.sustainable.scores$AgeCategory <- fashion$AgeCategory

#Check how the age categories score on the different factors
fashion.sustainable.scores_mean <- aggregate(. ~ AgeCategory, data=fashion.sustainable.scores, mean)
rownames(fashion.sustainable.scores_mean) <- fashion.sustainable.scores_mean[, 1]
fashion.sustainable.scores_mean <- fashion.sustainable.scores_mean[, -1]
names(fashion.sustainable.scores_mean) <- c("Knowledge_Q25", "Awareness_Q29", "Interest_Q31")
#Conclusion: Interest - age group 61+ (6); Knowledge - age group below 20 (1); Awareness - age group 51 to 60 (5)

#Visualize above results in a heatmap
heatmap.2(as.matrix(fashion.sustainable.scores_mean),
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          cexCol=1.2, main="Mean Factor Score\nby AgeCategory")

fashion.sustainable.scores_mean

##----------------------------
##  Save Factors for Q3/Q4   -
##----------------------------

#Save our factors for later use (fashion.factors)
#Lifestyle factors: "Trendy" (Q20), "Fashionista" (Q11), "Image" (Q13), "Casual" (Q19)
#Perception factors: "Knowledge" (Q25), "Awareness" (Q29), "Interest" (Q31)
fashion.factors <- cbind(fashion.lifestyle.scores[,1:4], fashion.sustainable.scores[,1:3])
names(fashion.factors) <- c("Trendy_Q20", "Fashionista_Q11", "Image_Q13", "Casual_Q19",
                            "Knowledge_Q25", "Awareness_Q29", "Interest_Q31")



##################################################################
##                          Question 2                          ##
##################################################################

#Question 2 ----

##----------------------##
##  Preparing the Data  ##
##----------------------##

#Create a dataframe for Question 2: add and scale necessary variables
fashion.q2 <- data.frame(select(fashion,PI1,WTR,PurchaseFreq))

#Add dummy education levels, based on Education from fashion df
#(High School=1, MBO=2, HBO=3, WO=4)
fashion.q2$edu_hs <- ifelse(fashion$Education == 1, 1, 0)
fashion.q2$edu_mbo <- ifelse(fashion$Education == 2, 1, 0)
fashion.q2$edu_hbo <- ifelse(fashion$Education == 3, 1, 0)
fashion.q2$edu_wo <- ifelse(fashion$Education == 4, 1, 0)



##----------------------------------##
##  Preliminary Work (Exploration)  ##
##----------------------------------##

## Are the focal variables showing some correlations?
library(corrplot)
corrplot.mixed(cor(fashion[ , c(4, 35, 37)]), upper="ellipse")
corrplot.mixed(cor(fashion[ , c(7, 35, 37)]), upper="ellipse")
### Note: Correlations close to zero are plotted as circular and gray (using the color scheme we specified),
# while magnitudes away from zero produce ellipses that are increasingly
# tighter and blue for positive correlation and red for negative.


################################
##   Preliminary PI1 Models   ##
################################
# Without Education Categories #

# Fitting a model with single predictor for Purchase Intention
dv_PI1.iv_Edu <-lm(PI1~Education, data=fashion)
summary(dv_PI1.iv_Edu)
## Adjusted R-squared 0.02073
## Explanatory power p-value = 0.04021 the model has a good explanatory power

## Checks that you should do before doing an ANOVA
str(dv_PI1.iv_Edu)
dv_PI1.iv_Edu$coefficients
confint(dv_PI1.iv_Edu)
### This confirms our computation by hand, that the best estimate for the relationship PI1 ∼ Education is -0.2072 (with slight differences due to rounding). It is a best practice to report the range of an estimate, not just the single best point.
anova(dv_PI1.iv_Edu)

# Fitting a model with multiple predictors (Education + Purchasefrequency) for Purchase Intention
dv_PI1.iv_Edu_PFreq <-lm(PI1~Education + PurchaseFreq, data=fashion)
summary(dv_PI1.iv_Edu_PFreq)
## Adjusted R-squared 0.04506. There was an increase of R-squared after adding purchasefreq
## Explanatory power p-value = 0.01088 the model has a good explanatory power

# AIC and BIC of models above
a_bic(dv_PI1.iv_Edu)
#AIC:  469.5958  |  BIC:  478.7454
a_bic(dv_PI1.iv_Edu_PFreq)
#AIC:  466.6548  |  BIC:  478.8542
## No significant difference


################################
##   Preliminary WTR Models   ##
################################
# Without Education Categories #

# Fitting a model with single predictor for Willingness to Recommend
dv_WTR.iv_Edu <-lm(WTR~Education, data=fashion)
summary(dv_WTR.iv_Edu)
## Adjusted R-squared -0.004925
## Explanatory power p-value = 0.6246 the model does not have a good explanatory power


# Fitting a model with multiple predictors (Education + Purchasefrequency) for Willingness to Recommend
dv_WTR.iv_Edu_PFreq <-lm(WTR~Education + PurchaseFreq, data=fashion)
summary(dv_WTR.iv_Edu_PFreq)
## Adjusted R-squared 0.002849. There was an increase of R-squared after adding purchasefreq
## Explanatory power p-value = 0.2977 the model does NOT have a good explanatory power


# AIC and BIC of models above
a_bic(dv_WTR.iv_Edu)
#AIC:  436.7201  |  BIC:  445.8696
a_bic(dv_WTR.iv_Edu_PFreq)
#AIC:  436.4923  |  BIC:  448.6918
## The second model is better
## WTR and education does not have a good explanatory power + the model does not explain much



##########################
##      PI1 Models      ##
##########################
## Education Categories ##
##   ANOVA Analysis 1   ##

##Question 2.1 ----


#PI1 Full Model: PI1 = edu_hs + edu_mbo + edu_hbo (dv_PI1.iv_EduCat_FULL)
#We exclude edu_wo from the model to use it as a benchmark
dv_PI1.iv_EduCat_FULL <- lm(PI1~edu_hs + edu_mbo + edu_hbo, data=fashion.q2)
summary(dv_PI1.iv_EduCat_FULL)
#(Intercept)   estimate:  2.4118;  p-value: <2e-16 ***
#edu_hs        estimate:  0.3660;  p-value: 0.3391
#edu_mbo       estimate:  0.5327;  p-value: 0.0637 .
#edu_hbo       estimate:  0.4079;  p-value: 0.0332 *
#Multiple R-squared:  0.03994,	Adjusted R-squared:  0.02099
#F-statistic: 2.108 on 3 and 152 DF,  p-value: 0.1016
a_bic(dv_PI1.iv_EduCat_FULL)
#AIC:  471.5152  |  BIC:  486.7645

#PI1 Reduced Model: PI1 = 1 (dv_PI1.iv_EduCat_REDUCED)
dv_PI1.iv_EduCat_REDUCED <- lm(PI1~1, data=fashion.q2)

#ANOVA PI1: Reduced vs. Full Model
# Homogeneity of slopes test
anova(dv_PI1.iv_EduCat_REDUCED,dv_PI1.iv_EduCat_FULL)
#p-value: 0.1016
#Conclusion: H0 cannot be rejected
## The reduced model is not significantly better than the full model
## Different educational levels have the same purchase intention
## No significance --> We can do an ANCOVA analysis


##########################
##      WTR Models      ##
##########################
## Education Categories ##
##   ANOVA Analysis 2   ##

#WTR Full Model: WTR = edu_hs + edu_mbo + edu_hbo (dv_WTR.iv_EduCat_FULL)
#We exclude edu_wo from the model to use it as a benchmark
dv_WTR.iv_EduCat_FULL <- lm(WTR~edu_hs + edu_mbo + edu_hbo, data=fashion.q2)
summary(dv_WTR.iv_EduCat_FULL)
#(Intercept)  estimate:  2.67647;  p-value: <2e-16 ***
#edu_hs       estimate:  -0.12092;  p-value: 0.727
#edu_mbo      estimate:  -0.06536;  p-value: 0.801
#edu_hbo      estimate:  -0.10270;  p-value: 0.551
#Multiple R-squared:  0.002664,	Adjusted R-squared:  -0.01702
#F-statistic: 0.1354 on 3 and 152 DF,  p-value: 0.9388
a_bic(dv_WTR.iv_EduCat_FULL)
#AIC:  440.5472  |  BIC:  455.7965

#WTR Reduced Model: WTR = 1 (dv_WTR.iv_EduCat_REDUCED)
dv_WTR.iv_EduCat_REDUCED <- lm(WTR~1, data=fashion.q2)


#ANOVA WTR: Reduced vs. Full Model
# Homogeneity of slopes test
anova(dv_WTR.iv_EduCat_REDUCED,dv_WTR.iv_EduCat_FULL)
#p-value: 0.9388
#Conclusion: H0 cannot be rejected
## The reduced model is not significantly better than the full model
## Different educational levels have the same willingness to recommend
## No significance --> We can do an ANCOVA analysis


##########################
##      PI1 Models      ##
##########################
## Education Categories ##
##  Purchase Frequency  ##
##   ANCOVA Analysis 1  ##

##Question 2.2 ----


#PI1 Reduced Model: PI1 = PurchaseFreq (dv_PI1.iv_PFreq_REDUCED)
dv_PI1.iv_PFreq_REDUCED <- lm(PI1 ~ PurchaseFreq, data=fashion.q2)
summary(dv_PI1.iv_PFreq_REDUCED)
#(Intercept)   estimate: 2.46752;  p-value: <2e-16 ***
#PurchaseFreq  estimate: 0.01959;  p-value: 0.0732 .
#Multiple R-squared:  0.0207,	Adjusted R-squared:  0.01434
#F-statistic: 3.255 on 1 and 154 DF,  p-value: 0.07318
a_bic(dv_PI1.iv_PFreq_REDUCED)
#AIC:  470.6107  |  BIC:  479.7603


#PI1 Restricted Model: PI1 = PurchaseFreq + edu_hs + edu_mbo + edu_hbo (dv_PI1.iv_PFreq_EduCat_RESTRICTED)
#We exclude edu_wo from the model to use it as a benchmark
dv_PI1.iv_PFreq_EduCat_RESTRICTED <- lm(PI1 ~ PurchaseFreq + edu_hs + edu_mbo + edu_hbo, data=fashion.q2)
summary(dv_PI1.iv_PFreq_EduCat_RESTRICTED)
#(Intercept)   estimate: 2.12023;  p-value: <2e-16 ***
#PurchaseFreq  estimate: 0.02605;   p-value: 0.0179 *
#edu_hs        estimate: 0.42599;   p-value: 0.2599
#edu_mbo       estimate: 0.65633;   p-value: 0.0229 *
#edu_hbo       estimate: 0.47438;   p-value: 0.0131 *
#Multiple R-squared:  0.07502,	Adjusted R-squared:  0.05052
#F-statistic: 3.062 on 4 and 151 DF,  p-value: 0.01848
a_bic(dv_PI1.iv_PFreq_EduCat_RESTRICTED)
#AIC:  467.7071  |  BIC:  486.0062


#ANOVA PI1: Reduced vs. Restricted Model
anova(dv_PI1.iv_PFreq_REDUCED,dv_PI1.iv_PFreq_EduCat_RESTRICTED)
#p-value: 0.03438 *
#Conclusion: H0 is rejected
#The Restricted Model is better
#Not all education levels have the same Purchase Intention,
# after accounting for the effect of Purchase Frequency.



#PI1 Full Model: PI1 = PurchaseFreq + edu_hs + edu_mbo + edu_hbo + PurchaseFreq*(edu_hs + edu_mbo + edu_hbo) (dv_PI1.iv_PFreq_EduCat_FULL)
#We exclude edu_wo from the model to use it as a benchmark
dv_PI1.iv_PFreq_EduCat_FULL <- lm(PI1 ~ PurchaseFreq + edu_hs + edu_mbo + edu_hbo +
                                      PurchaseFreq*(edu_hs + edu_mbo + edu_hbo), data=fashion.q2)
summary(dv_PI1.iv_PFreq_EduCat_FULL)
#(Intercept)           estimate: 2.073122;   p-value: <2e-16 ***
#PurchaseFreq          estimate: 0.030260;   p-value: 0.0173 *
#edu_hs                estimate: -0.593431;   p-value: 0.5406
#edu_mbo               estimate: 0.614006;   p-value: 0.3009
#edu_hbo               estimate: 0.716913;   p-value: 0.0219 *
#PurchaseFreq:edu_hs   estimate: 0.115775;   p-value: 0.2479
#PurchaseFreq:edu_mbo  estimate: 0.009669;   p-value: 0.9024
#PurchaseFreq:edu_hbo  estimate: -0.026829;   p-value: 0.3148
#Multiple R-squared:  0.09065,	Adjusted R-squared:  0.04764
#F-statistic: 2.108 on 7 and 148 DF,  p-value: 0.04611
a_bic(dv_PI1.iv_PFreq_EduCat_FULL)
#AIC:  471.0485  |  BIC:  498.4972

#ANCOVA PI1: Restricted vs. Full Model
# Homogeneity of slopes test
anova(dv_PI1.iv_PFreq_EduCat_RESTRICTED,dv_PI1.iv_PFreq_EduCat_FULL)
#p-value: 0.4698
#Conclusion: H0 cannot be rejected
## Full model is not better
##Effect of PurchaseFreq on PI1 is the same for all education levels


##########################
##      WTR Models      ##
##########################
## Education Categories ##
##  Purchase Frequency  ##
##   ANCOVA Analysis 2  ##


#WTR Reduced Model: WTR = PurchaseFreq (dv_WTR.iv_PFreq_REDUCED)
dv_WTR.iv_PFreq_REDUCED <- lm(WTR ~ PurchaseFreq, data=fashion.q2)
summary(dv_WTR.iv_PFreq_REDUCED)
#(Intercept)   estimate: 2.479178;  p-value: <2e-16 ***
#PurchaseFreq  estimate: 0.014992;  p-value: 0.123
#Multiple R-squared:  0.01536,	Adjusted R-squared:  0.008968
#F-statistic: 2.403 on 1 and 154 DF,  p-value: 0.1232
a_bic(dv_WTR.iv_PFreq_REDUCED)
#AIC:  434.5484  |  BIC:  443.6979


#WTR Restricted Model: WTR = PurchaseFreq + edu_hs + edu_mbo + edu_hbo (dv_WTR.iv_PFreq_EduCat_RESTRICTED)
#We exclude edu_wo from the model to use it as a benchmark
dv_WTR.iv_PFreq_EduCat_RESTRICTED <- lm(WTR ~ PurchaseFreq + edu_hs + edu_mbo + edu_hbo, data=fashion.q2)
summary(dv_WTR.iv_PFreq_EduCat_RESTRICTED)
#(Intercept)   estimate: 2.512940;  p-value: <2e-16 ***
#PurchaseFreq  estimate: 0.014612;  p-value: 0.145
#edu_hs        estimate: -0.087273;  p-value: 0.801
#edu_mbo       estimate: 0.004002;  p-value: 0.988
#edu_hbo       estimate: -0.065412;  p-value: 0.706
#Multiple R-squared:  0.01665,	Adjusted R-squared:  -0.009397
#F-statistic: 0.6392 on 4 and 151 DF,  p-value: 0.6353
a_bic(dv_WTR.iv_PFreq_EduCat_RESTRICTED)
#AIC:  440.3439  |  BIC:  458.643


#ANOVA WTR: Reduced vs. Restricted Model
# Homogeneity of slopes test
anova(dv_WTR.iv_PFreq_REDUCED,dv_WTR.iv_PFreq_EduCat_RESTRICTED)
#p-value: 0.9778
#Conclusion: H0 is not rejected
## Restricted model is not better
## All education levels have the same Willingness to Recommend,
## after accounting for the effect of PurchaseFreq.


#WTR Full Model: WTR = PurchaseFreq + edu_hs + edu_mbo + edu_hbo + PurchaseFreq*(edu_hs + edu_mbo + edu_hbo) (dv_WTR.iv_PFreq_Edu_FULL)
#We exclude edu_wo from the model to use it as a benchmark
dv_WTR.iv_PFreq_EduCat_FULL <- lm(WTR ~ PurchaseFreq + edu_hs + edu_mbo + edu_hbo +
  PurchaseFreq*(edu_hs + edu_mbo + edu_hbo), data=fashion.q2)
summary(dv_WTR.iv_PFreq_EduCat_FULL)
#(Intercept)           estimate: 2.495556   0.176006  14.179   <2e-16 ***
#PurchaseFreq          estimate: 0.016166   0.011604   1.393    0.166
#edu_hs                estimate: -0.292461   0.893084  -0.327    0.744
#edu_mbo               estimate: 0.180844   0.545949   0.331    0.741
#edu_hbo               estimate: -0.008694   0.285744  -0.030    0.976
#PurchaseFreq:edu_hs   estimate: 0.023486   0.092129   0.255    0.799
#PurchaseFreq:edu_mbo  estimate: -0.026297   0.072676  -0.362    0.718
#PurchaseFreq:edu_hbo  estimate: -0.006106   0.024552  -0.249    0.804
#Multiple R-squared:  0.01835,	Adjusted R-squared:  -0.02808
#F-statistic: 0.3952 on 7 and 148 DF,  p-value: 0.904
a_bic(dv_WTR.iv_PFreq_EduCat_FULL)
#AIC:  446.0741  |  BIC:  473.5228


#ANCOVA WTR: Restricted vs. Full Model
# Homogeneity of slopes test
anova(dv_WTR.iv_PFreq_EduCat_RESTRICTED,dv_WTR.iv_PFreq_EduCat_FULL)
#p-value: 0.9679
#Conclusion: H0 cannot be rejected
## Full model is not better
## Effect of PurchaseFreq on WTR is the same for all education levels,
## after accounting for Purchase Frequency



##################################################################
##                          Question 3                          ##
##################################################################

# Question 3 ----

#DV: PI1, WTR
#IVs: 7 factors (from Question 1) and Female binary variable

#Create a dataframe for Question 3: add necessary variables (factors, female binary, PI1, WTR)
#All variables are standardized, beside the dummy variable
fashion.q3 <- cbind(data.frame(scale(select(fashion,PI1,WTR))),fashion.factors)
fashion.q3$Female <- ifelse(fashion$Gender == 1, 1, 0)


##########################
##      PI1 Model       ##
##########################

# PI1 -> Female + Lifestyle Factors + Sustainability Factors
dv_PI1.iv_factors_female <- lm(PI1~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25, data=fashion.q3)
summary(dv_PI1.iv_factors_female)
#(Intercept)            estimate: -0.1572208; p-value: 0.069117 .
#Female                 estimate: 0.6454328;  p-value: 0.000500 ***
#Trendy_Q20             estimate: -0.0255960; p-value: 0.700007
#Fashionista_Q11        estimate: 0.0459259;  p-value: 0.512463
#Image_Q13              estimate: 0.2380257;  p-value: 0.000227 ***
#Casual_Q19             estimate: -0.0379368; p-value: 0.538471
#Knowledge_Q25          estimate: 0.0314371;  p-value: 0.681124
#Awareness_Q29          estimate: 0.1103011;  p-value: 0.146301
#Interest_Q31           estimate: 0.0008572;  p-value: 0.989870
#Multiple R-squared:  0.1978,	Adjusted R-squared:  0.1541
#F-statistic: 4.531 on 8 and 147 DF,  p-value: 6.05e-05

sort(abs(dv_PI1.iv_factors_female$coefficients[2:9]),decreasing=TRUE)
#Ranking of most important predictors of PI1
#1 Female (SIGNIFICANT)
#2 Image_Q13 (SIGNIFICANT)
#3 Awareness_Q29 (insignificant)
#4 Fashionista_Q11 (insignificant)
#5 Casual_Q19 (insignificant)
#6 Knowledge_Q25 (insignificant)
#7 Trendy_Q20 (insignificant)
#8 Interest_Q31 (insignificant)

#Linear Hypothesis Check
linearHypothesis(dv_PI1.iv_factors_female, "Female = Image_Q13")
#p-value :0.04479 *


##########################
##      WTR Model       ##
##########################

# WTR -> Female + Lifestyle Factors + Perception Factors
dv_WTR.iv_factors_female <- lm(WTR~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25, data=fashion.q3)
summary(dv_WTR.iv_factors_female)
#(Intercept)            estimate: -0.06834;  p-value: 0.45149
#Female                 estimate: 0.28054;   p-value: 0.14429
#Trendy_Q20             estimate: 0.10189;   p-value: 0.14704
#Fashionista_Q11        estimate: -0.03416;  p-value: 0.64387
#Image_Q13              estimate: 0.2380257; p-value: 0.00938 **
#Casual_Q19             estimate: -0.07593;  p-value: 0.24363
#Knowledge_Q25          estimate: 0.01999;   p-value: 0.80420
#Awareness_Q29          estimate: 0.12864;   p-value: 0.10831
#Interest_Q31           estimate: -0.09947;  p-value: 0.16364
#Model Fit: Multiple R-squared:  0.1085,	Adjusted R-squared:  0.05994
#F-statistic: 2.235 on 8 and 147 DF,  p-value: 0.02794

sort(abs(dv_WTR.iv_factors_female$coefficients[2:9]),decreasing=TRUE)
#Ranking of most important predictors of WTR
#1 Female (insignificant)
#2 Image_Q13 (SIGNIFICANT)
#3 Awareness_Q29 (insignificant)
#4 Trendy_Q20 (insignificant)
#5 Interest_Q31 (insignificant)
#6 Casual_Q19 (insignificant)
#7 Fashionista_Q11 (insignificant)
#8 Knowledge_Q25 (insignificant)






# Testing for Multicollinearity in IVs
vif_values <- vif(dv_PI1.iv_factors_female)
tolerance <- 1/vif_values
vif_values
## all values are below 4 thus there is no high collinearity
tolerance
## all values are not close to 0 thus there is no high collinearity

# Visualize VIF values
## create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, xlim = c(0,12), col = "red")
## add vertical line at 4 and 10
abline(v = 4, lwd = 3, lty = 2)
abline(v = 10, lwd = 3, lty = 2)
abline(v = 1, lwd = 3, lty = 2)

##################################################################
##                          Question 4                          ##
##################################################################

# Question 4 ----

#DV: PI1, WTR
#IVs: 7 factors (from Question 1), MoneySpent, Education levels, Female

#Create a dataframe for Question 4: add necessary variables (factors, PI1, WTR, MoneySpent, Education levels, Female)
## Mean-center the focal independent variables
fashion.q4 <- cbind(data.frame(center_scale(fashion.q3[,3:10])), data.frame(center_scale(fashion.q2[,c("edu_hs","edu_mbo","edu_hbo","edu_wo")])))
fashion.q4 <- cbind(fashion.q4,fashion.q3[,c("PI1","WTR")])
fashion.q4$MoneySpent <- c(center_scale(fashion$MoneySpent))



##############################
##        PI1 Model         ##
##############################
##        L&S Factors       ##
##        Money Spent       ##
##   Moderation Analysis 1  ##

##Question 4.1 ----


# PI1 mc: Estimate the model with the mean-centered independent variables
dv_PI1.iv_factors_mc <-lm (PI1 ~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25, data=fashion.q4)
summary(dv_PI1.iv_factors_mc)
#(Intercept)      estimate: -4.106e-16;  p-value: 1.000
#Female           estimate: 6.454e-01;  p-value: 0.0005 ***
#Trendy_Q20       estimate: -2.560e-02;   p-value: 0.700
#Fashionista_Q11  estimate: 4.593e-02;   p-value: 0.512
#Image_Q13        estimate: 2.380e-01;  p-value: 0.000227 ***
#Casual_Q19       estimate: -3.794e-02;   p-value: 0.538
#Awareness_Q29    estimate: 1.103e-01;   p-value: 0.146
#Interest_Q31     estimate: 8.572e-04;   p-value: 0.989
#Knowledge_Q25    estimate: 3.144e-02;  p-value: 0.681
#Multiple R-squared:  0.1286,	Adjusted R-squared:  0.08742
#F-statistic: 4.531 on 8 and 147 DF,  p-value: 0.0000605

sort(abs(dv_PI1.iv_factors_mc$coefficients[2:9]),decreasing=TRUE)
## Importance of the variables ranked from highest to lowest
#Image_Q13 (SIGNIFICANT)
#Awareness_Q29 (insignificant)
#Fashionista_Q11 (insignificant)
#Casual_Q19  (insignificant)
#Knowledge_Q25 (insignificant)
#Trendy_Q20 (insignificant)
#Interest_Q31 (insignificant)

# PI1 mc: with SYNERGY (MoneySpent)
dv_PI1.iv_factors_MoneySpent_mc <-lm (PI1 ~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25 + MoneySpent*(Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25), data=fashion.q4)
summary(dv_PI1.iv_factors_MoneySpent_mc)
#(Intercept)                 estimate: 1.923e-02;   p-value: 0.81565
#Female                      estimate: 6.236e-01;   p-value: 0.00159 **
#Trendy_Q20                  estimate: -5.655e-02;   p-value: 0.42093
#Fashionista_Q11             estimate: 3.813e-03;   p-value: 0.95815
#Image_Q13                   estimate: 1.912e-01;   p-value: 0.00933 **
#Casual_Q19                  estimate: -5.893e-02;   p-value: 0.35391
#Awareness_Q29               estimate:  1.105e-01;   p-value: 0.16979
#Interest_Q31                estimate: 2.115e-02;   p-value: 0.76324
#Knowledge_Q25               estimate: 9.124e-02;   p-value:  0.28027
#MoneySpent                  estimate: 2.435e-03;   p-value: 0.15217
#Trendy_Q20:MoneySpent       estimate: -7.252e-04;   p-value: 0.51140
#Fashionista_Q11:MoneySpent  estimate: -1.838e-05;   p-value: 0.98653
#Image_Q13:MoneySpent        estimate: -3.638e-04;   p-value: 0.79049
#Casual_Q19:MoneySpent       estimate: -8.614e-04;   p-value: 0.40343
#Awareness_Q29:MoneySpent    estimate: -3.571e-04;   p-value: 0.83747
#Interest_Q31:MoneySpent     estimate: 3.764e-04;   p-value: 0.79843 
#Knowledge_Q25:MoneySpent    estimate: 2.948e-03;   p-value: 0.07893 . 
#Multiple R-squared:  0.2376,	Adjusted R-squared:  0.1499 
#F-statistic: 2.708 on 16 and 139 DF,  p-value: 0.0009044

sort(abs(dv_PI1.iv_factors_MoneySpent_mc$coefficients),decreasing=TRUE)
## When people start spending more on clothing sustainable knowledge became the most important driver. (Knowledge_Q25:MoneySpent  2.948e-03)
## While for people who spend less on clothing image is most important factor to purchase the clothing. (Image_Q13  0.00933)
## There is a synergy effect: effect of PI is strengthened by spending more money when sustainable knowledge is involved.

## Importance of the variables ranked from highest to lowest
#Image_Q13 (SIGNIFICANT) <--
#Awareness_Q29 (insignificant)
#Knowledge_Q25 (insignificant)
#Casual_Q19 (insignificant)
#Trendy_Q20 (insignificant)
#Interest_Q31 (insignificant)
#Fashionista_Q11 (insignificant)
#Knowledge_Q25:MoneySpent (MARGINALLY SIGNIFICANT) <--
#MoneySpent (insignificant)
#Casual_Q19:MoneySpent (insignificant)
#Trendy_Q20:MoneySpent (insignificant)
#Interest_Q31:MoneySpent (insignificant)
#Image_Q13:MoneySpent (insignificant)
#Awareness_Q29:MoneySpent (insignificant)
#Fashionista_Q11:MoneySpent (insignificant)



#################################
##          WTR Model          ##
#################################
##         L&S Factors         ##
##        Edu Categories       ##
##     Moderation Analysis 2   ##

##Question 4.2 ----

# Estimate the model with the mean-centered independent variables
dv_WTR.iv_factors_EduCat_mc <-lm (WTR ~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25 + edu_hs + edu_mbo + edu_hbo, data=fashion.q4)
summary(dv_WTR.iv_factors_EduCat_mc)
#(Intercept)      estimate: 7.045e-17;  p-value: 1.0000
#Female           estimate: 3.063e-01;  p-value: 0.1269  
#Trendy_Q20       estimate: 1.068e-01;  p-value: 0.1360  
#Fashionista_Q11  estimate: -3.876e-02; p-value: 0.6092  
#Image_Q13        estimate: 1.732e-01;  p-value: 0.0112 *
#Casual_Q19       estimate: -7.024e-02; p-value: 0.2941  
#Awareness_Q29    estimate: 1.271e-01;  p-value: 0.1166  
#Interest_Q31     estimate: -9.589e-02; p-value: 0.1856  
#Knowledge_Q25    estimate: 3.351e-02;  p-value: 0.6926  
#edu_hs           estimate: 2.401e-02;  p-value: 0.9460  
#edu_mbo          estimate: -1.103e-01; p-value: 0.6865  
#edu_hbo          estimate: -9.487e-02; p-value: 0.6153  
#Multiple R-squared:  0.1106,	Adjusted R-squared: 0.04268
#F-statistic: 1.628 on 11 and 144 DF,  p-value: 0.0966 (MARGINALLY SIGNIFICANT)

sort(abs(dv_WTR.iv_factors_EduCat_mc$coefficients),decreasing=TRUE)
## People who have a higher education (university) most important driver is Image

## Importance of the variables ranked from highest to lowest
# Image_Q13 (SIGNIFICANT)
# Awareness_Q29 (insignificant)
# edu_mbo (insignificant)
# Trendy_Q20 (insignificant)
# Interest_Q31 (insignificant)
# edu_hbo (insignificant)
# Casual_Q19 (insignificant)
# Fashionista_Q11 (insignificant)
# Knowledge_Q25 (insignificant)
# edu_hs (insignificant)

# WTR mc: with SYNERGY (Education Categories)
dv_WTR.iv_factors_EduCat_mc_synergy <-lm (WTR ~ Female + Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19
  + Awareness_Q29 + Interest_Q31 + Knowledge_Q25 + edu_hs + edu_mbo + edu_hbo +
  edu_hs*(Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19 + Awareness_Q29 + Interest_Q31 + Knowledge_Q25) +
  edu_mbo*(Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19 + Awareness_Q29 + Interest_Q31 + Knowledge_Q25) +
  edu_hbo*(Trendy_Q20 + Fashionista_Q11 + Image_Q13 + Casual_Q19 + Awareness_Q29 + Interest_Q31 + Knowledge_Q25), data=fashion.q4)
summary(dv_WTR.iv_factors_EduCat_mc_synergy)
#(Intercept)              estimate: -0.01939; p-value: 0.84927
#Female                   estimate: 0.28442; p-value: 0.17797
#Trendy_Q20               estimate: 0.12290; p-value: 0.25470
#Fashionista_Q11          estimate: -0.07293; p-value: 0.38054
#Image_Q13                estimate: 0.22227; p-value: 0.00257 **
#Casual_Q19               estimate: -0.14029; p-value: 0.13419
#Awareness_Q29            estimate: 0.14129; p-value: 0.23848
#Interest_Q31             estimate: -0.07485; p-value: 0.54207
#Knowledge_Q25            estimate: 0.06119; p-value: 0.67904
#edu_hs                   estimate: 0.41089; p-value: 0.68092
#edu_mbo                  estimate: 0.20494; p-value: 0.54449
#edu_hbo                  estimate: -0.01877; p-value: 0.92169
#Trendy_Q20:edu_hs        estimate: 0.86310; p-value: 0.52763
#Fashionista_Q11:edu_hs   estimate: 0.10921; p-value: 0.77598
#Image_Q13:edu_hs         estimate: 0.52143; p-value: 0.36036
#Casual_Q19:edu_hs        estimate: 0.06049; p-value: 0.95529
#Awareness_Q29:edu_hs     estimate: 0.35093; p-value: 0.80544
#Interest_Q31:edu_hs      estimate: 0.78161; p-value: 0.64718
#Knowledge_Q25:edu_hs     estimate: 0.12935; p-value: 0.94929
#Trendy_Q20:edu_mbo       estimate: -0.24553; p-value: 0.31954
#Fashionista_Q11:edu_mbo  estimate: 0.52867; p-value: 0.09179 .
#Image_Q13:edu_mbo        estimate: -0.43509; p-value: 0.05679 .
#Casual_Q19:edu_mbo       estimate: 0.28669; p-value: 0.28799
#Awareness_Q29:edu_mbo    estimate: 0.13980; p-value: 0.62337
#Interest_Q31:edu_mbo     estimate: -0.33506; p-value: 0.29624
#Knowledge_Q25:edu_mbo    estimate: -0.01007; p-value: 0.97349
#Trendy_Q20:edu_hbo       estimate: -0.02216; p-value: 0.89107
#Fashionista_Q11:edu_hbo  estimate: -0.03692; p-value: 0.83689
#Image_Q13:edu_hbo        estimate: -0.08055; p-value: 0.57613
#Casual_Q19:edu_hbo       estimate: 0.24696; p-value: 0.10821
#Awareness_Q29:edu_hbo    estimate: -0.05345; p-value: 0.78731
#Interest_Q31:edu_hbo     estimate: -0.06199; p-value: 0.69341
#Knowledge_Q25:edu_hbo    estimate: 0.20671; p-value: 0.31834
#Multiple R-squared:  0.262,	Adjusted R-squared:  0.06996
#F-statistic: 1.364 on 32 and 123 DF,  p-value: 0.1169 (INSIGNIFICANT)
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

sort(abs(dv_WTR.iv_factors_EduCat_mc_synergy$coefficients),decreasing=TRUE)
# Trendy_Q20:edu_hs (insignificant) <-- High School
# Interest_Q31:edu_hs (insignificant)
# Fashionista_Q11:edu_mbo ( estimate: 0.52867; p-value: 0.09179 . ) <-- MBO
# Image_Q13:edu_hs (insignificant)
# Image_Q13:edu_mbo ( stimate: -0.43509; p-value: 0.05679 . ) <-- MBO
# edu_hs (insignificant)
# Awareness_Q29:edu_hs (insignificant)
# Interest_Q31:edu_mbo (insignificant)
# Casual_Q19:edu_mbo (insignificant)
# Female (insignificant)
# Casual_Q19:edu_hbo (insignificant) <-- HBO
# Trendy_Q20:edu_mbo (insignificant)
# Image_Q13 ( estimate: 0.22227; p-value: 0.00257 ** ) <-- University (WO)
# Knowledge_Q25:edu_hbo (insignificant)
# edu_mbo (insignificant)
# Awareness_Q29 (insignificant)
# Casual_Q19 (insignificant)
# Awareness_Q29:edu_mbo (insignificant)
# Knowledge_Q25:edu_hs (insignificant)
# Trendy_Q20 (insignificant)
# Fashionista_Q11:edu_hs (insignificant)
# Image_Q13:edu_hbo (insignificant)
# Interest_Q31 (insignificant)
# Fashionista_Q11 (insignificant)
# Interest_Q31:edu_hbo (insignificant)
# Knowledge_Q25 (insignificant)
# Casual_Q19:edu_hs (insignificant)
# Awareness_Q29:edu_hbo (insignificant)
# Fashionista_Q11:edu_hbo (insignificant)
# Trendy_Q20:edu_hbo (insignificant)
# (Intercept)
# edu_hbo (insignificant)
# Knowledge_Q25:edu_mbo (insignificant)

## Most important drivers for each educational level and its significance
## High school educated have a higher willingness to recommend (insignificant) when Trendy lifestyle is involved.
## MBO have a lower willingness to recommend (MARGINAL) when lifestyle Image  is involved.
## MBO have a higher willingness to recommend (MARGINAL) when Fashionista lifestyle is involved.
## HBO have a higher willingness to recommend (INSIGNIFICANT) when Casual lifestyle is involved.
## University have a higher willingness to recommend (SIGNIFICANT) when lifestyle Image is involved.
