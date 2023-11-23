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
librarian::shelf(nFactors,gplots,RColorBrewer,semPlot, lmtest, aTSA, ggplot2, ggcorrplot, stringr, plyr, formattable, fpc,
                 car,corrplot,dplyr,stats,bannerCommenter, vars, urca, clipr, cluster, mclust, kassambara/easyGgplot2)



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

seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}

##::::::::::
##  Data  ::
##::::::::::
# Data ----

## Import ----
herald.df <- read.csv("YOURPATH")

## Standardize ----
#We standardize the active variables/dimensions
herald.df_s <- herald.df
herald.df_s[,7:12] <- data.frame(scale(herald.df[,7:12]))

## Descriptives ----
### Means, Medians, etc. ----
summary(herald.df)
str(herald.df)
head(herald.df)
#median age 48


### Counting ----
ldply(herald.df, function(c) sum(c =="1"))
ldply(herald.df, function(c) sum(c =="2"))
ldply(herald.df, function(c) sum(c =="3"))
ldply(herald.df, function(c) sum(c =="4"))
#217 females, 203 males
#edu: 85,89,73,81,92

### Check Correlation ----
h.cor <- cor(herald.df_s[,7:12])
ggcorrplot(h.cor,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("red", "white", "green"))
#These correlations are not extreme, and would likely
# not lead to collinearity issues in the clustering.

#Active variables/Dimensions (6): mileage, power, design, comfort, entertainment, environment
#Passive variables/Identifying (4): gender, age, education, area



#################################################################
##                Hierarchical Cluster Analysis                ##
#################################################################
# Hierarchical Cluster Analysis ----
## Choosing the # of Clusters ----



##::::::::::::::::::::::::::::::::
##  Euclidean & Daisy Distance  ::
##::::::::::::::::::::::::::::::::

### Euclidean & Daisy Distance ----
euc.d <- dist(herald.df_s[,c(7:12)])     # dist function calculates Euclidean distance
as.matrix(euc.d)[1:5, 1:5]

herald.daisy <- daisy(herald.df_s[,c(7:12)]) #gives the same result as above since all of our variables are cont.; useful for mixed data types
as.matrix(herald.daisy)[1:5, 1:5]



##::::::::::::::::::::::::::::::::::::::::::::::::::
##  Linkage Methods /w Dendrograms & A.Schedules  ::
##::::::::::::::::::::::::::::::::::::::::::::::::::


### Linkage Methods /w Dendrograms & A.Schedules ----

#Useful Tip: positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number),
# negative values to singletons (true observations)

#### Single Linkage / Nearest Neighbor ----
herald.hc.single <- hclust(herald.daisy, method="single")
plot(herald.hc.single)
herald.agglo.single <- cbind(as.data.frame(herald.hc.single[1]),as.data.frame(herald.hc.single[2]))
herald.agglo.single
#goodness-of-fit metric for a hierarchical cluster solution - cophenetic correlation coefficient (CPCC)
#assesses how well a dendrogram matches the true distance metric (daisy)
#CPCC > 0.7 indicates a relatively strong fit (meaning hierarchical tree represents the distances between observations well)
cor(cophenetic(herald.hc.single),herald.daisy)
#CPCC = 0.3123886 => not a good fit

#### Complete Linkage ----
herald.hc.complete <- hclust(herald.daisy, method="complete")
plot(herald.hc.complete)
herald.agglo.complete <- cbind(as.data.frame(herald.hc.complete[1]),as.data.frame(herald.hc.complete[2]))
herald.agglo.complete
cor(cophenetic(herald.hc.complete),herald.daisy)
#CPCC = 0.478819 => better than Single Linkage

#### Average Linkage ----
herald.hc.avg <- hclust(herald.daisy, method="average")
plot(herald.hc.avg)
herald.agglo.avg <- cbind(as.data.frame(herald.hc.avg[1]),as.data.frame(herald.hc.avg[2]))
herald.agglo.avg
cor(cophenetic(herald.hc.avg),herald.daisy)
#CPCC = 0.5411245 => best so far

#### Centroid Linkage ----
herald.hc.centroid <- hclust(herald.daisy, method="centroid")
plot(herald.hc.centroid)
herald.agglo.centroid <- cbind(as.data.frame(herald.hc.centroid[1]),as.data.frame(herald.hc.centroid[2]))
herald.agglo.centroid
cor(cophenetic(herald.hc.centroid),herald.daisy)
#CPCC = 0.3368034 => not a good fit


#### Ward's Linkage ----
herald.hc.ward <- hclust(herald.daisy, method="ward.D2")
plot(herald.hc.ward)
herald.agglo.ward <- cbind(as.data.frame(herald.hc.ward[1]),as.data.frame(herald.hc.ward[2]))
herald.agglo.ward
cor(cophenetic(herald.hc.ward),herald.daisy)
#CPCC = 0.4991169


##:::::::::::::::::::::::::::::::::::
##  Linkage Method Verdict (CPCC)  ::
##:::::::::::::::::::::::::::::::::::

#### Which Linkage method to interpret? (CPCC verdict) ----
cor(cophenetic(herald.hc.avg),herald.daisy)
#CPCC = 0.5411245
cor(cophenetic(herald.hc.ward),herald.daisy)
#CPCC = 0.4991169
# In conclusion, Average Linkage (~0.54) and Ward's Linkage (~0.50), have the best fit.


##:::::::::::::::::::::::::::::::::::::::::
##  Dendrogram & A.Schedules Conclusion  ::
##:::::::::::::::::::::::::::::::::::::::::

#### Dendrogram Conclusion ----
plot(herald.hc.avg)
plot(herald.hc.ward)
# Avg. Linkage and Ward's Linkage are the best fit, so we judge by their dendrograms only,
# therefore for now we have our eyes on 5-7 clusters.

#### Agglomeration Schedule Conclusion ----
# We are only looking at the bottom of the A.S.
# Useful tip: Maarten suggested bottom 10 or so.
herald.agglo.avg[409:419,]
#merge.1 merge.2   height
#409     397     403 2.593962
#410     381     393 2.624121
#411     394     404 2.642673
#412     398     405 2.645860
#413     409     410 2.740376 <-- 7 clusters
#414     407     408 2.989426 <-- 6 clusters
#415     411     412 3.040448 <-- 5 clusters
#416     406     413 3.188129
#417     414     415 3.366040
#418     384     416 3.555047
#419     417     418 3.690821
# Observation: No big jumps, but if we look at 7 the cost is 2.7, 6 = 2.98, 5 = 3.04, so perhaps staying at 5-7 is reasonable, if we want to stay at 3 height and not more.
herald.agglo.ward[409:419,]
#    merge.1 merge.2    height
#409     379     400  9.193091
#410     397     403 10.598295
#411     395     402 10.636004
#412     398     410 12.362224
#413     405     408 13.102808 <-- 7 clusters
#414     407     409 13.447985 <-- 6 clusters
#415     404     411 14.936958 <-- 5 clusters
#416     406     412 17.516693
#417     413     414 19.869114
#418     415     416 23.326205
#419     417     418 33.054571
# Observation: It seems that after 5 clusters the cost jumps less incrementally than before, especially after 3;
# Conclusion: Probably 5-6 clusters. Next we observe the scree plots.


##:::::::::::::::::
##  Scree Plots  ::
##:::::::::::::::::

### Construct Scree Plots ----
# We will construct Scree Plots for Avg. and Ward Linkage
# Useful Tip: Maarten gave a great analogy with the elbow.
# For PCA we look from left to the elbow (eigen values)
# For CA we look from right to the elbow (clusters)

#### ScreePlot: Average Linkage ----
# Useful Tip: Maarten noted we should make sure to plot again only the 10 bottom or so heights.
herald.scree.avg <- sort(herald.agglo.avg[409:419,c(3)], decreasing = TRUE)
plot(herald.scree.avg, type="o", col="navyblue", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot of Avg. Linkage", xaxt="n")
axis(1, at=seq(1,11,by = 1))
# Observation: Looking from right to left, the first big jump is from 7 to 6 clusters,
# from there 6 to 5 seems harmless, but then after it's increasing a lot.

#### ScreePlot: Ward's Linkage ----
herald.scree.ward <- sort(herald.agglo.ward[409:419,c(3)], decreasing = TRUE)
plot(herald.scree.ward, type="o", col="blue", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot of Ward's Linkage", xaxt="n")
axis(1, at=seq(1,11,by = 1))
# Observation: Looking from right to left, there are no dramatic increases until going from 5 to 4 clusters.


##:::::::::::::::::::::::::::
##  Scree Plot Conclusion  ::
##:::::::::::::::::::::::::::

#### ScreePlots Conclusion ----
# Based on the two scree plots above, if we ignore the jump from 7 to 6 in Avg. Linkage's ScreePlot,
# then it seems the most reasonable amount of clusters is 5-6. This aligns with our dendrogram and A.Schedule conclusion.



##::::::::::::::::::::::::::::::::::::::
##  Dendrograms /w Cluster Proposals  ::
##::::::::::::::::::::::::::::::::::::::

### Dendrograms /w Cluster Proposals (Avg. & Ward) ----
# We will be looking at 5 and 6 clusters

#### Avg. Linkage 5 vs. 6 clusters ----
plot(herald.hc.avg)
rect.hclust(herald.hc.avg, k=5, border="red")
plot(herald.hc.avg)
rect.hclust(herald.hc.avg, k=6, border="red")
# 5 clusters looks more logical, as in 6 (from left to right) the second and third clusters look like they should be in one cluster.

#### Ward's Linkage 5 vs. 6 clusters ----
plot(herald.hc.ward)
rect.hclust(herald.hc.ward, k=5, border="red")
plot(herald.hc.ward)
rect.hclust(herald.hc.ward, k=6, border="red")
# 6 clusters might be logical afterall, if we look (from left to right) the small third cluster goes a big distance to combind with the fourth, so it might be a good one to leave in and do 6 clusters.



##:::::::::::::::::::::::::::::::::::::
##  Observations per Cluster Tables  ::
##:::::::::::::::::::::::::::::::::::::

### Observations per Cluster Tables : 5 vs. 6 Clusters (Avg. & Ward) ----

#### Average Linkage: 5 vs. 6 Clusters ----
herald.hc.avg.seg5 <- cutree(herald.hc.avg, k=5)
table(herald.hc.avg.seg5)
#  1   2   3   4   5
# 137 105 114  40  24
herald.hc.avg.seg6 <- cutree(herald.hc.avg, k=6)
table(herald.hc.avg.seg6)
#  1   2   3   4   5   6
# 137  59 114  40  46  24
# Observation: It might be more economic to go for 5 clusters, as with 6 we have three clusters of around 49

#### Ward Linkage: 5 vs. 6 Clusters ----
herald.hc.ward.seg5 <- cutree(herald.hc.ward, k=5)
table(herald.hc.ward.seg5)
#  1   2   3   4   5
# 104  90  99  42  85
herald.hc.ward.seg6 <- cutree(herald.hc.ward, k=6)
table(herald.hc.ward.seg6)
#  1   2   3   4   5   6
# 104  90  62  42  85  37
# Observation: It might be more economic to go for 5 clusters, as with 6 we have two clusters of around 40


##::::::::::::::::::::::::::::::::::::::::::::::::
##  Observations per Cluster Tables Conclusion  ::
##::::::::::::::::::::::::::::::::::::::::::::::::

#### Observations per Cluster Conclusion: ----
# Both 5 and 6 seem reasonable, but 5 clusters might be more economical.



##::::::::::::::::::::::::::::::::::::::::::
##  Robustness Check for Nr. of Clusters  ::
##::::::::::::::::::::::::::::::::::::::::::

### Robustness Check for Nr. of Clusters: K-means vs. HC-means (Avg. & Ward) ----
# To make the final decision on nr. of clusters we will check which cluster solution is more robust and stable

#note: we will check 2-7 clusters

#### HC Means (Avg. & Ward) ----

#Average Linkage - 5 CLUSTERS
herald.hc.avg.seg5 <- cutree(herald.hc.avg, k=5)
herald.hc.avg.mean5 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.avg.seg5)
herald.hc.avg.mean5
#  Group.1     mileage       power      design    comfort entertainment environment
# 1       1 -0.07319727  0.12969328 -0.53596735  0.5551568   -0.87371925  -0.7362582
# 2       2  0.52990369 -0.05785682 -0.07180322 -0.7642550    1.09630099   0.6726285
# 3       3 -0.97106752 -0.34050420  0.94385999 -0.0160259    0.24713351   0.4488910
# 4       4  1.06181458  1.51614550 -0.89754027 -0.5676459   -0.05536086  -0.9753113
# 5       5  0.94238553 -1.39672308  0.38618490  1.1967951   -0.89045215   0.7533439

#Average Linkage - 6 CLUSTERS
herald.hc.avg.seg6 <- cutree(herald.hc.avg, k=6)
herald.hc.avg.mean6 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.avg.seg6)
herald.hc.avg.mean6
#    Group.1     mileage        power     design    comfort entertainment environment
# 1       1 -0.07319727  0.129693278 -0.5359674  0.5551568   -0.87371925  -0.7362582
# 2       2  0.73103303 -0.004048218  0.6624776 -0.3397638    1.05781667   0.8240269
# 3       3 -0.97106752 -0.340504199  0.9438600 -0.0160259    0.24713351   0.4488910
# 4       4  1.06181458  1.516145501 -0.8975403 -0.5676459   -0.05536086  -0.9753113
# 5       5  0.27193345 -0.126872211 -1.0135982 -1.3087110    1.14566131   0.4784436
# 6       6  0.94238553 -1.396723082  0.3861849  1.1967951   -0.89045215   0.7533439


#Ward Linkage - 5 CLUSTERS
herald.hc.ward.seg5 <- cutree(herald.hc.ward, k=5)
herald.hc.ward.mean5 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.ward.seg5)
herald.hc.ward.mean5
#    Group.1     mileage      power     design    comfort entertainment environment
# 1       1  0.01289247  0.2268627 -1.0636879  0.2711422    -0.9282391 -0.91011808
# 2       2  0.61688283 -0.2388448  0.1326096 -0.5182415     0.8852540  0.80715418
# 3       3 -1.09386914 -0.1954185  0.9789584 -0.2778498     0.5352731  0.48836905
# 4       4  0.64632192  1.2179603 -0.9140356 -1.0465655     0.7937656 -0.55347715
# 5       5  0.28573238 -0.3988892  0.4724857  1.0577156    -0.8172492 -0.03640107

#Ward Linkage - 6 CLUSTERS
herald.hc.ward.seg6 <- cutree(herald.hc.ward, k=6)
herald.hc.ward.mean6 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.ward.seg6)
herald.hc.ward.mean6
#    Group.1     mileage      power     design     comfort entertainment environment
# 1       1  0.01289247  0.2268627 -1.0636879  0.27114222    -0.9282391 -0.91011808
# 2       2  0.61688283 -0.2388448  0.1326096 -0.51824151     0.8852540  0.80715418
# 3       3 -1.00088602  0.3996380  0.6885472 -0.46177940     0.6202127  0.05901773
# 4       4  0.64632192  1.2179603 -0.9140356 -1.04656554     0.7937656 -0.55347715
# 5       5  0.28573238 -0.3988892  0.4724857  1.05771562    -0.8172492 -0.03640107
# 6       6 -1.24967868 -1.1925402  1.4655934  0.03035656     0.3929420  1.20782261


#### Random Seed K-Means (rsk) ----

#Random Seed K-means - 5 CLUSTERS
set.seed(32902321)
herald.nhc.rsk5 <- kmeans(herald.df_s[,c(7:12)], centers=5, nstart = 30)
seg.summ(herald.df_s[,c(7:12)], herald.nhc.rsk5$cluster)
#   Group.1     mileage      power      design     comfort entertainment environment
# 1       1  0.05168568 -0.5566865 -0.06501103  0.96741776    -0.9489319  -0.1378888
# 2       2 -0.46694580 -1.0452944  0.94281369 -0.15192636     0.5552822   1.2094188
# 3       3 -0.57682224  0.7061500  0.97201186 -0.03831357     0.3887931  -0.0874417
# 4       4  0.39313963  0.9044006 -0.98460388 -0.06718624    -0.5852940  -1.1422455
# 5       5  0.47055534  0.3146240 -0.61612304 -1.18281580     1.1806805   0.3025010

#Random Seed K-Means - 6 CLUSTERS
set.seed(32902321)
herald.nhc.rsk6 <- kmeans(herald.df_s[,c(7:12)], centers=6, nstart = 30)
seg.summ(herald.df_s[,c(7:12)], herald.nhc.rsk6$cluster)
#   Group.1    mileage      power     design    comfort entertainment environment
# 1       1 -0.2896331 -0.1303839 -1.0024420  0.4322040    -0.9672700  -0.6965442
# 2       2 -0.5945293  0.7495608  1.0141847  0.1838324     0.1779064  -0.2809602
# 3       3  0.2232230  0.1978532 -0.5468484 -1.1304096     1.1951216   0.3851683
# 4       4 -0.5634789 -1.0234859  0.9895437 -0.2142994     0.5683526   1.2268045
# 5       5  0.4415864 -0.6423591  0.4676913  1.1110937    -0.7407439   0.1691180
# 6       6  1.0043354  1.3377199 -0.8734588 -0.3924850    -0.2771587  -1.1275699



#### Curiousity: Additional testing ----
set.seed(32902321)
herald.nhc.rsk7 <- kmeans(herald.df_s[,c(7:12)], centers=7, nstart = 30)
seg.summ(herald.df_s[,c(7:12)], herald.nhc.rsk7$cluster)
#   Group.1     mileage      power     design    comfort entertainment environment
# 1       1  0.95835198  1.3263913 -0.9475555 -0.4078837    -0.3128846  -1.1715965
# 2       2 -0.01240821  0.1161182 -0.7005666 -1.2830464     1.1439354   0.3742260
# 3       3  0.42789014 -1.0928220  0.4341298  1.0245026    -0.7812899   0.5238589
# 4       4 -0.12031282 -0.3284310 -1.1222910  0.4293949    -0.9442186  -0.5136071
# 5       5 -0.58070916  0.5963460  0.4930785  0.7669657    -0.6166517  -0.8102417
# 6       6  0.63963122  0.1201642  0.7613729 -0.1996339     0.9029278   0.7213140
# 7       7 -1.23728708 -0.8214732  1.2870447 -0.1423227     0.4482219   0.9020302


herald.hc.avg.seg7 <- cutree(herald.hc.avg, k=7)
herald.hc.avg.mean7 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.avg.seg7)
herald.hc.avg.mean7
#   Group.1     mileage        power     design    comfort entertainment environment
# 1       1 -0.07319727  0.129693278 -0.5359674  0.5551568   -0.87371925 -0.73625816
# 2       2  0.73103303 -0.004048218  0.6624776 -0.3397638    1.05781667  0.82402690
# 3       3 -0.97294988  0.499402846  0.7863070 -0.3616875    0.44926650 -0.08560726
# 4       4  1.06181458  1.516145501 -0.8975403 -0.5676459   -0.05536086 -0.97531130
# 5       5  0.27193345 -0.126872211 -1.0135982 -1.3087110    1.14566131  0.47844357
# 6       6  0.94238553 -1.396723082  0.3861849  1.1967951   -0.89045215  0.75334390
# 7       7 -0.96931278 -1.123468393  1.0907314  0.3062010    0.05870445  0.94715211


set.seed(32902321)
herald.nhc.rsk7 <- kmeans(herald.df_s[,c(7:12)], centers=7, nstart = 30)
seg.summ(herald.df_s[,c(7:12)], herald.nhc.rsk7$cluster)
#   Group.1     mileage      power     design    comfort entertainment environment
# 1       1  0.95835198  1.3263913 -0.9475555 -0.4078837    -0.3128846  -1.1715965
# 2       2 -0.01240821  0.1161182 -0.7005666 -1.2830464     1.1439354   0.3742260
# 3       3  0.42789014 -1.0928220  0.4341298  1.0245026    -0.7812899   0.5238589
# 4       4 -0.12031282 -0.3284310 -1.1222910  0.4293949    -0.9442186  -0.5136071
# 5       5 -0.58070916  0.5963460  0.4930785  0.7669657    -0.6166517  -0.8102417
# 6       6  0.63963122  0.1201642  0.7613729 -0.1996339     0.9029278   0.7213140
# 7       7 -1.23728708 -0.8214732  1.2870447 -0.1423227     0.4482219   0.9020302


herald.hc.ward.seg7 <- cutree(herald.hc.ward, k=7)
herald.hc.ward.mean7 <- seg.summ(herald.df_s[,c(7:12)], herald.hc.ward.seg7)
herald.hc.ward.mean7
#   Group.1    mileage      power     design     comfort entertainment environment
# 1       1 -0.1289633 -0.3452866 -1.0911179  0.35868564    -0.9620899 -0.47840701
# 2       2  0.6168828 -0.2388448  0.1326096 -0.51824151     0.8852540  0.80715418
# 3       3 -1.0008860  0.3996380  0.6885472 -0.46177940     0.6202127  0.05901773
# 4       4  0.6463219  1.2179603 -0.9140356 -1.04656554     0.7937656 -0.55347715
# 5       5  0.2857324 -0.3988892  0.4724857  1.05771562    -0.8172492 -0.03640107
# 6       6  0.2398617  1.1423014 -1.0197998  0.13107275    -0.8740778 -1.60085580
# 7       7 -1.2496787 -1.1925402  1.4655934  0.03035656     0.3929420  1.20782261



##:::::::::::::::::::::::::::::::::
##  Robustness Check Conclusion  ::
##:::::::::::::::::::::::::::::::::

#### Robustness Check Conclusion ----
# For Ward: 6 clusters
# 6 clusters seems to be more robust than 5. This actually aligns with our findings on line 257,
# furthermore, it makes sense in terms of the screeplot as well. Therefore we will go further with 6 clusters for Ward.

# For Avg.: 5 clusters
# We did not see a clear robustness, therefore we will keep Avg. with 5 clusters,
# because that aligns most with previous findings from dendrogram, scree plot, etc.

# Sidenote:
# We also tested 7 clusters and it does not seem that has higher robustness, plus it goes against our previous findings



### Naming Clusters (Initial), ANOVA & Tukey (Avg5 & Ward6) ----
# 5 Clusters Avg.
# 6 Clusters Ward

#### 5 Clusters: Average Linkage ----

##### NAMING SEGMENTS ----
herald.hc.avg.mean5
#  Group.1     mileage       power      design    comfort entertainment environment
# 1       1 -0.07319727  0.12969328 -0.53596735  0.5551568   -0.87371925  -0.7362582
# 2       2  0.52990369 -0.05785682 -0.07180322 -0.7642550    1.09630099   0.6726285
# 3       3 -0.97106752 -0.34050420  0.94385999 -0.0160259    0.24713351   0.4488910
# 4       4  1.06181458  1.51614550 -0.89754027 -0.5676459   -0.05536086  -0.9753113
# 5       5  0.94238553 -1.39672308  0.38618490  1.1967951   -0.89045215   0.7533439

# Comfort Cruisers (1), Entertained Eco-Travelers (2), Fabulous Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
#Note: Pilgrim travels longer than Traveler, Cruiser travels the least
# Flashy cares about design, Fabulous cares the most about design
# Eco indicates interest in environment-friendly

##### ANOVAs & Tukey ----
# Test each active variable for significance (ANOVA)
clusmembers.avg5 <- as.factor(herald.hc.avg.seg5)
herald.aovbase.avg5 <- cbind(clusmembers.avg5,herald.df_s[,2:12])
#ANOVA + Tukey (if sig.)

# MILEAGE
h.aov.avg5.mileage <- aov(mileage ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.mileage) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.mileage) # note: we include output only of insignificant differences
# 5-2 p=0.08, the Comfort Eco-Pilgrims (5) marginally significant on Mileage from Entertained Eco-Travelers (2)
# 5-4 p=0.97, the Comfort Eco-Pilgrims (5) not sig. diff. on Mileage from Power Pilgrims (4)

# POWER
h.aov.avg5.power <- aov(power ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.power) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.power) # note: we include output only of insignificant differences
# 2-1 p=0.37, the Entertained Eco-Travelers (2) not sig. diff. on Power from Comfort Cruisers (1)
# 3-2 p=0.069, the Fabulous Eco-Cruisers (3) marginally significant difference on Power from Entertained Eco-Travelers (2)

# DESIGN
h.aov.avg5.design <- aov(design ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.design) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.design) # note: we include output only of insignificant differences
# 4-1 p=0.065, the Power Pilgrims (4) marginally significant difference on Design from Comfort Cruisers (1)
# 5-2 p=0.063, the Comfort Eco-Pilgrims (5) marginally significant difference on Design from Entertained Eco-Travelers (2)

# COMFORT
h.aov.avg5.comfort <- aov(comfort ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.comfort) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.comfort) # note: we include output only of insignificant differences
# 4-2 p=0.68, the Power Pilgrims (4) not sig. difference on Comfort from Entertained Eco-Travelers (2)

# ENTERTAINMENT
h.aov.avg5.entertainment <- aov(entertainment ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.entertainment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.entertainment) # note: we include output only of insignificant differences
# 4-3 p=0.68, the Power Pilgrims (4) marginally significant difference on Ent. from Fabulous Eco-Cruisers (3)
# 5-1 p=0.99, the Comfort Eco-Pilgrims (5) not sig. diff. on Ent. from Comfort Cruisers (1)

# ENVIRONMENT
h.aov.avg5.environment <- aov(environment ~ clusmembers.avg5, data = herald.aovbase.avg5)
summary(h.aov.avg5.environment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.avg5.environment) # note: we include output only of insignificant differences
# 4-1 p=0.37, the Power Pilgrims (4) not sig. diff.on Enviro. from Comfort Cruisers (1)
# 3-2 p=0.16, the Fabulous Eco-Cruisers (3) not sig. diff. on Enviro. from Entertained Eco-Travelers (2)
# 5-2 p=0.99, the Comfort Eco-Pilgrims (5) not sig. diff. on Enviro. from Entertained Eco-Travelers (2)

###### Conclusion -----
# The results show difference between clusters. The ones who don't significantly differ, don't differ on characteristics that already visibly don't differ in the table
# and are logical in terms of their naming/definitions. The good thing is all ANOVAs are significant and we keep all of our active variables.

#### 6 Clusters: Ward Linkage ----

##### NAMING SEGMENTS ----
herald.hc.ward.mean6
#   Group.1     mileage      power     design     comfort entertainment environment
# 1       1  0.01289247  0.2268627 -1.0636879  0.27114222    -0.9282391 -0.91011808
# 2       2  0.61688283 -0.2388448  0.1326096 -0.51824151     0.8852540  0.80715418
# 3       3 -1.00088602  0.3996380  0.6885472 -0.46177940     0.6202127  0.05901773
# 4       4  0.64632192  1.2179603 -0.9140356 -1.04656554     0.7937656 -0.55347715
# 5       5  0.28573238 -0.3988892  0.4724857  1.05771562    -0.8172492 -0.03640107
# 6       6 -1.24967868 -1.1925402  1.4655934  0.03035656     0.3929420  1.20782261

# Daily Rider (1), Entertained Eco-Travelers (2), Entertained Flashy-Cruisers (3), Entertained Power-Travelers (4), Comfort Flashy-Riders (5), Fabulous Eco-Cruisers (6)
#Note: Pilgrim travels longer than Traveler, Rider travels less than the latter, Cruiser travels the least
# Flashy cares about design, Fabulous cares the most about design
# Eco indicates interest in environment-friendly

##### ANOVAs & Tukey ----
# Test each active variable for significance (ANOVA)
clusmembers.ward6 <- as.factor(herald.hc.ward.seg6)
herald.aovbase.ward6 <- cbind(clusmembers.ward6,herald.df_s[,2:12])
#ANOVA + Tukey (if sig.)

# MILEAGE
h.aov.ward6.mileage <- aov(mileage ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.mileage) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.mileage) # note: we include output only of insignificant differences
# 5-1 p=0.14, Comfort Flashy-Riders (5) do not differ on Mileage from Daily Riders (1)
# 4-2 p=0.99, Entertained Power-Travelers (4) do not differ on Mileaege from Entertained Eco-Travelers (2)
# 6-3 p=0.62, Fabulous Eco-Cruisers (6) do no differ on Mileage from Entertained Flashy-Cruisers (3)
# 5-4 p=0.12, Comfort Flashy-Riders (5) do not differ on Mileage from Entertained Power-Travelers (4)

# POWER
h.aov.ward6.power <- aov(power ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.power) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.power) # note: we include output only of insignificant differences
# 3-1 p=0.77, Entertained Flashy-Cruisers (3) do not differ on Power from Daily Riders (1)
# 5-2 p=0.78, Comfort Flashy-Riders (5) do not differ on Power from Entertained Eco-Travelers (2)

# DESIGN
h.aov.ward6.design <- aov(design ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.design) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.design) # note: we include output only of insignificant differences
# 4-1 p=0.71,  Entertained Power-Travelers (4) do not differ on Design from Daily Riders (1)
# 5-3 p=0.22, Comfort Flashy-Riders (5) do not differ on Design from Entertained Flashy-Cruisers (3)

# COMFORT
h.aov.ward6.comfort <- aov(comfort ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.comfort) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.comfort) # note: we include output only of insignificant differences
# 6-1 p=0.55,  Fabulous Eco-Cruisers (6) do not differ on Comfort from Daily Riders (1)
# 3-2 p=0.99, Entertained Flashy-Cruisers (3) do not differ on Comfort from Entertained Eco-Travelers (2)

# ENTERTAINMENT
h.aov.ward6.entertainment <- aov(entertainment ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.entertainment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.entertainment) # note: we include output only of insignificant differences
# 5-1 p=0.80, Comfort Flashy-Riders (5) do not differ on Entertainment from Daily Riders (1)
# 3-2 p=0.08, Entertained Flashy-Cruisers (3) do not differ on Entertainment from Entertained Eco-Travelers (2)
# 4-2 p=0.96, Entertained Power-Travelers (4) do not differ on Entertainment from Entertained Eco-Travelers (2)
# 4-3 p=0.67, Entertained Power-Travelers (4) do not differ on Entertainment from Entertained Flashy-Cruisers (3)
# 6-3 p=0.44, Fabulous Eco-Cruisers (6) do no differ on Entertainment from Entertained Flashy-Cruisers (3)

# ENVIRONMENT
h.aov.ward6.environment <- aov(environment ~ clusmembers.ward6, data = herald.aovbase.ward6)
summary(h.aov.ward6.environment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.ward6.environment) # note: we include output only of insignificant differences
# 4-1 p=0.07,  Entertained Power-Travelers (4) do not differ on Environment from Daily Riders (1)
# 5-3 p=0.97, Comfort Flashy-Riders (5) do not differ on Environment from Entertained Flashy-Cruisers (3)


###### Conclusion -----
# The clusters significantly differ from the perspective of ANOVA, but with Tukey we see there is a lot of overlap for especially Entertainment but also Mileage
# This makes the Ward clusters a bit meaningless.


### Final HClustering Conclusion ----
# Doing Average with 5 Clusters seems to produce more distinct segments than Ward with 6. While some segments between Ward and Avg are quite similar.

# We will rename some segments:
#AVG5 -> Daily Riders (1), Entertained Eco-Travelers (2), Fabulous Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
#WARD6-> Daily Riders (1), Entertained Eco-Travelers (2), Flashy Entertained-Cruisers (3), Power Entertained-Travelers (4), Comfort Flashy-Riders (5), Fabulous Eco-Cruisers (6)

#Naming logic: 2 adjectives + 1 noun ->
# 1st adj. score is highest for that segment (mileage is not considered)
# 2nd adj. score is second highest (mileage is not considered)
# the noun represents how much this segment travels (here is where mileage is represented)
# e.g. if mileage is higher than design, but design is the highest other than mileage we will not make adj.1 to refer to mileage, mileage is only showcased in the noun

#HOWEVER, this shows that our two linkage methods have quite similar segments, furthermore our difference analysis shows that Ward has segments overlapping a lot on things such as entertainment

#Which ones are contained in both:
#Daily Riders (1), Entertained Eco-Travelers (2), Fabulous Eco-Cruisers (3)

#meanwhile, these: Flashy Entertained-Cruisers (3), Power Entertained-Travelers (4), Comfort Flashy-Riders (5) for WARD6 have better defined counterparts in AVG5

#this is why we drop WARD6 for our profiling and final cluster solution.




##::::::::::::::::::::::::::::::::::::
##  Final HCluster Solution (AVG5)  ::
##::::::::::::::::::::::::::::::::::::


### Final HCluster Solution (AVG5) ----
# Cluster Names, Preferences, Differences, Sizes, Socio-demographics
# Note: some sections are repeated from above

#### Final Cluster Names, Preferences & Differences ----
# Daily Riders (1), Entertained Eco-Travelers (2), Fabulous Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
# New naming: Daily Riders (1), Entertained Eco-Pilgrims (2), Designer Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
# Note on Mileage: Who has highest preference for mileage most? Pilgrims > Riders > Cruisers

# PREFERENCES:
#  Group.1     mileage       power      design    comfort entertainment environment
# 1       1 -0.07319727  0.12969328 -0.53596735  0.5551568   -0.87371925  -0.7362582
# 2       2  0.52990369 -0.05785682 -0.07180322 -0.7642550    1.09630099   0.6726285
# 3       3 -0.97106752 -0.34050420  0.94385999 -0.0160259    0.24713351   0.4488910
# 4       4  1.06181458  1.51614550 -0.89754027 -0.5676459   -0.05536086  -0.9753113
# 5       5  0.94238553 -1.39672308  0.38618490  1.1967951   -0.89045215   0.7533439


# DIFFERENCES:
# We changed the names of Pilgrims and Travelers based on the fact that differences are not significant. To not be misleading

# MILEAGE
# 5-2 p=0.08, the Comfort Eco-Pilgrims (5) marginally significant on Mileage from Entertained Eco-PILGRIMS (2)
# 5-4 p=0.97, the Comfort Eco-Pilgrims (5) not sig. diff. on Mileage from Power Pilgrims (4)

# POWER
# 2-1 p=0.37, the Entertained Eco-PILGRIMS (2) not sig. diff. on Power from Comfort Cruisers (1)
# 3-2 p=0.069, the Designer Eco-Cruisers (3) marginally significant difference on Power from Entertained Eco-Travelers (2)

# DESIGN
# 4-1 p=0.065, the Power Pilgrims (4) not sig. diff. on Design from Comfort Cruisers (1)
# 5-2 p=0.063, the Comfort Eco-Pilgrims (5) not sig. diff. on Design from Entertained Eco-PILGRIMS (2)

# COMFORT
# 4-2 p=0.68, the Power Pilgrims (4) not sig. diff. on Comfort from Entertained Eco-PILGRIMS (2)

# ENTERTAINMENT
# 4-3 p=0.68, the Power Pilgrims (4) not sig. diff. on Ent. from Designer Eco-Cruisers (3)
# 5-1 p=0.99, the Comfort Eco-Pilgrims (5) not sig. diff. on Ent. from Comfort Cruisers (1)

# ENVIRONMENT
# 4-1 p=0.37, the Power Pilgrims (4) not sig. diff.on Enviro. from Comfort Cruisers (1)
# 3-2 p=0.16, the Designer Eco-Cruisers (3) not sig. diff. on Enviro. from Entertained Eco-PILGRIMS (2)
# 5-2 p=0.99, the Comfort Eco-Pilgrims (5) not sig. diff. on Enviro. from Entertained Eco-PILGRIMS (2)


#### Cluster Sizes & Socio-demographics ----
herald.hcsolution.df <- cbind(clusmembers.avg5,herald.df_s[,2:6])
names(herald.hcsolution.df)[1] <- "cluster"

#RECODING
#age into age groups
#where to start?
min(herald.hcsolution.df$age) #lowest age in sample is 18 - logical as well since that is the legal driving age
max(herald.hcsolution.df$age) #highest age in sample is 75
#age_group
herald.hcsolution.df[herald.hcsolution.df$age > 17 & herald.hcsolution.df$age <= 28, "age_group"] <- "18-28"
herald.hcsolution.df[herald.hcsolution.df$age > 28 & herald.hcsolution.df$age <= 38, "age_group"] <- "29-38"
herald.hcsolution.df[herald.hcsolution.df$age > 38 & herald.hcsolution.df$age <= 48, "age_group"] <- "39-48"
herald.hcsolution.df[herald.hcsolution.df$age > 48 & herald.hcsolution.df$age <= 58, "age_group"] <- "49-58"
herald.hcsolution.df[herald.hcsolution.df$age > 58 & herald.hcsolution.df$age <= 68, "age_group"] <- "59-68"
herald.hcsolution.df[herald.hcsolution.df$age > 68, "age_group"] <- "68 and older"

#gender to string
herald.hcsolution.df[herald.hcsolution.df$gender == 1, "gender_string"] <- "female"
herald.hcsolution.df[herald.hcsolution.df$gender == 2, "gender_string"] <- "male"
herald.hcsolution.df[herald.hcsolution.df$gender == 3, "gender_string"] <- "other"

#cluster nr to cluster name string
# Daily Riders (1), Entertained Eco-Pilgrims (2), Designer Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
herald.hcsolution.df[herald.hcsolution.df$cluster == 1, "cluster_name"] <- "(1) Daily Riders"
herald.hcsolution.df[herald.hcsolution.df$cluster == 2, "cluster_name"] <- "(2) Entertained Eco-Pilgrims"
herald.hcsolution.df[herald.hcsolution.df$cluster == 3, "cluster_name"] <- "(3) Designer Eco-Cruisers"
herald.hcsolution.df[herald.hcsolution.df$cluster == 4, "cluster_name"] <- "(4) Power Pilgrims"
herald.hcsolution.df[herald.hcsolution.df$cluster == 5, "cluster_name"] <- "(5) Comfort Eco-Pilgrims"

#area nr to area name string
# 1 = metropolitan
# 2 = urban
# 3 = suburban
# 4 = countryside
herald.hcsolution.df[herald.hcsolution.df$area == 1, "area_name"] <- "(1) Metropolitan"
herald.hcsolution.df[herald.hcsolution.df$area == 2, "area_name"] <- "(2) Urban"
herald.hcsolution.df[herald.hcsolution.df$area == 3, "area_name"] <- "(3) Suburban"
herald.hcsolution.df[herald.hcsolution.df$area == 4, "area_name"] <- "(4) Countryside"

#education to education name
# 1 = high school profession-oriented degree = HS Profession
# 2 = high school theory-oriented degree = HS Theory
# 3 = higher education non-university degree = HE Non-Uni
# 4 = university degree = UNI
# 5 = other = Other
herald.hcsolution.df[herald.hcsolution.df$education == 1, "education_name"] <- "(1) HS Profession"
herald.hcsolution.df[herald.hcsolution.df$education == 2, "education_name"] <- "(2) HS Theory"
herald.hcsolution.df[herald.hcsolution.df$education == 3, "education_name"] <- "(3) HE Non-Uni"
herald.hcsolution.df[herald.hcsolution.df$education == 4, "education_name"] <- "(4) University"
herald.hcsolution.df[herald.hcsolution.df$education == 5, "education_name"] <- "(5) Other"


#RELATIVE (%) NR. OF RESPONDENTS per cluster
hcsolution.size <- herald.hcsolution.df %>%
  group_by(cluster_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(hcsolution.size))
#                  cluster_name n.respondents respondents.percent
# 1             (1) Daily Riders           137              32.62%
# 2 (2) Entertained Eco-Pilgrims           105              25.00%
# 3    (3) Designer Eco-Cruisers           114              27.14%
# 4           (4) Power Pilgrims            40               9.52%
# 5     (5) Comfort Eco-Pilgrims            24               5.71%

#GENDER distribution per cluster
hcsolution.gender <- herald.hcsolution.df %>%
  group_by(cluster_name,gender_string) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(hcsolution.gender))
#                  cluster_name gender_string n.respondents respondents.percent
# 1             (1) Daily Riders        female            45              32.85%
# 2             (1) Daily Riders          male            92              67.15%
# 3 (2) Entertained Eco-Pilgrims        female            58              55.24%
# 4 (2) Entertained Eco-Pilgrims          male            47              44.76%
# 5    (3) Designer Eco-Cruisers        female            90              78.95%
# 6    (3) Designer Eco-Cruisers          male            24              21.05%
# 7           (4) Power Pilgrims          male            40             100.00%
# 8     (5) Comfort Eco-Pilgrims        female            24             100.00%

#AGE distribution per cluster
hcsolution.agegroup <- herald.hcsolution.df %>%
  group_by(cluster_name,age_group) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(hcsolution.agegroup))
#                  cluster_name    age_group n.respondents respondents.percent
# 1              (1) Daily Riders        39-48            10               7.30%
# 2              (1) Daily Riders        49-58            29              21.17%
# 3              (1) Daily Riders        59-68            63              45.99%
# 4              (1) Daily Riders 68 and older            35              25.55%
# 5  (2) Entertained Eco-Pilgrims        18-28            59              56.19%
# 6  (2) Entertained Eco-Pilgrims        29-38            27              25.71%
# 7  (2) Entertained Eco-Pilgrims        39-48            12              11.43%
# 8  (2) Entertained Eco-Pilgrims        49-58             6               5.71%
# 9  (2) Entertained Eco-Pilgrims        59-68             1               0.95%
# 10    (3) Designer Eco-Cruisers        18-28            13              11.40%
# 11    (3) Designer Eco-Cruisers        29-38            25              21.93%
# 12    (3) Designer Eco-Cruisers        39-48            41              35.96%
# 13    (3) Designer Eco-Cruisers        49-58            20              17.54%
# 14    (3) Designer Eco-Cruisers        59-68            12              10.53%
# 15    (3) Designer Eco-Cruisers 68 and older             3               2.63%
# 16           (4) Power Pilgrims        18-28             2               5.00%
# 17           (4) Power Pilgrims        29-38            11              27.50%
# 18           (4) Power Pilgrims        39-48            11              27.50%
# 19           (4) Power Pilgrims        49-58             6              15.00%
# 20           (4) Power Pilgrims        59-68             5              12.50%
# 21           (4) Power Pilgrims 68 and older             5              12.50%
# 22     (5) Comfort Eco-Pilgrims        39-48             2               8.33%
# 23     (5) Comfort Eco-Pilgrims        49-58             8              33.33%
# 24     (5) Comfort Eco-Pilgrims        59-68             6              25.00%
# 25     (5) Comfort Eco-Pilgrims 68 and older             8              33.33%


#AREA distribution per cluster
hcsolution.area <- herald.hcsolution.df %>%
  group_by(cluster_name,area_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(hcsolution.area))
#                   cluster_name        area_name n.respondents respondents.percent
# 1              (1) Daily Riders (1) Metropolitan            29              21.17%
# 2              (1) Daily Riders        (2) Urban            42              30.66%
# 3              (1) Daily Riders     (3) Suburban            39              28.47%
# 4              (1) Daily Riders  (4) Countryside            27              19.71%
# 5  (2) Entertained Eco-Pilgrims (1) Metropolitan             5               4.76%
# 6  (2) Entertained Eco-Pilgrims        (2) Urban            17              16.19%
# 7  (2) Entertained Eco-Pilgrims     (3) Suburban            36              34.29%
# 8  (2) Entertained Eco-Pilgrims  (4) Countryside            47              44.76%
# 9     (3) Designer Eco-Cruisers (1) Metropolitan            74              64.91%
# 10    (3) Designer Eco-Cruisers        (2) Urban            33              28.95%
# 11    (3) Designer Eco-Cruisers     (3) Suburban             7               6.14%
# 12           (4) Power Pilgrims (1) Metropolitan             2               5.00%
# 13           (4) Power Pilgrims        (2) Urban            10              25.00%
# 14           (4) Power Pilgrims     (3) Suburban             9              22.50%
# 15           (4) Power Pilgrims  (4) Countryside            19              47.50%
# 16     (5) Comfort Eco-Pilgrims     (3) Suburban             7              29.17%
# 17     (5) Comfort Eco-Pilgrims  (4) Countryside            17              70.83%

#EDUCATION distribution per cluster
hcsolution.edu <- herald.hcsolution.df %>%
  group_by(cluster_name,education_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(hcsolution.edu))
#                 cluster_name    education_name n.respondents respondents.percent
# 1              (1) Daily Riders (1) HS Profession            23              16.79%
# 2              (1) Daily Riders     (2) HS Theory            45              32.85%
# 3              (1) Daily Riders    (3) HE Non-Uni            24              17.52%
# 4              (1) Daily Riders    (4) University            20              14.60%
# 5              (1) Daily Riders         (5) Other            25              18.25%
# 6  (2) Entertained Eco-Pilgrims (1) HS Profession            17              16.19%
# 7  (2) Entertained Eco-Pilgrims     (2) HS Theory            19              18.10%
# 8  (2) Entertained Eco-Pilgrims    (3) HE Non-Uni            18              17.14%
# 9  (2) Entertained Eco-Pilgrims    (4) University            28              26.67%
# 10 (2) Entertained Eco-Pilgrims         (5) Other            23              21.90%
# 11    (3) Designer Eco-Cruisers (1) HS Profession            16              14.04%
# 12    (3) Designer Eco-Cruisers     (2) HS Theory            16              14.04%
# 13    (3) Designer Eco-Cruisers    (3) HE Non-Uni            29              25.44%
# 14    (3) Designer Eco-Cruisers    (4) University            24              21.05%
# 15    (3) Designer Eco-Cruisers         (5) Other            29              25.44%
# 16           (4) Power Pilgrims (1) HS Profession            29              72.50%
# 17           (4) Power Pilgrims     (2) HS Theory             9              22.50%
# 18           (4) Power Pilgrims    (3) HE Non-Uni             2               5.00%
# 19     (5) Comfort Eco-Pilgrims    (4) University             9              37.50%
# 20     (5) Comfort Eco-Pilgrims         (5) Other            15              62.50%






#######################################################################
##                  Non-Hierarchical Cluster Analysis                ##
#######################################################################
# Non-Hierarchical Cluster Analysis ----



## Combined K-Means ----

# Do K-means with the outcomes of the hierarchical cluster as starting values
# We choose 5 Clusters based on the cumulative conclusions from Dendrograms,
# A.Schedules, ScreePlots and HC-Means (done for both Avg. Linkage & Ward Linkage)

#Here we use HC-mean from Avg at 5 Clusters
kstart <- herald.hc.avg.mean5[,c(2:7)]
herald.nhc.k5 <- kmeans(herald.df_s[,c(7:12)],kstart)

### Comparing Combined K-means vs. RS K-means (local optima?) & HC-Avg. Means (confirmation?) ----
seg.summ(herald.df_s[,c(7:12)], herald.nhc.k5$cluster)
#  Group.1    mileage      power     design    comfort entertainment environment
# 1       1 -0.2591953  0.1624302 -0.6211107  0.6248499  -0.952242113  -0.8508116
# 2       2  0.3739799 -0.1293350 -0.3177337 -0.9141179   1.215883283   0.7472291
# 3       3 -1.0109362 -0.3300484  1.1746703 -0.1948118   0.426614251   0.5749511
# 4       4  0.9617836  1.2618013 -0.6235208 -0.4807406  -0.005749057  -0.8629723
# 5       5  0.3452403 -0.8957069  0.4105152  0.9603073  -0.670498345   0.4381120



# Random Seed K-means (5) <- The reuslt of RS K-means (5) have some similarities! => Perhaps a weak local optima
#   Group.1     mileage      power      design     comfort entertainment environment
# 1       1  0.05168568 -0.5566865 -0.06501103  0.96741776    -0.9489319 -0.1378888
# 2       2 -0.46694580 -1.0452944  0.94281369 -0.15192636     0.5552822   1.2094188
# 3       3 -0.57682224  0.7061500  0.97201186 -0.03831357     0.3887931  -0.0874417
# 4       4  0.39313963  0.9044006 -0.98460388 -0.06718624    -0.5852940  -1.1422455
# 5       5  0.47055534  0.3146240 -0.61612304 -1.18281580     1.1806805   0.3025010

# HC-Means (AVG5) <- The results of the HC-Means (AVG5) are quite similar to the Combined K-Means! => confirmation
#  Group.1     mileage       power      design    comfort entertainment environment
# 1       1 -0.07319727  0.12969328 -0.53596735  0.5551568   -0.87371925  -0.7362582
# 2       2  0.52990369 -0.05785682 -0.07180322 -0.7642550    1.09630099   0.6726285
# 3       3 -0.97106752 -0.34050420  0.94385999 -0.0160259    0.24713351   0.4488910
# 4       4  1.06181458  1.51614550 -0.89754027 -0.5676459   -0.05536086  -0.9753113
# 5       5  0.94238553 -1.39672308  0.38618490  1.1967951   -0.89045215   0.7533439

#=> Naming stays the same, further confrims lack of mileage diffrence in Cluster 5

#### ClusPlot Combined K-Means & HCAVG5-Means ----
clusplot(herald.df_s[,c(7:12)], herald.nhc.k5$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="Combined K-means Cluster Plot", lwd = 5, col.p = "black", col.clus = brewer.pal(5,"YlOrRd"))

clusplot(herald.df_s[,c(7:12)], herald.hc.avg.seg5, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="HC-Means (AVG) Cluster Plot", lwd = 5, col.p = "black", col.clus = brewer.pal(5,"YlOrRd"))

# They look similar, but Combined K-Means seems to do it better, let's finish the full analysis with CKM and see hwo it is.






### ANOVAs & Tukey ----
# Test each active variable for significance (ANOVA)
clusmembers.k5 <- as.factor(herald.nhc.k5$cluster)
herald.aovbase.k5 <- cbind(clusmembers.k5,herald.df_s[,2:12])
#ANOVA + Tukey (if sig.)

# MILEAGE
h.aov.k5.mileage <- aov(mileage ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.mileage) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.mileage) # note: we include output only of insignificant differences
# 5-2 p=0.99, the Comfort Eco-Pilgrims (5) not sig. diff. on Mileage from Entertained Eco-Pilgrims (2)

# POWER
h.aov.k5.power <- aov(power ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.power) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.power) # note: we include output only of insignificant differences
# 2-1 p=0.07, the Entertained Eco-Pilgrims (2) not sig. diff. on Power from Comfort Cruisers (1)
# 3-2 p=0.40, the Designer Eco-Cruisers (3) not sig. diff. on Power from Entertained Eco-Pilgrims (2)


# DESIGN
h.aov.k5.design <- aov(design ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.design) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.design) # note: we include output only of insignificant differences
# 4-1 p=0.99, the Power Pilgrims (4) not sig. diff. on Design from Comfort Cruisers (1)
# 4-2 p=0.06, the Power Pilgrims (4) not sig. diff.on Design from Entertained Eco-Pilgrims (2)


# COMFORT
h.aov.k5.comfort <- aov(comfort ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.comfort) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.comfort) # note: we include output only of insignificant differences
# 4-3 p=0.11, the Power Pilgrims (4) not sig. difference on Comfort from  Designer Eco-Cruisers (3)

# ENTERTAINMENT
h.aov.k5.entertainment <- aov(entertainment ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.entertainment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.entertainment) # note: we include output only of insignificant differences
# All the are significantly different!

# ENVIRONMENT
h.aov.k5.environment <- aov(environment ~ clusmembers.k5, data = herald.aovbase.k5)
summary(h.aov.k5.environment) # SIGNIFICANT (p<0.001) averages are different between clusters, but which? => TukeyHSD
TukeyHSD(h.aov.k5.environment) # note: we include output only of insignificant differences
# 4-1 p=0.99, the Power Pilgrims (4) not sig. diff.on Enviro. from Comfort Cruisers (1)
# 3-2 p=0.46, the Designer Eco-Cruisers (3) not sig. diff. on Enviro. from Entertained Eco-Pilgrims (2)
# 5-3 p=0.73, the Comfort Eco-Pilgrims (5) not sig. diff. on Enviro. from Designer Eco-Cruisers (3)


#### Conclusion -----
# The results show difference between clusters. The ones who don't significantly differ, don't differ on characteristics that already visibly don't differ in the table
# and are logical in terms of their naming/definitions. The good thing is all ANOVAs are significant and we keep all of our active variables.


### Cluster Sizes & Socio-demographics ----
#note: we add recoded variables and redo cluster_name
herald.nhcsolution.df <- cbind(clusmembers.k5,herald.hcsolution.df[,c(7,8,10,11)])
names(herald.nhcsolution.df)[1] <- "cluster"

# Daily Riders (1), Entertained Eco-Travelers (2), Designer Eco-Cruisers (3), Power Pilgrims (4), Comfort Eco-Pilgrims (5)
herald.nhcsolution.df[herald.nhcsolution.df$cluster == 1, "cluster_name"] <- "(1) Daily Riders"
herald.nhcsolution.df[herald.nhcsolution.df$cluster == 2, "cluster_name"] <- "(2) Entertained Eco-Pilgrims"
herald.nhcsolution.df[herald.nhcsolution.df$cluster == 3, "cluster_name"] <- "(3) Designer Eco-Cruisers"
herald.nhcsolution.df[herald.nhcsolution.df$cluster == 4, "cluster_name"] <- "(4) Power Pilgrims"
herald.nhcsolution.df[herald.nhcsolution.df$cluster == 5, "cluster_name"] <- "(5) Comfort Eco-Pilgrims"

#RELATIVE (%) NR. OF RESPONDENTS per cluster
nhcsolution.size <- herald.nhcsolution.df %>%
  group_by(cluster_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(nhcsolution.size))
#                  cluster_name n.respondents respondents.percent
# 1             (1) Daily Riders           106              25.24%
# 2 (2) Entertained Eco-Pilgrims            88              20.95%
# 3    (3) Designer Eco-Cruisers            92              21.90%
# 4           (4) Power Pilgrims            67              15.95%
# 5     (5) Comfort Eco-Pilgrims            67              15.95%
# sizes seem more equally distributed than hc solution

#GENDER distribution per cluster
nhcsolution.gender <- herald.nhcsolution.df %>%
  group_by(cluster_name,gender_string) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(nhcsolution.gender))
#                  cluster_name gender_string n.respondents respondents.percent
# 1              (1) Daily Riders        female            27              25.47%
# 2              (1) Daily Riders          male            79              74.53%
# 3  (2) Entertained Eco-Pilgrims        female            38              43.18%
# 4  (2) Entertained Eco-Pilgrims          male            50              56.82%
# 5     (3) Designer Eco-Cruisers        female            76              82.61%
# 6     (3) Designer Eco-Cruisers          male            16              17.39%
# 7            (4) Power Pilgrims        female            12              17.91%
# 8            (4) Power Pilgrims          male            55              82.09% <- still mostly male, but less so (before 100%)
# 9      (5) Comfort Eco-Pilgrims        female            64              95.52% <- still mostly female (before 100%)
# 10     (5) Comfort Eco-Pilgrims          male             3               4.48%

#AGE distribution per cluster
nhcsolution.agegroup <- herald.nhcsolution.df %>%
  group_by(cluster_name,age_group) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(nhcsolution.agegroup))
#                  cluster_name    age_group n.respondents respondents.percent
# 1              (1) Daily Riders        39-48             5               4.72%
# 2              (1) Daily Riders        49-58            17              16.04%
# 3              (1) Daily Riders        59-68            53              50.00%
# 4              (1) Daily Riders 68 and older            31              29.25%
# 5  (2) Entertained Eco-Pilgrims        18-28            58              65.91%
# 6  (2) Entertained Eco-Pilgrims        29-38            23              26.14%
# 7  (2) Entertained Eco-Pilgrims        39-48             4               4.55%
# 8  (2) Entertained Eco-Pilgrims        49-58             2               2.27%
# 9  (2) Entertained Eco-Pilgrims        59-68             1               1.14%
# 10    (3) Designer Eco-Cruisers        18-28            11              11.96%
# 11    (3) Designer Eco-Cruisers        29-38            25              27.17%
# 12    (3) Designer Eco-Cruisers        39-48            41              44.57%
# 13    (3) Designer Eco-Cruisers        49-58            12              13.04%
# 14    (3) Designer Eco-Cruisers        59-68             3               3.26%
# 15           (4) Power Pilgrims        18-28             5               7.46%
# 16           (4) Power Pilgrims        29-38            14              20.90%
# 17           (4) Power Pilgrims        39-48            18              26.87%
# 18           (4) Power Pilgrims        49-58            15              22.39%
# 19           (4) Power Pilgrims        59-68            10              14.93%
# 20           (4) Power Pilgrims 68 and older             5               7.46%
# 21     (5) Comfort Eco-Pilgrims        29-38             1               1.49%
# 22     (5) Comfort Eco-Pilgrims        39-48             8              11.94%
# 23     (5) Comfort Eco-Pilgrims        49-58            23              34.33%
# 24     (5) Comfort Eco-Pilgrims        59-68            20              29.85%
# 25     (5) Comfort Eco-Pilgrims 68 and older            15              22.39%


#AREA distribution per cluster
nhcsolution.area <- herald.nhcsolution.df %>%
  group_by(cluster_name,area_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(nhcsolution.area))
#                    cluster_name        area_name n.respondents respondents.percent
# 1              (1) Daily Riders (1) Metropolitan            27              25.47%
# 2              (1) Daily Riders        (2) Urban            39              36.79%
# 3              (1) Daily Riders     (3) Suburban            28              26.42%
# 4              (1) Daily Riders  (4) Countryside            12              11.32%
# 5  (2) Entertained Eco-Pilgrims (1) Metropolitan             7               7.95%
# 6  (2) Entertained Eco-Pilgrims        (2) Urban            21              23.86%
# 7  (2) Entertained Eco-Pilgrims     (3) Suburban            23              26.14%
# 8  (2) Entertained Eco-Pilgrims  (4) Countryside            37              42.05%
# 9     (3) Designer Eco-Cruisers (1) Metropolitan            66              71.74%
# 10    (3) Designer Eco-Cruisers        (2) Urban            16              17.39%
# 11    (3) Designer Eco-Cruisers     (3) Suburban            10              10.87%
# 12           (4) Power Pilgrims (1) Metropolitan             5               7.46%
# 13           (4) Power Pilgrims        (2) Urban            11              16.42%
# 14           (4) Power Pilgrims     (3) Suburban            15              22.39%
# 15           (4) Power Pilgrims  (4) Countryside            36              53.73%
# 16     (5) Comfort Eco-Pilgrims (1) Metropolitan             5               7.46%
# 17     (5) Comfort Eco-Pilgrims        (2) Urban            15              22.39%
# 18     (5) Comfort Eco-Pilgrims     (3) Suburban            22              32.84%
# 19     (5) Comfort Eco-Pilgrims  (4) Countryside            25              37.31%

#EDUCATION distribution per cluster
nhcsolution.edu <- herald.nhcsolution.df %>%
  group_by(cluster_name,education_name) %>%
  summarise(n.respondents = n()) %>%
  mutate(respondents.percent = formattable::percent(n.respondents / sum(n.respondents)))

print(as.data.frame(nhcsolution.edu))
#                  cluster_name    education_name n.respondents respondents.percent
# 1              (1) Daily Riders (1) HS Profession            18              16.98%
# 2              (1) Daily Riders     (2) HS Theory            35              33.02%
# 3              (1) Daily Riders    (3) HE Non-Uni            15              14.15%
# 4              (1) Daily Riders    (4) University            19              17.92%
# 5              (1) Daily Riders         (5) Other            19              17.92%
# 6  (2) Entertained Eco-Pilgrims (1) HS Profession             8               9.09%
# 7  (2) Entertained Eco-Pilgrims     (2) HS Theory            19              21.59%
# 8  (2) Entertained Eco-Pilgrims    (3) HE Non-Uni            16              18.18%
# 9  (2) Entertained Eco-Pilgrims    (4) University            24              27.27%
# 10 (2) Entertained Eco-Pilgrims         (5) Other            21              23.86%
# 11    (3) Designer Eco-Cruisers (1) HS Profession            15              16.30%
# 12    (3) Designer Eco-Cruisers     (2) HS Theory            13              14.13%
# 13    (3) Designer Eco-Cruisers    (3) HE Non-Uni            19              20.65%
# 14    (3) Designer Eco-Cruisers    (4) University            17              18.48%
# 15    (3) Designer Eco-Cruisers         (5) Other            28              30.43%
# 16           (4) Power Pilgrims (1) HS Profession            43              64.18%
# 17           (4) Power Pilgrims     (2) HS Theory            18              26.87%
# 18           (4) Power Pilgrims    (3) HE Non-Uni             5               7.46%
# 19           (4) Power Pilgrims         (5) Other             1               1.49%
# 20     (5) Comfort Eco-Pilgrims (1) HS Profession             1               1.49%
# 21     (5) Comfort Eco-Pilgrims     (2) HS Theory             4               5.97%
# 22     (5) Comfort Eco-Pilgrims    (3) HE Non-Uni            18              26.87%
# 23     (5) Comfort Eco-Pilgrims    (4) University            21              31.34%
# 24     (5) Comfort Eco-Pilgrims         (5) Other            23              34.33%



