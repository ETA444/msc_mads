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

# Handy package that Installs, Updates and Loads
# packages from CRAN, Github & Bioconductor
# install.packages("librarian")
library("librarian")

#install & load packages with shelf function
librarian::shelf(nFactors,gplots,RColorBrewer,semPlot, lmtest, aTSA, ggplot2, ggcorrplot, stringr, plyr, formattable, fpc, dplyr, fastDummies,
                 car,corrplot,dplyr,stats,bannerCommenter, vars, urca, clipr, cluster, mclust, kassambara/easyGgplot2, tidyr, reshape2, ISOweek, lubridate, mice, tidyverse, VIM)

##::::::::::
##  Data  ::
##::::::::::
# DATA ----

#setwd("X")
#lemonade <- read.csv("Lemonade Tidy format 2022.csv")
lemonade <- read.csv(file.choose())
lemonade$Chain <- as.factor(lemonade$Chain)
lemonade$Brand <- as.factor(lemonade$Brand)
lemonade$Date <- ISOweek2date(sub("(wk) (\\d{2}) (\\d{2})","20\\3-W\\2-1", lemonade$Week))
lemonade$Quarter <- as.factor(quarter(lemonade$Date))

str(lemonade) #types of variables examined: correct.
summary(lemonade) #unitsales 741k!;128 NA's;

#missing values
sum(is.na(lemonade)) 
#check missed data in a specific attribute
summary.na<-lemonade %>% summarise_all(funs(sum(is.na(.))))
View(summary.na)
dim(summary.na)
#UnitSales, PricePU, PricePL, BasePricePU, BasePricePU - Each of the 4 variables have NAs.

#Visualize missing data
#percentage + histogram + pattern
aggr_plot <- aggr(lemonade, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))

# 1.make graphs of variables over time ----
# extract week num from time data
lemonade <- lemonade %>%
  separate(Week, c('removed', 'week_num', 'year'), ' ') %>%
  mutate(cleaned_wk = trimws((week_num),which='left',whitespace='0')) 
lemonade$cleaned_wk <- as.numeric(unlist(lemonade$cleaned_wk))
  
# in order to generate graphs over time, reassign week num according to year no. with if else statements
attach(lemonade)
lemonade$week_yrs <- ifelse(year=='17',seq(1,52),
                            ifelse(year=='18',seq(53,104),
                                   ifelse(year=='19',seq(105,156),
                                          ifelse(year=='20',seq(157,208),NA))))


# create variable 'sales'
lemonade$sales <- UnitSales*PricePU

# create variables 'discountPU_raw' (raw price difference)
# and 'discountPU_perc' (percent discount)
lemonade$discount_raw <- BasePricePU - PricePU
lemonade$discount_perc <- ((BasePricePU - PricePU) / BasePricePU) * 100

#create ADJUSTED PROMOTION VARIABLES
str(which(lemonade$FeatDisp+lemonade$FeatOnly+lemonade$DispOnly > 100)) # just see how many rows there are (not used in function below)

#Create dummy which says 1 if the sum is more than 100 (used in the variable creation ifstatement)
lemonade$sum100 <- ifelse(lemonade$FeatDisp+lemonade$FeatOnly+lemonade$DispOnly > 100, 1, 0)

#Creating the adjusted promotion colums
lemonade$FeatOnly_Adjusted <- ifelse(lemonade$sum100 =='1',
                                     ((lemonade$FeatOnly / (lemonade$FeatDisp+lemonade$FeatOnly+lemonade$DispOnly))*100), lemonade$FeatOnly)

lemonade$DispOnly_Adjusted <- ifelse(lemonade$sum100 =='1',
                                     ((lemonade$DispOnly / (lemonade$FeatDisp+lemonade$FeatOnly+lemonade$DispOnly))*100), lemonade$DispOnly)

lemonade$FeatDisp_Adjusted <- ifelse(lemonade$sum100 =='1',
                                     ((lemonade$FeatDisp / (lemonade$FeatDisp+lemonade$FeatOnly+lemonade$DispOnly))*100), lemonade$FeatDisp)


# do some cleaning up
lemonade <- lemonade[,-c(3,4)]
lemonade <- relocate(lemonade, week_yrs, .after = Brand)
lemonade <- relocate(lemonade, sales, .after = UnitSales)
lemonade <- relocate(lemonade, cleaned_wk, .after = Brand)
lemonade <- relocate(lemonade, discount_raw, .after = BasePricePL)
lemonade <- relocate(lemonade, discount_perc, .after = discount_raw)
lemonade <- relocate(lemonade, sum100, .after = FeatDisp_Adjusted)
lemonade <- relocate(lemonade, FeatOnly_Adjusted, .after = FeatDisp)
lemonade <- relocate(lemonade, DispOnly_Adjusted, .after = FeatOnly_Adjusted)
lemonade <- relocate(lemonade, FeatDisp_Adjusted, .after = DispOnly_Adjusted)

#Gaining some general insights
#Total unit sales
plot(lemonade$UnitSales, xlab = "Observation", ylab = "Unit Sales", main = "Unit Sales",type = "o", pch = 20,col="dodgerblue")

#Identifying median and outliers
boxplot(lemonade$BasePricePU[lemonade$BasePricePU>0]~lemonade$Brand[lemonade$BasePricePU>0],col="firebrick1",ylab = "Prices (\u20AC)", main = "Prices per brand", xlab = NULL)

boxplot(lemonade$BasePricePU[lemonade$BasePricePU>0]~lemonade$Chain[lemonade$BasePricePU>0],col=c("dodgerblue", "orange", "firebrick2", "dodgerblue", "gold","yellowgreen", "grey") ,ylab = "Prices (\u20AC)", xlab = NULL, main = "Prices per chain" )

BrandNames <- levels(unique(lemonade$Brand))
ChainNames <- levels(unique(lemonade$Chain))
ChainColors <- c("dodgerblue", "orange", "firebrick2", "dodgerblue","gold","yellowgreen","grey")
names(ChainColors) <- c("Albert Heijn", "Coop", "Deen", "Hoogvliet", "Jumbo", "Plus", "TotalOnlineSales")

BoxPlotChainColors <- NULL
for (i in 1:length(ChainNames)) {
  BoxPlotChainColors <- c(BoxPlotChainColors, rep(ChainColors[ChainNames[i]],length(BrandNames)))
}

op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1 Temporarily increase X-margin to allow for long brand names

boxplot(lemonade$BasePricePU[lemonade$BasePricePU>0]~lemonade$Brand[lemonade$BasePricePU>0] + lemonade$Chain[lemonade$BasePricePU>0],las=2,col=BoxPlotChainColors, main="Prices per chain per brand",ylab="Price (\u20AC)",names=rep(BrandNames,7),xlab = NULL)

StartText <- 4

for (i in 1:length(ChainNames)) {
  text(StartText+(i-1)*6,3.2,ChainNames[i],col=ChainColors[ChainNames[i]])
}

op <- par(mar = c(5,4,4,2) + 0.1) ## set margins back to default, which is c(5,4,4,2) + 0.1

# make graphs of variables over time

# sales by channel; legend brand
lemonade %>%
  filter(Chain=='Albert Heijn') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Albert Heijn') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Coop') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Coop') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Deen') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Deen') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Hoogvliet') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Hoogvliet') +
  xlab("Week") + ylab("Unit Sales (\u20ac)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Jumbo') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Jumbo') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Plus') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Plus') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='TotalOnlineSales') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Unit Sales per Brand Over Time\nChannel: Online (total)') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

# sales by brand; legend channel
lemonade %>%
  filter(Brand=='EuroShopper') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: EuroShopper') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='KarvanCevitam') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Sales per Channel Over Time\nBrand: Karavan Cevitam') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='PrivateLabel') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: Private Labels') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Raak') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Slimpie') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: Slimpie') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Teisseire') %>%
  ggplot(aes(x=Date, y=UnitSales)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: Teisseire') +
  xlab("Week") + ylab("Unit Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))


# discount (%) by channel; legend brand
lemonade %>%
  filter(Chain=='Albert Heijn') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Albert Heijn') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Coop') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Coop') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Deen') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Deen') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Hoogvliet') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Hoogvliet') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Jumbo') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Jumbo') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='Plus') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: Plus') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Chain=='TotalOnlineSales') %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Brand)) +
  ggtitle('Discount (%) per Brand Over Time\nChannel: TotalOnlineSales') +
  xlab("Week") + ylab("Discount (%)") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))



# 2.1 building brand position map----
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x), na.rm = TRUE))}
lemonade_bp <- seg.summ(lemonade[, c("UnitSales", "PricePU")], lemonade$Brand)

ggplot(data=lemonade_bp, 
       aes(x = UnitSales, y = PricePU, label = Group.1)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Group.1),size = 6) +
  ggtitle("Brand Maps - UnitSales X Price") +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size=20))+
  theme(plot.title = element_text(size=20))+
  xlim(-50000,100000) #including negative value only to show the full label name in the chart for aesthetics purpose!

# 2.2 investigate frequency of promotion across different brands and supermarket formulas?----
# create dummies for each kind of promotion

lemonade$FeatOnlyD <- as.integer(ifelse(lemonade$FeatOnly>0,1,0))
lemonade$DispOnlyD <- as.integer(ifelse(lemonade$DispOnly>0,1,0))
lemonade$FeatDispD <- as.integer(ifelse(lemonade$FeatDisp>0,1,0))

# frequency of promotion across different brands
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) sum(data.frame(x)))}
lemonade_promotion_fq <- seg.summ(lemonade[, c("FeatOnlyD", "DispOnlyD", "FeatDispD")], lemonade$Brand)
colnames(lemonade_promotion_fq)[1] <- 'Brand'
lemonade_promotion_fq <- melt(lemonade_promotion_fq, id = c("Brand")) 
ggplot(data=lemonade_promotion_fq,
       aes(x=reorder(lemonade_promotion_fq$variable,lemonade_promotion_fq$value,decreasing = TRUE),
           y=lemonade_promotion_fq$value, fill=lemonade_promotion_fq$Brand)) +
  ggtitle('Promotion Frequency per Brand') +
  xlab("Promotion Type") + ylab("Total Promotions") + labs(fill = "Brand") + 
  geom_col(position = position_dodge())

# frequency of promotion across different supermarket formulas
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) sum(data.frame(x)))}
lemonade_promotion_fq <- seg.summ(lemonade[, c("FeatOnlyD", "DispOnlyD", "FeatDispD")], lemonade$Chain)
colnames(lemonade_promotion_fq)[1] <- 'Chain'
lemonade_promotion_fq <- melt(lemonade_promotion_fq, id = c("Chain")) 
ggplot(data=lemonade_promotion_fq,
       aes(x=reorder(lemonade_promotion_fq$variable,lemonade_promotion_fq$value,decreasing = TRUE),
           y=lemonade_promotion_fq$value, fill=lemonade_promotion_fq$Chain)) +
  ggtitle('Promotion Frequency per Channel') +
  xlab("Promotion Type") + ylab("Total Promotions") + labs(fill = "Channel") + 
    geom_col(position = position_dodge())

# depth of promotion across different brands
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(x,na.rm=TRUE))}
lemonade_promotion_depth <- seg.summ(lemonade[, c("discount_perc")], lemonade$Brand)
colnames(lemonade_promotion_depth)[1] <- 'Brand'
ggplot(data=lemonade_promotion_depth,
       aes(x=reorder(lemonade_promotion_depth$Brand,lemonade_promotion_depth$x,decreasing = TRUE),
           y=lemonade_promotion_depth$x, fill=lemonade_promotion_depth$Brand)) +
  ggtitle('Promotion Depth per Brand') +
  xlab("Brand") + ylab("Aggregated mean value of discount in %") + labs(fill = "Brand") + 
  geom_col(position = position_dodge())

# depth of promotion across different supermarket formulas
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(x,na.rm=TRUE))}
lemonade_promotion_depth <- seg.summ(lemonade[, c("discount_perc")], lemonade$Chain)
colnames(lemonade_promotion_depth)[1] <- 'Chain'
ggplot(data=lemonade_promotion_depth,
       aes(x=reorder(lemonade_promotion_depth$Chain,lemonade_promotion_depth$x,decreasing = TRUE),
           y=lemonade_promotion_depth$x, fill=lemonade_promotion_depth$Chain)) +
  ggtitle('Promotion Depth per Chain') +
  xlab("Brand") + ylab("Aggregated mean value of discount in %") + labs(fill = "Chain") + 
  geom_col(position = position_dodge())

# 3.(seasonal influences) ----
anova_quarter <- aov(UnitSales ~ Quarter, data = lemonade)
summary(anova_quarter)

# visualization for the seasonal influence
lemonadeRaak <- lemonade[lemonade$Brand == "Raak",] # make dataframe with only Raak at all chains
boxplot((lemonadeRaak$UnitSales[lemonadeRaak$UnitSales>0])/1000 ~ lemonadeRaak$Quarter[lemonadeRaak$UnitSales>0] + lemonadeRaak$Chain[lemonadeRaak$UnitSales>0],las=2,col="#0DBDC2", main="Raak UnitSales per chain per quarter",ylab="Unit sales (x 1000)",xlab = NULL)

# investigate the seasonal variation more formally
attach(lemonade)
lemonade$quarter <- ifelse(cleaned_wk==c(1:13),1,
                           ifelse(cleaned_wk==c(14:26),2,
                                  ifelse(cleaned_wk==c(27:39),3,
                                         ifelse(cleaned_wk==c(40:52),4,NA))))
lemonade <- dummy_cols(lemonade,select_columns = "quarter")
lemonade <- na.omit(lemonade)
le_endo <- lemonade[,c("UnitSales","sales","PricePU","PricePL","BasePricePU","BasePricePL","discount_perc","FeatOnly_Adjusted","DispOnly_Adjusted","FeatDisp_Adjusted")]
le_exo <- lemonade[,c("quarter_1","quarter_2","quarter_3")]
# since BIC especially performs pretty well, we therefore choose 5
VARselect(le_endo,lag.max = 10,type="both",exogen = le_exo)
lemonade_est <- VAR(y=le_endo,p=5,type="both",exogen = le_exo)
summary(lemonade_est,"UnitSales") #affected
summary(lemonade_est,"sales") #affected
summary(lemonade_est,"PricePU") #affected
summary(lemonade_est,"PricePL") #affected
summary(lemonade_est,"BasePricePU") #affected
summary(lemonade_est,"BasePricePL") #affected
summary(lemonade_est,"discount_perc") #affected
summary(lemonade_est,"FeatOnly_Adjusted") #affected
summary(lemonade_est,"DispOnly_Adjusted") #affected
summary(lemonade_est,"FeatDisp_Adjusted") #affected

# 4.simple linear regression to see the effect of price on sales----

ModelAllBrandsAllChains <- lm(UnitSales ~ PricePU, data = lemonade)
summary(ModelAllBrandsAllChains)
by(lemonade,lemonade[,c("Chain", "Brand")],function(x) summary(lm(UnitSales~PricePU,data = x)))

# Effect of Price (per brand)
by(lemonade,lemonade[,c("Brand")],function(x) summary(lm(UnitSales~PricePU,data = x)))
# Effect of Discount (per brand)
by(lemonade,lemonade[,c("Brand")],function(x) summary(lm(UnitSales~discount_perc,data = x)))
# Effect of Feature Promotion (per brand)
by(lemonade,lemonade[,c("Brand")],function(x) summary(lm(UnitSales~FeatOnly_Adjusted,data = x)))
# Effect of Display Promotion (per brand)
by(lemonade,lemonade[,c("Brand")],function(x) summary(lm(UnitSales~DispOnly_Adjusted,data = x)))
# Effect of F&D Promotion (per brand)
by(lemonade,lemonade[,c("Brand")],function(x) summary(lm(UnitSales~FeatDisp_Adjusted,data = x)))

# 5.more graph analysis on the selected brand Raak----
# help me to inspect data more easily...
# write.csv(lemonade,file="lemonade.csv")

lemonade %>%
  filter(Brand=='Raak') %>%
  ggplot(aes(x=Date, y=UnitSales/1000)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Unit Sales per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("Unit Sales x 1000") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Raak') %>%
  ggplot(aes(x=Date, y=sales/1000)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Sales per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("Sales x 1000") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Raak') %>%
  ggplot(aes(x=Date, y=PricePU)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('PricePU per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("PricePU") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Raak') %>%
  ggplot(aes(x=Date, y=BasePricePU)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('BasePricePU per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("BasePricePU") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

lemonade %>%
  filter(Brand=='Raak'& Chain=="Albert Heijn") %>%
  ggplot(aes(x=Date, y=discount_perc)) +
  geom_line(aes(colour=Chain)) +
  ggtitle('Discount Percentage per Channel Over Time\nBrand: Raak') +
  xlab("Week") + ylab("discount_perc") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

# Relational Plots
glimpse(lemonade[,c("UnitSales","PricePU","FeatOnly_Adjusted","DispOnly_Adjusted","FeatDisp_Adjusted","Quarter","discount_perc")])
# Correlation Matrix between key variables
str(which(lemonade$Brand == "Raak"))
str(lemonade[lemonade$Brand == "Raak",][,c("sales","PricePU","FeatOnly_Adjusted","DispOnly_Adjusted","FeatDisp_Adjusted","discount_perc")])
lemonade.cor <- cor(lemonade[lemonade$Brand == "Raak",][,c("sales","PricePU","FeatOnly_Adjusted","DispOnly_Adjusted","FeatDisp_Adjusted","discount_perc")])
ggcorrplot(lemonade.cor,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors =  brewer.pal(3,"RdBu"))

# Regression plots
#Feature
ggplot(subset(lemonade, Brand == "Raak"), aes(x = FeatOnly_Adjusted, y= sales ))+
  geom_point()+
  stat_smooth(method=lm) +
  ggtitle('Feature Promotion effect on Sales \n Brand: Raak') +
  xlab("Feature Promotion") + ylab("Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))
#Display
ggplot(subset(lemonade, Brand == "Raak"), aes(x = DispOnly_Adjusted, y= sales ))+
  geom_point()+
  stat_smooth(method=lm) +
  ggtitle('Display Promotion effect on Sales \n Brand: Raak') +
  xlab("Display Promotion") + ylab("Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))
#Feature & Display
ggplot(subset(lemonade, Brand == "Raak"), aes(x = FeatDisp_Adjusted, y= sales ))+
  geom_point()+
  stat_smooth(method=lm) +
  ggtitle('Feature & Display Promotion effect on Sales \n Brand: Raak') +
  xlab("F&D Promotion") + ylab("Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))
#Discount
ggplot(subset(lemonade, Brand == "Raak"), aes(x = discount_perc, y= sales ))+
  geom_point()+
  stat_smooth(method=lm) +
  ggtitle('Discount effect on Sales \n Brand: Raak') +
  xlab("Discount") + ylab("Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))
#Price (per unit)
ggplot(subset(lemonade, Brand == "Raak"), aes(x = PricePU, y= sales ))+
  geom_point()+
  stat_smooth(method=lm) +
  ggtitle('Price (per unit) effect on Sales \n Brand: Raak') +
  xlab("Price") + ylab("Sales") +
  theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))

# Dominant promotion influence
# Create Promotion dominance groups
# : 1-Feature dominant, 2-Display dominant, 3-Combination dominant (if there is no clear dominance then NA, so that it's excluded from the analysis)
lemonade$PromoDominance <- ifelse(lemonade$FeatOnly_Adjusted > lemonade$DispOnly_Adjusted & lemonade$FeatOnly_Adjusted > lemonade$FeatDisp_Adjusted,1,
                                  ifelse(lemonade$DispOnly_Adjusted > lemonade$FeatOnly_Adjusted & lemonade$DispOnly_Adjusted > lemonade$FeatDisp_Adjusted,2,
                                         ifelse(lemonade$FeatDisp_Adjusted > lemonade$FeatOnly_Adjusted & lemonade$FeatDisp_Adjusted > lemonade$DispOnly_Adjusted,3,NA)))
anova_promo <- aov(UnitSales ~ as.factor(PromoDominance), data = lemonade)
summary(anova_promo)
TukeyHSD(anova_promo)

# Second part: model specification ----
#Followed by Elias’s scripts 
### Data Preparation
# dataset - each Chain 

Lemonade <- lemonade

for (i in unique(Lemonade$Chain)){
  df <-Lemonade[Lemonade$Chain ==i,]
  assign(  paste(i,'Lemonade', sep = ""), df )
}
AHLemonade <- Lemonade[Lemonade$Chain =="Albert Heijn",] # AH - prefer short name

# dataset - only Raak 
LemonadeRaak <- Lemonade[Lemonade$Brand == "Raak",] # make dataframe with only Karvan Cevitam at all chains

# dataset Raak - each chains
for (i in unique(Lemonade$Chain)){
  df <-Lemonade[Lemonade$Chain ==i,]
  df <-df[df$Brand == "Raak",]
  assign(  paste(i,'Lemonade','Raak', sep = ""), df )
}
AHLemonadeRaak <- AHLemonade[AHLemonade$Brand == "Raak",]# AH - prefer short name


###
#Plot----------------

boxplot(UnitSales/1000~Quarter, data = AHLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at Albert Heijn",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = JumboLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at Jumbo",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = PlusLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at Plus ",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = CoopLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at Coop",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = DeenLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at Deen",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = HoogvlietLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at  Hoogvliet",col=ChainColors["Albert Heijn"])
boxplot(UnitSales/1000~Quarter, data = TotalOnlineSalesLemonadeRaak, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly Raak sales per quarter at TotalOnlineSales",col=ChainColors["Albert Heijn"])

anova_out <- aov (UnitSales ~ Quarter, data = LemonadeRaak)
summary(anova_out)


##############################OVERAL
minUnitSales <- min(LemonadeRaak$UnitSales/1000,na.rm = TRUE)
maxUnitSales <- max(LemonadeRaak$UnitSales/1000,na.rm = TRUE)

plot(LemonadeRaak$Date[LemonadeRaak$Chain=="Albert Heijn"],LemonadeRaak$UnitSales[LemonadeRaak$Chain=="Albert Heijn"]/1000,col=ChainColors["Albert Heijn"],ylab="Unit sales per week (x 1000)",xlab = "Time", main = "Development of weekly unit sales of Raak over time",type = "l", ylim = c(minUnitSales,maxUnitSales))
for (i in 2:length(ChainNames)) {
  lines(LemonadeRaak$Date[LemonadeRaak$Chain==ChainNames[i]],LemonadeRaak$UnitSales[LemonadeRaak$Chain==ChainNames[i]]/1000,col=ChainColors[ChainNames[i]])
}
legend("topleft",inset = c(.005,0.01),ChainNames,col=ChainColors, lty = rep(1,6))

###################################OVERAL
minBasePrice <- min(LemonadeRaak$BasePricePU,na.rm = TRUE)   # this results in 0, which is not very useful
minBasePrice <- min(LemonadeRaak$BasePricePU[LemonadeRaak$BasePricePU>0],na.rm = TRUE)
maxBasePrice <- max(LemonadeRaak$BasePricePU,na.rm = TRUE)

plot(LemonadeRaak$Date[LemonadeRaak$Chain=="Albert Heijn"],LemonadeRaak$BasePricePU[LemonadeRaak$Chain=="Albert Heijn"],col=ChainColors["Albert Heijn"],ylab="Base price per unit (\u20AC)",xlab = "Time", main = "Development of base price of Raak over time",type = "l", ylim = c(minBasePrice,maxBasePrice))
for (i in 2:length(ChainNames)) {
  lines(LemonadeRaak$Date[LemonadeRaak$Chain==ChainNames[i]],LemonadeRaak$BasePricePU[LemonadeRaak$Chain==ChainNames[i]],col=ChainColors[ChainNames[i]])
}

legend("bottomleft",inset = c(.005,0.01),ChainNames,col=ChainColors, lty = rep(1,6))

###################################OVERAL: Sales -Price

ModelAllBrandsAllChains <- lm(UnitSales ~ PricePU, data = LemonadeRaak)
summary(ModelAllBrandsAllChains)

by(LemonadeRaak,LemonadeRaak[,c("Chain", "Brand")],function(x) summary(lm(UnitSales~PricePU,data = x)))

###################################OVERAL: Sales - Display_ajst

ModelAllBrandsAllChains <- lm(UnitSales ~ DispOnly_Adjusted, data = LemonadeRaak)
summary(ModelAllBrandsAllChains)
by(LemonadeRaak,LemonadeRaak[,c("Chain", "Brand")],function(x) summary(lm(UnitSales~DispOnly_Adjusted,data = x)))
###################################OVERAL: Sales - F_ajst

ModelAllBrandsAllChains <- lm(UnitSales ~ FeatOnly_Adjusted, data = LemonadeRaak)
summary(ModelAllBrandsAllChains)
by(LemonadeRaak,LemonadeRaak[,c("Chain", "Brand")],function(x) summary(lm(UnitSales~FeatDisp_Adjusted,data = x)))
###################################OVERAL: Sales - F+D_adiusted

ModelAllBrandsAllChains <- lm(UnitSales ~ FeatDisp_Adjusted, data = LemonadeRaak)
summary(ModelAllBrandsAllChains)
by(LemonadeRaak,LemonadeRaak[,c("Chain", "Brand")],function(x) summary(lm(UnitSales~FeatDisp_Adjusted,data = x)))


# Unit-by-unit models
# Not pooled 
' 1. AH Raak======='
library(corrplot)
library(RColorBrewer)
M <-cor(LemonadeRaak[,-c(1,2,5,20,21)])
corrplot(M, type="lower", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=AHLemonadeRaak)
summary(MultiplicativeModel)
# only variable with positive values can be the BASE, that is in the log()









# 2. get coeff
get_alpha <- function(){
  ## Apply the anti-log transformation to alpha_hat_star ----
  alpha_hat_star <- summary(MultiplicativeModel)$coefficients[1,1]
  sd_alpha_hat_star <- summary(MultiplicativeModel)$coefficients[1,2]
  alpha_hat <- exp(alpha_hat_star) * exp(-0.5*(sd_alpha_hat_star^2))
  sprintf("alpha_hat = %.2f", alpha_hat )
}
## For beta1, the anti-log transformation is not needed! ----
get_beta1_hat <- function(){
  sprintf("beta1_hat = %.2f", MultiplicativeModel$coefficients[2])
}
get_beta2_hat <- function(){
  sprintf("beta2_hat = %.2f",MultiplicativeModel$coefficients[3])
}
## Apply the anti-log transformation to beta2 ----
get_beta3_hat <- function(){
  beta3_hat_star <- MultiplicativeModel$coefficients[4]
  sd_beta3_hat_star <- summary(MultiplicativeModel)$coefficients[4,2]
  beta3_hat <- exp(beta3_hat_star) * exp(-0.5*(sd_beta3_hat_star^2))
  sprintf("beta3_hat = %.2f", beta3_hat )
}
## Apply the anti-log transformation to beta3 ----
get_beta4_hat <- function(){
  beta4_hat_star <- MultiplicativeModel$coefficients[5]
  sd_beta4_hat_star <- summary(MultiplicativeModel)$coefficients[5,2]
  beta4_hat <- exp(beta4_hat_star) * exp(-0.5*(sd_beta4_hat_star^2))
  sprintf("beta4_hat = %.2f", beta4_hat )
}

# use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat()
# re-get beta 2 for AH - cannot be 0
sprintf("beta2_hat = %.4f",MultiplicativeModel$coefficients[3])

##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
RaakSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(AHLemonadeRaak$Date,AHLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Albert Heijn (units)",main = "Comparing actual and fitted sales of Raak at Albert Heijn")
lines(AHLemonadeRaak$Date,AHLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(AHLemonadeRaak$Date,RaakSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
plot(MultiplicativeModel, 1)

#model assumption - plots
install.packages('ggfortify')
library(ggfortify)
autoplot(MultiplicativeModel)


'==2.Coop=========='
# 1. COOP Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=CoopLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat()


##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
CoopSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(CoopLemonadeRaak$Date,CoopLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Coop (units)",main = "Comparing actual and fitted sales of Raak at Coop")
lines(CoopLemonadeRaak$Date,CoopLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(CoopLemonadeRaak$Date,CoopSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
autoplot(MultiplicativeModel)

# '====3.Deen=========='
# 1. Deen Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=DeenLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
###!!!!!!!!!!!!!!!!!
#NA as a coefficient in a regression indicates that the variable in question is linearly related to the other variables. 
#In your case, this means that Q3=aÃ Q1+bÃ Q2+c for some a,b,c. If this is the case, then there's no unique solution to the regression 
#without dropping one of the variable
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
DeenSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(DeenLemonadeRaak$Date,DeenLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Deen (units)",main = "Comparing actual and fitted sales of Raak at Deen")
lines(DeenLemonadeRaak$Date,DeenLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(DeenLemonadeRaak$Date,DeenSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
autoplot(MultiplicativeModel)

# '====4.Hoog=========='
# 1. Hoog Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=HoogvlietLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
HoogSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(HoogvlietLemonadeRaak$Date,HoogvlietLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Hoogvliet (units)",main = "Comparing actual and fitted sales of Raak at Hoogvliet")
lines(HoogvlietLemonadeRaak$Date,HoogvlietLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(HoogvlietLemonadeRaak$Date,HoogSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
autoplot(MultiplicativeModel)


# '====5.Jumbo=========='
# 1. Jumbo Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=JumboLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
JumboSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(JumboLemonadeRaak$Date,JumboLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Jumbo (units)",main = "Comparing actual and fitted sales of Raak at Jumbo")
lines(JumboLemonadeRaak$Date,JumboLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(JumboLemonadeRaak$Date,JumboSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
autoplot(MultiplicativeModel)


# '====6. Plus========='
# 1. Plus Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=PlusLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
PlusSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(PlusLemonadeRaak$Date,PlusLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Plus (units)",main = "Comparing actual and fitted sales of Raak at Plus")
lines(PlusLemonadeRaak$Date,PlusLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(PlusLemonadeRaak$Date,PlusSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)



# '======7. Total Online====='
# 1. Total Online Raak 
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=TotalOnlineSalesLemonadeRaak)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
TotalOnlineSalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

## 4.Make the comparison plot ---- actual and fitted
plot(TotalOnlineSalesLemonadeRaak$Date,TotalOnlineSalesLemonadeRaak$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Total Online Sales (units)",main = "Comparing actual and fitted sales of Raak at Total Online Sales")
lines(TotalOnlineSalesLemonadeRaak$Date,TotalOnlineSalesLemonadeRaak$UnitSales,lwd=2,col="dodgerblue")
lines(TotalOnlineSalesLemonadeRaak$Date,TotalOnlineSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)
autoplot(MultiplicativeModel)
########
'Pool part - LemonadeRaak -one model on all chains'

LemonadeRaak2 <- LemonadeRaak
LemonadeRaak2$Brand <- rep('brd',times = 1456)

MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+FeatOnly_Adjusted+DispOnly_Adjusted+FeatDisp_Adjusted,data=LemonadeRaak2)
summary(MultiplicativeModel)

# 2. use function to get coefficients
get_alpha()
get_beta1_hat()
get_beta2_hat()
get_beta3_hat()
get_beta4_hat() 
##  Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeModel)$sigma

## 3. Calculate the fitted values ----
SalesFit <- exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)
autoplot(MultiplicativeModel)

