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
librarian::shelf(RColorBrewer, lmtest, aTSA, ggplot2, ggcorrplot, dplyr, fastDummies,
                 car,corrplot,dplyr, vars, urca, mclust, tidyr, reshape2, ISOweek, lubridate, mice, tidyverse, VIM, quiet = TRUE)

##::::::::::
##  Data  ::
##::::::::::
# DATA ----
setwd("YOURPATH") #set working directory#
lemonade <- read.csv("YOURPATH", header = TRUE)
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


# model corrected - by Elias Date: 01/06/2022----
Lemonade <- lemonade

# remove unnecessary stuff from the environment
rm(aggr_plot,anova_promo,anova_quarter,le_endo,le_exo,lemonade_bp,lemonade_est,lemonade_promotion_depth,lemonade_promotion_fq,lemonade.cor,lemonadeRaak,ModelAllBrandsAllChains,op,summary.na)
rm(BoxPlotChainColors,ChainColors,i,StartText,seg.summ)
rm(lemonade)

# create smooth version of BasePricePU
#Check for inconsistiencies:

Lemonade$Promotion <- Lemonade$BasePricePU - Lemonade$PricePU
percentagefalse <- (sum(Lemonade$Promotion < 0, na.rm=TRUE)/nrow(Lemonade))*100

Lemonade$BasePricePU_Smooth <- rep(0,nrow(Lemonade)) #create a new variable in the data frame - a smoothed version of BasePricePU - initially filled up with zeros

#Loop over chains and brands: within this loop, loess regression is used to create a smoothed version of BasePricePU

for(ch in ChainNames){
  for(br in BrandNames){
    loess_temp <- loess(Lemonade$BasePricePU[Lemonade$Chain==ch & Lemonade$Brand==br]~c(1:length(Lemonade$BasePricePU[Lemonade$Chain==ch & Lemonade$Brand==br])),span=0.10)
    if (length(Lemonade$BasePricePU_Smooth[Lemonade$Chain==ch & Lemonade$Brand==br]) != length(loess_temp$fitted)) {
      fill_up <- c(loess_temp$fitted,rep(NA,length(Lemonade$BasePricePU_Smooth[Lemonade$Chain==ch & Lemonade$Brand==br])-length(loess_temp$fitted)))
      Lemonade$BasePricePU_Smooth[Lemonade$Chain==ch & Lemonade$Brand==br] <- fill_up
    } else {
      Lemonade$BasePricePU_Smooth[Lemonade$Chain==ch & Lemonade$Brand==br] <- loess_temp$fitted
    }
  }
}

#Now that smoothing is done, replace any remaining smoothed BasePricePU values by PricePU values if they are lower than PricePU
Lemonade$BasePricePU_Smooth <- ifelse(Lemonade$BasePricePU_Smooth > Lemonade$PricePU, Lemonade$BasePricePU_Smooth, Lemonade$PricePU)

#Now check again if there are inconsistencies
Lemonade$Promotion <- Lemonade$BasePricePU_Smooth - Lemonade$PricePU
percentagefalse <- (sum(Lemonade$Promotion < 0, na.rm=TRUE)/nrow(Lemonade))*100 # now no inconsistencies anymore

#Just a visual check how it looks like for one brand-chain combination:
plot(Lemonade$PricePU[Lemonade$Chain=="Jumbo" & Lemonade$Brand=="Raak"],type="l")
lines(Lemonade$BasePricePU_Smooth[Lemonade$Chain=="Jumbo" & Lemonade$Brand=="Raak"],type="l",col="red")

# combine covid data
# Download COVID data from the web ----
Covid_cases <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Data cleaning ----

## Only focus on data from the Netherlands ----
Covid_cases_Reduced <- Covid_cases[Covid_cases$Country.Region == "Netherlands",]

## Only focus on data from the main country ----
Covid_cases_Reduced_Further <- Covid_cases_Reduced[Covid_cases_Reduced$Province.State == "",]

## Transpose the dataframe and exclude the first four entries ----
Covid_cases_df <- data.frame(t(Covid_cases_Reduced_Further[,-c(1:4)]))

## Extract a date variable from the row names ----
Covid_cases_df$Date <- as.Date(row.names(Covid_cases_df),"X%m.%d.%y")

## Rename focal variable ----
names(Covid_cases_df)[names(Covid_cases_df)=="X201"] <- "CovidCases"

## Make a plot of the focal variable ----
plot(Covid_cases_df$Date,Covid_cases_df$CovidCases,type="l")

## Create a new variable, consisting of new cases ----
Covid_cases_df$NewCovidCases <- c(NA,diff(Covid_cases_df$CovidCases,1))

## Make a plot of the new variable ----
plot(Covid_cases_df$Date,Covid_cases_df$NewCovidCases,type="l")

## Replace one very extreme outlier ----
Covid_cases_df$NewCovidCases[Covid_cases_df$NewCovidCases > 150000] <- max(Covid_cases_df$NewCovidCases[Covid_cases_df$NewCovidCases < 150000],na.rm = TRUE)

## Make a plot of the new variable again - does this look better? ----
plot(Covid_cases_df$Date,Covid_cases_df$NewCovidCases,type="l")

## Add a week variable ----
library(lubridate)
Covid_cases_df$Week <- week(Covid_cases_df$Date)
Covid_cases_df$Year <- year(Covid_cases_df$Date)
## Calculate weekly averages ----
Years <- unique(Covid_cases_df$Year)
Weeks <- unique(Covid_cases_df$Week)
for (iYear in Years) {
  for (iWeek in Weeks) {
    Covid_cases_df$WeekAverage[Covid_cases_df$Year == iYear & Covid_cases_df$Week == iWeek] <- mean(Covid_cases_df$NewCovidCases[Covid_cases_df$Year == iYear & Covid_cases_df$Week == iWeek],na.rm = TRUE)
  }
}

## Make a plot to check whether we created the weekly average correctly
plot(Covid_cases_df$Date,Covid_cases_df$NewCovidCases,type="l")
lines(Covid_cases_df$Date,Covid_cases_df$WeekAverage,type="l", col="Red")

# Clean up after getting external data ----
rm(Covid_cases,Covid_cases_Reduced,Covid_cases_Reduced_Further,iWeek,Weeks)

# Combine lemonade data and covid data ----
Covid_cases_df <- subset(Covid_cases_df,select = c("Week","Year","WeekAverage"))
Covid_cases_df <- Covid_cases_df %>% distinct(Year, Week, .keep_all = TRUE)
Lemonade$year <- year(Lemonade$Date)
Lemonade_extended <- data.frame()
for(ch in ChainNames){
  for(br in BrandNames){
    Lemonade_ch_br <- merge(subset(Lemonade[Lemonade$Chain==ch & Lemonade$Brand==br, ]),Covid_cases_df,by.x = c("year","cleaned_wk"),by.y = c("Year","Week"),all.x = TRUE)
    Lemonade_extended <- rbind(Lemonade_extended, Lemonade_ch_br)
  }
}

# combine extended data and weather data----
WeatherDF <- read.csv("etmgeg_280.csv", header = TRUE)
WeatherDF$Date <- as.Date(as.character(WeatherDF$YYYYMMDD),"%Y%m%d")
WeatherDF$Week <- ISOweek(WeatherDF$Date)
TempAvg <- aggregate(TG/10~Week, FUN=mean, data=WeatherDF, na.rm=TRUE)

UniqueDates <- unique(Lemonade_extended$Date)

Lemonade_extended$Temp <- rep(0,nrow(Lemonade_extended))

for (i in 1:length(UniqueDates)) {
  Lemonade_extended$Temp[Lemonade_extended$Date==UniqueDates[i]] <- TempAvg[ISOweek2date(paste(TempAvg$Week,"1",sep="-")) == UniqueDates[i],"TG/10"]
}

rm(WeatherDF)

# include dynamic effect - partial adjustment
# first, select subset of Raak
LemonadeRaak <- subset(Lemonade_extended[Lemonade_extended$Brand == "Raak", ])
# create variable lag of unitsales
LemonadeRaak$UnitSalesLag <- c(NA,LemonadeRaak$UnitSales[1:nrow(LemonadeRaak)-1])

LemonadeRaak$UnitSalesLag[LemonadeRaak$year == "2017" & LemonadeRaak$cleaned_wk == "1"] <- NA

# add competitors price

LemonadeRaak$PricePUPL <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$PricePUSlimpie <- rep(0,nrow(LemonadeRaak))

Dates <- unique(LemonadeRaak$Date)

Chains <- unique(LemonadeRaak$Chain)

for (i in Dates) {
  for (j in Chains) {
    LemonadeRaak$PricePUPL[LemonadeRaak$Chain == j & LemonadeRaak$Date == i] <- Lemonade$PricePU[Lemonade$Brand == "PrivateLabel" & Lemonade$Chain == j & Lemonade$Date == i]
    LemonadeRaak$PricePUSlimpie[LemonadeRaak$Chain == j & LemonadeRaak$Date == i] <- Lemonade$PricePU[Lemonade$Brand == "Slimpie" & Lemonade$Chain == j & Lemonade$Date == i]
  }
}

# create price index = price per unit/base price
LemonadeRaak$PriceIndex <- LemonadeRaak$PricePU/LemonadeRaak$BasePricePU_Smooth

#Building model----
#replace na with 0 in week average covid cases to keep degree of freedom
LemonadeRaak$WeekAverage[is.na(LemonadeRaak$WeekAverage)] <- 0
#unit by unit; unpooled
R2s <- data.frame(Chains) #Make the dataframe

for (i in Chains) { #loop over chains
  MultiplicativeTemp <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                           + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                           data = LemonadeRaak[LemonadeRaak$Chain==i,])
  #This object changes for every value of i, and is not accessible outside of the loop
  message("Output for ",i,":",sep="") #Writes this sentence in red to the console window
  print(summary(MultiplicativeTemp)) #Prints the output to the screen
  R2s$R2[which(Chains==i)] <- summary(MultiplicativeTemp)$r.squared
  print(summary(aov(MultiplicativeTemp)))
  #Stores the R2-value in the right row of the R2 variable in the data frame R2s
}
#total residual=4.374+4.665+9.1+6.193+1.842+9.051+8.729 !!!

print(R2s) #the R2 values are now accessible outside of the loop

#Pooled version of the model:
PooledModel <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                  + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                  data = LemonadeRaak)
summary(PooledModel)
summary(aov(PooledModel))

#Partially pooled version of the model:
#First create dummies for the chains:
LemonadeRaak$D_AH    <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_Jumbo <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_Plus  <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_Coop  <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_Deen  <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_HV    <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$D_TOS   <- rep(0,nrow(LemonadeRaak))

LemonadeRaak$D_AH[LemonadeRaak$Chain=="Albert Heijn"]      <- 1
LemonadeRaak$D_Jumbo[LemonadeRaak$Chain=="Jumbo"]          <- 1
LemonadeRaak$D_Plus[LemonadeRaak$Chain=="Plus"]            <- 1
LemonadeRaak$D_Coop[LemonadeRaak$Chain=="Coop"]            <- 1
LemonadeRaak$D_Deen[LemonadeRaak$Chain=="Deen"]            <- 1
LemonadeRaak$D_HV[LemonadeRaak$Chain=="Hoogvliet"]         <- 1
LemonadeRaak$D_TOS[LemonadeRaak$Chain=="TotalOnlineSales"] <- 1

#Option 1: estimate model without intercept
PartiallyPooledModelTemp1 <- lm(log(UnitSales) ~ -1 + D_AH + D_Jumbo + D_Plus + D_Coop + D_Deen + D_HV + D_TOS +
                                  log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                                + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                data = LemonadeRaak)
summary(PartiallyPooledModelTemp1)

#Option 2: estimate model with intercept
PartiallyPooledModelTemp2 <- lm(log(UnitSales) ~ D_AH + D_Jumbo + D_Plus + D_Coop + D_Deen + D_HV + D_TOS +
                                  log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                                + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                data = LemonadeRaak)
summary(PartiallyPooledModelTemp2)
summary(aov(PartiallyPooledModelTemp2))

partially_pooled_sum_sq <- summary(aov(PartiallyPooledModelTemp2))[1][[1]][[2]][[17]] #69.8
pooled_sum_sq <- summary(aov(PooledModel))[1][[1]][[2]][[11]] #170.1
unit_by_unit_sum_sq <- 4.374+4.665+9.1+6.193+1.842+9.051+8.729 #43.954

# Please use a part of the data for estimation, and save a part for validation

LemonadeRaak_Calibrate <- LemonadeRaak[LemonadeRaak$Date < "2020-08-01",]
LemonadeRaak_Validate <- LemonadeRaak[LemonadeRaak$Date >= "2020-08-01",]

length(unique(LemonadeRaak_Calibrate$Date))
length(unique(LemonadeRaak_Validate$Date))

#performing the chow test----
#degree of freedom
#df_pooled = 7*207-11=1438
#df_unpooled = 7*(207-11)=1372 <---------- added up instead: 1378 = 196(AH)+196(Coop)+198(Deen)+196(Hoogvliet)+198(Jumbo)+196(Plus)+198(Online)
#df_partially_pooled = 7*207-17=1432
F_UnitbyUnit_P<- (
  (pooled_sum_sq-unit_by_unit_sum_sq)/(1438-1372))/(
    unit_by_unit_sum_sq/1372)
F_UnitbyUnit_P
#when F(66,1372), p-value = 0, fully pooled is not allowed
F_partiallyP_P <- (
  (partially_pooled_sum_sq-unit_by_unit_sum_sq)/(1432-1372))/(
    unit_by_unit_sum_sq/1372)
F_partiallyP_P
#when F(60,1372), p-value = 0 partially pooled is not allowed
#Therefore, we choose not to pool!

#performing the chow test (UPDATED - pls check it out just in case and delete this text) ----
#degree of freedom
#df_pooled = 7*207-11=1438
#df_unpooled = 7*(207-11)=1372 <---------- added up instead: 1378 = 196(AH)+196(Coop)+198(Deen)+196(Hoogvliet)+198(Jumbo)+196(Plus)+198(Online)
#df_partially_pooled = 7*207-17=1432
F_UnitbyUnit_P_update<- (
  (pooled_sum_sq-unit_by_unit_sum_sq)/(1438-1378))/(
    unit_by_unit_sum_sq/1378)
F_UnitbyUnit_P_update # = 65.89476
#when F(60,1378), p-value = 0, fully pooled is not allowed
F_partiallyP_P_update <- (
  (partially_pooled_sum_sq-unit_by_unit_sum_sq)/(1432-1378))/(
    unit_by_unit_sum_sq/1378)
F_partiallyP_P_update # = 14.99258
#when F(54,1378), p-value = 0 partially pooled is not allowed
#Therefore, we choose not to pool!

# vif test----
MultiplicativeAH <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                       + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                       data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain=="Albert Heijn",])
vif(MultiplicativeAH)
summary(MultiplicativeAH)
# log(BasePricePU_Smooth)         log(PriceIndex)     log(PricePUSlimpie)
#                1.215898                5.087799                1.130562
#          log(PricePUPL)       FeatOnly_Adjusted       DispOnly_Adjusted
#                1.116053               17.354348                1.235906
#       FeatDisp_Adjusted       log(UnitSalesLag)         log(Temp + 273)
#               11.547643                1.071903                1.095113
#             WeekAverage
#                1.154179

MultiplicativePlus <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                         + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                         data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain=="Plus",])
vif(MultiplicativePlus)
# log(BasePricePU_Smooth)         log(PriceIndex)     log(PricePUSlimpie)
#                1.341361                4.061166                1.065802
#          log(PricePUPL)       FeatOnly_Adjusted       DispOnly_Adjusted
#                1.285438                7.901394                1.671563
#       FeatDisp_Adjusted       log(UnitSalesLag)         log(Temp + 273)
#                9.516049                1.691277                1.153694
#             WeekAverage
#                1.192421

MultiplicativeCoop <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                         + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                         data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain=="Coop",])
vif(MultiplicativeCoop)
# log(BasePricePU_Smooth)         log(PriceIndex)     log(PricePUSlimpie)
#                1.261248               10.474991                1.024517
#          log(PricePUPL)       FeatOnly_Adjusted       DispOnly_Adjusted
#                1.342303                8.520145                1.207626
#       FeatDisp_Adjusted       log(UnitSalesLag)         log(Temp + 273)
#                1.873290                1.409149                1.466902
#             WeekAverage
#                1.220419

MultiplicativeHoogvliet <- lm(log(UnitSales) ~ log(BasePricePU_Smooth)+log(PriceIndex)+log(PricePUSlimpie)+log(PricePUPL)
                              + FeatOnly_Adjusted + DispOnly_Adjusted + FeatDisp_Adjusted + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                              data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain=="Hoogvliet",])
vif(MultiplicativeHoogvliet)
summary(MultiplicativeHoogvliet)
# log(BasePricePU_Smooth)         log(PriceIndex)     log(PricePUSlimpie)
#                1.691111                5.503598                1.293660
#          log(PricePUPL)       FeatOnly_Adjusted       DispOnly_Adjusted
#                1.402924                1.992625                1.173843
#       FeatDisp_Adjusted       log(UnitSalesLag)         log(Temp + 273)
#                4.401795                1.089811                1.164649
#             WeekAverage
#                1.201136



# other 3 chains (excluded Jumbo, Deen and TOS, since they are missing data wrt F, D and FD) got multi-collinearity issue as well...

# CONCLUSION: We will solve the multicollinearity for the Hoogvliet chain and continue with it because it has the best VIF values overall.

# solve multi-collinearity: Hoogvliet ----

# We recode the Feat/Disp/FeatDisp variables and Price Index into new combined variables
# which signify the presence of both price and advertising promotion based on index

LemonadeRaak_Calibrate$PriceIndex # we already have our price index variable

#CUT-OFF LOGIC: to determine cut off value we look at boxplots and histograms
# BOXPLOT (F, D and F&D)
# Conclusion: if we look at where the majority of outliers start:
# for F it is around 8, for D around 2, for F&D around 41 => the cut off value we will use is 17
boxplot(LemonadeRaak_Calibrate[c("FeatOnly_Adjusted", "DispOnly_Adjusted", "FeatDisp_Adjusted")],
        main = "Boxplots of 3 Promotion types",
        at = c(1,2,3),
        names = c("F Only", "D Only", "F&D"),
        las = 1,
        col = c("red","blue", "green"),
        border = c("red","blue", "green"),
        horizontal = FALSE,
        notch = TRUE
)

# BOXPLOT (PriceIndex)
# Conclusion: we choose 0.766
hist(LemonadeRaak_Calibrate[c("PriceIndex")],
     main="Feature Only Promotion Histogram",
     col="darkmagenta",
     freq=TRUE
)

#create cutoff variables for easy change if needed
promocutoff1 <- 17
pricecutoff1 <- 0.766

#recode price and promotion variables
# variable meaning:
# pf1: price index if there is feature only support (>17), otherwise 1
# pd1: price index if there is display only support (>17), otherwise 1
# pfd1: price index if there is feature & display support (>17), otherwise 1
# pwo1: price index if display only, feature only AND f&d support is (<=17), otherwise 1
# fwo1: feature only support, but no price cut (> 0.761), otherwise 0
# dwo1: display only support, but no price cut (> 0.761), otherwise 0
# fdwo1: f&d support, but no price cut (> 0.761), otherwise 0

#1: create and populate variables (which we replace in step 2 based on the cutoff)
LemonadeRaak_Calibrate$pf1 <- rep(1,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$pd1 <- rep(1,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$pfd1 <- rep(1,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$pwo1 <- rep(1,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$fwo1 <- rep(0,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$dwo1 <- rep(0,nrow(LemonadeRaak_Calibrate))
LemonadeRaak_Calibrate$fdwo1 <- rep(0,nrow(LemonadeRaak_Calibrate))
#2: replace based on the logic of the cutoff value
LemonadeRaak_Calibrate$pf1[LemonadeRaak_Calibrate$FeatOnly_Adjusted > promocutoff1] <- LemonadeRaak_Calibrate$PriceIndex[LemonadeRaak_Calibrate$FeatOnly_Adjusted > promocutoff1]
LemonadeRaak_Calibrate$pd1[LemonadeRaak_Calibrate$DispOnly_Adjusted > promocutoff1] <- LemonadeRaak_Calibrate$PriceIndex[LemonadeRaak_Calibrate$DispOnly_Adjusted > promocutoff1]
LemonadeRaak_Calibrate$pfd1[LemonadeRaak_Calibrate$FeatDisp_Adjusted > promocutoff1] <- LemonadeRaak_Calibrate$PriceIndex[LemonadeRaak_Calibrate$FeatDisp_Adjusted > promocutoff1]
LemonadeRaak_Calibrate$pwo1[LemonadeRaak_Calibrate$FeatOnly_Adjusted <= promocutoff1 & LemonadeRaak_Calibrate$DispOnly_Adjusted <= promocutoff1 & LemonadeRaak_Calibrate$FeatDisp_Adjusted <= promocutoff1] <- LemonadeRaak_Calibrate$PriceIndex[LemonadeRaak_Calibrate$FeatOnly_Adjusted <= promocutoff1 & LemonadeRaak_Calibrate$DispOnly_Adjusted <= promocutoff1 & LemonadeRaak_Calibrate$FeatDisp_Adjusted <= promocutoff1]

LemonadeRaak_Calibrate$fwo1[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1] <- LemonadeRaak_Calibrate$FeatOnly_Adjusted[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1]
LemonadeRaak_Calibrate$dwo1[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1] <- LemonadeRaak_Calibrate$DispOnly_Adjusted[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1]
LemonadeRaak_Calibrate$fdwo1[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1] <- LemonadeRaak_Calibrate$FeatDisp_Adjusted[LemonadeRaak_Calibrate$PriceIndex > pricecutoff1]


summary(LemonadeRaak_Calibrate[c("pf1","pd1","pfd1","pwo1","fwo1","dwo1","fdwo1")])
#     pf1              pd1              pfd1             pwo1
#  Min.   :0.5169   Min.   :0.6906   Min.   :0.5176   Min.   :0.6696
#  1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.9933
#  Median :1.0000   Median :1.0000   Median :1.0000   Median :0.9995
#  Mean   :0.9856   Mean   :0.9990   Mean   :0.9914   Mean   :0.9901
#  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000
#  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000
#       fwo1              dwo1             fdwo1
#  Min.   :  0.000   Min.   : 0.0000   Min.   : 0.0000
#  1st Qu.:  0.000   1st Qu.: 0.0000   1st Qu.: 0.0000
#  Median :  0.000   Median : 0.0000   Median : 0.0000
#  Mean   :  1.274   Mean   : 0.7898   Mean   : 0.9627
#  3rd Qu.:  0.000   3rd Qu.: 0.0000   3rd Qu.: 0.0000
#  Max.   :100.000   Max.   :89.0000   Max.   :73.9837

library(Hmisc)
rcorr(as.matrix(LemonadeRaak_Calibrate[c("pf1","pd1","pfd1","pwo1","fwo1","dwo1","fdwo1")]))
#       pf1    pd1    pfd1   pwo1   fwo1   dwo1   fdwo1
# pf1          0.0000 0.0000 0.0039 0.0000 0.4351 0.0000
# pd1   0.0000        0.0000 0.3996 0.3521 0.0000 0.2673
# pfd1  0.0000 0.0000        0.0236 0.0000 0.9514 0.0000
# pwo1  0.0039 0.3996 0.0236        0.2744 0.0010 0.1018
# fwo1  0.0000 0.3521 0.0000 0.2744        0.6104 0.0000
# dwo1  0.4351 0.0000 0.9514 0.0010 0.6104        0.1320
# fdwo1 0.0000 0.2673 0.0000 0.1018 0.0000 0.1320


# HOOGVLIET
# Recoded model: Hoogvliet (removed old promotion and pricing variables + added new variables)
MultiplicativeHoogvliet_Recoded <- lm(log(UnitSales) ~ log(PricePUSlimpie)+log(PricePUPL)
                                      +log(pf1)+log(pd1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1
                                      + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                      data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])

vif(MultiplicativeHoogvliet_Recoded) # All VIF's are less than 5!
# log(PricePUSlimpie)      log(PricePUPL)            log(pf1)            log(pd1)
#            1.185285            1.245755            4.963173            1.195184
#           log(pfd1)           log(pwo1)                fwo1                dwo1
#            4.959370            1.094106            2.250392            1.173885
#               fdwo1   log(UnitSalesLag)     log(Temp + 273)         WeekAverage
#            2.573918            1.115350            1.104925            1.159633

# Next we will build a recoded REDUCED model based on
# which variables may not have enough observations (5 observation rule of thumb)
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("pf1")] < 1) #17 observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("pd1")] < 1) #2 observations <- too few observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("pfd1")] < 1)  #16 observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("pwo1")] < 1)  #53  observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("fwo1")] > 0)  #11 observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("dwo1")] > 0) #5 observations
sum(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet",c("fdwo1")] > 0) #10 observations

# Recoded REDUCED model: Hoogvliet (removed new variables that didn't have enough observations for the chain)
MultiplicativeHoogvliet_Recoded.REDUCED <- lm(log(UnitSales) ~ log(PricePUSlimpie)+log(PricePUPL)
                                              +log(pf1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1
                                              + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                              data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])


vif(MultiplicativeHoogvliet_Recoded.REDUCED)
# log(PricePUSlimpie)      log(PricePUPL)            log(pf1)           log(pfd1)
#            1.183987            1.241649            4.932409            4.917970
#           log(pwo1)                fwo1                dwo1               fdwo1
#            1.092554            2.249794            1.113537            2.537682
#   log(UnitSalesLag)     log(Temp + 273)         WeekAverage
#            1.110788            1.085707            1.158645

# Conclusion and 2 reasons
# All VIFs are within reasonable values for marketing variables (1)
# furthermore, as we are building a predictive model it is also reasonable to not exclude pf1 and pfd1 (2)
summary(MultiplicativeHoogvliet_Recoded.REDUCED)
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         -1.099e+01  3.978e+00  -2.764 0.006331 **
# log(PricePUSlimpie)  3.279e-02  1.569e-01   0.209 0.834717
# log(PricePUPL)       6.345e-01  2.423e-01   2.618 0.009617 **
# log(pf1)            -1.898e+00  3.760e-01  -5.048 1.12e-06 ***
# log(pfd1)           -1.395e+00  3.757e-01  -3.714 0.000275 ***
# log(pwo1)            1.512e+00  1.632e+00   0.926 0.355600
# fwo1                -3.469e-04  1.788e-03  -0.194 0.846348
# dwo1                -5.102e-03  7.034e-03  -0.725 0.469231
# fdwo1                7.545e-03  1.715e-03   4.399 1.89e-05 ***
# log(UnitSalesLag)    1.621e-01  3.783e-02   4.285 3.02e-05 ***
# log(Temp + 273)      3.231e+00  7.082e-01   4.561 9.56e-06 ***
# WeekAverage          2.712e-04  9.023e-05   3.006 0.003042 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.1899 on 174 degrees of freedom
#   (1 observation deleted due to missingness)
# Multiple R-squared:  0.7755,	Adjusted R-squared:  0.7613
# F-statistic: 54.63 on 11 and 174 DF,  p-value: < 2.2e-16


# Heteroscedasticity: Hoogvliet ----
# VISUAL TEST: seems there are no Heteroscedasticity issues
df.heteroscedasticity.visual <- LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ]
is.na(df.heteroscedasticity.visual$UnitSalesLag) #NA in row 1
df.heteroscedasticity.visual <- df.heteroscedasticity.visual[-c(1),] #removed the NA in UnitSalesLag
df.heteroscedasticity.visual$residuals <- MultiplicativeHoogvliet_Recoded.REDUCED$residuals
ggplot(data = df.heteroscedasticity.visual, aes(y = residuals, x = week_yrs)) + geom_point(col = 'blue') + geom_abline(slope = 0)

# FORMAL TEST - Goldfield-Quandt: insignificant => Heteroscedasticity is not present
#https://www.statology.org/goldfeld-quandt-test-in-r/
# model: The linear regression model created by the lm() command.
# order.by: The predictor variable(s) in the model.
# data: The name of the dataset.
# fraction*: The number of central observations to remove from the dataset. Typically we choose to remove around 20% of the total observations.
gqtest(MultiplicativeHoogvliet_Recoded.REDUCED, order.by = ~log(PricePUSlimpie)+log(PricePUPL)+log(pf1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1+log(UnitSalesLag)+log(Temp+273)+WeekAverage, data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ], fraction = 36)
# GQ = 0.38192, df1 = 63, df2 = 63, p-value = 0.9999
# alternative hypothesis: variance increases from segment 1 to 2


# Autocorrelation: Hoogvliet ----
dwtest(MultiplicativeHoogvliet_Recoded.REDUCED)
# DW = 1.6873, p-value = 0.007732
# alternative hypothesis: true autocorrelation is greater than 0
# FINDINGS: We can reject the null hypothesis, therefore there is autoccrelation
# HOWEVER: the DW is 1.68 which may signify lack of autocorrelation since it's close to 2
# CONCLUSION: Further testing is needed

# We use the DW statistic (1.6873); Number of observations: n = 186; Number of Independent Vars: k' = 11
# to look at the Durbin Watson table (https://www3.nd.edu/~wevans1/econ30331/Durbin_Watson_tables.pdf)
# => dL = 1.561   dU = 1.791 => This puts 1.6873 in the grey zone. The autocorrelation is not significant.

# To further test this we employ ACF
#(https://www.codingprof.com/3-easy-ways-to-test-for-autocorrelation-in-r-examples/)
library(stats)
acf(MultiplicativeHoogvliet_Recoded.REDUCED$residuals, type='correlation')
# The plot also shows no autocorrelation

# Finally, we tested with Breusch-Godfrey
bgtest(MultiplicativeHoogvliet_Recoded.REDUCED,order = 3)
# 	Breusch-Godfrey test for serial correlation of order up to 3
#
# data:  MultiplicativeHoogvliet_Recoded.REDUCED
# LM test = 6.7218, df = 3, p-value = 0.08131
# Conclusion: We cannot reject the null hypothesis => there is not autocorrelation


# Nonnormality: Hoogvliet ----

# (1) VISUAL TESTING:

# HISTOGRAM+curve: we look at the distribution, which looks normal
hist(MultiplicativeHoogvliet_Recoded.REDUCED$residuals,probability = TRUE)
curve(dnorm(x, mean=mean(MultiplicativeHoogvliet_Recoded.REDUCED$residuals), sd=sd(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)), add=TRUE, col="red")

# QQ-Plot: we are now looking if the tails, they seem to deviate on both ends
# => this may indicate non-normality, therefore we will perform formal tests.
res_std <- rstandard(MultiplicativeHoogvliet_Recoded.REDUCED)
qqnorm(res_std,ylab="Standardized Residuals",xlab="Normal Scores")
qqline(res_std,col="red")

# (2) FORMAL TESTING:
# each test differs in how it checks to see if your distribution matches the normal. For example, the KS test looks at the quantile where your empirical cumulative distribution function differs maximally from the normal's theoretical cumulative distribution function. This is often somewhere in the middle of the distribution, which isn't where we typically care about mismatches. The SW test focuses on the tails, which is where we typically do care if the distributions are similar. As a result, the SW is usually preferred.
# so, there's no normality issue in our case, as Shapiro-Wilk normality test gives a p-value of 0.02329.

#SHAPIRO-WILK test: significant => nonnormality
shapiro.test(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)
# W = 0.9015, p-value = 8.925e-10

#LILLIE/KOLMOGOROV-SMIRNOV test: significant => nonnormality
library(nortest)
lillie.test(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)
# D = 0.12929, p-value = 4.963e-08

#JARQUE-BERA test: significant => nonnormality
library(fBasics)
jarqueberaTest(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)
#   STATISTIC:
#     X-squared: 360.6834
#   P VALUE:
#     Asymptotic p Value: < 2.2e-16

# CONCLUSION: We can conclude there is nonnormality in the distribution of the residuals.


# SOLUTION of Nonnormality: Bootstrapping
# Note: this bootstrapping generates slightly different p-values each time
set.seed(444) #does not affect bootstrapping p-values
library(boot)
lmbootstrap <- function(formula,data){
  boot.run <- function(data, indices){
    data <- data[indices,] # select obs. in bootstrap sample
    mod <- lm(formula, data=data)
    coefficients(mod) # return coefficient vector
  }
  boot_aux <- boot(data,boot.run, 1999)
  Coefficient <- names(boot_aux$t0)
  boot_out <- data.frame(Coefficient)
  for (i in 1:length(boot_aux$t0)) {
    boot_out$Estimate[i] <- boot_aux$t0[i]
    boot_out$Std.Error[i] <- sd(boot_aux$t[,i])
    boot_out$Bias[i] <- mean(boot_aux$t[,i])-boot_aux$t0[i]
    boot_out$p_bootstrap[i] <- dt(mean(boot_aux$t[,i])/sd(boot_aux$t[,i]),boot_aux$R-dim(boot_aux$t)[2])
  }
  return(boot_out)
}

MultiplicativeHoogvliet_Recoded.REDUCED_BOOT <- lmbootstrap(log(UnitSales) ~ log(PricePUSlimpie)+log(PricePUPL)
                                                            +log(pf1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1
                                                            + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                                            data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])

#            Coefficient      Estimate    Std.Error          Bias    p_bootstrap       original p-value
# 1          (Intercept) -1.099416e+01 3.811649e+00  6.284694e-01   9.933567e-03 **       0.006331 **
# 2  log(PricePUSlimpie)  3.278785e-02 1.090610e-01 -5.612982e-03   3.866936e-01          0.834717
# 3       log(PricePUPL)  6.344801e-01 2.185924e-01 -2.578733e-02   8.308538e-03 **       0.009617 **
# 4             log(pf1) -1.897935e+00 8.007942e-01 -2.465592e-01   1.110742e-02 *        1.12e-06 ***
# 5            log(pfd1) -1.395303e+00           NA            NA             NA          0.000275 ***  <- ? question for teacher below
# 6            log(pwo1)  1.511665e+00 9.921345e-01 -2.933107e-01   1.876519e-01          0.355600
# 7                 fwo1 -3.469044e-04 1.269894e-02 -3.816791e-03   3.780073e-01          0.846348
# 8                 dwo1 -5.101938e-03           NA            NA             NA          0.469231      <- ? question for teacher below
# 9                fdwo1  7.545001e-03 1.064314e-02  2.451891e-03   2.565808e-01          1.89e-05 ***
# 10   log(UnitSalesLag)  1.620716e-01 6.730316e-02  1.459285e-02   1.277964e-02 *        3.02e-05 ***
# 11     log(Temp + 273)  3.230611e+00 7.059730e-01 -1.329554e-01   2.743028e-05 ***      9.56e-06 ***
# 12         WeekAverage  2.711903e-04 8.834144e-05 -1.487158e-05   5.967026e-03 **       0.003042 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ORIGINAL MODEL (BEFORE BOOTSTRAPPTING)
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         -1.099e+01  3.978e+00  -2.764 0.006331 **
# log(PricePUSlimpie)  3.279e-02  1.569e-01   0.209 0.834717
# log(PricePUPL)       6.345e-01  2.423e-01   2.618 0.009617 **
# log(pf1)            -1.898e+00  3.760e-01  -5.048 1.12e-06 ***
# log(pfd1)           -1.395e+00  3.757e-01  -3.714 0.000275 ***
# log(pwo1)            1.512e+00  1.632e+00   0.926 0.355600
# fwo1                -3.469e-04  1.788e-03  -0.194 0.846348
# dwo1                -5.102e-03  7.034e-03  -0.725 0.469231
# fdwo1                7.545e-03  1.715e-03   4.399 1.89e-05 ***
# log(UnitSalesLag)    1.621e-01  3.783e-02   4.285 3.02e-05 ***
# log(Temp + 273)      3.231e+00  7.082e-01   4.561 9.56e-06 ***
# WeekAverage          2.712e-04  9.023e-05   3.006 0.003042 **




# <--- QUESTIONS FOR TUTORIAL --->

# (0) Is it ok to leave the variables in the REDUCED model of Hoogvliet that have VIF ~4.9? Is the reasoning ok?

# (1) Is it REALLY nonnormal? I read these tests are way too strict and the first plot seems fine.
# However, the QQ does show the two tails like in the summary (page 111)

# (2) Is it okay that bootstrapping generates slightly different p-values each time?

# (3) Why does our bootstrapped model have NAs while the non-bootstrapped doesn't?

# Statistical Validity Testing ----
## Information criteria ----
#Calculate AIC

extractAIC(MultiplicativeHoogvliet_Recoded.REDUCED)
# [1]   12.0000 -606.3786

#Check calculations
T <- length(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)            #Number of observations
K <- length(MultiplicativeHoogvliet_Recoded.REDUCED$coefficients)-1       #Number of regressors 
RSS_MultiplicativeHoogvliet_Recoded.REDUCED <- anova(MultiplicativeHoogvliet_Recoded.REDUCED)$`Sum Sq`[K+1]

#Calculate AIC
AIC_check <- T*log(RSS_MultiplicativeHoogvliet_Recoded.REDUCED/T)+2*(K+1) #Correct!!!
AIC_check # AIC: -606.3786
#Calculate BIC
BIC <- T*log(RSS_MultiplicativeHoogvliet_Recoded.REDUCED/T)+log(T)*(K+1)
BIC # BIC:  -567.6696

# Robust estimation:
plot(MultiplicativeHoogvliet_Recoded.REDUCED$residuals)
plot(rstudent(MultiplicativeHoogvliet_Recoded.REDUCED))
# There is no discernible pattern! GOOD.

# Assessing Outliers
outlierTest(MultiplicativeHoogvliet_Recoded.REDUCED) # Bonferonni p-value for most extreme obs
# rstudent unadjusted p-value Bonferroni p
# 4434 -6.290424         2.5077e-09   4.6643e-07
# 4440  5.847877         2.4289e-08   4.5178e-06
# Points 4440 and 4434 are outliers
qqPlot(MultiplicativeHoogvliet_Recoded.REDUCED, main="QQ Plot") #qq plot for studentized resid
# 4434 4440 
# 66   72

library(sur)

h <- leverage(MultiplicativeHoogvliet_Recoded.REDUCED) # leverage values 

leveragePlots(MultiplicativeHoogvliet_Recoded.REDUCED) # leverage plots 

library(car)

# Influential Observations
# added variable plots
avPlots(MultiplicativeHoogvliet_Recoded.REDUCED)
# Cook's D plot
# identify D values > 4/(T-K-1)
cutoff <- 4/((nrow(LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])-length(MultiplicativeHoogvliet_Recoded.REDUCED$coefficients)-2))
plot(MultiplicativeHoogvliet_Recoded.REDUCED, which=4, cook.levels=cutoff)
## point 4528 has a Cook's above 1, this is a high influence point

# Influence Plot
influencePlot(MultiplicativeHoogvliet_Recoded.REDUCED, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
## point 4440, 4434, 4483, 4528 are high leverage outliers
#        StudRes       Hat     CookD
# 4434 -6.290424 0.2061735 0.7010263
# 4440  5.847877 0.2232314 0.6877700
# 4483  3.039328 0.5631039 0.9473182
# 4528  1.938606 0.8691999 2.0487040

# We can see the following:
# Observation 4528 is a high-leverage point (but no outlier) and has the highest influence of all on the regression.
# Observations 4440, 4483, 4434 are a high-leverage OUTLIER point with moderately high influence on the regression

cooksd <- cooks.distance(MultiplicativeHoogvliet_Recoded.REDUCED)

library(robustbase)

MultiplicativeHoogvliet_Recoded.REDUCEDRobust <- lmrob(log(UnitSales) ~ log(PricePUSlimpie)+log(PricePUPL)
                                            +log(pf1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1
                                            + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                            data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])
summary(MultiplicativeHoogvliet_Recoded.REDUCEDRobust)
# Coefficients:
#                        Estimate   Std. Error t value Pr(>|t|)    
#   (Intercept)         -4.395e+00  2.257e+00  -1.948  0.05307 .  
#   log(PricePUSlimpie) -4.601e-02  7.975e-02  -0.577  0.56476    
#   log(PricePUPL)       3.762e-01  1.247e-01   3.016  0.00294 ** 
#   log(pf1)            -3.217e+00  1.415e-01 -22.740  < 2e-16 ***
#   log(pfd1)            2.947e-01  1.546e-01   1.906  0.05831 .  
#   log(pwo1)            5.783e-01  7.383e-01   0.783  0.43448    
#   fwo1                 5.198e-04  5.790e-04   0.898  0.37054    
#   dwo1                 1.435e-02  4.743e-03   3.026  0.00286 ** 
#   fdwo1                8.310e-03  1.564e-03   5.314 3.25e-07 ***
#   log(UnitSalesLag)    2.497e-01  2.380e-02  10.492  < 2e-16 ***
#   log(Temp + 273)      1.948e+00  4.030e-01   4.833 2.94e-06 ***
#   WeekAverage          1.220e-04  4.969e-05   2.456  0.01502 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#   Robust residual standard error: 0.09744 
#   Multiple R-squared:  0.9028,	Adjusted R-squared:  0.8967 
#   Convergence in 22 IRWLS iterations

NullModel <-  lmrob(log(UnitSales)~1,data=LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])

anova(NullModel,MultiplicativeHoogvliet_Recoded.REDUCEDRobust,test="Deviance")

# Predictive Validity Testing ----

#1: create and populate variables to the validation sample (which we replace in step 2 based on the cutoff)
LemonadeRaak$pf1 <- rep(1,nrow(LemonadeRaak))
LemonadeRaak$pd1 <- rep(1,nrow(LemonadeRaak))
LemonadeRaak$pfd1 <- rep(1,nrow(LemonadeRaak))
LemonadeRaak$pwo1 <- rep(1,nrow(LemonadeRaak))
LemonadeRaak$fwo1 <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$dwo1 <- rep(0,nrow(LemonadeRaak))
LemonadeRaak$fdwo1 <- rep(0,nrow(LemonadeRaak))
#2: replace based on the logic of the cutoff value
LemonadeRaak$pf1[LemonadeRaak$FeatOnly_Adjusted > promocutoff1] <- LemonadeRaak$PriceIndex[LemonadeRaak$FeatOnly_Adjusted > promocutoff1]
LemonadeRaak$pd1[LemonadeRaak$DispOnly_Adjusted > promocutoff1] <- LemonadeRaak$PriceIndex[LemonadeRaak$DispOnly_Adjusted > promocutoff1]
LemonadeRaak$pfd1[LemonadeRaak$FeatDisp_Adjusted > promocutoff1] <- LemonadeRaak$PriceIndex[LemonadeRaak$FeatDisp_Adjusted > promocutoff1]
LemonadeRaak$pwo1[LemonadeRaak$FeatOnly_Adjusted <= promocutoff1 & LemonadeRaak$DispOnly_Adjusted <= promocutoff1 & LemonadeRaak$FeatDisp_Adjusted <= promocutoff1] <- LemonadeRaak$PriceIndex[LemonadeRaak$FeatOnly_Adjusted <= promocutoff1 & LemonadeRaak$DispOnly_Adjusted <= promocutoff1 & LemonadeRaak$FeatDisp_Adjusted <= promocutoff1]

LemonadeRaak$fwo1[LemonadeRaak$PriceIndex > pricecutoff1] <- LemonadeRaak$FeatOnly_Adjusted[LemonadeRaak$PriceIndex > pricecutoff1]
LemonadeRaak$dwo1[LemonadeRaak$PriceIndex > pricecutoff1] <- LemonadeRaak$DispOnly_Adjusted[LemonadeRaak$PriceIndex > pricecutoff1]
LemonadeRaak$fdwo1[LemonadeRaak$PriceIndex > pricecutoff1] <- LemonadeRaak$FeatDisp_Adjusted[LemonadeRaak$PriceIndex > pricecutoff1]
#3:
LemonadeRaak_Validate <- LemonadeRaak[LemonadeRaak$Date >= "2020-08-01",]
MultiplicativeHoogvliet_Recoded.REDUCEDPredictions <- predict(MultiplicativeHoogvliet_Recoded.REDUCED,newdata = LemonadeRaak_Validate[LemonadeRaak_Validate$Chain == "Hoogvliet", ])

# quick look at RMSE (predictive errors) by using packages
library(caret)
library(glmnet)

real_value<- LemonadeRaak_Validate[LemonadeRaak_Validate$Chain == "Hoogvliet", ]$UnitSales
MultiplicativeHoogvliet_Recoded.REDUCEDPredictions <- exp(1)^MultiplicativeHoogvliet_Recoded.REDUCEDPredictions
rmse <- RMSE(MultiplicativeHoogvliet_Recoded.REDUCEDPredictions, real_value)
# RMSE: 119348.2

# predictive graphs

plot(1:208,LemonadeRaak[LemonadeRaak$Chain == "Hoogvliet",]$UnitSales,col="black",bg="black",pch=21,xlab = "Weeks",ylab="Sales of Raak at Hoogvliet",ylim = c(0,60000),main="Predictive validity for Raak Model at Hoogvliet")
lines(1:208,LemonadeRaak[LemonadeRaak$Chain == "Hoogvliet",]$UnitSales)
# MultiplicativeHoogvliet_Recoded.REDUCEDPredictions <- exp(1)^MultiplicativeHoogvliet_Recoded.REDUCEDPredictions

lines(188:208,MultiplicativeHoogvliet_Recoded.REDUCEDPredictions,pch=21,col="red",bg="red",type="o",lty=2)
sd_residuals <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$sigma
RaakHoogvlietSalesFit <- exp(MultiplicativeHoogvliet_Recoded.REDUCED$fitted.values)*exp(1/2*sd_residuals^2)

lines(2:187,RaakHoogvlietSalesFit,pch=21,col="blue",bg="blue",type="o",lty=2)
lines(c(187.5,187.5),c(0,60000),col="black",lty=2)
text(130,50000,pos=2,"Estimation sample",cex = .9)
text(187.5,50000,pos=4,"Validation sample",cex = .9)

legend(-5, 60000, c("Observed values","Fitted values","Predicted values"), col=c("black","blue","red"), pt.bg=c("black","blue","red"), pch=c(21,21,21), lty=c(1,2,2),cex = 0.6)

APE <- sum(real_value-MultiplicativeHoogvliet_Recoded.REDUCEDPredictions)/(21)
# -64213.43 the predictions are larger than the actual values.
ASPE <- sum((real_value-MultiplicativeHoogvliet_Recoded.REDUCEDPredictions)^2)/(21)
# 14243994247 after weighted larger errors.
RASPE <- sqrt(ASPE)
# 119348.2 also known as RMSE, the lower the better, it seems good. 
MAPE <- (1/21)*sum(abs( (real_value-MultiplicativeHoogvliet_Recoded.REDUCEDPredictions) /
                         (real_value) ) )
# 4.857552 MAPE values range from 0 to infinity, where the lower the value the more accurate the predictions are.
RAE <- sum(abs(real_value-MultiplicativeHoogvliet_Recoded.REDUCEDPredictions)) / 
  sum(abs(real_value-LemonadeRaak[LemonadeRaak$Chain == "Hoogvliet",]$UnitSales[187:207]))
# 7.828423 for the validation sample, model didnt outperform naive model.
TheilsU <- sqrt( sum((real_value-MultiplicativeHoogvliet_Recoded.REDUCEDPredictions)^2) / 
                   sum((real_value-LemonadeRaak[LemonadeRaak$Chain == "Hoogvliet",]$UnitSales[187:207])^2))
# 8.692934 for the validation sample, model didnt outperform naive model.

# anti log of model's coefficients----

MultiplicativeHoogvliet_Recoded.REDUCED <- lm(log(UnitSales) ~ log(PricePUSlimpie)+log(PricePUPL)
                                              +log(pf1)+log(pfd1)+log(pwo1)+fwo1+dwo1+fdwo1
                                              + log(UnitSalesLag) + log(Temp+273) + WeekAverage,
                                              data = LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ])
summary(MultiplicativeHoogvliet_Recoded.REDUCED)

## Apply the anti-log transformation to alpha_hat_star ----
alpha_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[1,1]
sd_alpha_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[1,2]
alpha_hat <- exp(alpha_hat_star) * exp(-0.5*(sd_alpha_hat_star^2))
sprintf("alpha_hat = %.20f", alpha_hat )

## For beta_log(PricePUSlimpie), the anti-log transformation is not needed! ----
sprintf("beta_log(PricePUSlimpie) = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[2])

## For beta_log(PricePUPL), the anti-log transformation is not needed! ----
sprintf("beta_log(PricePUPL) = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[3])

## For beta_log(pf1) , the anti-log transformation is not needed! ----
sprintf("beta_log(pf1)  = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[4])

## For beta_log(pfd1), the anti-log transformation is not needed! ----
sprintf("beta_log(pfd1) = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[5])

## For beta_log(pwo1), the anti-log transformation is not needed! ----
sprintf("beta_log(pwo1) = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[6])

## Apply the anti-log transformation to beta_fwo1 ----
beta2_hat_star <- MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[7]
sd_beta2_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[7,2]
beta2_hat <- exp(beta2_hat_star) * exp(-0.5*(sd_beta2_hat_star^2))
sprintf("beta_fwo1 = %.2f", beta2_hat )

## Apply the anti-log transformation to beta_dwo1 ----
beta3_hat_star <- MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[8]
sd_beta3_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[8,2]
beta3_hat <- exp(beta3_hat_star) * exp(-0.5*(sd_beta3_hat_star^2))
sprintf("beta_dwo1 = %.2f", beta3_hat )

## Apply the anti-log transformation to beta_fdwo1 ----
beta4_hat_star <- MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[9]
sd_beta4_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[9,2]
beta4_hat <- exp(beta4_hat_star) * exp(-0.5*(sd_beta4_hat_star^2))
sprintf("beta_fdwo1 = %.2f", beta4_hat )

## For beta_log(UnitSalesLag) , the anti-log transformation is not needed! ----
sprintf("beta_log(UnitSalesLag)  = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[10])

## For beta_log(Temp + 273), the anti-log transformation is not needed! ----
sprintf("beta_log(Temp + 273) = %.2f", MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[11])

## Apply the anti-log transformation to beta_WeekAverage ----
beta5_hat_star <- MultiplicativeHoogvliet_Recoded.REDUCED$coefficients[12]
sd_beta5_hat_star <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$coefficients[12,2]
beta5_hat <- exp(beta5_hat_star) * exp(-0.5*(sd_beta5_hat_star^2))
sprintf("beta_WeekAverage = %.20f", beta5_hat )

## Obtain the standard deviation of the residuals ----
sd_residuals <- summary(MultiplicativeHoogvliet_Recoded.REDUCED)$sigma

## Calculate the fitted values ----
RaakHoogvlietSalesFit <- exp(MultiplicativeHoogvliet_Recoded.REDUCED$fitted.values)*exp(1/2*sd_residuals^2)
new_data <- LemonadeRaak_Calibrate[LemonadeRaak_Calibrate$Chain == "Hoogvliet", ]
new_data <- new_data[order(new_data$Date),]
view(new_data)

## Make the comparison plot ----
plot(new_data$Date[2:187],new_data$UnitSales[2:187],type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Raak at Hoogvliet (units)",main = "Comparing actual and fitted sales of Raak at Hoogvliet")
lines(new_data$Date[2:187],new_data$UnitSales[2:187],lwd=2,col="dodgerblue")
lines(new_data$Date[2:187],RaakHoogvlietSalesFit,col="red",lty=2,lwd=2)
legend("topleft",inset = c(.75,0.02), c("Actual sales","Fitted sales"), cex=0.8, col=c("dodgerblue","red"), pch=c(21,NA),lty = 1:2,lwd=2)