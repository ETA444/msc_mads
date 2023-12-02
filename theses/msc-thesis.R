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

# [ REDACTED ]: Limited access until further notice. #
