##################################################################
#Program Name: Duda_Analysis.R
#Analyst:      Eduarda Espindola
#
##################################################################

##################################################################
# Introduction
#
# We have the following research question:
#
# Given the data on cancer incidences for several counties, how is 
# the health insurance coverage related to cancer mortatility
# rates?
#
# First of all, we have to load our data:
cancer.df <- read.csv("Documents/Mestrado/W203 - Stats/Lab1/W203Lab1/cancer.csv")
#
# Now let us have a peek on this data:
head(cancer.df,10)
#
# We need to see if our data makes sense. Using the summary function will help us
# do some sanity checks in our data:
summary(cancer.df)
#
# We can already point out some inconsistencies just by checking the summary for
# the variables. The maximum median age per county seems to be 624 years old, 
# which is certainly a mistake: most likely a wrong placement of the dot
# separating the decimals. Besides that, we notice that we have a lot of NA's
# regarding PctSomeCol18_24 (2285 of 3047 observations). Given that, we are going
# to proceed with our analysis, excluding the datapoints where MedianAge is
# above 100 years old and we are not using the variables X (just an index for 
# each datapoint) and PctSomeCol18_24, due to its large incidence of NA's.
#
# We can now filter out the rows where we have a MedianAge above 100:
cancer.df<-subset(cancer.df, MedianAge<=100)
cancer.df$X<-NULL
cancer.df$PctSomeCol18_24<-NULL
#
# Now we must analyze some key variables. We have chosen to focus on these:
#  Variable Name  |  Description 
# --------- | ---------------
#  deathRate | Our output variable
#  avgAnnCount | 2009-2013 mean incidences per county
#  popEst2015 | Estimated population by county 2015
#  PctPrivateCoverage | Percentage of the population with private insurance coverage
#  PctPublicCoverage | Percentage of the population with public insurance coverage
#  PctEmpPrivCoverage | Percentage of the population with private insurance coverage through employment
#  povertyPercent | Percent of population below poverty line
#  MedianAge | Median population age
#  medIncome | Median Income
#
# In order to display a nice scatterplot matrix for our chosen key variables, we
# will use the car library:
library(car)
scatterplotMatrix(~ deathRate + avgAnnCount + popEst2015 + PctPrivateCoverage + PctPublicCoverage + PctEmpPrivCoverage + povertyPercent + MedianAge + medIncome, data = cancer.df)
