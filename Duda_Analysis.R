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
cancer.df <- read.csv("cancer.csv")
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
#
# Now we have displayed our chosen key variables, how they relate to each other
# (no transformation applied). We will now dig deeper into the variables we are
# mainly going to use to answer our research question. Those will be our primary
# key variables:
#
#  deathRate | Our output variable
#  avgAnnCount | 2009-2013 mean incidences per county
#  PctPrivateCoverage | Percentage of the population with private insurance coverage
#  PctPublicCoverage | Percentage of the population with public insurance coverage
#  PctEmpPrivCoverage | Percentage of the population with private insurance coverage through employment
#
# The Death Rate is going to be our outcome variable, (we want to see how health
# insurance coverage realtes to it). So let us start by digging deeper into it:
summary(cancer.df$deathRate)
# The whole range seems to be useful and with meaningful information, since it is
# ranging from 59.7 and 362.8 deaths per 100k people.
# 
# Our next step now is to take a look at a histogram, to check how it is 
# distributed:
hist(cancer.df$deathRate,breaks = 50, main = "Histogram of Cancer Death Rates", xlab= "Cancer Death Rates", ylab = "Frequency", col = "lightyellow4")
# The distribution is quite close to normal, but we should now dig deeper into
# the outliers (below 90 and above 300):
#
# Below 90:
cancer.df[cancer.df$deathRate < 90,]
#
# Observation #1942 catches the eye, because it has a much higher poverty percentage
# than the other 3 lower outliers, and also because we have seen in the scatterplot
# matrix that poverty is positively correlated to death rates. However, this might
# be due to the very low number of incidences: less people with cancer naturally 
# leads to a smaller percentage of deaths caused by cancer.
#
# Above 90:
cancer.df[cancer.df$deathRate > 300,]
# There is only one record for the maximum value, and the other variables in this
# observation does not seem to stand out in any other way.
#
# Now we will analyze another primary key variable: the mean number of cancer 
# incidences per county between 2009 and 2013:
summary(cancer.df$avgAnnCount)
# The maximum value catchwa our attention, mainly because it is in much higher
# order of magnitude. Let's check the histogram for more enlightment:
hist(cancer.df$avgAnnCount, xlab = "Mean Number of Incidences per County (2009-2013)", ylab="Frequency", main = "Histogram of Mean Cancer Incidences")
#
# By just analuzing the histogram, we see that its distribution is extremely right 
# skewed. Let's check its density plot:
densityPlot(cancer.df$avgAnnCount)
# That extremily right-skewed