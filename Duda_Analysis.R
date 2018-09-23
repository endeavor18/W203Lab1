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
hist(cancer.df$avgAnnCount, xlab = "Mean Number of Incidences per County (2009-2013)", ylab="Frequency", main = "Histogram of Mean Cancer Incidences", col= "deepskyblue4")
#
# By just analuzing the histogram, we see that its distribution is extremely right 
# skewed. Let's check its density plot:
densityPlot(cancer.df$avgAnnCount,xlab = "Mean Number of Incidences per County (2009-2013)", ylab="Density", main = "Density Plot of Mean Cancer Incidences", col="cadetblue4")
# That extremely right-skewed distribution is an indicative that we could use
# a log() transformation in this variable. 
#
summary(log(cancer.df$avgAnnCount))
# It appears it was a good idea to do that. So we will transform log(avgAnnCount) 
# into a new variable in our dataset
cancer.df$logavgAnnCount=log(cancer.df$avgAnnCount)
# Just confirming if everything went well:
summary(cancer.df$logavgAnnCount)
#
# Checking the distribution by plotting a histogram:
#
hist(cancer.df$logavgAnnCount, main = "Histogram of log(avgAnnCount)", ylab= "frequency", xlab="log(avgAnnCount)", col = "aquamarine3")
#
# That second smaller peak is something worth investigating:
cancer.df[cancer.df$logavgAnnCount > 7.25 & cancer.df$logavgAnnCount < 8 , ]
# By looking at the data, something that stands out are avgAnnCount with the value
# 1962.668, while all other values appear to be integers. It seems rather odd
# that such an specific value would be the same in 205 different counties. The other
# variables (except the derived one), seem to be fine. So we are setting both
# avgAnnCount and logavgAnnCount to NA when avgAnnCount is 1962.668
cancer.df[cancer.df$avgAnnCount==1962.667684, ]
cancer.df$avgAnnCount[cancer.df$avgAnnCount==1962.667684] = NA
cancer.df$logavgAnnCount[cancer.df$logavgAnnCount==log(1962.667684)] = NA
# Now checking again the charts and the summaries:
summary(cancer.df$avgAnnCount)
hist(cancer.df$avgAnnCount, xlab = "Mean Number of Incidences per County (2009-2013)", ylab="Frequency", main = "Corrected Histogram of Mean Cancer Incidences", col= "deepskyblue4")
densityPlot(cancer.df$avgAnnCount,xlab = "Mean Number of Incidences per County (2009-2013)", ylab="Density", main = "Corrected Density Plot of Mean Cancer Incidences", col="cadetblue4")
summary(cancer.df$logavgAnnCount)
hist(cancer.df$logavgAnnCount, main = "Corrected Histogram of log(avgAnnCount)", ylab= "frequency", xlab="log(avgAnnCount)", col = "aquamarine3")
# The distribution of log(avgAnnCount) is fairly close to normal. Now we will
# analyze the PctPrivateCoverage, the percentage of the population with private 
# insurance coverage.
summary(cancer.df$PctPrivateCoverage)
# At first glance, the values for these variables seem to make sense. Let's
# check its histogram:
hist(cancer.df$PctPrivateCoverage, xlab="Percent of population with private health insurance coverage", ylab= "Frequency", main = "Histogram of Percentage of Private Coverage", col = "paleturquoise")
# It is a bit of a left-skewed distribution, but other than that, very close
# to a normal distribution
# Now we will analyze another type of health insurance: the private provided by
# employer.
summary(cancer.df$PctEmpPrivCoverage)
# Histogram:
hist(cancer.df$PctEmpPrivCoverage, xlab = "Percentage of the population with private health insurance coverage through employment", ylab = "Frequency", main = "Histogram of Percentage of Private Coverage by Employer", col = "paleturquoise")
# A very normal-like distribution
# Now, for the last type of coverage, the public:
summary(cancer.df$PctPublicCoverage)
# Histogram:
hist(cancer.df$PctPublicCoverage, xlab = "Percentage of the population with public health insurance coverage", ylab = "Frequency", main = "Histogram of Percentage of Public Coverage", col = "paleturquoise")
# Also seems quite a normal distribution.
# Our research question aims to understand the relation between Cancer Death Rates
# and the health insurance coverage. We begin by analyzing the scatterplots.
plot(cancer.df$PctPrivateCoverage,cancer.df$deathRate, xlab = "Percentage of population with private health insurance coverage", ylab="Death Rate", main= "Private Coverage vs Death Rate")
# Since we want to check the relation between those two variables, adding a
# linear regression line to the plot is an easy way to summarize the correlation
# between these two variables:
abline(lm(cancer.df$deathRate ~ cancer.df$PctPrivateCoverage))
# We can visually notice that there is a negative correlation between private
# coverage and death rates. We can make it more explicit by actually calculating
# the correlation between those two variables.
cor(x=cancer.df$PctPrivateCoverage,y=cancer.df$deathRate)
# It confirms that we do have a moderate negative correlation between these two 
# variables. Correlation is not causation, but it seems that counties that have
# higher private health insurance coverage have smaller cancer death rates.
# We can gain further insight by analyzing a boxplot. But not before binning the 
# private coverage variable.
cancer.df$PctPrivateCoverageCat<-cut(cancer.df$PctPrivateCoverage, seq(0,100,10), right=FALSE)
boxplot(deathRate ~ PctPrivateCoverageCat, data= cancer.df, main = "Death Rate by Private Coverage", xlab="Private Coverage", ylab="Death Rate")
#
#
# Now the Employer Private Coverage
#
plot(cancer.df$PctEmpPrivCoverage,cancer.df$deathRate, xlab = "Percentage of population with private health insurance by employment coverage", ylab="Death Rate", main= "Private Coverage by Employment vs Death Rate")
# Since we want to check the relation between those two variables, adding a
# linear regression line to the plot is an easy way to summarize the correlation
# between these two variables:
abline(lm(cancer.df$deathRate[!is.na(cancer.df$PctEmpPrivCoverage)] ~ cancer.df$PctPrivateCoverage[!is.na(cancer.df$PctEmpPrivCoverage)]))
# We can visually notice that there is a negative correlation between private
# coverage by employment and death rates. We can make it more explicit by actually
# calculating the correlation between those two variables.
cor(x=cancer.df$PctEmpPrivCoverage,y=cancer.df$deathRate, use = "complete.obs")
# It confirms that we do have a moderate negative correlation between these two 
# variables. Correlation is not causation, but it seems that counties that have
# higher private health insurance by employment coverage have smaller cancer death
#rates.
# We can gain further insight by analyzing a boxplot. But not before binning the 
# private coverage variable.
cancer.df$PctEmpPrivCoverageCat<-cut(cancer.df$PctEmpPrivCoverage, seq(0,100,10), right=FALSE)
boxplot(deathRate ~ PctEmpPrivCoverageCat, data= cancer.df, main = "Death Rate by Private Coverage by Employment", xlab="Private Coverage by Employment", ylab="Death Rate")
#
# Public Coverage
#
plot(cancer.df$PctPublicCoverage,cancer.df$deathRate, xlab = "Percentage of population with public health insurance coverage", ylab="Death Rate", main= "Public Coverage vs Death Rate")
# Since we want to check the relation between those two variables, adding a
# linear regression line to the plot is an easy way to summarize the correlation
# between these two variables:
abline(lm(cancer.df$deathRate ~ cancer.df$PctPublicCoverage))
# We can visually notice that there is a positive correlation between public
# coverage and death rates. We can make it more explicit by actually calculating
# the correlation between those two variables.
cor(x=cancer.df$PctPublicCoverage,y=cancer.df$deathRate)
# It confirms that we do have a moderate positive correlation between these two 
# variables. Correlation is not causation, but it seems that counties that have
# higher public health insurance coverage have higher cancer death rates.
# We can gain further insight by analyzing a boxplot. But not before binning the 
# private coverage variable.
cancer.df$PctPublicCoverageCat<-cut(cancer.df$PctPublicCoverage, seq(0,100,10), right=FALSE)
boxplot(deathRate ~ PctPublicCoverageCat, data= cancer.df, main = "Death Rate by Public Coverage", xlab="Public Coverage", ylab="Death Rate")
# We are now able to develop some hypothesis.
# 1. Private Health Insurance provide a wider range of procedures and quicker
# diagnosys and treatment, key factors for cancer survival
# 2. Employer Private Health Insurance also do that, but maybe not as efficiently 
# as a Private Insurance
# 3. Public Health Insurance may take a longer time to provide the necessary 
# diagnosys and treatment, and the stage in which cancer is found and treated
# is of fundamental importance for higher survival rates
# 
# However, other factors might be influencing both the coverage by type of health
# insurance and the death rates, so it is a good idea to dig deeper into what might
# be affecting health insurance coverage by type. Let's see if we can find any
# relation between coverage of each type of health insurance and the number of
# incidences of cancer.
#
plot(cancer.df$PctPrivateCoverage,cancer.df$logavgAnnCount, xlab = "Percentage of population with private health insurance coverage", ylab="Average Annual Incidences", main= "Annual Incidences by Private Coverage")
# Let's include a linear regression line to have a clearer view:
abline(lm(cancer.df$logavgAnnCount ~ cancer.df$PctPrivateCoverage))
# Check correlation
cor(x=cancer.df$PctPrivateCoverage, y=cancer.df$logavgAnnCount, use="complete.obs")
# There is no important correlation between the Average Annual Incidences of Cancer
# and the Private Health Insurance Coverage
#
plot(cancer.df$PctEmpPrivCoverage,cancer.df$avgAnnCount, xlab = "Percentage of population with private health insurance by employment coverage", ylab="Average Annual Incidences", main= "Annual Incidences by Private Coverage by Employment")
# Let's include a linear regression line to have a clearer view:
abline(lm(cancer.df$avgAnnCount ~ cancer.df$PctEmpPrivCoverage))
# Check correlation
cor(x=cancer.df$PctEmpPrivCoverage, y=cancer.df$avgAnnCount, use="complete.obs")
# There is no important correlation between the Average Annual Incidences of Cancer
# and the Private Health Insurance by Employment Coverage
#
plot(cancer.df$PctPublicCoverage,cancer.df$avgAnnCount, xlab = "Percentage of population with public health insurance coverage", ylab="Average Annual Incidences", main= "Annual Incidences by Public Coverage")
# Let's include a linear regression line to have a clearer view:
abline(lm(cancer.df$avgAnnCount ~ cancer.df$PctPublicCoverage))
# Check correlation
cor(x=cancer.df$PctPublicCoverage, y=cancer.df$avgAnnCount, use="complete.obs")
# There is no important correlation between the Average Annual Incidences of Cancer
# and the Public Health Insurance Coverage.
#
# We are now going to evaluate the relation between the median income and the
# percentage of health insurance coverage by type.
#
#
plot(cancer.df$medIncome,cancer.df$PctPrivateCoverage, xlab = "Median Income" , ylab="Percentage of population with public health insurance coverage", main= "Median Income vs Private Health Insurance Coverage")
# Let's include a linear regression line to have a clearer view:
abline(lm(cancer.df$PctPrivateCoverage ~ cancer.df$medIncome))
# It seems that we have a strong positive correlation between the median income
# and the Percentage of Population with Private Health Insurance Coverage. We
# can check the correlation between these variables.
cor(x=cancer.df$medIncome, y=cancer.df$PctPrivateCoverage)
# As expected, we have a high positive correlation between median income and
# the percentage of health insurance coverage.
# Let us check the boxplot by the binned median income and the binned percetage of 
# private health insurance coverage.

# not strictly linear (try logarithmi)
cancer.df$IncomeCat<-cut(cancer.df$medIncome, seq(0,160000,20000), right=FALSE, labels=c("0 - 20k","20k - 40k", "40k - 60k", "60k - 80k", "80k - 100k", "100k - 120k", "120k - 140k", "140k - 160k"))
summary(cancer.df$IncomeCat)
boxplot(PctPrivateCoverageCat ~ IncomeCat, data= cancer.df, main = "Percentage of Private Health Insurance Coverage by Median Income", xlab="Median Income", ylab="Private Coverage")
