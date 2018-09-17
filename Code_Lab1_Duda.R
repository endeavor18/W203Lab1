cancer <- read.csv("Documents/Mestrado/W203 - Stats/Lab1/W203Lab1/cancer.csv")
head(cancer,10)
cor(cancer [, c("deathRate","avgAnnCount","medIncome", "popEst2015","povertyPercent", "MedianAge", "MedianAgeMale", "MedianAgeFemale", "AvgHouseholdSize", "PercentMarried", "PctNoHS18_24", "PctHS18_24", "PctSomeCol18_24", "PctBachDeg18_24", "PctHS25_Over", "PctBachDeg25_Over", "PctEmployed16_Over", "PctUnemployed16_Over", "PctPrivateCoverage", "PctPublicCoverage", "PctWhite", "PctBlack", "PctAsian", "PctOtherRace", "PctMarriedHouseholds", "BirthRate")], use = "complete.obs")
#medIncome            -0.410435619
#povertyPercent        0.401176537
#PctHS25_Over          0.405652014
#PctBachDeg25_Over    -0.458860201
#PctEmployed16_Over   -0.393190901
#PctUnemployed16_Over  0.339835064
#PctPrivateCoverage   -0.372199978
#PctPublicCoverage     0.379728790
#PctBlack              0.277205711
#PctMarriedHouseholds -0.290109211
cancer$IncomeCat<-cut(cancer$medIncome, seq(0,150000,25000), right=FALSE, labels=c("0 - 25k","25k - 50k", "50k - 75k", "75k - 100k", "100k - 125k", "125k - 150k"))
boxplot( deathRate ~ IncomeCat, data=cancer, main = "Death Rate by Median Income Range",xlab="Median Income Range",ylab="Death Rate")
plot(x = cancer$povertyPercent, y=cancer$deathRate, main = "Death Rate by Poverty Percent", xlab= "Poverty Percent", ylab="Death Rate")
abline(lm(cancer$deathRate ~ cancer$povertyPercent))
cancer[which.max(cancer$deathRate),10]