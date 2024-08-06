#rwmove all variables stored previously
rm(list = ls()) 
#install packages
install.packages("Hmisc") 
#import packages
library(Hmisc)
#load the data
data <- read.csv("~/Desktop/Covid19 Data Analysis/covid_19_data.csv") 
describe(data)

#clean the death column 
data$death_dummy <- as.integer(data$death !=0)
#death rate
sum(data$death_dummy) / nrow(data)

#AGE
#claim : peaple who die older

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy== 0)

mean(dead$age, na.rm=TRUE)
mean(alive$age, na.rm=TRUE)

#significantly test 
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

#p-value<0.05 we reject null hypothesis
#here, p-value ~ 0, so we reject the null hypothesis and 
#conclude this is statistically significant

#Gender
#claim : gender has no effect

men = subset(data, gender == "male")
women = subset(data, gender== "female")

mean(men$death_dummy, na.rm=TRUE)
mean(women$death_dummy, na.rm=TRUE)

#significantly test 
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)

# 99% confidence: men have from 0.8% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistical significant
