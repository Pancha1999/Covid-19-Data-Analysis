#rwmove all variables stored previously
rm(list = ls()) 
#install packages
install.packages("Hmisc") 
if (!require(ggplot2)) 
install.packages("ggplot2")
#import packages
library(Hmisc)
library(ggplot2)
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

# Calculate mean ages
mean_dead_age <- mean(dead$age, na.rm = TRUE)
mean_alive_age <- mean(alive$age, na.rm = TRUE)

# Perform t-test
age_t_test_result <- t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

# Print the results
mean_dead_age
mean_alive_age
age_t_test_result

# Create a data frame for visualization
age_data <- data.frame(
  Age = c(dead$age, alive$age),
  Status = c(rep("Deceased", length(dead$age)), rep("Alive", length(alive$age)))
)

# Create the box plot for age distribution
ggplot(age_data, aes(x = Status, y = Age, fill = Status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Deceased" = "red", "Alive" = "blue")) +
  labs(title = "Age Distribution of Deceased and Surviving Individuals",
       x = "Status",
       y = "Age",
       fill = "Status") +
  theme_minimal() +
  annotate("text", x = 1.5, y = max(age_data$Age, na.rm = TRUE), label = paste("p-value:", round(age_t_test_result$p.value, 4)), vjust = -1)

#p-value<0.05 we reject null hypothesis
#here, p-value ~ 0, so we reject the null hypothesis and 
#conclude this is statistically significant

#Gender
#claim : gender has no effect

men = subset(data, gender == "male")
women = subset(data, gender== "female")

# Calculate mean death rates
mean_men_death_rate <- mean(men$death_dummy, na.rm = TRUE)
mean_women_death_rate <- mean(women$death_dummy, na.rm = TRUE)

# Print the mean death rates
mean_men_death_rate
mean_women_death_rate

# Perform t-test
t_test_result <- t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)

# Print the t-test result
t_test_result

# Create a data frame for visualization
gender_death_rate <- data.frame(
  Gender = c("Male", "Female"),
  DeathRate = c(mean_men_death_rate, mean_women_death_rate)
)

# Create the bar plot for death rates by gender
ggplot(gender_death_rate, aes(x = Gender, y = DeathRate, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Death Rates by Gender",
       x = "Gender",
       y = "Death Rate",
       fill = "Gender") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(DeathRate)), vjust = -0.3) +
  annotate("text", x = 1.5, y = max(gender_death_rate$DeathRate), label = paste("p-value:", round(t_test_result$p.value, 4)), vjust = -1)
# 99% confidence: men have from 0.8% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistical significant
