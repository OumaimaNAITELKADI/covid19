rm(list=ls())
library(Hmisc)
data <- read.csv("C:/Users/Admin/Desktop/covid19_R/COVID19_line_list_data.csv")
describe(data)

data$death_dummy <- as.integer(data$death!=0) #clean death column
#death rate
sum(data$death_dummy)/nrow(data)
#age
#claim: people who die are older 
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm =TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided",conf.level=0.95)
# normally if p_value<0.05 we reject null hypothesis
# here we have p_value = 0, so we have concluded that indeed the people who die from the coronavirus 
# conclusion: we conclude that this is statistically significant

#gender
#claim: gender has no effect 
men = subset(data, gender == "male") # 8.5%
women = subset(data, gender == "female") #3.7%
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm =TRUE)
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided",conf.level=0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance of dying
# p_alue =0.002<0.05, so this is statistically significant