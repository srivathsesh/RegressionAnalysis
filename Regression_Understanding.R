# Montogomery Example 2.1

# read file
df <- readr::read_csv(file = 'linear_regression_5e_data_sets/Chapter 2/Examples/data-ex-2-1 (Rocket Prop).csv')

# Plot scatter
library (ggplot2)
ggplot(data = df,mapping = aes(x = `Age of Propellant`, y = `Shear Strength`)) + geom_point() + geom_smooth(method = 'lm',color = 'Red') + theme_bw()

# Estimate model parameters
# B1 = Sxy/Sxx
#Sxy = Summation of yi(xi - Xbar) , sxx = summation (xi - xbar)^2

library(dplyr)
df <- df %>% mutate(sxyi = (`Age of Propellant` - mean(`Age of Propellant`))*`Shear Strength`) %>% 
 mutate(sxxi = (`Age of Propellant` - mean(`Age of Propellant`))^2)

B1 <- sum(df$sxyi)/sum(df$sxxi)

# B0 = ybar - B1(xbar)

B0 <- mean(df$`Shear Strength`) - B1*mean(df$`Age of Propellant`)

# Check results with linear model

model <- lm(`Shear Strength`~ `Age of Propellant`, df)
summary(model)

anova(model)

# variance of B1 is sigma^2/Sxx
var(df$`Shear Strength`)/sum(df$sxxi)

# broom augment
modelResult <- broom::augment(model)
