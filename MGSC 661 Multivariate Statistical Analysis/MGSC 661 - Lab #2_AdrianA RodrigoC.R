# IMPORT DATASET AND LIBRARIES 
board_games = read.csv("C:/_McGill MMA 2023/2. Fall 2023/MGSC 661 Multivariate Statistical Analysis/Labs/Lab 2/board_games_fall_2023.csv")
attach(board_games) 
View(board_games)

library(car)
library(ggplot2)
require(lmtest)
require(plm)
require(psych)
library(splines)
require(caTools)
require(methods)
library(boot)

install.packages("stargazer")
library(stargazer)

install.packages("gridExtra")
library(gridExtra)


# 1. Model Issues: Non-linearity 
###################################

## Residual Plots
reg1 = lm(avg_rating~year+avg_timeplay+weight)
residualPlots(reg1)


# 2.  Model Issues: Heteroskedasticity 
########################################

## Run regression
reg2 = lm(avg_rating~avg_timeplay)
summary(reg2)

## Detect Heteroskedasticity
residualPlot(reg2)
ncvTest(reg2)

## Correct Heteroskedasticity
coeftest(reg2, vcov=vcovHC(reg2, type="HC1"))


# 3. Model Issues: Outliers 
############################

## Bonferroni test 
reg3 = lm(avg_rating~min_players+age+num_votes)
summary(reg3)

outlierTest(reg3)

## Identify others outliers visually
qqPlot(reg3, envelope = list(style="none"))

board_games[1197, 'name']
board_games[3124, 'name']

## Remove outlier
board_games_2 = board_games[-c(1197, 3124),]

reg3_2 = lm(avg_rating~min_players+age+num_votes, data = board_games_2)
summary(reg3_2)


# 4. Model Issues: Collinearity
################################

## Correlation Matrix
quantvars = board_games[,c('year','age','min_timeplay','max_timeplay')]
corr_matrix = cor(quantvars)
round(corr_matrix, 2)

## Variance Inflation Factor (VFI) test
reg4 = lm(avg_rating~year+age+min_timeplay+max_timeplay)
vif(reg4)

## Re-run the model without "min_timeplay"
reg4_2 = lm(avg_rating~year+age+max_timeplay)
summary(reg4)
summary(reg4_2)


# 5. Presenting professional regression tables 
###############################################

## Run regressions
reg5a = lm(avg_rating~avg_timeplay)
reg5b = lm(avg_rating~min_players)
reg5c = lm(avg_rating~max_players)
mreg5 = lm(avg_rating~avg_timeplay+min_players+max_players)

stargazer(reg5a, reg5b, reg5c, mreg5, type="html")

summary(reg5b)


# 6. Polynomial regression: Age
################################

## Run regressions
reg6a = lm(avg_rating~age)
reg6b = lm(avg_rating~poly(age,2))
reg6c = lm(avg_rating~poly(age,3))
reg6d = lm(avg_rating~poly(age,4))

stargazer(reg6a, reg6b, reg6c, reg6d, type="html")

## Scatterplot matrix
plot1 = ggplot(data = data.frame(age, avg_rating), aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = "1st"), show.legend = TRUE) +
  labs(title = "Polynomial Regression 1", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("1st" = "blue")) +
  theme(legend.key.size = unit(0.5, "lines"))

plot2 = ggplot(data = data.frame(age, avg_rating), aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "2nd"), show.legend = TRUE) +
  labs(title = "Polynomial Regression 2", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("2nd" = "blue")) +
  theme(legend.key.size = unit(0.5, "lines"))

plot3 = ggplot(data = data.frame(age, avg_rating), aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "3rd"), show.legend = TRUE) +
  labs(title = "Polynomial Regression 3", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("3rd" = "blue")) +
  theme(legend.key.size = unit(0.5, "lines"))

plot4 = ggplot(data = data.frame(age, avg_rating), aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), aes(color = "4th"), show.legend = TRUE) +
  labs(title = "Polynomial Regression 4", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("4th" = "blue")) +
  theme(legend.key.size = unit(0.5, "lines"))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

## Run ANOVA test
anova(reg6a, reg6b, reg6c, reg6d)


# 7. Polynomial regression: avg_timeplay
########################################

## Run regressions
reg7a = lm(avg_rating~avg_timeplay)
reg7b = lm(avg_rating~poly(avg_timeplay,2))
reg7c = lm(avg_rating~poly(avg_timeplay,3))
reg7d = lm(avg_rating~poly(avg_timeplay,4))

stargazer(reg7a, reg7b, reg7c, reg7d, type="html")

## Scatterplot matrix
plot1 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(x=avg_timeplay, y=avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula=y~x, aes(color="1st"), show.legend=TRUE) +
  labs(title = "Polynomial Regression 1", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("1st"="green")) +
  theme(legend.key.size=unit(0.5, "lines"))

plot2 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(x=avg_timeplay, y=avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~poly(x, 2), aes(color="2nd"), show.legend=TRUE) +
  labs(title = "Polynomial Regression 2", color = "Degree") +
  theme_minimal() +
  scale_color_manual(values = c("2nd"="green")) +
  theme(legend.key.size=unit(0.5, "lines"))

plot3 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(x=avg_timeplay, y=avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~poly(x, 3), aes(color="3rd"), show.legend=TRUE) +
  labs(title="Polynomial Regression 3", color="Degree") +
  theme_minimal() +
  scale_color_manual(values = c("3rd"="green")) +
  theme(legend.key.size=unit(0.5, "lines"))

plot4 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(x=avg_timeplay, y=avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~poly(x, 4), aes(color="4th"), show.legend=TRUE) +
  labs(title="Polynomial Regression 4", color="Degree") +
  theme_minimal() +
  scale_color_manual(values = c("4th"="green")) +
  theme(legend.key.size=unit(0.5, "lines"))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

## Run ANOVA test
anova(reg7a, reg7b, reg7c, reg7d)


# 8. Multiple polynomial regression
####################################

reg8 = lm(avg_rating~avg_timeplay+poly(age,4))
summary(reg8)


# 9. Spline regression 
#######################

## Define the knots
k1 = quantile(avg_timeplay, 0.25)
k2 = quantile(avg_timeplay, 0.50)
k3 = quantile(avg_timeplay, 0.75)

## Run spline regression
reg9a = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree=1))
reg9b = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree=2))
reg9c = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree=3))

stargazer(reg9a, reg9b, reg9c, type="html")

## Scatterplots
plot1 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(y=avg_rating, x=avg_timeplay)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3), degree=1), aes(color="1st")) +
  geom_vline(xintercept=c(k1,k2,k3), linetype="dashed") +
  labs(title = "Spline Regression 1", color = "Degree") +
  scale_color_manual(values = c("1st"="red")) +
  theme(legend.key.size=unit(0.5, "lines"))

plot2 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(y=avg_rating, x=avg_timeplay)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3), degree=2), aes(color="2nd")) +
  geom_vline(xintercept=c(k1,k2,k3), linetype="dashed") +
  labs(title = "Spline Regression 2", color = "Degree") +
  scale_color_manual(values = c("2nd"="red")) +
  theme(legend.key.size=unit(0.5, "lines"))

plot3 = ggplot(data = data.frame(avg_timeplay, avg_rating), aes(y=avg_rating, x=avg_timeplay)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3), degree=3), aes(color="3rd")) +
  geom_vline(xintercept=c(k1,k2,k3), linetype="dashed") +
  labs(title = "Spline Regression 3", color = "Degree") +
  scale_color_manual(values = c("3rd"="red")) +
  theme(legend.key.size=unit(0.5, "lines"))

grid.arrange(plot1, plot2, plot3, ncol = 2, nrow = 2)


# 10. Validation set test 
##########################

## Split the data
sample = sample.split(board_games$avg_rating, SplitRatio = 0.5)
train_set = subset(board_games, sample==TRUE)
test_set = subset(board_games, sample==FALSE)

## Calculate the MSE for each polynomial model
average_mse = rep(NA,10)

for (degree in 1:10) {
  mse_values = rep(NA,30)
  for (i in 1:30) {
    fit = lm(avg_rating~poly(weight, degree), data=train_set)
    prediction = predict(fit, test_set)
    mse = mean((test_set$avg_rating - prediction)^2)
    mse_values[i] = mse
  }
  average_mse[degree] = mean(mse_values)
}

for (degree in 1:10) {
  cat(sprintf("Avg_MSE degree %d: %f\n", degree, average_mse[degree]))
}


# 11. LOOCV test 
#################

## Run simple regression model 
fit = glm(avg_rating~weight, data=board_games)
mse = cv.glm(board_games, fit)$delta[1]
mse

## Calculate the MSE for each polynomial model
mse_values = rep(NA,10)

for (degree in 1:10) {
  fit = glm(avg_rating~poly(weight, degree), data=board_games)
  mse_values[degree] = cv.glm(board_games, fit)$delta[1]
}

for (degree in 1:10) {
  cat(sprintf("MSE degree %d: %f\n", degree, mse_values[degree]))
}


# 12. K-fold cross-validation
##############################

## Calculate the MSE for each polynomial model
mse_values_2 = rep(NA,10)

for (degree in 1:10) {
  fit = glm(avg_rating~poly(num_votes, degree), data=board_games)
  mse_values_2[degree] = cv.glm(board_games, fit, K=5)$delta[1]
}

for (degree in 1:10) {
  cat(sprintf("MSE degree %d: %f\n", degree, mse_values_2[degree]))
}


# 13. Cross-validation in a multiple spline model 
##################################################