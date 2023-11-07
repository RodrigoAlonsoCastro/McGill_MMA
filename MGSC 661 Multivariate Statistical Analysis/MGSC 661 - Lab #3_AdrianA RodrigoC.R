election = read.csv("C:/_McGill MMA 2023/2. Fall 2023/MGSC 661 Multivariate Statistical Analysis/Labs/Lab 3/election_data.csv")
wine = read.csv("C:/_McGill MMA 2023/2. Fall 2023/MGSC 661 Multivariate Statistical Analysis/Labs/Lab 3/wine_data.csv")

library(ggplot2)

attach(election)

# 1. LINEAR PROBABILITY MODEL
##############################

## Explore the target variables
table(winner)

## Create a binary variable for candidate election
election$vote = ifelse(winner == 'Clinton',1,0)
attach(election)

## A) Run a Linear Probability Model
lpm = lm(vote~pop2014+under18+density+black+hispanic)
summary(lpm)

## C) Obtain Probability of voting for Trump
data_predict = data.frame(
  pop2014 = 250000,      
  under18 = 0.10,    
  density = 175,        
  black = 0.45,         
  hispanic = 0.10)

1-predict(lpm, data_predict)

## D) Obtain Probability of voting for Trump
data_predict = data.frame(
  pop2014 = 1000000,      
  under18 = 0.20,    
  density = 1000,        
  black = 0.85,         
  hispanic = 0.05)

1 - predict(lpm, data_predict)



# 2. LOGISTIC REGRESSION
#########################

## A) Run a Logistic Regression Model
logm = glm(vote~pop2014+under18+density+black+hispanic, family='binomial')
summary(logm)

## C) Obtain Probability of voting for Trump
data_predict = data.frame(
  pop2014 = 1000000,      
  under18 = 0.20,    
  density = 1000,        
  black = 0.85,         
  hispanic = 0.05)

1 - predict(logm, data_predict, type='response')

## D) Obtain the R-squared of the model
require(rms)

logm2 = lrm(vote~pop2014+under18+density+black+hispanic, data=election)
logm2

## E) Obtain the % of Black people that equalizes to odds to vote for any candidate
b0 = coef(logm2)[1]
b1 = coef(logm2)[2]
b2 = coef(logm2)[3]
b3 = coef(logm2)[4]
b4 = coef(logm2)[5]
b5 = coef(logm2)[6]

  # 0 = b0 + b1*250000 + b2*0.25 + b3*100 + b4*black + b5*0.1
black = (- b0 - b1*250000 - b2*0.25 - b3*100 - b5*0.1)/b4
black


## F) Obtain likelihood function L(bo,b1,b2) 
b0 = -0.75
b1 = 0.03
b2 = 0.01

m1 = exp(b0 + b1*0.15 + b2*0.2)
m2 = exp(b0 + b1*0.25 + b2*0.55)
m3 = exp(b0 + b1*0.05 + b2*0.05)
m4 = exp(b0 + b1*0.75 + b2*0.1)

p1 = m1/(1+m1)
p2 = m2/(1+m2)
p3 = m3/(1+m3)
p4 = m4/(1+m4)

L = (1-p1) * p2 * (1-p3) * p4
L

## G) Obtain likelihood function L(bo,b1,b2) 
b0 = -1
b1 = 2
b2 = 4

m1 = exp(b0 + b1*0.15 + b2*0.2)
m2 = exp(b0 + b1*0.25 + b2*0.55)
m3 = exp(b0 + b1*0.05 + b2*0.05)
m4 = exp(b0 + b1*0.75 + b2*0.1)

p1 = m1/(1+m1)
p2 = m2/(1+m2)
p3 = m3/(1+m3)
p4 = m4/(1+m4)

L = (1-p1) * p2 * (1-p3) * p4
L

## H) Run the new regression model
logm3 = glm(vote~hispanic+undergrad, family='binomial')
summary(logm3)

detach(election)


# 3. LINEAR DISCRIMINANT ANALYSIS
##################################

library(MASS)
library(klaR)

wine$cultivar=as.factor(wine$cultivar)

attach(wine)

## A) Calculate prior probabilities
mylda = lda(cultivar~alcohol+acid)
mylda

## B) Plot histograms for each class (x = alcohol)
hists = ggplot(wine, aes(x=alcohol))+geom_histogram(bins = 50)+facet_grid(cultivar) +
          theme(axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18))
hists

## C) Plot histograms for each class (x = acid)
hists = ggplot(wine, aes(x=acid))+geom_histogram(bins = 50)+facet_grid(cultivar) +
        theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))
hists

## F) Plot classifications regions
partimat(cultivar~alcohol+acid, method="lda",image.colors=c("light grey", "light green", "white")) 

## I) Making predictions (obtain probabilities)
predict(mylda, data.frame(alcohol=14, acid=3))



# 4. QUADRATICS DISCRIMINANT ANALYSIS
######################################

## A) Run quadratic model
myqda = qda(cultivar~alcohol+acid)
myqda

## C) Plot classifications regions
partimat(cultivar~alcohol+acid, method="qda",image.colors=c("light grey", "light green", "white")) 
