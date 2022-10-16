# LECTION 1

# stefano.peluso@unimib.it
# Associate prof in Statistics

# Contents:
# James et al (google ISLR). Chapters 3-9
# 1. Regression (linear, polynomial,KNN)
# 2. Classification (logistic,LDA,QDA,KNN)
# 3. Cross validation and bootstrap
# 4. Model selection and regularization (lasso, ridge)
# 5. Non-linearities
# 6. Tree-based methods
# 7. Support vector machines



library(ISLR) # to load all datasets used in the book

# Statistical/Machine learning: set of tools for understanding data
# Mainly divided in:
# 1. supervised --> you also observe y
#   ex: polynomial regression
# 2. unsupervised --> you do not observe y
# Ex: cluster analysis

# Onother importatn division:
# 1. Regression problem --> for quantitative Y
# Ex: local regression
# 2. Classification problem --> for qualitative Y
# Ex: discriminant analysis

# Big assumption: asssume the existence of a relation
# Y = f(X) + eps
# we want to estimate f for two objectives:
# 1. to predict Y
# 2. inference on how X affects Y
# We can estimate f with
# 1. parametric models --> we assume a specific form for
#    f and then reduce the problem of estimating f to the
#    problem of estimating a set of parameters
#    Ex: in linear regression f(x)=a+bX
#    Typically: high bias, low variance
# 2. nonparametric models --> do not assume a specific form
#    Ex: spline 
#    Typically: low bias, high variance

# For prediction, no method will exactly predict Y, since:
# - there is a part of error that can be reduced
#   by the choice of an appropriate model (reducible error)
# - there is a part of error which is intrinsic of the Y
#   variabiliy (eps) (irreducible error)

# Data (or part of data) we use to learn f(), is called
# training sample, which is different from the testing
# sample, over which you measure how well you estimated f()

# In simple models, restrictive form of f --> bias, but 
# nonparametric models can lead to a phenomenon called
# overfitting: 
# we are explaining our data too well, that is we are following
# too much the dynamics in the current data, causing a 
# poor performance in other datasets
# --> high variance of the estimate
# we see: measure of performance ok on training data but
# worse on test data

# how do we assess model accuracy?
# 1. In regression problems, Y is quantitative and 
# then we measure the distance btw predicted and true Y
# through the Mean Square Error (MSE): mean((Y-Y.hat)^2)
# low MSE means a better model
# If you measure MSE on the training data, you have 
# Training MSE, but, if feasible, it is better to measure in
# on the test data (Test MSE). But: often you only have 
# training data --> methods to estimate Test MSE based on 
# training data (validation and bootstrap)
# --> Test and Training MSE can behave quite differently

# On all methods, we assist to this trade-off between 
# bias and variance. The test MSE can be decomposed in
# three parts: variance of f.hat, bias of f.hat and the
# irreducible variance of the statistical error

# 2. In classification problems, Y is qualitative, then 
# we look at the performance measure called Training/Test
# error rate: mean(Y!=Y.hat)
# statistical models for classification are called 
# classifiers, since correct prediction of Y means 
# attribution of the statistical unit to the correct class
# or category
# An example of classifier: Bayes classifier, that assign
# units to that class with highest posterior probability
# Another example if KNN classifier: (K nearest neighbours)
# assign unit to that class most frequent among the k units
# closest

#########################
# REGRESSION ############
#########################
# Chapter 3 of ISLR
# Supervised learning approach
# I have a quantitative Y, that I want to predict according
# to some covariates/predictors/features X, ot I want to
# infer the relationship between X and Y

# Simple linear regression ######################################
# Simple: unique X, and dependence btw X and Y assumed
# linear: Y approx beta0+beta1*X
#D <- read.csv("../data/Advertising.csv")
setwd("C:/Users/UTENTE/OneDrive/Desktop/Universita_Bicocca/02.Anno/ML_universita")
getwd()
D <- read.csv("data/Advertising.csv")
head(D)
# sales approx b0+b1*TV
# b0 = intercept
# b1 = slope
# find linear relationship that best explains dependence
# in (x1,y1), (x2,y2),...
D <- D[,-1] # remove first column
plot(D$TV,D$sales) # to have a first idea of relat.
M <- lm(D$sales~D$TV)  # lm stays for linear model
M
summary(M) # to have more info
abline(M, lwd=3) # to add the estimated regression lin
# optimal since it minimizes distances btw Y and Y.hat
segments(D$TV, D$sales, D$TV, M$fitted.values)
RSS <- sum((D$sales - M$fitted.values)^2)  # optimal RSS

## Example on optimality of RSS
b0s <- seq(6.5, 7.5, by=0.01)  # intercepts to try
b1s <- seq(0.045, 0.05, length = length(b0s)) # slopes to try
RSS <- c() # to be filled
for(b1 in b1s){
  for(b0 in b0s){
    y.pred <- b0+b1*D$TV # predicted Y
    RSS.current <- sum((D$sales-y.pred)^2)
    RSS <- c(RSS,RSS.current) 
  }
}
RSS <- matrix(RSS,length(b0s),length(b1s))
contour(b0s,b1s,RSS,nlevels = 100)
points(M$coefficients[1],M$coefficients[2],col=2,lwd=3)
M$coefficients
persp(b0s,b1s,RSS)

# Ex on the difference between population regression line
# and estimated (OLS) regression line:
# Even if a linear relat exists, there is this difference
X   <- rnorm(100, 0, 0.5) # covariate
eps <- rnorm(100, 0, 2)   # error
Y   <- 2 + 3*X + eps        # dependent var
plot(X,Y)
abline(2, 3,    col = 2, lwd = 3)  # pop regression line
abline(lm(Y~X), col = 1, lwd = 3) # estimated regression line

# OLS line changes in each sample:
x.ax <- seq(-2,2,length=100) # x axis
y.ax <- seq(-10,10,length=100) # y axis
plot(x.ax,y.ax,type="n")
for(i in 1:10){ # 10 different samples
  X   <- rnorm(100,0,0.5) # covariate
  eps <- rnorm(100,0,2)   # error
  Y   <- 2+3*X+eps        # dependent var
  abline(lm(Y~X),col=1,lwd=3) # estimated regression line
}
abline(2,3,col=2,lwd=3)  # pop regression line

# back to the advertising example:
# richer output with
M.summary <- summary(M)
M.summary$coefficients
# both b0 and b1 signif different from 0
# from the output, we also have the model accuracy
# Two ways: RSE (residual standard error) and R2
# 1. RSE is a measure of lack of fit
# (low is better) RSE = sqrt(RSS/(n-2))
M.summary # select model with lower RSE
# 2. R^2 (proportion of variance explained by X)
# R^2 = 1 - RSS/TSS
M.summary$r.squared

# More examples on simple regressions:
names(D)
M.radio <- lm(sales~radio,data = D)
summary(M.radio)$coeff
# there is a linear ret also with radio

M.news <- lm(sales~newspaper,data = D)
summary(M.news)$coeff
# there is a linear ret also with newspaper

# Why should we use a unique model with all Xs in it?
# - with three simple lms, we are not able to predict
# sales for given investments in TV, radio, newsp
# - with three simple lms, we are neglecting the 
#   correlation among Xs
# --> multiple lm , where the single coef expresses
# the dependence of that media with sales,keeping fixed
# investments in other media

# A first simulated ex on multiple lm
x1 <- rnorm(100)
x2 <- rnorm(100)
y  <- 1+x1+2*x2+rnorm(100,0,2)
M  <- lm(y~x1+x2)
betas <- M$coefficients

# to visualize the residuals:
f <- function(x1,x2){
  y<-betas[1]+betas[1]*x1+betas[2]*x2
}
y.out <- outer(x1,x2,f) # y.out[i,j]=f(x1[i],x2[j])
plot.lm <- plot3D::persp3D(x1,x2,y.out,theta=50,phi=0,col=2)
# now we have a OLS plane, and to add points:
mypoints <- trans3d(x1,x2,y,plot.lm)
points(mypoints,lwd=2)

# back to advertising:
M.all <- lm(sales~.,data=D) 
# "." after ~ means "all others in the dataset"
round(summary(M.all)$coeff,4)
# incoherence with simple lms since now
# newsp in not linked to sales
# apparent incoherence: in multiple lm, the slope is
# the effect of newsp on sales, keeping fixed TV and radio
# investiments. In simple lm, the slope in the marginal
# effect on newsp on sales (without considering TV and radio)
cor(D)
# high correlation btw newsp and radio: I tend to see
# higher newsp investments when radio investments are high
# --> then I can confuse the effect on sales 
# Absurd example: lm reveals a dependence between
# ice cream sales and shark attacks --> if you include
# also temperature this dependnce disappears

# Important questions:
# 1. Is at least one X useful?
# 2. Do all predictors help to explain Y?
# 3. How well the model fits the data?
# 4. Given a value of Xs, how do you predict Y?

# 1. answer looking at the F statistic
summary(M.all)
# since pval small --> reject null hypothesis and at least
# one regressor is useful
# I am testing H0: beta1=beta2=beta3=0
# vs H1: betai<>0 for some i in {1,2,3}
# we can then remove newsp from lm:
M.radioTV <- lm(sales~.-newspaper,data=D)
# An edvantage is that I can predict sales in correspondence
# of given values of Xs
predict(M.radioTV,data.frame(TV=100,radio=20,
                             newspaper=100),
        interval = "confidence",level=0.95)
# to predict average sales when we invest 100 in TV,...

# Why do I need to look at the F test if I already have
# t tests associated to the single covariates?
# Are F test and t tests always coherent? If not, what should
# I do?
# Not always coherent and priority to F test
# Reason: t tests are affected by a problem of multiple HT
# it becomes easier and easier to find a false significant
# coefficient as you increase the num of Xs
# Example: p=100 regressors and Type I-error prob is
# alpha=0.05, then you expect 5 t tests to be significant
# when in reality all betas are 0

# 2. Are all the Xs useful for predicting Y?
# variable selection problem: identify redundant Xs and
# remove from the model. Difficult problem since the num
# of models to compare is huge even for a moderate num
# of regressors
# With p=2 regressors: we compare (1) null model with no Xs,
# (2) full model with both Xs, (3) model with only X1,
# (4) model with only X2
# and then we use R^2 (adjusted) or Mallows Cp, AIC, BIC
# In general, with p regressors, 2^p models to compare
# easier to become huge --> methods to compare models
# without exploring the whole model space:
# a) forward selection. You start from the null model.
# You include only one X, the one showing lowest RSS
# (comparison among p models). Then you include another
# X among the p-1 remaining, the one showing lowest RSS.
# Continue in this way up to the point where none of the
# remaining predictors are significant if included
# b) backward selection. You start from the full model and
# then you remove that X that is least significant 
# (highest pvalue) and so on, up to the point where pvals
# of included Xs are below a given threshold
# c) mixed selection. Backward selection can't be used
# for high-dimensional problems (p>=n) and forward selection
# is a greedy approach (X already included can't be 
# exluded anymore, even if other includes regressors
# make this X not significant). You start from the null model
# You include that X with lowest RSS (like in forward), but
# when an X has pval above a threshold, this is excluded
# (like in backward). Up to the point where all pvals of 
# included Xs are below a threshold and all Xs exluded 
# wouls not be significant if included

