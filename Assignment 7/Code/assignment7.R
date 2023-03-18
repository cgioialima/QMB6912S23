
################################################################################
#
# QMB 6912: Capstone in Business Analytics
# Analyzing the Dependent Variable
##
# Name: Carolina Lima
# College of Business
# University of Central Florida
#
# Date: March 19, 2023
#
################################################################################
#
# DATA
##################################################
rm(list=ls(all=TRUE))

data_path <- 'C:/Users/carol/OneDrive/Documentos/GitHub/QMB6912-23/Assignment 7'
setwd(data_path)

# Set data directory.
data_dir <- 'Data'
# Set directory for storing figures.
fig_dir <- 'Figures'
# Set directory for storing tables.
tab_dir <- 'Tables'

# Data
file_name <- sprintf('%s/%s', data_dir, 'HomeSales.dat')

data <-read.table(file=file_name)


attach(data)
names(data)

#rename columns
colnames(data) <- c("year", "num_beds", "num_baths", "floor_space", "lot_size", "has_garage", "has_encl_patio", "has_security_gate", "has_pool", "transit_score", "school_score", "type_of_buyer", "price")

##################################################
# LIBRARIES
##################################################
library(sm)
library(grid)
library(vcd)
library(cluster)
library(gclus)
library(ggplot2)
#library(tidyverse)
library(xtable)
library(car)
#library(EnvStats)
#library(caTools)
library(texreg)



##################################################
# New variables
##################################################
print('Creating new variable - log_price.')
data[, 'log_price'] <- log(data[, 'price'])

print('Creating new variable - age.')
data[, 'age'] <- 2021-(data[, 'year'])

##################################################
# Data preparation

##################################################

# Create categories for the transit_score variable.

summary(data[, 'transit_score'])
# Collect observations into groups with similar transit_score
data[, 'ts_cat'] <- NA
sel_obs <- data[, 'transit_score'] < 3
data[sel_obs, 'ts_cat'] <- '0-3'
sel_obs <- data[, 'transit_score'] >= 3 & 
  data[, 'transit_score'] < 6
data[sel_obs, 'ts_cat'] <- '3-6'
sel_obs <- data[, 'transit_score'] >= 6 & 
  data[, 'transit_score'] < 8
data[sel_obs, 'ts_cat'] <- '6-8'
sel_obs <- data[, 'transit_score'] >= 8
data[sel_obs, 'ts_cat'] <- '8+'

# Set it as a factor to keep the variables in order.

data[, 'ts_cat'] <- factor(data[, 'ts_cat'], 
                                    levels = c('0-3', '3-6', 
                                               '6-8',  '8+'))

table(data[, 'ts_cat'], useNA = 'ifany')  
#0-3 3-6 6-8  8+ 
#407 422 440 593 



# Create categories for the lot_size variable.

summary(data[, 'lot_size'])
# Collect observations into groups with similar transit_score
data[, 'lot_cat'] <- NA
sel_obs <- data[, 'lot_size'] < 5050
data[sel_obs, 'lot_cat'] <- '0-5050'
sel_obs <- data[, 'lot_size'] >= 5050 & 
  data[, 'transit_score'] < 6650
data[sel_obs, 'lot_cat'] <- '5050-6650'

sel_obs <- data[, 'lot_size'] >= 6650 & 
  data[, 'lot_size'] < 7500
data[sel_obs, 'lot_cat'] <- '6650-7500'

sel_obs <- data[, 'lot_size'] >= 7500 & 
  data[, 'lot_size'] < 8060
data[sel_obs, 'lot_cat'] <- '7500-8060'

sel_obs <- data[, 'lot_size'] >= 8060
data[sel_obs, 'lot_cat'] <- '8060+'

# Set it as a factor to keep the variables in order.

data[, 'lot_cat'] <- factor(data[, 'lot_cat'], 
                           levels = c('0-5050', '5050-6650', 
                                      '6650-8060',  '8060+'))

table(data[, 'lot_cat'], useNA = 'ifany')  
#0-5050 5050-6650 6650-8060     8060+ 
#464       525       408       465 


# use school_score as factor
table(data[, 'school_score'], useNA = 'ifany')  

#1   2   3   4   5   6   7   8   9  10 
#117 136 164 193 207 162 225 274 250 134 

##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
# All variables included (with linear specification).
##################################################

# Estimate a regression model.
model_full <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) + floor_space+ lot_size +
                   has_garage + has_encl_patio + has_security_gate + has_pool + 
                   transit_score + school_score +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_full))
##################################################
# Estimating a Regression Model
# Best reduced model from a strictly linear specification 
# from assignment 6.
##################################################

# Estimate a regression model.
best_red <- lm(log_price ~ as.factor(num_beds) +as.factor(num_baths) + lot_size +
                has_garage + has_encl_patio + has_security_gate + has_pool + 
                transit_score + type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(best_red))

##################################################
# Estimating a Regression Model
# use full model
# school_score categorical variable
##################################################
model_school <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) + floor_space+ lot_size +
                   has_garage + has_encl_patio + has_security_gate + has_pool + 
                   transit_score + as.factor(school_score) +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_school))

# school score is now significant using as factors - when it was not before
# floor_space still shows insignificant in the model

##################################################
# Estimating a Regression Model
# use full model
# transit_score categorical variable
##################################################
# Suppose for a moment that the transit_score variable were measured
# as a categorical variable.

# Estimate a regression model.
model_ts_cat <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) + floor_space+ lot_size +
                     has_garage + has_encl_patio + has_security_gate + has_pool + 
                     transit_score +ts_cat +
                     as.factor(school_score) +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_ts_cat))

# none of the ts categories are more valuable than the transit_score
# none of the categories are significant, so we will keep transit score as is

##################################################
# Estimating a Regression Model
# use full model
# lot_size categorical variable
##################################################
# Suppose for a moment that the lot_size variable were measured
# as a categorical variable.

# Estimate a regression model.
model_lot_cat <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) + 
                      floor_space+ lot_size + lot_cat +
                      has_garage + has_encl_patio + has_security_gate + has_pool + 
                      transit_score +
                      as.factor(school_score) +type_of_buyer + age, data = data)


# Output the results to screen.
print(summary(model_lot_cat))

# Notice that the 5050-6650 -lot size is the only statically significant category
# the continuous variable seems to do better than the categories



# Print the output to a LaTeX file.
tab_file_name <- 'cat_comp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(list(model_full,
            best_red,
            model_school,
            model_ts_cat,
            model_lot_cat),
       file = out_file_name,
       label = 'tab:cat_comp',
       caption = "Dollar Value of Home Prices")

library(performance)
compare_performance(model_full,
                    best_red,
                    model_school,
                    model_ts_cat,
                    model_lot_cat, rank=TRUE)

#model_lot_cat highest R2 and highest RMSE, model_school and model_ts_cat seem to be very similar
##################################################
#
# Introduce a Nonlinear Functional Form
#
# Consider a polynomial functional form for lot_size
# Idea: lot_size improves performance up to a limit,
# then extra square foot does not add value
#
# 1. Generate the squared variable.
# 2. Hypothesize the signs.
# 3. Add the squared lot_size term to the regression equation.
# 4. Estimate the revised model.
# 5. Analyze the resulting estimates.
# 6. Make recommendation for the new model.
#
##################################################
# Create a variable sq_lot_size
# to investigate quadratic relationship of sale price to lot_size
data[, 'sq_lot_size'] <- data[, 'lot_size']^2

##################################################
# Reconsider other variables dropped before
# Using this new functional form for horsepower
##################################################

# Estimate the regression model.# use all variables
model_ls_all <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) + floor_space+ 
                        lot_size +sq_lot_size+
                        has_garage + has_encl_patio + has_security_gate + has_pool + 
                        transit_score + as.factor(school_score) +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_ls_all))

#floor_space not significant
#sq_lot_size no significance compared to alpha .05

##################################################
# Estimating a Regression Model
# With quadratic form for lot_size
# Omit floor_space 
##################################################

# Estimate a regression model.
model_ls_red <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) +  
                          lot_size +sq_lot_size+
                          has_garage + has_encl_patio + has_security_gate + has_pool + 
                          transit_score + as.factor(school_score) +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_ls_red))

##################################################
# Estimating a Regression Model
# With quadratic form for lot_size
# Omit floor_space and sq_lot of low significance
##################################################

# Estimate a regression model.
model_ls_red2 <- lm(log_price ~  as.factor(num_beds) +as.factor(num_baths) +  
                     lot_size +#sq_lot_size+
                     has_garage + has_encl_patio + has_security_gate + has_pool + 
                     transit_score + as.factor(school_score) +type_of_buyer + age, data = data)

# Output the results to screen.
print(summary(model_ls_red2))

# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_lot.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(model_full, 
                model_school,
                model_ls_all,
                model_ls_red,
                model_ls_red2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_lot',
       caption = "Quadratic Models for Home Prices")

compare_performance(model_full, 
                    model_school,
                    model_ls_all,
                    model_ls_red,
                    model_ls_red2, rank=TRUE)

# model_ls_red best model but not really any difference in R2 adjusted and RMSE

##################################################
#
# Exercise: Test exclusion of seasonal indicators
#   An example of joint hypothesis testing.
print("Test for exclusion of indicators")
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(model_ls_all$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(model_ls_red$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################
# Need sample size and number of variables.

num_obs <- nrow(data)
num_vars <- 27 # including the intercept.

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 1

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat) #1.29224

# This value is greater than 1, 2.86517, 
# no significant difference when removing floor_space from the model
anova(model_ls_all, model_ls_red)
#no significant difference  so we go with simpler model



##################################################
# Box-Tidwell Transformation
##################################################

# The boxTidwell function tests for non-linear relationships
# to the mean of the dependent variable.
# The nonlinearity is in the form of an
# exponential transformation in the form of the Box-Cox
# transformation, except that the transformation is taken
# on the explanatory variables.

#--------------------------------------------------
# Transformation of lot_size
#--------------------------------------------------

# Modified from the linear model:
# log_saleprice ~ horsepower + squared_horsepower +
#   age + enghours +
#   diesel + fwd + manual + johndeere + cab
# This specification allows a single exponential
# transformation on horsepower, rather than a quadratic form.

bt_lot <- boxTidwell(formula =
                       log_price ~ lot_size ,
                    other.x = ~
                      # lot_size + sq_lot_size +
                      as.factor(num_beds) + as.factor(num_baths) +  has_garage + has_encl_patio +
                      has_security_gate + has_pool + transit_score + as.factor(school_score) + 
                      type_of_buyer + age,
                    data = data,
                    verbose = TRUE)
# Note: The "MLE of lambda" is the exponent on horsepower.
# Similar to the Box-Cox transformation,
# with Box-Tidwell, the exponents are on the explanatory variables
# and are all called lambda.
# The exponent is significantly different from 0,
# although it is a small positive value,
# which suggests an increasing but sharply declining relationship.

# The summary method is not available.
# summary(bt_hp)

# The output is a test on the exponent.
print(bt_lot)

# What does this transformation look like?
lot_grid <- seq(15, 500, by = 5)
bt_lot_lambda_hat <- 0.1368128 #iter = 7     powers = 0.1368128 
lot_bt_grid <- lot_grid^bt_lot_lambda_hat

plot(lot_grid,
     lot_bt_grid,
     xlab = 'Lot Size',
     ylab = 'Transformation of Lot Size',
     type = 'l',
     lwd = 3,
     col = 'coral2')

# Print the output to a LaTeX file.
tab_file_name <- 'bt_lot.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_lot)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

#--------------------------------------------------
# Transformation of age
#--------------------------------------------------
library(dplyr)
new.data <- data %>% filter(age > 0)

bt_age <- boxTidwell(formula =
                       log_price ~ age ,
                     other.x = ~
                       lot_size + sq_lot_size +
                       as.factor(num_beds) + as.factor(num_baths) +  has_garage + has_encl_patio +
                       has_security_gate + has_pool + transit_score + as.factor(school_score) + 
                       type_of_buyer, #age,
                     data = new.data,
                     verbose = TRUE)

print(bt_age) #iter = 3     powers = 0.6994056

# This coefficient is effectively 1, which is more evidence of
# a purely linear relationship between log_saleprice
# and age: the percentage depreciation rate is constant.
age_grid <- seq(15, 500, by = 5)
bt_age_lambda_hat <- 0.6994056 
age_bt_grid <- age_grid^bt_age_lambda_hat

plot(age_grid,
     age_bt_grid,
     xlab = 'Age',
     ylab = 'Transformation of Age',
     type = 'l',
     lwd = 3,
     col = 'steelblue')

# Print the output to a LaTeX file.
tab_file_name <- 'bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_age)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)
#--------------------------------------------------
# Transformation of lot_size and age
#--------------------------------------------------

bt_full <- boxTidwell(formula =
             log_price ~ lot_size + age ,
           other.x = ~
             #lot_size + sq_lot_size +
             as.factor(num_beds) + as.factor(num_baths) +  has_garage + has_encl_patio +
             has_security_gate + has_pool + transit_score + as.factor(school_score) + 
             type_of_buyer, #age,
           data = new.data,
           verbose = TRUE)
print(bt_full)


# This confirms the result of the above,
# with the nonlinear transformations
# for lot_size and age
# This suggests an additional model with
# these transformations for lot size and age
tab_file_name <- 'bt_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


##################################################
# NEW MODEL WITH TRANSFORMATIONS
##################################################
data[, 'sq_age'] <- data[, 'age']^2

model_trans<-lm(log_price ~ lot_size + sq_lot_size + 
                  as.factor(num_beds) + as.factor(num_baths) +  has_garage + has_encl_patio +
                  has_security_gate + has_pool + transit_score + as.factor(school_score) + 
                  type_of_buyer +
                  age + sq_age,
                data = data)
summary(model_trans)

compare_performance(model_ls_red,
                    model_trans, rank=TRUE)
