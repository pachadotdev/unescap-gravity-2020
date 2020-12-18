# Packages ----

library(haven) # read dta format (Stata)
library(janitor) # tidy column names
library(dplyr) # chained operations
library(sandwich) # covariance based estimators
library(lmtest) # econometric tests
library(broom) # tidy regression results

# Data ----

gravity <- clean_names(read_dta("01-data/gravity-data.dta"))
