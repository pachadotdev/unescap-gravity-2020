# Use Fixed Effects to control for MRs ----

## Call previous scripts

source("01-packages-and-data.R")
source("02-prepare-data.R")

## Create Remoteness Importer Index
## This step is VERY DIFFERENT to Stata, in Stata you need to create 
## 830 0-1 columns, here you only need 2 factor columns

gravity2 <- gravity2 %>% 
  mutate(
    exp_time = as.factor(paste(exporter, year, sep = "_")),
    imp_time = as.factor(paste(importer, year, sep = "_"))
  )

## Remove 0 flows
## IMPORTANT: If we don't do this, lm fails because log(0) = -Inf

gravity2 <- gravity2 %>%
  filter(exporter != importer, trade > 0)

## See how many dummy variables we are adding to the OLS model
## There's a high risk of collinearity!

length(levels(gravity2$exp_time)) + length(levels(gravity2$imp_time))

## Adjust 

fe_formula <- as.formula("log_trade ~ log_dist + cntg + lang + clny + 
                         exp_time + imp_time")

model5 <- lm(fe_formula, data = gravity2)

## Check for collinear terms

collinear_terms <- alias(model5)

collinear_matrix <- collinear_terms$Complete

rownames(collinear_matrix)

## Compute clustered standard errors

vcov_cluster5 <- vcovCL(model5, cluster = gravity2[, "pair"], 
                        df_correction = TRUE)

coef_test5 <- tidy(coeftest(
  model5,
  vcov_cluster5[
    which(!grepl("^exp_time|^imp_time", rownames(vcov_cluster5))),
    which(!grepl("^exp_time|^imp_time", colnames(vcov_cluster5)))
  ]
))

coef_test5

summary(model5)$r.squared
