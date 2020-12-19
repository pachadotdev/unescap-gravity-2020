# OLS estimates ----

## Call previous scripts

source("01-packages-and-data.R")
source("02-prepare-data.R")

## Remove 0 flows
## IMPORTANT: If we don't do this, lm fails because log(0) = -Inf

gravity2 <- gravity2 %>%
  filter(exporter != importer, trade > 0)

## Estimate Gravity & Store Results

model1 <- lm(log_trade ~ log_dist + cntg + lang + clny + log_output +
               log_expenditure, data = gravity2)

summary(model1) # no clustered std error here!

## Compute clustered standard errors

vcov_cluster1 <- vcovCL(model1, cluster = gravity2[, "pair"], 
                       df_correction = TRUE)

coef_test1 <- tidy(coeftest(model1, vcov_cluster1))

## Obtain correct (clustered) F distribution based statistics

wald1 <- tidy(waldtest(model1, vcov = vcov_cluster1, test = "F"))
fs1 <- wald1$statistic[2]
fp1 <- wald1$p.value[2]

## Obtain Root MSE

rss1 <- as.numeric(crossprod(model1$residuals))
rmse1 <- sqrt(rss1 / length(model1$residuals))

## Create a list with all the information

length(model1$coefficients) - 1 # df1 for F-statistic
length(unique(gravity2$pair)) - 1 #  df2 for F-statistic

model1_results <- list(
  summary = tibble(
    n_obs = nrow(gravity2),
    f_stat = fs1,
    prob_f = fp1,
    r_sq = summary(model1)$r.squared,
    root_mse = rmse1
  ),
  coefficients = coef_test1
)

model1_results

## Perform RESET test

gravity2 <- gravity2 %>% 
  mutate(fit2 = predict(model1)^2)
 
model2 <- lm(log_trade ~ log_dist + cntg + lang + clny + log_output +
               log_expenditure + fit2, data = gravity2)

vcov_cluster2 <- vcovCL(model2, cluster = gravity2[, "pair"], 
                        df_correction = TRUE)

coef_test2 <- tidy(coeftest(model2, vcov_cluster2))

coef_test2
