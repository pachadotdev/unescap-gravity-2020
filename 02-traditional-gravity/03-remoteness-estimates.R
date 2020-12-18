# Use Remoteness Indexes to control for MRs ----

## Call previous scripts
source("02-traditional-gravity/00-packages-and-data.R")
source("02-traditional-gravity/01-prepare-data.R")

## Create Remoteness indexes

gravity2 <- gravity2 %>%
  # Replicate rem_exp
  group_by(exporter, year) %>%
  mutate(
    rem_exp = log(sum(expenditure * dist) / sum(expenditure))
  ) %>% 
  
  # Replicate rem_imp
  group_by(importer, year) %>%
  mutate(
    rem_imp = log(sum(output * dist) / sum(output))
  ) %>% 
  
  ungroup()

## Adjust 

rem_formula <- as.formula("log_trade ~ log_dist + cntg + lang + clny + 
  log_output + log_expenditure + rem_exp + rem_imp")
model3 <- lm(rem_formula, data = gravity2)

## Compute clustered standard errors

vcov_cluster3 <- vcovCL(model3, cluster = gravity2[, "pair"], 
                        df_correction = TRUE)

coef_test3 <- tidy(coeftest(model3, vcov_cluster3))

## Obtain correct (clustered) F distribution based statistics

wald3 <- tidy(waldtest(model3, vcov = vcov_cluster3, test = "F"))
fs3 <- wald3$statistic[2]
fp3 <- wald3$p.value[2]

## Obtain Root MSE

rss3 <- as.numeric(crossprod(model3$residuals))
rmse3 <- sqrt(rss3 / length(model3$residuals))

## Create a list with all the information

length(model3$coefficients) - 1 # df1 for F-statistic
length(unique(gravity2$pair)) - 1 #  df2 for F-statistic

model3_results <- list(
  summary = tibble(
    n_obs = nrow(gravity2),
    f_stat = fs3,
    prob_f = fp3,
    adj_r_sq = summary(model3)$adj.r.squared,
    root_mse = rmse3
  ),
  coefficients = coef_test3
)

model3_results

## Perform RESET test

gravity2 <- gravity2 %>% 
  mutate(fit2 = predict(model3)^2)

model4 <- lm(update(rem_formula, ~ . + fit2), data = gravity2)

vcov_cluster4 <- vcovCL(model4, cluster = gravity2[, "pair"], 
                        df_correction = TRUE)

coef_test4 <- tidy(coeftest(model4, vcov_cluster4))

coef_test4

