# Prepare the data ----

## Prepare interval data

gravity2 <- gravity %>% 
  filter(year %in% seq(1986, 2006, 4))

## Create and transform some variables that are needed later

gravity2 <- gravity2 %>% 
  mutate(
    log_trade = log(trade),
    log_dist = log(dist)
  ) %>% 
  
  group_by(exporter, year) %>% 
  mutate(
    output = sum(trade),
    log_output = log(output)
  ) %>% 

  group_by(importer, year) %>% 
  mutate(
    expenditure = sum(trade),
    log_expenditure = log(expenditure)
  ) %>% 
  
  ungroup()

## Create pair ID and symmetric pair ID variables
## IMPORTANT: Here we don't need to create pair_id and symm_id as in Stata

gravity2 <- gravity2 %>% 
  mutate(
    pair = paste(exporter, importer, sep = "_"),
    first = ifelse(exporter < importer, exporter, importer),
    second = ifelse(exporter < importer, importer, exporter),
    symm = paste(first, second, sep = "_")
  )

## Additional step for the lm function: remove 0 flows
## IMPORTANT: If we don't do this, lm fails because log(0) = -Inf

gravity2 <- gravity2 %>% 
  filter(exporter != importer, trade > 0)
