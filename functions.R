#Functions

#HW 3


calc.perc <- function(dat, variable){
  sum = dat[, .N]
  tab <- dat[, .N, by = variable][, Perc := N/sum*100]
  setorderv(tab, cols = "Perc", order = -1)
  return(tab)
}



round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}


engagement_rate_respondent <- function(dat, states.of.engagement, respondent.variables, respond.variable.selection){
  numer <- dat[get(states.of.engagement) == 1 & get(respondent.variables) == respond.variable.selection][, .(Num = .N), by = product.name]
  denoma <- dat[get(respondent.variables) == respond.variable.selection][, .(Denom = .N), by = product.name]
  
  rate = merge(numer, denoma, by = product.name)
  rate = rate[, Rate := Num/Denom *100, by = product.name]
  rate = rate[, lapply(X = .SD, FUN = "round.numerics", digits = 1)]
  rate = rate[, .(Product, Rate)]
  setorderv(x = rate, cols = "Rate", order = -1)
  return(rate[1:5])
  
}



engagement_rate_respondent_two <- function(dat, states.of.engagement, respondent.variables, respond.variable.selection, respondent.variables1, respond.variable.selection1){
  numer <- dat[get(states.of.engagement) == 1 & get(respondent.variables) == respond.variable.selection & get(respondent.variables1) >= respond.variable.selection1][, .(Num = .N), by = product.name]
  denoma <- dat[get(respondent.variables) == respond.variable.selection & get(respondent.variables1) >= respond.variable.selection1 & 
                  (get(states.of.engagement) == 1 | get(states.of.engagement) == 0)][, .(Denom = .N), by = product.name]
  
  rate = merge(numer, denoma, by = product.name)
  rate = rate[, Rate := Num/Denom *100, by = product.name]
  rate = rate[, lapply(X = .SD, FUN = "round.numerics", digits = 1)]
  rate = rate[, .(Product, Rate)]
  setorderv(x = rate, cols = "Rate", order = -1)
  return(rate[1:5])
  
}



invert_func = function(x){
  return(10 - x)
}


#Question 4

engagement_rate_gap <- function(dat, engagement1, engagement2){
  x1 <- dat[, .(X1 = mean(get(engagement1), na.rm = TRUE)), by = Product]
  
  x2 <-dat[, .(X2 = mean(get(engagement2), na.rm = TRUE)), by = Product]
  
  mer <- merge(x1, x2)
  
  
  Gap_df <- mer[, .(Gap = (X1 - X2)*100), by = Product][order(-rank(Gap))][1:5, .(Product, Gap)][,lapply(X = .SD, FUN = "round.numerics", digits = 1)]
  
  ggplot(Gap_df, aes(x = reorder(Product, -Gap), Gap)) + geom_bar(stat = "identity", fill = "steelblue") + labs(x = "Product", y = "% Difference" ,title = "Top 5 Products with largest Gap")
  
}

#Part b


engagement_rate_outcome_gap <- function(dat, engagement1, outcome){
  x1 <- dat[, .(X1 = mean(get(engagement1), na.rm = TRUE)), by = Product]
  
  x2 <-dat[, .(X2 = mean(get(outcome), na.rm = TRUE)/10), by = Product]
  
  mer <- merge(x1, x2)
  
  
  Gap_df <- mer[, .(Gap = (X1 - X2)*100), by = Product][order(-rank(Gap))][1:5, .(Product, Gap)][,lapply(X = .SD, FUN = "round.numerics", digits = 1)]
  
  ggplot(Gap_df, aes(x = reorder(Product, -Gap), Gap)) + geom_bar(stat = "identity", fill = "steelblue") + labs(x = "Product", y = "% Difference" ,title = "Top 5 Products with largest Gap")
}



#Question 5



log_regression <- function(dat, product_type, engagement, alpha, digits ){
  base = dat[, .SD, .SDcols = -c(8:24)][, .SD[1], keyby = "id"]
  
  first = dat[Product != product_type][, .("aggregate_engagement" = mean(get(engagement), na.rm = TRUE)), by = id]
  
  second = dat[Product == product_type, get(engagement), by = id]
  
  final = Reduce(merge, list(first, second, base))
  
  
  mod = glm(V1 ~ age_group + Gender + income_group + Region + Persona + aggregate_engagement, family = "binomial", data = final)
  
  glm.coefs <- as.data.table(summary(mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  setnames(x = glm.coefs, old = "Pr(>|z|)", new = "P-Value")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(kable(glm.coefs[, c("Variable", "P-Value", "Odds.Ratio", "OR.Lower.95", "OR.Upper.95")]))
  
  
  
}



lin_regression <- function(dat, product_type, engagement, digits , alpha ){
  base = dat[, .SD, .SDcols = -c(8:24)][, .SD[1], keyby = "id"]
  
  first = dat[Product != product_type][, .("aggregate_engagement" = mean(get(engagement), na.rm = TRUE)), by = id]
  
  second = dat[Product == product_type, get(engagement), by = id]
  
  final = Reduce(merge, list(first, second, base))
  
  
  mod = lm(V1 ~ age_group + Gender + income_group + Region + Persona + aggregate_engagement, data = final)
  
  lm.coefs <- as.data.table(summary(mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  setnames(x = lm.coefs, old = "Pr(>|t|)", new = "P-Value")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(kable(lm.coefs[, c("Variable", "Estimate", "P-Value", "Coef.Lower.95", "Coef.Upper.95")]))
  
  
  
}


