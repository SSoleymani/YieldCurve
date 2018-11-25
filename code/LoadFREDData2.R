library(readxl)
require(ggplot2)
require(dplyr)
library(lubridate)
library(pracma)


### bring in risk free rate: first col should be term, second col should be spread
fetch_rf = function(T, ValDate) {
  pacman::p_load(quantmod)
  pacman::p_load(pracma)
  valdate = ValDate
  #valdate = as.Date(ValDate,origin = "1899-12-30")
  
  data1MO = getSymbols(Symbols = 'DGS1MO', src = "FRED", auto.assign = FALSE)
  data3MO = getSymbols(Symbols = 'DGS3MO', src = "FRED", auto.assign = FALSE)
  data6MO = getSymbols(Symbols = 'DGS6MO', src = "FRED", auto.assign = FALSE)
  data1 = getSymbols(Symbols = 'DGS1', src = "FRED", auto.assign = FALSE)
  data2 = getSymbols(Symbols = 'DGS2', src = "FRED", auto.assign = FALSE)
  data3 = getSymbols(Symbols = 'DGS3', src = "FRED", auto.assign = FALSE)
  data5 = getSymbols(Symbols = 'DGS5', src = "FRED", auto.assign = FALSE)
  data7 = getSymbols(Symbols = 'DGS7', src = "FRED", auto.assign = FALSE)
  data10 = getSymbols(Symbols = 'DGS10', src = "FRED", auto.assign = FALSE)
  data20 = getSymbols(Symbols = 'DGS20', src = "FRED", auto.assign = FALSE)
  data30 = getSymbols(Symbols = 'DGS30', src = "FRED", auto.assign = FALSE)
  
  data_full = merge(data1MO, data3MO, data6MO, data1, data2, data3, data5, data7, data10, data20, data30, all = TRUE)
  rf = data_full[valdate]
  Term = c(0.125, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
  selected_rf = interp1(x = Term, y = as.numeric(rf), xi = T)
  return(selected_rf / 100)
}

fetch_rf_full = function(ValDate) {
  pacman::p_load(quantmod)
  pacman::p_load(pracma)
  valdate = ValDate
  #valdate = as.Date(ValDate,origin = "1899-12-30")
  
  data1MO = getSymbols(Symbols = 'DGS1MO', src = "FRED", auto.assign = FALSE)
  data3MO = getSymbols(Symbols = 'DGS3MO', src = "FRED", auto.assign = FALSE)
  data6MO = getSymbols(Symbols = 'DGS6MO', src = "FRED", auto.assign = FALSE)
  data1 = getSymbols(Symbols = 'DGS1', src = "FRED", auto.assign = FALSE)
  data2 = getSymbols(Symbols = 'DGS2', src = "FRED", auto.assign = FALSE)
  data3 = getSymbols(Symbols = 'DGS3', src = "FRED", auto.assign = FALSE)
  data5 = getSymbols(Symbols = 'DGS5', src = "FRED", auto.assign = FALSE)
  data7 = getSymbols(Symbols = 'DGS7', src = "FRED", auto.assign = FALSE)
  data10 = getSymbols(Symbols = 'DGS10', src = "FRED", auto.assign = FALSE)
  data20 = getSymbols(Symbols = 'DGS20', src = "FRED", auto.assign = FALSE)
  data30 = getSymbols(Symbols = 'DGS30', src = "FRED", auto.assign = FALSE)
  
  data_full = merge(data1MO, data3MO, data6MO, data1, data2, data3, data5, data7, data10, data20, data30, all = TRUE)
  rf = as.numeric(data_full[valdate])
  Term = c(0.125, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
  rf_table = data.frame(Term = Term, rf = rf)
  return(rf_table)
}

add_rf = function(df_term_spreads, ValDate) {
  if (!(is.Date(ValDate) & is.data.frame(df_term_spreads))) {
    break ("Plaese revisit inputs structure")
  }
  pacman::p_load(xts)
  data_full = fetch_rf_full(ValDate = ValDate)
  # qxts <- xts(data_full[, -1], order.by = as.POSIXct(data_full$Term))
  # data_full = as.xts(data_full)
  rf = as.numeric(data_full[, -1])
  Term = c(0, 0.125, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30,150)
  rf_table = data.frame(Term = Term, rf = c(rf[1], rf , rf[length(rf)]))
  df_term_spreads[, "rf"] = interp1(x = rf_table$Term, y = rf_table$rf, xi = as.numeric(df_term_spreads$Term)) * 100
  df_term_spreads[, "yield"] = df_term_spreads$OAS + df_term_spreads$rf
  return(df_term_spreads)
}
