universal_function_remove_outlier = function(outlier_detection_method_name = "chull-95", input_data)
{
  ################################## Remove Outlier ##############################
  removed_outlier <- input_data
  
  if(outlier_detection_method_name == "Mixture Model")
  {
    pacman::p_load(mclust)
    fitted_vals <- Mclust(input_data[ , c("Term", "yield")])
    threshold = 0.05
    removed_outlier <- input_data[which(fitted_vals$uncertainty > threshold),]
  }
  else if(startsWith(outlier_detection_method_name, "chull"))
  {
    limit = switch(outlier_detection_method_name, "chull-95" = 0.95, "chull-90" = 0.9, "chull-85" = 0.85, "chull-80" = 0.8, "chull-75" = 0.75, "chull-70" = 0.7, "chull-65" = 0.65, "chull-60" = 0.6 )
    dataLen <- length(input_data[,1])
    final_data = input_data
    ratio = 1.0
    while(TRUE){
      chullIdx <- chull(final_data[ , c("Term", "yield")])  
      chullData <- final_data[setdiff(1:length(final_data[,1]),chullIdx),]  
      ratio = length(chullData[,1])/dataLen
      if(ratio > limit){
        final_chull = final_data
        final_data = chullData
        final_chullIdx = chullIdx
      }
      else{
        break
      }
    }
    removed_outlier <- final_data
  }
  else if(outlier_detection_method_name == "Interquartile-Static-Bin")
  {
    sorted_input_data = input_data[with(input_data, order(Term)), ]
    clean_input_data = sorted_input_data[1,]
    for(i in seq(2,50, by = 3)){
      box = sorted_input_data[sorted_input_data$Term >=i & sorted_input_data$Term <= i+3, ]
      quant =  quantile(box$yield)
      iqr = quant[4] - quant[2]
      box = box[(box$yield <= quant[4] + (1.5 * iqr)) & (box$yield >= (quant[2] - 1.5 * iqr)),]
      clean_input_data =  rbind(clean_input_data, box)
    }
    removed_outlier <- clean_input_data[-c(1),]
  }
  else if(outlier_detection_method_name == "Interquartile-Dynamic-Bin")
  {
    sorted_input_data = input_data[with(input_data, order(Term)), ]
    clean_dynamic = sorted_input_data[1,]
    boxSize = 100
    for(i in seq(boxSize,length(sorted_input_data[,1])-boxSize, by = boxSize)){
      box = sorted_input_data[i:(i+boxSize-1), ]
      quant =  quantile(box$yield)
      iqr = quant[4] - quant[2]
      box = box[(box$yield <= quant[4] + (1.5 * iqr)) & (box$yield >= (quant[2] - 1.5 * iqr)),]
      clean_dynamic =  rbind(clean_dynamic, box)
    }
    removed_outlier <- clean_dynamic[-c(1),]
  }
  return(removed_outlier)
}

universal_function_regression = function(regression_method_name, input_data)
{
  ################################## Regression ##############################
  
  if(regression_method_name == "lm")
  {
    pacman::p_load(car)
    btw <- boxTidwell(yield ~ Term ,data=input_data)
    lmFit <- lm(yield~I(Term ^ btw$result[3]),data = input_data)
    return(cbind2(input_data$Term, fitted(lmFit)))
  }
  else if(regression_method_name == "rlm")
  {
    pacman::p_load(MASS)
    btw <- boxTidwell(yield ~ Term ,data=input_data)
    rlmFit <- rlm(yield~I(Term ^ btw$result[3]),data = input_data)
    return(cbind2(input_data$Term, fitted(rlmFit)))
  }
  else if(regression_method_name == "loess")
  {
    loess_result <- loess(input_data$yield ~ input_data$Term, span = 0.6, degree = 2, family="symmetric" )
    return(cbind2(loess_result$x, loess_result$fitted))
  }
  else if(cbind2(regression_method_name =="KernelSmoothing"))
  {
    pacman::p_load(KernSmooth)
    ksmooth_result <- ksmooth(x = input_data$Term,y = input_data$yield, 
                              bandwidth = 10, range.x = c(min(input_data[,1]),max(input_data[,1])))
    return(cbind2(ksmooth_result$x, ksmooth_result$y))
  }
  else if(regression_method_name == "locpoly")
  {
    pacman::p_load(KernSmooth)
    locpoly_result <- locpoly(x = input_data$Term,y = input_data$yield, 
                              bandwidth = 30, range.x = c(min(input_data[,1]),max(input_data[,1])),
                              degree = 3, bwdisc = 25, gridsize =400)
    return(cbind2(locpoly_result$x, locpoly_result$y))
  }
  else if(regression_method_name == "ridge")
  {
    pacman::p_load(glmnet)
    ts = cbind(input_data$Term,input_data$Term^2, input_data$Term^3)
    glmfit = cv.glmnet(ts , input_data$yield, alpha = 0, lambda = 10^seq(10, -10, by = -.1))
    opt_lambda = glmfit$lambda.min
    
    ridge_vals = rbind()
    min_term = min(input_data$Term);
    max_term = max(input_data$Term);
    for(i in seq(min_term,max_term,0.01))
    {
      ridge_vals = rbind(ridge_vals, predict(glmfit, s = opt_lambda, newx = cbind(i,i^2,i^3)))
    }
    return(cbind2(seq(min_term,max_term,0.01), ridge_vals[,1]))
  }
  else if(regression_method_name == "Capital IQ")
  {
    library(sqldf)
    ticker_names <- sqldf("SELECT Ticker FROM input_data GROUP BY Ticker Having COUNT(1) > 3 ")
    DataByTicker <- sqldf("SELECT * FROM input_data WHERE Ticker IN ticker_names ORDER BY Term")
    
    minValTerm = min(DataByTicker$Term)
    maxValTerm = max(DataByTicker$Term)
    stepTerm = 0.5
    
    final_value = data.frame(Term = 0, yield = 0)
    
    for(i in seq(minValTerm, maxValTerm, stepTerm)){
      avg_val = 0
      counter = 0
      for(ticker_name in ticker_names[,1]){
        vals = DataByTicker[DataByTicker$Ticker == ticker_name,] 
        for(term in seq(1,length(vals[,1])-1,1)){
          if(i >= vals[term,1] && i <= vals[term + 1, 1]){
            avg_val = avg_val + ((vals[term + 1, 2] - vals[term, 2])*(i-vals[term, 1])/(vals[term + 1, 1] - vals[term, 1])) + vals[term, 2]
            counter = counter + 1
            break
          }
        }
      }
      final_value = rbind(final_value, c(i, avg_val/counter))
    }
    final_value = final_value[-c(1),]
    return(cbind2(final_value$Term, final_value$yield))
  }
}
double_smoothing = function(data)
{
  first_loess = loess(data[,2]~data[,1])
  second_loess = loess(first_loess$fitted~first_loess$x)
  return(cbind2(second_loess$x, second_loess$fitted))
}

fully_smooth = function(data){
  i=0
  while(!identical(data, double_smoothing(data)) & i < 4){
  data = double_smoothing(data)
  i = i + 1
  }
  return(data)
}


# result = universal_function("Mixture Model", "loess", BBB1Data)
# result = result[order(result[,1]),]
# 
# 
# pacman::p_load(ggplot2)
# ggplot() +
#   geom_point(aes(x = result[,1], y = result[,2], colour = "result point")) + 
#   geom_line(aes(x = result[,1], y = result[,2], colour = "result")) + 
#   geom_line(aes(x = Bloomberg_data$Term, y = Bloomberg_data$`BS187 Mid Yld USD US INDUSTRIALS BBB+ BVAL YIELD CURVE 12/30/16 CUSIP`, colour = "Bloomberg BBB+")) + 
#   geom_line(aes(x = Bloomberg_data$Term, y = Bloomberg_data$`BS188 Mid Yld USD US INDUSTRIALS BBB- BVAL YIELD CURVE 12/30/16 CUSIP`, colour = "Bloomberg BBB-")) 
# 
