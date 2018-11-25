
# Load libraries ----------------------------------------------------------

library(readxl)
require(ggplot2)
library(lubridate)
library(pracma)
library(outliers)
library(cluster)
library(pspline)
library(YieldCurve)
library(NMOF)
library(dplyr)
select = dplyr::select

# Source functions --------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("LoadFREDData2.R")
source("UniversalFunction2.R")
#setwd("../resources")



# Load External Data and build ML data ------------------------------------------------------

Bloomberg_data = read_excel("C:/Users/soleysi/Desktop/Channel 1/Facebook_IBR_14 September 2018/US USD Composite Curves.xlsx", sheet = "10-12-2018")
Bloomberg_data$Term = c(0.25,0.5,1,2,3,4,5,6,7,8,9,10,12,15,20,25,30)

### for a Company Specific Bond
# ML = read_xlsx(path = "Eikon IBR Bond Template_v2.xlsx", skip = 4,col_types = c("text", "text", "text","numeric","text","numeric", "date", "date", "numeric","numeric","numeric","text","text","text","text","text"))

### for Eikon Bond universe
ML = read_xlsx(path = "C:/Users/soleysi/Desktop/Channel 1/Facebook_IBR_14 September 2018/Eikon OAS Model_v2.xlsm", skip = 1, sheet = "Eikon Index",
               col_types = c("text", "skip", "skip", "text", "text","text","date", "text","text", "numeric","numeric", "logical", "text","numeric", "text", "text", "date","text","text","text", "text","text","text","text","numeric",rep("skip",15)))

#ValDate = ymd("2018-10-26")
ValDate = ymd(read_xlsx(path = "C:/Users/soleysi/Desktop/Channel 1/Facebook_IBR_14 September 2018/Eikon OAS Model_v2.xlsm", 
                        sheet = "Summary", range = "F8:F9", col_types = "date")[[1]])

ML = ML[complete.cases(ML$Maturity),] # remove last full NAs

### for Yan's version which has liquidity filters
# ML = read_xlsx(path = "C:/Users/soleysi/Desktop/Channel 1/Facebook_IBR_14 September 2018/clean data multiple dates.xlsx", skip = 0,
#                col_types = c("text", "skip", "skip", "text", "text","text","date", "text","text", "numeric","numeric", "logical", "text","numeric", "text", "text", "date","text","text","text", "text","text","text","text","numeric", "date", "numeric","numeric","text", "text", "text","text","text", "text","text","text","text","text","text"))



colnames(ML) = make.names(colnames(ML), unique = TRUE)
colnames(ML)
ML$Maturity = as.Date(ML$Maturity, format = "%m/%d/%Y")
ML$Term = round(as.numeric((ML$Maturity - ValDate) / 365.25),2)

# ML_rated = ML %>%
#   # filter(Analysis.Date == as.Date("2018-09-28"))%>%          # for Yan's version
#   # filter(Traded.1W == "KEEP") %>%              
#   filter(OAS > 0, Seniority == "Sr Unsecured", Term <=30) %>%
#   # filter(Sector == "Industrials") %>%
#   dplyr::select(Identifier, Issuer, Ticker, Rating = Composite.Rating, OAS, Term)
# 
# ML_rated = arrange(as.data.frame(ML_rated[complete.cases(ML_rated),]),Term)
# 
# ML_rated =add_rf(ML_rated, ValDate = ValDate)#, cbind2(ML_rated$Composite.Rating, ML_rated$Ticker))

ML = ML %>%
  # filter(Analysis.Date == as.Date("2018-09-28"))%>%          # for Yan's version
  # filter(Traded.1W == "KEEP") %>%              
  filter(OAS > 0, Seniority == "Sr Unsecured") %>%
  filter(Term <=30) %>%
  dplyr::select(Identifier, Issuer, Ticker, Sector, Industry, Issue.Date, Coupon, Maturity, Term, OAS, Rating = Composite.Rating) %>%
  filter(complete.cases(.)) %>%
  as.data.frame() %>%
  arrange(Term) %>%
  add_rf(ValDate = ValDate) %>%
  filter(Sector %in% c("Industrials","Financial")) %>%
  dplyr::select(Identifier, Issuer, Ticker, Sector, Industry, Issue.Date, Coupon, Maturity, Term, yield , Rating)

# all scenarios
RatingTable = c("AAA", "AA.overall", "AA+", "AA", "AA-", "A.overall", "A+", "A", "A-", "BBB.overall", "BBB+", "BBB", "BBB-", "BB.overall", "B.overall","BB+", "BB", "BB-", "B+", "B", "B-")
RegressionMethods = c(#"Ridge", 
  "Loess", "CapitalIQ", "Spline", "NS", "Midpoint","Bloomberg")

# creating empty data frame to store data, 4th line set the Term as you wish
OutputNames = paste(rep(RatingTable, each = length(RegressionMethods)), RegressionMethods,sep = ".")
output <- as.data.frame(matrix(data = NA, nrow= length(Bloomberg_data$Term), ncol = 1 + length(OutputNames)))
names(output) = c("Term",OutputNames)
output$Term = Bloomberg_data$Term


# Code body ---------------------------------------------------------------

##### Method 1: old by removing outliers
for(SelectedRating in  RatingTable){
  
  ImpliedRating = case_when(
    SelectedRating == "AA.overall" ~ c("AA+", "AA", "AA-"),
    SelectedRating == "A.overall" ~ c("A+", "A", "A-"),
    SelectedRating == "BBB.overall" ~ c("BBB+", "BBB", "BBB-"),
    SelectedRating == "BB.overall" ~ c("BB+", "BB", "BB-"),
    SelectedRating == "B.overall" ~ c("B+", "B", "B-"),
    # SelectedRating == "CCC.overall" ~ c("CCC+", "CCC", "CCC-"),
    TRUE ~ SelectedRating
  )
  
  #SelectedRating %in% c("B-", "B", "B+","BB-","BB","BB+")
  
  if (!SelectedRating %in% c("B-", "B", "B+","BB-","BB","BB+")){
    ML_rated = ML %>% 
      filter(Rating %in% ImpliedRating)
    
    
    # keep last 90% observations
    outlier_threshold = 0.6
    # outlier_addback = (1- outlier_threshold)/4
    outlier_method = paste("chull-",100*outlier_threshold,sep = "")
    
    ML_rated2 = universal_function_remove_outlier(outlier_detection_method_name = outlier_method, input_data = ML_rated)
    
    
    ML_deleted = ML_rated %>%
      anti_join(ML_rated2) %>%
      # filter(!Rating %in% c('B-','B','B+','BB-','BB','BB+') | Term<12) %>%
      arrange(yield) %>%
      xts::last(round((nrow(ML_rated)*(1- outlier_threshold))/4))
    
    ML_rated_clean = ML_rated %>%
      anti_join(ML_deleted)
    
    # ggplot() +
    #   geom_point(aes(x = ML_rated_clean$Term, y = ML_rated_clean$yield , colour = "Clean Data")) + 
    #   geom_point(aes(x = ML_deleted$Term, y = ML_deleted$yield , colour = "Deleted Data")) 
    
    
    ### So the trick here is to mirror the last XX points on the 30-year term to make it flat and extend to 30 years. the more XX flatter.
    XX = round(nrow(ML_rated_clean)/40)
    last_XX_points = data.frame(Term = 30, yield = xts::last(ML_rated_clean$yield,XX), Ticker = xts::last(ML_rated_clean$Ticker,XX))
    ML_rated_clean2 = arrange(ML_rated_clean,yield)
    first_XX_points = data.frame(Term = jitter(rep(0,XX)), yield = first(ML_rated_clean2$yield,XX), Ticker = first(ML_rated_clean2$Ticker,XX))
    ML_rated_extended = rbind(first_XX_points, ML_rated_clean[,c("Term","yield","Ticker")], last_XX_points)
    
    ### uncomment to draw but not inside the loop
    # ggplot() +
    #   geom_point(aes(x = ML_rated_extended$Term, y = ML_rated_extended$yield , colour = "to be Regerssed Data")) 
    
    ### uncomment to draw but  not inside the loop
    # ggplot() +
    #   geom_point(aes(x = ML_rated$Term, y = ML_rated$yield , colour = "Original Data")) +
    #   geom_point(aes(x = ML_rated_extended$Term, y = ML_rated_extended$yield , colour = "Extended Data"))
    
    # 
    # last_points_20years = ML_rated %>%
    #   filter(Term>=15) %>%
    #   mutate(Ticker = "XXXX", Term = 50)
    # 
    # 
    # ML_rated_extended = rbind(ML_rated, last_points_20years)
    
    
    ##### Method 2: dominant ticker
    
    # b = ML %>%
    #   filter(Sector == "Industrials") %>%
    #   group_by(Rating, Ticker) %>%
    #   summarise(count = n()) %>%
    #   group_by(Rating) %>%
    #   top_n(1)
    # 
    # ML_rated = ML %>%
    #   filter(Ticker %in% b$Ticker) %>%
    #   filter(Rating == "B-") %>%
    #   select(Term,yield,Ticker)
    # 
    # last_XX_points = data.frame(Term = 30, yield = last(ML_rated$yield,1), Ticker = "XXXX")
    # ML_rated_extended = rbind(ML_rated, last_XX_points)
    
    NSParameters <- Nelson.Siegel(rate = as.xts(x = ML_rated_extended$yield/100, order.by = ValDate + 365.25*ML_rated_extended$Term), maturity = ML_rated_extended$Term);
    #yM <- NS(NSParameters[1,],Bloomberg_data$Term*12);
    my_timestep = seq(1/12,30, 1/12)
    yM <- NSrates(xts(NSParameters,order.by = ValDate) , maturity = my_timestep);
    
    # Ml_regress_ridge = ML_rated_extended %>% 
    #   universal_function_regression(regression_method_name = "ridge")
     
    Ml_regress_loess = universal_function_regression(regression_method_name = "loess",input_data = ML_rated_extended)
    
    
    Ml_regress_capIQ = double_smoothing(universal_function_regression(regression_method_name = "Capital IQ",input_data = ML_rated_extended))
    
    ML_rated_extended = arrange(ML_rated_extended, Term)
    #Ml_regress_spline = pspline::sm.spline(x = ML_rated_extended$Term, y = ML_rated_extended$yield, norder = 2)
    Ml_regress_spline_new = smooth.spline(x = ML_rated_extended$Term, y = ML_rated_extended$yield, cv= TRUE, df = 4)
    #Ml_regress_spline_ext = predict(Ml_regress_spline, x = seq(1,30,0.5))
    
    #Ml_regress_spline2 = cbind2(Ml_regress_spline$x,as.numeric(Ml_regress_spline$ysmth))
    Ml_regress_spline2 = cbind2(Ml_regress_spline_new$x,as.numeric(Ml_regress_spline_new$y))
    
    # output[paste0(SelectedRating,".","Ridge")] = approx(x = Ml_regress_ridge[,1], y = Ml_regress_ridge[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Loess")] = approx(x = Ml_regress_loess[,1], y = Ml_regress_loess[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","CapitalIQ")] = approx(x = Ml_regress_capIQ[,1], y = Ml_regress_capIQ[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Spline")] = approx(x = Ml_regress_spline2[,1], y = Ml_regress_spline2[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","NS")] = approx(x = my_timestep, y = as.numeric(yM)*100, xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Bloomberg")] = NA
    output[paste0(SelectedRating,".","Midpoint")] = output %>%
      select(starts_with(paste0(SelectedRating,"."))) %>%
      apply(MARGIN = 1, FUN = median, na.rm = TRUE)
    try({output[paste0(SelectedRating,".","Bloomberg")] = Bloomberg_data[SelectedRating]*100}, silent = TRUE)
    
    print(
      ggplot() +
        geom_point(aes(x = ML_rated_clean$Term, y = ML_rated_clean$yield , colour = "Original Data")) + 
        # geom_line(aes(x = Ml_regress_ridge[,1], y = Ml_regress_ridge[,2] , colour = "Ridge Regression")) +
        geom_line(aes(x = Ml_regress_loess[,1], y = Ml_regress_loess[,2] , colour = "Loess Regression")) +
        geom_line(aes(x = Ml_regress_capIQ[,1], y = Ml_regress_capIQ[,2] , colour = "Capital IQ")) +
        geom_line(aes(x = Ml_regress_spline2[,1], y = Ml_regress_spline2[,2] , colour = "Spline")) +
        # geom_line(aes(x = seq(0,30,0.5), y = Ml_regress_spline_ext),linetype = 3, colour = "Spline Prediction") +
        geom_line(aes(x = output$Term, y= output[,paste0(SelectedRating,".","Midpoint")], colour = "Midpoint")) +
        geom_line(aes(x = my_timestep, y = as.numeric(yM)*100, colour = "Nelson-Siegel")) +
        #geom_line(aes(x = Bloomberg_data$Term, y= Bloomberg_data[[SelectedRating]]*100, colour = "Bloomberg")) +
        labs(x = "Term", y = "Yield", title = paste(as.character(SelectedRating),"Yield curve with",outlier_threshold,"oulier removal"))
    )
    
  } else if (SelectedRating %in% c("B-", "B", "B+","BB-","BB","BB+")){
    overall = case_when(
      SelectedRating %in% c("B-", "B", "B+") ~ "B.overall",
      SelectedRating %in% c("BB-", "BB", "BB+") ~ "BB.overall"
    )
    
    if(sum(is.na(output[paste0(overall,".Midpoint")]))!=0){warning("overall data empty")}  
    
    ML_rated = ML %>% 
      filter(Rating %in% SelectedRating)
    
    
    # keep last 90% observations
    outlier_threshold = 0.6
    # outlier_addback = (1- outlier_threshold)/4
    outlier_method = paste("chull-",100*outlier_threshold,sep = "")
    
    ML_rated2 = universal_function_remove_outlier(outlier_detection_method_name = outlier_method, input_data = ML_rated)
    
    
    ML_deleted = ML_rated %>%
      anti_join(ML_rated2) %>%
      # filter(!Rating %in% c('B-','B','B+','BB-','BB','BB+') | Term<12) %>%
      arrange(yield) %>%
      xts::last(round((nrow(ML_rated)*(1- outlier_threshold))/3))
    
    ML_rated_clean = ML_rated %>%
      anti_join(ML_deleted)
    
    ### uncomment to draw but only drwas outside loop
    # ggplot() +
    #   geom_point(aes(x = ML_rated_clean$Term, y = ML_rated_clean$yield , colour = "Clean Data")) + 
    #   geom_point(aes(x = ML_deleted$Term, y = ML_deleted$yield , colour = "Deleted Data")) 
    
    ### So the trick here is to mirror the last XX points on the 30-year term to make it flat and extend to 30 years. the more XX flatter.
    XX = round(nrow(ML_rated_clean)/40)
    last_XX_points = data.frame(Term = 30, yield = xts::last(ML_rated_clean$yield,XX), Ticker = xts::last(ML_rated_clean$Ticker,XX))
    ML_rated_clean2 = arrange(ML_rated_clean,yield)
    first_XX_points = data.frame(Term = jitter(rep(0,XX)), yield = first(ML_rated_clean2$yield,XX), Ticker = first(ML_rated_clean2$Ticker,XX))
    complimentary_points = as.data.frame(approx(x = output$Term, y = output[[paste0(overall,".Midpoint")]], xout = seq(10, 30, length.out = nrow(ML_rated_clean)/8),method = "linear",rule = 2)) %>% mutate(Ticker = "Complimentary")
    colnames(complimentary_points) = colnames(first_XX_points)
    ML_rated_extended = rbind(first_XX_points, ML_rated_clean[,c("Term","yield","Ticker")], last_XX_points, complimentary_points)
    ML_rated_extended = arrange(ML_rated_extended, Term)
    
    ### uncomment to draw but only drwas outside loop
    # ggplot() +
    #   geom_point(aes(x = ML_rated_extended$Term, y = ML_rated_extended$yield , colour = "to be Regerssed Data")) 
    
    
    NSParameters <- Nelson.Siegel(rate = as.xts(x = ML_rated_extended$yield/100, order.by = ValDate + 365.25*ML_rated_extended$Term), maturity = ML_rated_extended$Term);
    #yM <- NS(NSParameters[1,],Bloomberg_data$Term*12);
    my_timestep = seq(1/12,30, 1/12)
    yM <- NSrates(xts(NSParameters,order.by = ValDate) , maturity = my_timestep);
    
    # Ml_regress_ridge = ML_rated_extended %>% 
    #   universal_function_regression(regression_method_name = "ridge")
    
    Ml_regress_loess = double_smoothing(universal_function_regression(regression_method_name = "loess",input_data = ML_rated_extended))
    
    
    Ml_regress_capIQ = double_smoothing(universal_function_regression(regression_method_name = "Capital IQ",input_data = ML_rated_extended))
    
    ML_rated_extended = arrange(ML_rated_extended, Term)
    #Ml_regress_spline = pspline::sm.spline(x = ML_rated_extended$Term, y = ML_rated_extended$yield, norder = 2)
    Ml_regress_spline_new = smooth.spline(x = ML_rated_extended$Term, y = ML_rated_extended$yield, cv= TRUE,df = 4)
    #Ml_regress_spline_ext = predict(Ml_regress_spline, x = seq(1,30,0.5))
    
    #Ml_regress_spline2 = cbind2(Ml_regress_spline$x,as.numeric(Ml_regress_spline$ysmth))
    Ml_regress_spline2 = cbind2(Ml_regress_spline_new$x,as.numeric(Ml_regress_spline_new$y))
    
    # output[paste0(SelectedRating,".","Ridge")] = approx(x = Ml_regress_ridge[,1], y = Ml_regress_ridge[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Loess")] = approx(x = Ml_regress_loess[,1], y = Ml_regress_loess[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","CapitalIQ")] = approx(x = Ml_regress_capIQ[,1], y = Ml_regress_capIQ[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Spline")] = approx(x = Ml_regress_spline2[,1], y = Ml_regress_spline2[,2],xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","NS")] = approx(x = my_timestep, y = as.numeric(yM)*100, xout = output$Term, method = "linear", rule = 2)$y
    output[paste0(SelectedRating,".","Bloomberg")] = NA
    output[paste0(SelectedRating,".","Midpoint")] = output %>%
      select(starts_with(paste0(SelectedRating,"."))) %>%
      apply(MARGIN = 1, FUN = median, na.rm = TRUE)
    try({output[paste0(SelectedRating,".","Bloomberg")] = Bloomberg_data[SelectedRating]*100}, silent = TRUE)
    
    
    print(
      ggplot() +
        geom_point(aes(x = ML_rated_clean$Term, y = ML_rated_clean$yield , colour = "Original Data"))+ 
        # geom_line(aes(x = Ml_regress_ridge[,1], y = Ml_regress_ridge[,2] , colour = "Ridge Regression")) +
        geom_line(aes(x = Ml_regress_loess[,1], y = Ml_regress_loess[,2] , colour = "Loess Regression")) +
        geom_line(aes(x = Ml_regress_capIQ[,1], y = Ml_regress_capIQ[,2] , colour = "Capital IQ")) +
        geom_line(aes(x = Ml_regress_spline2[,1], y = Ml_regress_spline2[,2] , colour = "Spline")) +
        # geom_line(aes(x = seq(0,30,0.5), y = Ml_regress_spline_ext),linetype = 3, colour = "Spline Prediction") +
        geom_line(aes(x = output$Term, y= output[,paste0(SelectedRating,".","Midpoint")], colour = "Midpoint")) +
        geom_line(aes(x = my_timestep, y = as.numeric(yM)*100, colour = "Nelson-Siegel")) +
        # geom_line(aes(x = Bloomberg_data$Term, y= Bloomberg_data[[SelectedRating]]*100, colour = "Bloomberg")) +
        labs(x = "Term", y = "Yield", title = paste(as.character(SelectedRating),"Yield curve with",outlier_threshold,"oulier removal"))
    )
    
  }
}

output_midpoints = output %>%
  select_at(vars(matches(paste(c(".Midpoint", "Bloomberg"), collapse= "|")))) %>%
  select(-ends_with("overall.Midpoint")) 

output_midpoints = cbind(output$Term, output_midpoints)
colnames(output_midpoints)[1] = "Term"

output_midpoints_sorted = output_midpoints %>%
  select(-ends_with(".Bloomberg")) 

### restrict by sorting on rows to make sure AAA is lowest and ... B- highest
output_midpoints_sorted[,-1] = t(apply(output_midpoints_sorted[-1], MARGIN = 1, FUN=sort))

### All Rating regressed except overall ratings
AllRating = RatingTable[-grep("\\.",RatingTable)]

# for (r in AllRating){
#   print(
#     ggplot() + 
#       geom_line(aes(x = output$Term, y= select(output_midpoints_sorted, starts_with(paste0(r,".Midpoint")))[,1] , colour = "Midpoint")) +
#       geom_line(aes(x = output$Term, y= select(output_midpoints, starts_with(paste0(r,".Bloomberg")))[,1], colour = "Bloomberg")) +
#       labs(x = "Term", y = "Yield", title = paste(as.character(r),"Yield curve without fully smoothing and correction"))
#     
#   )}

### Do final round of smoothing
# my_timestep = seq(1/12,30, 1/12)
output_final = data.frame(Term = Bloomberg_data$Term)
for (r in AllRating){
  NSParameters <- Nelson.Siegel(rate = as.xts(x = output_midpoints_sorted[paste0(r,".Midpoint")]/100, order.by = ValDate + 365.25*output_midpoints_sorted$Term), maturity = output_midpoints_sorted$Term);
  output_final[,paste0(r,".Final")] <- as.numeric(NSrates(xts(NSParameters,order.by = ValDate) , maturity = output_final$Term))
}
output_final[,-1] = t(apply(output_final[-1], MARGIN = 1, FUN=sort))


for (r in AllRating){
  print(
    ggplot() + 
      #geom_point(data = ML %>% filter(Rating == r), aes(x = Term, y = yield, color = "original data")) +
      #geom_line(aes(x = output$Term, y= select(output_midpoints_sorted, starts_with(paste0(r,".Midpoint")))[,1] , colour = "Midpoint")) +
      geom_line(aes(x = output_final$Term, y= select(output_final, starts_with(paste0(r,".Final")))[,1]*100 , colour = "Final")) +
      geom_line(aes(x = output$Term, y= select(output_midpoints, starts_with(paste0(r,".Bloomberg")))[,1], colour = "Bloomberg")) +
      labs(x = "Term", y = "Yield", title = paste(as.character(r),"Yield curve without fully smoothing and correction")) +
      ylim(c(200,1000))
    
  )}

### DRAW ALL
myoutput_midpoints = melt(data = output_final, id = "Term")
ggplot() + geom_line(data = myoutput_midpoints, aes(x = Term, color = variable, y = value)) 





