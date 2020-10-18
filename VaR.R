#####################################################################
# Value at Risk & Expected Shortfall for historical simulation      #
# input data:                                                       #           
# r          <- returns                                             #
# p          <- significance level                                  #
# h          <- time horizon (in days)                              #                                           
#####################################################################

VaRhist <- function(r, p, h=1){
  
  rVaR     <- sort(r)              
  NVaR     <- floor(length(rVaR)*p)      # observation for p-quantile                 
  VaR      <- -rVaR[NVaR]*sqrt(h)         # p-quantile - VaR
  rES      <- rVaR[1:NVaR]               # returns below VaR
  ES       <- -mean(rES)*sqrt(h)          # average of returns below VaR - Expected Shortfall
  return( list(VaR = VaR, ES = ES) )
}

VaRhist(rAAPL, 0.01)

#####################################################################
# Value at Risk & Expected Shortfall for normal distribution        #
# input data:                                                       #           
# r          <- returns                                             #
# p          <- significance level                                  #
# h          <- time horizon (in days)                              #                                           
#####################################################################

VaR_N <- function(r, p, h=1){
  m <- mean(r)                          # mean of returns
  s <- sd(r)                            # standard deviation of returns
  
  VaR_N     <- (-m - s*qnorm(p))*sqrt(h)  
  ES_N      <- (-m + s*(dnorm(qnorm(p))/p))*sqrt(h)
  
  return( list(VaR_N = VaR_N, ES_N = ES_N) )
}

VaR_N(rAAPL, 0.01, 1)

#####################################################################
# Value at Risk & Expected Shortfall for t-student distribution     #
# input data:                                                       #           
# r          <- returns                                             #
# p          <- significance level                                  #
# df         <- degrees of freedom (default - 5)                    #                                           
# h          <- time horizon (in days)                              #                                           
#####################################################################

VaR_t <- function(r, p, df=5, h=1){
  m <- mean(r)                          # mean of returns
  s <- sd(r)                            # standard deviation of returns
  
  VaR_t     <- (-m - s*qt(p,df) * sqrt((df-2)/df))*sqrt(h)     
  ES_t       <- -m + s*((pt(qt(p,df),df))/(p))*((df + qt(p,df)^2)/(df-1))
  return( list(VaR_t = VaR_t, ES_t = ES_t) )
}

VaR_t(rAAPL, 0.01)