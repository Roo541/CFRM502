library(quantmod)
library(forecast)
library(urca)
library(tseries)
library(BatchGetSymbols)

set.seed(100)
start <- "2016-01-03"
end <- "2023-03-25"
df <- data.frame(ticker1 = character(),
                 ticker2 = character(),
                 kpss = numeric(),
                 one_pct = numeric(), 
                 P = numeric(),
                 rank = numeric())

sp500 <- GetSP500Stocks()
for (i in 1:length(sp500$Tickers)){
  ticker1 <- sp500$Tickers[i]
  x1 <- tryCatch(
    {
      df1 <- getSymbols(c(ticker1), from=start, to=end, env=NULL, timeout=30)
    },
    error = function(e) {
      print('error with ticker')
    }
  )
  if (length(x1)>2){
    
  }
  else{
    Sys.sleep(1)
    next
  }
  for (j in (i+1):length(sp500$Tickers)){
    cat(ticker1, sp500$Tickers[j],"\n")
    ticker2 <- sp500$Tickers[j]

    x2 <- tryCatch(
      {
        df2 <- getSymbols(c(ticker2), from=start, to=end, env=NULL, timeout=30)
      },
      error = function(e) {
        print('error with ticker')
      }
    )
      if (length(x2)>2){
        #setup dataframe of logprices for both tickers
        log.prices <- log(na.omit(merge(df1[,6],df2[,6])))
        #plot(log.prices, legend.loc="top", main ="Log prices")
        names(log.prices) <- c(ticker1, ticker2)
        #fit a linear regression for our given ticker to iterated ticker
        fit <- lm(log.prices[,1]~log.prices[,2])
        
        cc <- coefficients(fit)[[2]]
        #define our new Z function which is our cointegration
        z <- as.numeric(log.prices[,1] - cc*log.prices[,2])
        
        #extract our Philips Olarius and 1pct critical val
        coint.po <- ca.po(log.prices, demean="constant")
        wow <- summary(coint.po)
        one_pct <- wow@cval[3]
        P <- wow@teststat
        value <- kpss.test(z)
        kpss <- value$p.value
        
        if((P > one_pct)&(kpss > 0.1)){
          #plot our new Z function and look for stationarity
          #plot(as.numeric(log.prices[,1] - cc*log.prices[,2]), type="l", ylab="Z", main=sp500$Tickers[i])
          #our intercept from our linear regression model
          #abline(h = coefficients(fit)[[1]], col=2)
          cat(P, one_pct, kpss, "\n")
          rank <- P-one_pct
          df[nrow(df) + 1,] <- c(ticker1=as.character(ticker1),ticker2=as.character(ticker2),
                                 kpss = as.numeric(kpss),
                                 one_pct=as.numeric(one_pct),
                                 P=as.numeric(P), Rank=as.numeric(rank))
          
        }
      }
      else{
        Sys.sleep(1)
        next
    }
  }
}


#write.csv(df, "sp500_2001_2007.csv", row.names = FALSE)















