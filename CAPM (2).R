library(quantmod)
library(dplyr)
library(ggplot2)

##### CAPM and Market Line 


MARKETLINE = function(stocks){
  getSymbols("^GDAXI", src = "yahoo", from = "2024-01-01", to = "2025-01-01", periodicity = "daily")
  adjusted_prices_market <- Ad(GDAXI)
  returns_market <- monthlyReturn(adjusted_prices_market)
  returns_market <- na.omit(returns_market)
  risk_free_rate <- 0.001
  MRP <- mean(returns_market) - risk_free_rate
  Adj_P_list <- list()
  returns_list <- list()
  BETA_list <- list()
  EXPECTED_RETURN_list <- list()
  for (stock in stocks){
    getSymbols(stock, src = "yahoo", from = "2024-01-01", to = "2025-01-01", periodicity = "daily")
    Adj_P_list[[stock]] <- Ad(get(stock))
    returns_list[[stock]] <- monthlyReturn(Adj_P_list[[stock]])
    returns_list[[stock]] <- na.omit(returns_list[[stock]])
    min_length <- min(nrow(returns_list[[stock]]), nrow(returns_market))
    aligned_stock_returns <- tail(returns_list[[stock]], min_length)
    aligned_market_returns <- tail(returns_market, min_length)
    BETA_list[[stock]] <- cov(aligned_stock_returns,aligned_market_returns)/var(aligned_market_returns)
    EXPECTED_RETURN_list[[stock]] <- risk_free_rate + BETA_list[[stock]]*MRP
  }
  MARKET_LINE <- data.frame(Stock = stocks,
                            Beta = unlist(BETA_list),
                            Expected_return = unlist(EXPECTED_RETURN_list))
  
  MARKET_LINE_GRAPHIC <- ggplot(MARKET_LINE, aes(x = Beta, y = Expected_return, label = Stock)) +
    geom_point(color = "blue", size = 3) + 
    #geom_smooth(method = "lm")
    geom_text(vjust = -1, size = 4) +       
    geom_line(color = "red") +              
    labs(title = "Security Market Line (SML)", x = "Beta", y = "Expected Return (CAPM)") +
    theme_minimal()
  print(MARKET_LINE_GRAPHIC)
  return(MARKET_LINE)
}
stocks <- c("MC.PA","OR.PA","SAF.PA","TTE.PA","SU.PA","BNP.PA","LR.PA","DG.PA","AI.PA")
MARKETLINE(stocks)






