if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

start <- as.Date("2016-01-01")
end <- as.Date("2016-10-01")

getSymbols("AAPL", src = "yahoo", from = start, to = end)

class(AAPL)

head(AAPL)

png(filename="AAPL_Line_Chart.png")
plot(AAPL[, "AAPL.Close"], main = "AAPL")
dev.off()

png(filename="AAPL_Candlestick_Chart.png")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
dev.off()

png(filename="AAPL_Candlestick_Chart_Zoom.png")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
zoomChart("2016-05-01/2016-05-30")
dev.off()

getSymbols(c("MSFT", "GOOG"), src = "yahoo", from = start, to = end)

stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"]))
head(stocks)

plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)

plot(as.zoo(stocks[, c("AAPL.Close", "MSFT.Close")]), screens = 1, lty = 1:2, 
     xlab = "Date", ylab = "Price")
par(new = TRUE)
plot(as.zoo(stocks[, "GOOG.Close"]), screens = 1, lty = 3, xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
axis(4)
mtext("Price", side = 4, line = 3)
legend("topleft", c("AAPL (left)", "MSFT (left)", "GOOG"), lty = 1:3, cex = 0.5)

stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% as.xts

head(stock_return)

plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)


stock_change = stocks %>% log %>% diff
head(stock_change)

plot(as.zoo(stock_change), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)

candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addSMA(n = 20)

start = as.Date("2010-01-01")
getSymbols(c("AAPL", "MSFT", "GOOG"), src = "yahoo", from = start, to = end)

candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = 20)

candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = c(20, 50, 200))


