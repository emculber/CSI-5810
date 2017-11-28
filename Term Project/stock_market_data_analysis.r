if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

Sys.setenv(TZ = "UTC")
currency('USD')

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

getSymbols("SPY", 
           src = "yahoo", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)
yahoo.SPY <- SPY
rm(SPY)
getSymbols("SPY", 
           src = "google", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)
google.SPY <- SPY

png(filename="yahoo_vs_google.png")
bind_rows(as.data.frame(yahoo.SPY) %>% 
          mutate(Src = "Yahoo"), 
        as.data.frame(google.SPY) %>% 
          mutate(Src = "Google")) %>% 
gather(key, value, 1:4, na.rm = TRUE) %>% 
ggplot(aes(x = key, y = value, fill = Src)) + 
geom_boxplot() + 
theme_bw() + 
theme(legend.title = element_blank(), legend.position = "bottom") + 
ggtitle("Google vs. Yahoo! (non-NA)")
dev.off()

as.data.frame(google.SPY) %>% 
  mutate(Date = index(google.SPY)) %>% 
  select(Date, starts_with("SPY"), -SPY.Volume) %>% 
  filter(is.na(SPY.Open))

getSymbols(c("MSFT", "GOOG"), src = "yahoo", from = start, to = end)

stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"]))
head(stocks)

png(filename="Multiple_Line_Chart.png")
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)
dev.off()

plot(as.zoo(stocks[, c("AAPL.Close", "MSFT.Close")]), screens = 1, lty = 1:2, 
     xlab = "Date", ylab = "Price")
par(new = TRUE)
plot(as.zoo(stocks[, "GOOG.Close"]), screens = 1, lty = 3, xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "")
axis(4)
mtext("Price", side = 4, line = 3)
legend("topleft", c("AAPL (left)", "MSFT (left)", "GOOG"), lty = 1:3, cex = 0.5)

stock_return = apply(stocks, 1, function(x) {(x / stocks[1,])-1}) %>% 
  t %>% as.xts

head(stock_return)

png(filename="Multiple_Normalize_Return_Line_Chart.png")
plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)
dev.off()


stock_change = stocks %>% log %>% diff
head(stock_change)

png(filename="Multiple_Normalize_Change_Line_Chart.png")
plot(as.zoo(stock_change), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)
dev.off()

png(filename="20_SMA_Chart.png")
candleChart(AAPL, up.col = "green", dn.col = "red", theme = "white")
addSMA(n = 20)
dev.off()

start = as.Date("2010-01-01")
getSymbols(c("AAPL", "MSFT", "GOOG"), src = "yahoo", from = start, to = end)

candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = 20)

png(filename="Multiple_SMA_Chart.png")
candleChart(AAPL, up.col = "green", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = c(20, 50, 200))
dev.off()


