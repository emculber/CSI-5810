# https://timtrice.github.io/backtesting-strategies/index.html
library(quantstrat)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(tidyr)
library(webshot)

Sys.setenv(TZ = "UTC")
currency('USD')

init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE

basic_symbols <- function() {
  symbols <- c(
               "IWM", # iShares Russell 2000 Index ETF
               "QQQ", # PowerShares QQQ TRust, Series 1 ETF
               "SPY" # SPDR S&P 500 ETF Trust
               )
}

enhanced_symbols <- function() {
  symbols <- c(
               basic_symbols(), 
               "TLT", # iShares Barclays 20+ Yr Treas. Bond ETF
               "XLB", # Materials Select Sector SPDR ETF
               "XLE", # Energy Select Sector SPDR ETF
               "XLF", # Financial Select Sector SPDR ETF
               "XLI", # Industrials Select Sector SPDR ETF
               "XLK", # Technology  Select Sector SPDR ETF
               "XLP", # Consumer Staples  Select Sector SPDR ETF
               "XLU", # Utilities  Select Sector SPDR ETF
               "XLV", # Health Care  Select Sector SPDR ETF
               "XLY" # Consumer Discretionary  Select Sector SPDR ETF
               )
}

global_symbols <- function() {
  symbols <- c(
               enhanced_symbols(), 
               "EFA", # iShares EAFE
               "EPP", # iShares Pacific Ex Japan
               "EWA", # iShares Australia
               "EWC", # iShares Canada
               "EWG", # iShares Germany
               "EWH", # iShares Hong Kong
               "EWJ", # iShares Japan
               "EWS", # iShares Singapore
               "EWT", # iShares Taiwan
               "EWU", # iShares UK
               "EWY", # iShares South Korea
               "EWZ", # iShares Brazil
               "EZU", # iShares MSCI EMU ETF
               "IGE", # iShares North American Natural Resources
               "IYR", # iShares U.S. Real Estate
               "IYZ", # iShares U.S. Telecom
               "LQD", # iShares Investment Grade Corporate Bonds
               "SHY" # iShares 42372 year TBonds
               )
}

checkBlotterUpdate <- function(port.st = portfolio.st, 
                               account.st = account.st, 
                               verbose = TRUE) {
  # Guy Yollin, 2014
  # http://www.r-programming.org/papers
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
                  sapply(
                         syms, 
                         FUN = function(x) eval(
                                                parse(
                                                      text = paste("sum(p$symbols", 
                                                                   x, 
                                                                   "posPL.USD$Net.Trading.PL)", 
                                                                   sep = "$")))))

  port.sum.tot <- sum(p$summary$Net.Trading.PL)

  if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }

  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))

  if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match account P&L")
  }

  if(sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if(verbose)print("duplicate timestamps in portfolio summary")

  }

  if(sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if(verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}

print(basic_symbols())

symbols <- basic_symbols()

## =====================================================
## =====================================================
## CHAPER 4: GET SYMBOLS ===============================
## =====================================================
## =====================================================

# =====================================================
# YAHOO ===============================================
# =====================================================
getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

head(IWM)
tail(IWM)
summary(IWM)

# Clear symbols
rm(list=basic_symbols())

# =====================================================
# GOOGLE ==============================================
# =====================================================
getSymbols(Symbols = symbols, 
           src = "google", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

head(IWM)
tail(IWM)
summary(IWM)

# Clear symbols
rm(list=basic_symbols())

# =====================================================
# MySQL ===============================================
# =====================================================
# getSymbols(Symbols = symbols, 
#           src = "MySQL", 
#           dbname = db, 
#           user = user, 
#           password = pw, 
#           host = host,
#           index.class = "POSIXct",
#           from = start_date, 
#           to = end_date, 
#           adjust = adjustment)

# head(IWM)
# tail(IWM)
# summary(IWM)

# Clear symbols
# rm(list=basic_symbols())

# =====================================================
# FRED ================================================
# =====================================================
getSymbols(Symbols = "DGS10", src = "FRED")

chartSeries(DGS10)

# Clear symbols
rm(DGS10)

## =====================================================
## =====================================================
## CHAPER 5: BASIC STRATEGY ============================
## =====================================================
## =====================================================

# =====================================================
# Strategy Setup ======================================
# =====================================================
symbols <- basic_symbols()

getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

stock(symbols, 
      currency = "USD", 
      multiplier = 1)

portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

# =====================================================
# Add Indicators ======================================
# =====================================================
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 10),
              label = "nFast")

add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 30), 
              label = "nSlow")

# =====================================================
# Add Indicators ======================================
# =====================================================

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

# =====================================================
# Add Rules ===========================================
# =====================================================
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.0005,
                          prefer = "High", 
                          TxnFees = -10, 
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "short", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "long", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")

# =====================================================
# Apply Strategy ======================================
# =====================================================
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)

## =====================================================
## =====================================================
## CHAPER 6: DATA QUALITY ==============================
## =====================================================
## =====================================================

# =====================================================
# Yahoo! vs. Google ===================================
# =====================================================
getSymbols("SPY", 
           src = "yahoo", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)

yahoo.SPY <- SPY
summary(yahoo.SPY)

rm(SPY)
getSymbols("SPY", 
           src = "google", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)

google.SPY <- SPY
summary(google.SPY)

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

as.data.frame(google.SPY) %>% 
  mutate(Date = index(google.SPY)) %>% 
  select(Date, starts_with("SPY"), -SPY.Volume) %>% 
  filter(is.na(SPY.Open))

# =====================================================
# Examining Trades ====================================
# =====================================================
rm.strat(portfolio.st)
rm.strat(account.st)
symbols <- basic_symbols()
getSymbols(Symbols = symbols, src = "yahoo", index.class = "POSIXct", 
           from = start_date, to = end_date, adjust = adjustment)
initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
         initEq = init_equity)
initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

chart.Posn(portfolio.st, Symbol = "SPY", Dates="2008-01-01::2008-07-01", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")

le <- as.data.frame(mktdata["2008-02-25::2008-03-07", c(1:4, 7:10)])
DT::datatable(le, 
              rownames = TRUE,
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE,
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                                                "Table 6.1: mktdata object for Feb. 25, 2008 to Mar. 7, 2008"))

ob <- as.data.table(getOrderBook(portfolio.st)$Quantstrat$SPY)
DT::datatable(ob, 
              rownames = FALSE,
              filter = "top",
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE, 
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                                                "Table 6.2: Order book for SPY"))

chart.Posn(portfolio.st, Symbol = "SPY", Dates="2009-08-01::2009-12-31", 
                      TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")

chart.Posn(portfolio.st, Symbol = "SPY", 
                      TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")

## =====================================================
## =====================================================
## CHAPER 7: PARAMETER OPTIMIZATION ====================
## =====================================================
## =====================================================

.fastSMA <- (1:30)
.slowSMA <- (20:80)
.nsamples <- 5

portfolio.st <- "Port.Luxor.MA.Opt"
account.st <- "Acct.Luxor.MA.Opt"
strategy.st <- "Strat.Luxor.MA.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date)

initOrders(portfolio = portfolio.st,
           initDate = init_date)

# =====================================================
# Add Distribution ====================================
# =====================================================
add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list(n = .fastSMA),
                 label = "nFAST")

add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = .slowSMA),
                 label = "nSLOW")

# =====================================================
# Add Distribution Constraint =========================
# =====================================================
add.distribution.constraint(strategy.st,
                            paramset.label = "SMA",
                            distribution.label.1 = "nFAST",
                            distribution.label.2 = "nSLOW",
                            operator = "<",
                            label = "SMA.Constraint")

# =====================================================
# Running Parallel ====================================
# =====================================================
library(parallel)

if( Sys.info()['sysname'] == "Windows") {
      library(doParallel)
    registerDoParallel(cores=detectCores())
} else {
      library(doMC)
    registerDoMC(cores=detectCores())
}

# =====================================================
# Apply Paramset ======================================
# =====================================================
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- apply.paramset(strategy.st,
                            paramset.label = "SMA",
                            portfolio.st = portfolio.st,
                            account.st = account.st, 
                            nsamples = .nsamples)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)

## =====================================================
## =====================================================
## CHAPER 8: STOP-LOSS ORDERS ==========================
## =====================================================
## =====================================================

.fast <- 10
.slow <- 30
.threshold <- 0.0005
.orderqty <- 100
.txnfees <- -10
.stoploss <- 3e-3 # 0.003 or 0.3%

portfolio.st <- "Port.Luxor.Stop.Loss"
account.st <- "Acct.Luxor.Stop.Loss"
strategy.st <- "Strat.Luxor.Stop.Loss"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

# =====================================================
# Add Indicators ======================================
# =====================================================
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .fast),
              label = "nFast")

add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .slow),
              label = "nSlow")

# =====================================================
# Add Signals =========================================
# =====================================================
add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long"
           )

add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

# =====================================================
# Add Rules ===========================================
# =====================================================
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long" ,
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = +.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          prefer = "Low",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = -.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocoshort"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain", 
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = FALSE)

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain", 
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled = FALSE)

# =====================================================
# Add Position Limit ==================================
# =====================================================
for(symbol in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = symbol,
              timestamp = init_date,
              maxpos = .orderqty)
}

# =====================================================
# Enable Rules ========================================
# =====================================================
enable.rule(strategy.st, 
            type = "chain", 
            label = "StopLoss")

# =====================================================
# Apply Strategy ======================================
# =====================================================
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)

## =====================================================
## =====================================================
## CHAPER 9: STOP LOSS OPTIMIZATION ====================
## =====================================================
## =====================================================

.StopLoss = seq(0.05, 0.6, length.out = 48)/100

strategy.st <- "Luxor.Stop.Loss.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date)

initOrders(portfolio = portfolio.st,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

# =====================================================
# Add Indicators ======================================
# =====================================================
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .fast),
              label = "nFast")

add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .slow),
              label = "nSlow")

# =====================================================
# Add Signals =========================================
# =====================================================
add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long"
           )

add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

# =====================================================
# Add Rules ===========================================
# =====================================================
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long" ,
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = +.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          prefer = "Low",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = -.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocoshort"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain", 
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = FALSE)

add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain", 
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled = FALSE)

# =====================================================
# Add Position Limit ==================================
# =====================================================
for(symbol in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = symbol,
              timestamp = init_date,
              maxpos = .orderqty)
}

# =====================================================
# Add Distribution ====================================
# =====================================================
add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossLONG",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossLONG")

add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossSHORT",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossSHORT")

# =====================================================
# Add Distribution Constraint =========================
# =====================================================
add.distribution.constraint(strategy.st,
                            paramset.label = "StopLoss",
                            distribution.label.1 = "StopLossLONG",
                            distribution.label.2 = "StopLossSHORT",
                            operator = "==",
                            label = "StopLoss")

# =====================================================
# Enable Rules ========================================
# =====================================================
enable.rule(strategy.st, 'chain', 'StopLoss')

# =====================================================
# Apply Paramset ======================================
# =====================================================
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- apply.paramset(strategy.st, 
                            paramset.label = "StopLoss", 
                            portfolio.st = portfolio.st, 
                            account.st = account.st, 
                            nsamples = .nsamples, 
                            verbose = TRUE)

  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)
