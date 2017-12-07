library(twitteR)
library(RMySQL)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "RbfkGMoAd8JYlRCOhYOK4Vkss"
consumer_secret <- "y2GjuHcNcI4TREs2FQYW3Fc0zsx0ohFVmQfwD8pzJ2T9IauchA"
access_token <- "934878379576365057-oH23nDyJc6G0CwaycS74J3wgn87DEYv"
access_secret <- "GyRZkzRDDKh8Ev3OTMwOQySeqAu0aehDLPuVggR6roffu"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
con <- dbConnect(MySQL(), user="root", password="emc090993", dbname="twitter", host="localhost")
register_db_backend(con)

search <- c("stocks", "stockstowatch", "stockmarket", "programming", "programminglanguages", "c", "cpp", "cplusplus", "python", "java", "python", "csharp", "javascript", "dotnet", "php", "rscript", "rlang", "rstats", "matlab", "ruby", "golang", "perl", "visualbasic", "scratch", "swift")

print(Sys.time())

timer_count <- 0
limit <- 8000
for (lan in search) {
  print(paste0("Grabbing ", lan))
  count = search_twitter_and_store(paste0("#", lan), n=limit, lan)
  timer_count <- timer_count + count
  print(sprintf("%s: (%i)\n", lan, count))
  print(paste0("Rate Limit: ", getCurRateLimitInfo(c("search"))))
  if(timer_count >= limit) {
    print("15 min pause")
    print(Sys.time())
    Sys.sleep(900)
  }
}

# Sys.sleep(900)
# print(Sys.time())
# print("Grabbing Stock")
# stock_n = search_twitter_and_store("#stocks", "stocks")
# print(sprintf("stock: (%i)\n", stock_n))
# Sys.sleep(900)

# print("Grabbing Stock to Watch")
# stockstowatch_n = search_twitter_and_store("#stockstowatch", "stockstowatch")
# print(sprintf("stockstowatch: (%i)\n", stockstowatch_n))
# Sys.sleep(900)

# print("Grabbing Stock Market")
# stockmarket_n = search_twitter_and_store("#stockmarket", "stockmarket")
# print(sprintf("stockmarket: (%i)\n", stockmarket_n))
# # Sys.sleep(900)
# 
# print("Grabbing Programming")
# programming_n = search_twitter_and_store("#programming", "programming")
# print(sprintf("programming: (%i)\n", programming_n))
# # Sys.sleep(900)
# 
# print("Grabbing Programming Languages")
# programminglanguages_n = search_twitter_and_store("#programminglanguages", "programminglanguages")
# print(sprintf("programminglanguages: (%i)\n", programminglanguages_n))
# # Sys.sleep(900)
# 
# print("Grabbing C")
# programminglanguages_n = search_twitter_and_store("#C", "C")
# print(sprintf("C: (%i)\n", programminglanguages_n))
# 
# print("Grabbing Java")
# programminglanguages_n = search_twitter_and_store("#Java", "Java")
# print(sprintf("Java: (%i)\n", programminglanguages_n))
# 
# print(sprintf("stock: (%i, %i, %i)\n", stock_n, stockstowatch_n, stockmarket_n))
# print(sprintf("programming: (%i, %i)\n", programming_n, programminglanguages_n))

on.exit(dbDisconnect(con))
# tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
# d = twitteR::twListToDF(tw)
# print(d)
