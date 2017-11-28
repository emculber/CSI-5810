library(twitteR)
library(RMySQL)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "RbfkGMoAd8JYlRCOhYOK4Vkss"
consumer_secret <- "y2GjuHcNcI4TREs2FQYW3Fc0zsx0ohFVmQfwD8pzJ2T9IauchA"
access_token <- "934878379576365057-oH23nDyJc6G0CwaycS74J3wgn87DEYv"
access_secret <- "GyRZkzRDDKh8Ev3OTMwOQySeqAu0aehDLPuVggR6roffu"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
con <- dbConnect(MySQL(), user="root", password="password", dbname="twitter", host="localhost")

register_db_backend(con)
Sys.sleep(900)
print("Grabbing Stock")
stock_n = search_twitter_and_store("#stocks", "stocks")
print(sprintf("stock: (%i)\n", stock_n))
Sys.sleep(900)

print("Grabbing Stock to Watch")
stockstowatch_n = search_twitter_and_store("#stockstowatch", "stockstowatch")
print(sprintf("stockstowatch: (%i)\n", stockstowatch_n))
Sys.sleep(900)

print("Grabbing Stock Market")
stockmarket_n = search_twitter_and_store("#stockmarket", "stockmarket")
print(sprintf("stockmarket: (%i)\n", stockmarket_n))
Sys.sleep(900)

print("Grabbing Programming")
programming_n = search_twitter_and_store("#programming", "programming")
print(sprintf("programming: (%i)\n", programming_n))
Sys.sleep(900)

print("Grabbing Programming Languages")
programminglanguages_n = search_twitter_and_store("#programminglanguages", "programminglanguages")
print(sprintf("programminglanguages: (%i)\n", programminglanguages_n))
Sys.sleep(900)

print(sprintf("stock: (%i, %i, %i)\n", stock_n, stockstowatch_n, stockmarket_n))
print(sprintf("programming: (%i, %i)\n", programming_n, programminglanguages_n))

on.exit(dbDisconnect(con))
# tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
# d = twitteR::twListToDF(tw)
# print(d)
