require(dplyr)
require(rvest)
require(lubridate)
require(readr)


base_url <- "https://coinmarketcap.com/"
period <- "?start=20180101&end=20191206"

coin <- c("bitcoin", "ethereum", "xrp",
          "tether", "litecoin", "eos",
          "stellar", "cardano", "tron",
          "monero", "tezos", "chainlink", 
          "neo", "iota", "dash", 
          "maker", "dogecoin", "zcash",
          "decred", "qtum", "augur", 
          "nano")

dat <- list()


for (i in 1:length(coin)) {
  
  message("Coin: ", coin[i])
  
  currency <- paste0("currencies/", coin[i], "/historical-data/")
  
  page <- paste0(base_url, currency, period)
  
  result <- read_html(page) %>% 
    html_table() %>% 
    .[[3]] %>% 
    rename(y = "Close**", ds = Date) %>% 
    select(y, ds) %>% 
    mutate(ds = mdy(ds)) %>% 
    mutate(y = gsub(",", "", y) %>% as.numeric(.),
           coin = coin[i])
  
  dat[[i]] <- result
  
  Sys.sleep(sample(1:10, 1))
  
}


dat <- bind_rows(dat)

write_csv(dat, path = "../data/cypto_coins.csv")
