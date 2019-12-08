require(readr)
require(tsibble)
require(dplyr)
require(ggplot2)


(dat <- read_csv("../data/cypto_coins.csv"))

dat %>% 
  ggplot(., aes(ds, y, group = coin)) +
  geom_line() +
  scale_y_log10() +
  theme_bw()


(dat_ts <- as_tsibble(dat, key = coin, index = ds))


monthly <- dat_ts %>% 
  group_by_key() %>% 
  index_by(year_month = ~ yearmonth(.)) %>% 
  summarise(
    avg_y = mean(y),
    n = n()
  )

monthly

monthly %>% 
  ggplot(., aes(year_month, avg_y, group = coin)) + 
  geom_line() +
  scale_y_log10() +
  theme_bw()


dat_ts %>% 
  group_by_key() %>% 
  mutate(moving_avg_y = slide_dbl(y, ~mean(.), .size = 7))


has_gaps(dat_ts)

set.seed(42)
dat_ts_na <- dat %>% 
  sample_n(., 15000) %>% 
  as_tsibble(., key = coin, index = ds)
has_gaps(dat_ts_na)

scan_gaps(dat_ts_na)

(gaps <- count_gaps(dat_ts_na))

gaps %>% 
  ggplot(., aes(x = coin)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme_bw()


dat_ts_na_filled <- dat_ts_na %>% 
  fill_gaps(.full = TRUE)
