require(readr)
require(tsibble)
require(dplyr)
require(ggplot2)


# Загрузка данных:
(dat <- read_csv("../data/cypto_coins.csv"))

# Визуализация данных:
dat %>% 
  ggplot(., aes(ds, y, group = coin)) +
  geom_line() +
  scale_y_log10() +
  theme_bw()

# Преобразование данных в объект класса tsibble:
(dat_ts <- as_tsibble(dat, key = coin, index = ds))

# Расчет среднемесячных значений y по каждому временному ряду:
monthly <- dat_ts %>% 
  group_by_key() %>% 
  index_by(year_month = ~ yearmonth(.)) %>% 
  summarise(
    avg_y = mean(y),
    n = n()
  )

monthly

# Визуализация полученных среднемесячных значений:
monthly %>% 
  ggplot(., aes(year_month, avg_y, group = coin)) + 
  geom_line() +
  scale_y_log10() +
  theme_bw()

# Расчет скользящего среднего:
dat_ts %>% 
  group_by_key() %>% 
  mutate(moving_avg_y = slide_dbl(y, ~mean(.), .size = 7))

# Диагностика наличия пропущенных значений:
has_gaps(dat_ts)

# Создание набора данных с пропущенными значениями:
set.seed(42)
dat_ts_na <- dat %>% 
  sample_n(., 15000) %>% 
  as_tsibble(., key = coin, index = ds)
has_gaps(dat_ts_na)

# Детальный отчет по пропущенным значениям:
scan_gaps(dat_ts_na)

(gaps <- count_gaps(dat_ts_na))

# Визуализация пропущенных значений:
gaps %>% 
  ggplot(., aes(x = coin)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme_bw()

# Замена пропущенных значений на NA:
dat_ts_na_filled <- dat_ts_na %>% 
  fill_gaps(.full = TRUE)
dat_ts_na_filled

# Замена пропущенных значений на медианные значения (по каждому ряду отдельно):
dat_ts_na_filled_median <- dat_ts_na %>% 
  group_by_key() %>% 
  fill_gaps(y = median(y, na.rm = TRUE), .full = TRUE)
dat_ts_na_filled_median
