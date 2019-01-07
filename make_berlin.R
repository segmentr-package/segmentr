require(rdwd)
require(tidyverse)
require(lubridate)
require(magrittr)

berlin_stations <- findID("Berlin", exactmatch = FALSE) %>%
  enframe() %>%
  filter(str_detect(name, "^Berlin-"))

berlin_sources <- berlin_stations %>%
  deframe() %>%
  selectDWD(id=., res="daily", var="kl", per="historical")

download_dwd <- possibly(~dataDWD(., read=FALSE, dir=tempdir(), quiet=TRUE), NULL)

berlin_weather <- berlin_sources %>%
  map(download_dwd) %>%
  compact() %>%
  map(readDWD) %>%
  map_dfr(as_tibble)

berlin_grid <- berlin_weather %>%
  select(STATIONS_ID, MESS_DATUM, TMK) %>%
  rename(station = STATIONS_ID, date = MESS_DATUM, temperature = TMK) %>%
  drop_na(temperature) %>%
  filter(year(date) %in% 2010:2011) %>%
  arrange(station, date) %>%
  spread(date, temperature) %>%
  filter(rowSums(is.na(.)) < 100) %>%
  left_join(berlin_stations, by = c(station = "value"))

berlin <- berlin_grid %>%
  select(-name, -station) %>%
  as.matrix %>%
  set_rownames(berlin_grid$name)

devtools::use_data(berlin, overwrite = TRUE)
