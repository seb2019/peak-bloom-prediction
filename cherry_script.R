library(tidyverse)
library(rnoaa)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))


get_temperature <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2023-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

historic_temperatures <-
  tibble(location = "washingtondc", get_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_temperature("CA001108395")))
8+7
