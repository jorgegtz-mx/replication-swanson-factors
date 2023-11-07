library(tidyverse)
library(yahoofinancer)
library(readxl)

# Load FED shock factors. I will use the date vector to get the prices of the IPC and MXN futures series that were posted on those dates.
fed_factors <- read_xlsx("data/pre-and-post-ZLB-factors-extended.xlsx", sheet = "Data2", skip = 1, 
                         col_types = c("text", rep("numeric", 4)),
                         col_names = c("date", "ffr_shock", "fw_shock", 
                                       "lsap_shock", "lsap_schock2")) %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) 

#Query values in the vicinity of +-2 days of the FED announcement.
date_vector <- fed_factors$Date
longer_date_vector <- date_vector
for (i in 1:2){
  add <- c(date_vector+i, date_vector-i)
  longer_date_vector <- c(longer_date_vector, add)
}

#Query the "Mexican Peso Futures" series from yahoo! finance.
mxn_futures <- Ticker$new('6m=f') #Tell the API the "ticker" name of the Mexican Peso Futures
results <- tibble() #results will be saved here.

#Query series on a date by date basis. I tried to specify the full-sample period but I
#lose the connection with the API. 
#Pull the data points between the day before and after for each date given.
for (i in 1:length(longer_date_vector)) {
  start_date <- (longer_date_vector[i]-1) %>% as.character()
  end_date <- (longer_date_vector[i]+1) %>% as.character()
  results <- bind_rows(results, 
                       mxn_futures$get_history(start = start_date, end = end_date, interval = '1d'))
}

mxn_futures_table <- results %>%
  distinct() %>% 
  transmute(Date = as.Date(date), mxn.futures.open = open,
            mxn.futures.close = close, log.diff.mxn.futures = log(close)-log(open)) %>% 
  arrange(Date)

#Query the IPC series from Yahoo! Finance.
#I ran a manual query where the earliest year is 2000. I want to try if
#I can pull older data using this method.
bmv_ipc <- Ticker$new("^MXX")
results2 <- tibble()

#query day by day
#split query into two batches because the API will not let me run all requets
first_third <- round(length(longer_date_vector)/3)
second_third <- first_third*2

for (i in (1):first_third) {
  start_date <- (longer_date_vector[i]-1) %>% as.character()
  end_date <- (longer_date_vector[i]+1) %>% as.character()
  results2 <- bind_rows(results2, bmv_ipc$get_history(start = start_date, end = end_date))
}

for (i in (first_third+1):second_third) {
  start_date <- (longer_date_vector[i]-1) %>% as.character()
  end_date <- (longer_date_vector[i]+1) %>% as.character()
  results2 <- bind_rows(results2, bmv_ipc$get_history(start = start_date, end = end_date))
}

# Later I need to add a clock between queries to let the API refresh.

for (i in (second_third+1):length(longer_date_vector)) {
  start_date <- (longer_date_vector[i]-1) %>% as.character()
  end_date <- (longer_date_vector[i]+1) %>% as.character()
  results2 <- bind_rows(results2, bmv_ipc$get_history(start = start_date, end = end_date))
}

#Format table with results
bmv_ipc_series_table <- results2 %>% 
  distinct() %>% 
  transmute(Date = as.Date(date), bmv.ipc.volume = volume, bmv.ipc.open = open,
            bmv.ipc.close = close) %>% 
  arrange(Date)

#viz series
ggplot(results2) + 
  geom_line(aes(x = date, y = open)) +
  geom_smooth(aes(x = date, y = open), formula = "y ~ x", method = "lm")

#Merge both tables. Save into a spreadsheet.
query_from_yahoo_finance <- bmv_ipc_series_table %>% 
  full_join(mxn_futures_table, by = 'Date')

write_excel_csv(query_from_yahoo_finance, 'data/query_from_yahoo_finance.xlsx')
