library(tidyverse)
library(readxl)
library(yahoofinancer) # retrieve prices of mexican futures

# Set working directory

# Load FED shock factors
fed_factors <- read_xlsx("data/pre-and-post-ZLB-factors-extended.xlsx", sheet = "Data2", skip = 1, 
                         col_types = c("text", rep("numeric", 4)),
                         col_names = c("date", "ffr_shock", "fw_shock", "lsap_shock", "lsap_schock2"))

# Load securities info
securities <- read_xlsx("data/naftrac-daily-prices-bloomberg.xlsx", 
                        col_types = c("date", rep("numeric", 5)),
                        col_names = c("date", "naftrac_close", "naftrac_open", "naftrac_high",
                                      "naftrac_low", "cetrg028_index"))

# Load treasuries data
treasuries <- read_xlsx("data/mexico-treasuries-daily-yields-since-2003.xlsx", sheet = "data", 
                        col_types = c("date", rep("numeric", 13)),
                        guess_max = 4000) %>% 
  mutate(Fecha = as.Date(Fecha))


# Compute change in prices
securities_calculated <- securities %>% 
  mutate(date_string = as.Date(date),
         dlog_naftrac = log(naftrac_close) - log(naftrac_open))

# See NAFTRAC price change evolution
ggplot(securities_calculated) +
  geom_line(aes(x = date, y = dlog_naftrac))

ggplot(securities_calculated) + 
  geom_line(aes(x = date, y = naftrac_open)) +
  geom_smooth(aes(x = date, y = naftrac_open), formula = "y ~ x", method = "lm")

# merge securities and fed factor
db <- fed_factors %>% 
  mutate(date_string = as.Date(date, format = "%Y-%m-%d")) %>% 
  inner_join(securities_calculated, by = "date_string") %>% 
  select(-c("date.x", "date.y")) %>% 
  select(date_string, everything()) %>% 
  filter(!is.na(date_string))

# See what dates have NULL values for the price changes
mx_holidays <- (db %>% 
  filter(is.na(dlog_naftrac)) %>% 
  select(date_string))$date_string

print(mx_holidays) # día de la independencia, día de la Guadalupana, día de Muertos, día del trabajo

# compute the change between the last close price and the next business day open price
proxy_changes <- securities_calculated %>% 
  filter(date_string %in% c((mx_holidays - 1), (mx_holidays + 1))) %>% 
  arrange(date_string) %>% 
  transmute(date_string, naftrac_open, naftrac_close = lag(naftrac_close),
            dlog_naftrac = log(naftrac_open) - log(naftrac_close)) %>% 
  filter(date_string %in% (mx_holidays + 1)) %>% 
  mutate(date_string = date_string - 1)

# Drop deltas with null values and replace with proxy of price changes
db_replacement <- db %>% 
  filter(date_string %in% mx_holidays) %>% 
  select(date_string:lsap_schock2) %>% 
  left_join(proxy_changes, by = "date_string")

# main data with proxy of price changes
db2 <- db %>% 
  filter(!(date_string %in% mx_holidays)) %>% 
  bind_rows(db_replacement) %>% 
  arrange(date_string)

db2

mxn_futures <- Ticker$new('6m=f')
results <- tibble()

Ticker$new
#retrieve prices from Yahoo! Finance
for (i in 1:length(db2$date_string)) {
  start_date <- db2$date_string[i] %>% as.character()
  end_date <- (db2$date_string[i]+1) %>% as.character()
  results <- bind_rows(results, mxn_futures$get_history(start = start_date, end = end_date))
}

mxn_futures_table <- results %>% 
  transmute(date_string = as.Date(date), mxn_futures_pctdelta = (close/open)-1)

#retrieve IPC series from Yahoo! Finance
bmv_ipc <- Ticker$new("^MXX")
results2 <- tibble()

#query day by day
for (i in 1:length(db2$date_string)) {
  start_date <- db2$date_string[i] %>% as.character()
  end_date <- (db2$date_string[i]+1) %>% as.character()
  results2 <- bind_rows(results2, bmv_ipc$get_history(start = start_date, end = end_date))
}

bmv_ipc_series_table <- results2 %>% 
  transmute(date_string = as.Date(date), dlog_bmvipc = log(close)-log(open))

#viz series
ggplot(results2) + 
  geom_line(aes(x = date, y = open)) +
  geom_smooth(aes(x = date, y = open), formula = "y ~ x", method = "lm")

# merge mxn future changes with db2
# sadly, most changes are 0
db3 <- db2 %>% 
  left_join(mxn_futures_table, by = "date_string") %>% 
  left_join(bmv_ipc_series_table, by = "date_string")

# compute CETES one day yield change
treasuries_change <- treasuries %>% 
  transmute(Fecha, dcetes28 = cetes28-lag(cetes28), dcetes91 = cetes91-lag(cetes91),
            dcetes182 = cetes182-lag(cetes182), dcetes364 = cetes364-lag(cetes364),
            dcetes2a = cetes2a - lag(cetes2a))

# compute CETES yield change for Mexican holidays
treasuries_proxy_changes <- treasuries %>% 
  filter(Fecha %in% c((mx_holidays - 1), (mx_holidays + 1))) %>% 
  arrange(Fecha) %>% 
  transmute(Fecha, dcetes28 = cetes28-lag(cetes28), dcetes91 = cetes91-lag(cetes91),
            dcetes182 = cetes182-lag(cetes182), dcetes364 = cetes364-lag(cetes364),
            dcetes2a = cetes2a - lag(cetes2a)) %>% 
  filter(Fecha %in% (mx_holidays + 1)) %>% 
  mutate(Fecha = Fecha- 1)

# both treasury change tables
treasuries_change_complete <- bind_rows(treasuries_proxy_changes, treasuries_change) %>% 
  arrange(Fecha) %>% 
  rename(date_string = Fecha)

#merge treasuries yield change to main table
#2-year cetes were issued for the first time recently, i won't include this in my analysis
db4 <-  db3 %>% 
  left_join(treasuries_change_complete, by = "date_string") %>% 
  rename(date = date_string)

#glimpse final result
glimpse(db4)

# Export table
fed_factors2 <- fed_factors %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Standardize date frequency to estimate IRF using Newey West (Stata requires that time interval is constant)
# I believe this should not change the results
db4 %>% 
  filter(!is.na(dlog_bmvipc), !is.na(dcetes28)) %>%
  select(-cetrg028_index, -dcetes2a) %>% 
  write_excel_csv("swanson a la mexicana (irregular frequency).csv")

ggplot(db4) +
  geom_line(aes(x = date, y = naftrac_open))
glimpse(db4)


constant_months <- sort(map_vec(seq(0, 134, by = 1), ~as.Date("2022-12-01")-months(.)))
db4$date <- constant_months

db4 %>% 
  filter(!is.na(dlog_bmvipc), !is.na(dcetes28)) %>%
  select(-cetrg028_index, -dcetes2a) %>% 
  write_excel_csv("swanson a la mexicana (regular frequency).csv")

### ignore the rest of the code for now
# Project changes for changes in naftrac price
projection <- tibble()

for (i in 0:180) {
  h_df <- securities %>% 
    mutate(date = as.Date(date), weekday_name = strftime(date, "%A")) %>% 
    filter(!(weekday_name %in% c("Saturday", "Sunday"))) %>% 
    select(-cetrg028_index) %>% 
    left_join(fed_factors2, by = "date")  %>% 
    fill(naftrac_close, .direction = "down") %>% 
    mutate(dlog_naftrac_h = lead(log(naftrac_close), i) - lag(log(naftrac_close))) %>% 
    filter(!is.na(ffr_shock)) %>% 
    transmute(date, dlog_naftrac_h, h = i)
  
  projection <- bind_rows(projection, h_df)
  
}

projection <- projection %>% 
  arrange(date)

# Replace missing values in Mexican holidays with previous value
treasuries_inlieu_holidays <- treasuries %>% 
  filter(Fecha %in% c((mx_holidays - 1), (mx_holidays + 1))) %>% 
  arrange(Fecha) %>% 
  transmute(date = as.Date(Fecha), cetes364) %>% 
  filter(date %in% (mx_holidays + 1)) %>% 
  mutate(date = date - 1)

projection_treasuries <- tibble()

for (i in 0:180){
  h_df <- treasuries %>% 
    transmute(date = as.Date(Fecha), cetes364)  %>% 
    bind_rows(treasuries_inlieu_holidays) %>% 
    arrange(date) %>% 
    left_join(fed_factors2, by = "date") %>% 
    mutate(dcetes364 = lead(cetes364, i) - lag(cetes364))  %>% 
    filter(!is.na(ffr_shock)) %>% 
    transmute(date, dcetes364, h = i)
  
  projection_treasuries <- bind_rows(projection_treasuries, h_df)
}

projection_treasuries <- projection_treasuries %>% 
  arrange(desc(date))

# Merge projection with mp shocks and export output
projection %>% 
  left_join(fed_factors2) %>% 
  write_csv("projection_naftrac.csv")

projection_treasuries %>% 
  left_join(fed_factors2) %>% 
  write_csv("projection_cetes.csv")

projection_treasuries %>% filter(dcetes364 < -2)
