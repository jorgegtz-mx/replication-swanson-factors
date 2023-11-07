library(tidyverse)
library(readxl)

# (Manually) Set working directory

# Load data sets
# Load FED shock factors
fed_factors <- read_xlsx("data/pre-and-post-ZLB-factors-extended.xlsx", sheet = "Data2", skip = 1, 
                         col_types = c("text", rep("numeric", 4)),
                         col_names = c("date", "ffr_shock", "fw_shock", "lsap_shock", "lsap_schock2")) %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) %>% 
  #drop unformatted date column
  select(-date)

# Load NAFTRAC data
naftrac <- read_xlsx("data/naftrac-daily-prices-bloomberg.xlsx", 
                        col_types = c("date", rep("numeric", 5)),
                        col_names = c("date", "naftrac_close", "naftrac_open", "naftrac_high",
                                      "naftrac_low", "cetrg028_index")) %>% 
  transmute(Date = as.Date(date), dlog_naftrac = log(naftrac_close) - log(naftrac_open))


# Load Mexico's treasuries data
treasuries <- read_xlsx("data/mexico-treasuries-daily-yields-since-2003.xlsx", sheet = "data", 
                        col_types = c("date", rep("numeric", 13)),
                        guess_max = 4000) %>% 
  mutate(Fecha = as.Date(Fecha))

# Load IPC series I got from Yahoo! Finance API
bmv_ipc_yahoo <- read_csv("data/query_from_yahoo_finance.xlsx")

# Load exchange rate series
exchange_rates <- read_xlsx('data/mxn-usd-exchange-rate.xlsx', skip = 17) %>% 
  #format date and rename columns
  transmute(Date = as.Date(paste(Year, Month, Day, sep = '-'), format = '%Y-%m-%d'), 
            exchange.rate.open = SF43784, exchange.rate.close = SF43786)

# Consolidate Swanson factors, treasury yields, IPC index and NAFTRAC price series into one data set.
# I will consolidate the tables separately because I need to calculate the price changes differently when the FED meeting coincides with a Mexico's National Holiday.

#First, merge "fed_factors" with "bmv_ipc_yahoo"

#The first date with market volume is May 15th 2001.
bmv_ipc_yahoo_with_volume <- bmv_ipc_yahoo %>% 
  filter(bmv.ipc.volume>0) %>% 
  transmute(Date, log.diff.bmv.ipc)

# Merge Swanson factors table with the BMV/IPC  series from Yahoo! Finance.
db <- fed_factors %>% 
  left_join(bmv_ipc_yahoo_with_volume, by = "Date") %>%
  select(Date, everything()) %>% 
  #this is first date when the volume of the BMV/IPC is > 0.
  filter(Date >= as.Date("2001-05-15"))

# Fill in the blanks for dates we don't have data
mexico_holidays <- (db %>% 
  filter(is.na(log.diff.bmv.ipc)))$Date

proxy_changes_bmv_ipc_holidays <- bmv_ipc_yahoo %>% 
  filter(!is.na(bmv.ipc.open) | Date %in% c(mexico_holidays)) %>% 
  fill(bmv.ipc.open, .direction = "up") %>% 
  fill(bmv.ipc.close, .direction = "down") %>% 
  #calculate the change between the previous close value (before the holiday) and
  #the immediate next open value (when the market opens after the holiday)
  mutate(log.diff.bmv.ipc = log(bmv.ipc.close)-log(bmv.ipc.open)) %>% 
  filter(Date %in% c(mexico_holidays))

# Append results to merged table
db2 <- db %>% 
  bind_rows(fed_factors %>% 
              inner_join(proxy_changes_bmv_ipc_holidays, by = "Date") %>%
              select(Date, everything()))

# Now I will append the NAFTRAC prices. Use the alternative method for calculating the price change
# on the Mexico's National Holidays.
proxy_changes_naftrac <- naftrac %>% 
  filter(Date %in% c((mx_holidays - 1), (mx_holidays + 1))) %>% 
  arrange(Date) %>% 
  #Calculate the change between the last close price (the last business day before the holiday)
  #and the open price after the holiday.
  transmute(Date, naftrac_open, naftrac_close = lag(naftrac_close),
            dlog_naftrac = log(naftrac_open) - log(naftrac_close)) %>% 
  filter(date_string %in% (mx_holidays + 1)) %>% 
  mutate(date_string = date_string - 1)


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
