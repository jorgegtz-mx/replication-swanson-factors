library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)

# (Manually) Set working directory

# Load data sets
# Load FED shock factors
fed_factors <- read_xlsx("data/pre-and-post-ZLB-factors-extended.xlsx", sheet = "Data2", skip = 1, 
                         col_types = c("text", rep("numeric", 4)),
                         col_names = c("date", "ffr_shock", "fw_shock", "lsap_shock", "lsap_shock2")) %>% 
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) %>% 
  #drop unformatted date column
  select(-date)

# Load NAFTRAC data
naftrac <- read_xlsx("data/naftrac-daily-prices-bloomberg.xlsx", 
                        col_types = c("date", rep("numeric", 5)),
                        col_names = c("date", "naftrac_close", "naftrac_open", "naftrac_high",
                                      "naftrac_low", "cetrg028_index")) %>% 
  mutate(Date = as.Date(date)) %>% 
  filter(!is.na(Date)) %>% 
  select(Date, everything(), -date)


# Load Mexico's treasuries data
treasuries <- read_xlsx("data/mexico-treasuries-daily-yields-since-2003.xlsx", sheet = "data", 
                        col_types = c("date", rep("numeric", 13)),
                        guess_max = 4000) %>% 
  mutate(Date = as.Date(Fecha)) %>% 
  select(-Fecha)

# Load IPC series I got from Yahoo! Finance API
bmv_ipc_yahoo <- read_csv("data/query_from_yahoo_finance.xlsx") %>% 
  select(Date, bmv.ipc.volume, bmv.ipc.open, bmv.ipc.close)

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
  mutate(log.diff.bmv.ipc = log(bmv.ipc.close)-log(bmv.ipc.open))

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
  arrange(Date) %>% 
  fill(bmv.ipc.open, .direction = "up") %>% 
  fill(bmv.ipc.close, .direction = "down") %>% 
  #calculate the change between the previous close value (before the holiday) and
  #the immediate next open value (when the market opens after the holiday)
  mutate(log.diff.bmv.ipc = log(bmv.ipc.close)-log(bmv.ipc.open)) %>% 
  filter(Date %in% c(mexico_holidays)) %>% 
  select(Date, bmv.ipc.open, bmv.ipc.close, log.diff.bmv.ipc)

# Append results to merged table
db2 <- db %>% 
  #remove rows whereDate is a Mexico holiday
  filter(!(Date %in% mexico_holidays)) %>% 
  #append rows that has proxy changes for dates that are Mexico's holidays.
  bind_rows(fed_factors %>% 
              inner_join(proxy_changes_bmv_ipc_holidays, by = "Date") %>%
              select(Date, everything())) %>% 
  arrange(Date)

### Now I will append the NAFTRAC prices. Use the alternative method for calculating the price change on the Mexico's National Holidays.

mexico_holidays2 <- sort(c(mexico_holidays, as.Date("2003-09-16"), as.Date("2006-12-12")))

proxy_changes_naftrac <- naftrac %>% 
  filter(Date %in% c((mexico_holidays2 - 1), (mexico_holidays2 + 1))) %>% 
  arrange(Date) %>% 
  #Calculate the change between the last close price (the last business day before the holiday)
  #and the open price after the holiday.
  transmute(Date, naftrac_open, naftrac_close = lag(naftrac_close),
            log.diff.naftrac = log(naftrac_open) - log(naftrac_close)) %>% 
  filter(Date %in% (mexico_holidays2 + 1)) %>% 
  mutate(Date = Date- 1) %>% 
  #we have data for sep-09-12, remove proxy calculation
  filter(Date != as.Date("2012-09-13"))

naftrac_clean <-  naftrac %>% 
  filter(!is.na(naftrac_close)) %>% 
  mutate(log.diff.naftrac =  log(naftrac_close) - log(naftrac_open)) %>%
  bind_rows(proxy_changes_naftrac) %>% 
  arrange(Date) %>% 
  select(colnames(proxy_changes_naftrac))

#Check if there is any announcement date that has a missing NAFTRAC data.
fed_factors %>% 
  left_join(naftrac_clean, by = 'Date') %>% 
  filter(Date >= min(naftrac_clean$Date), is.na(naftrac_open))

# Merge db2 with NAFTRAC prices
db3 <- db2 %>% 
  left_join(naftrac_clean, by = "Date")

### Now I will append the treasury yield changes.
proxy_changes_treasuries <- treasuries %>% 
  filter(Date %in% c((mexico_holidays2 - 1), (mexico_holidays2 + 1))) %>% 
  arrange(Date) %>%
  transmute(Date, dcetes28 = cetes28-lag(cetes28),
            dcetes91 = cetes91-lag(cetes91),
            dcetes182 = cetes182-lag(cetes182), 
            dcetes364 = cetes364-lag(cetes364)) %>% 
  filter(Date %in% (mexico_holidays2 + 1)) %>% 
  mutate(Date = Date - 1) %>% 
  #we have data for sep-09-12, remove proxy calculation
  filter(Date != as.Date("2012-09-13"))

# Bind all treasuries data with the proxy changees.
# compute CETES one day yield change
treasuries_clean <- treasuries %>% 
  #calculate the change in the average daily yield of treasuries between 
  #a day before and the day of the announcement
  transmute(Date, dcetes28 = cetes28-lag(cetes28), dcetes91 = cetes91-lag(cetes91),
            dcetes182 = cetes182-lag(cetes182), dcetes364 = cetes364-lag(cetes364)) %>% 
  #remove this row because is empty
  filter(Date != "2003-05-16", !(Date %in% proxy_changes_treasuries$Date)) %>% 
  bind_rows(proxy_changes_treasuries) %>% 
  arrange(Date)

#Check if there is any announcement date that has a missing yield.
fed_factors %>% 
  left_join(treasuries_clean, by = 'Date') %>% 
  filter(Date >= min(treasuries_clean$Date), is.na(dcetes28))

# Merge db3 with table with changes of treasury yields.
db4 <-db3 %>% 
  left_join(treasuries_clean, by = 'Date')

##Now I will append exchange rate changes

#I need to add new dates to the "mexico_holidays" vector. Two of them are earlier than for the previous tables. I found data for the last two. It is weird, because they are holidays too but there was data for the NAFTRAC prices and Treasury yields.

mexico_holidays3 <- sort(c(mexico_holidays2, as.Date("1997-02-05"), as.Date("2000-03-21")))

proxy_exchange_rates_changes <- exchange_rates%>% 
  filter(Date %in% c((mexico_holidays3 - 1), (mexico_holidays3 + 1))) %>% 
  arrange(Date) %>% 
  mutate(log.diff.exchange.rate = log(lag(exchange.rate.close))-log(exchange.rate.open)) %>% 
  filter(Date %in% (mexico_holidays3 + 1)) %>% 
  mutate(Date = Date - 1) %>% 
  #we have data for sep-09-12, remove proxy calculation
  filter(Date != as.Date("2012-09-13"))

exchange_rates_clean <- exchange_rates %>% 
  #calculate the change between the close and open rates. 
  #The close price happens after the announcement
  mutate(log.diff.exchange.rate = log(exchange.rate.close)-log(exchange.rate.open)) %>% 
  filter(!(Date %in% proxy_exchange_rates_changes$Date)) %>% 
  bind_rows(proxy_exchange_rates_changes) %>% 
  arrange(Date)

#Check if there is any announcement date that has a exchange rate.
fed_factors %>% 
  left_join(exchange_rates_clean, by = 'Date') %>% 
  filter(Date >= min(exchange_rates_clean$Date), is.na(log.diff.exchange.rate))

# Merge fed factors table with changes on the exchange rate
db5 <- fed_factors %>% 
  filter(Date >= as.Date("1992-01-02")) %>% 
  left_join(exchange_rates_clean, by = "Date")
  
#since the exchange rate series matches more dates in the fed_factors table, I will merge
#db4 into db5
db6 <- db5 %>% 
  left_join(db4 %>% select(-c(ffr_shock, fw_shock, lsap_shock, lsap_shock2)), by = "Date")

# Standardize date frequency to estimate IRF using Newey-West 
# (Stata requires that time interval is constant)
# I believe this should not change the results
regular_monthly_frequency <- sort(map_vec(seq(1, nrow(db6), by = 1), 
                                          ~as.Date("2022-12-01")-months(.)))

db6$Date.Regular.Freq <- regular_monthly_frequency

db7 <- db6 %>% select(Date, Date.Regular.Freq, everything())

#Check that none date is duplicates
sum(duplicated(db7$Date))

#Save data set for modeling
write_csv(db7, "data/dataset_modeling_swanson_mexico.csv")

#Quality Checks
#What date does the first obs. of each series matches in the fed_factors table?
earliest_ipc_date <- (db7 %>% filter(!is.na(bmv.ipc.open)))$Date %>% min()
earliest_naftrac_date <- (db7 %>% filter(!is.na(naftrac_open)))$Date %>% min()
earliest_cetes_date <- (db7 %>% filter(!is.na(dcetes28)))$Date %>% min()
earliest_exchangerate_date <- (db7 %>% filter(!is.na(exchange.rate.open)))$Date %>% min()
c("earliest ipc date"=earliest_ipc_date, 
  "earliest naftrac date"=earliest_naftrac_date,
  "earliest cetes date"=earliest_cetes_date,
  "earliest exchage rate date"=earliest_exchangerate_date)


### Data for Jorda local projections

## NAFTRAC
# Append an index column to the table of prices.
naftrac_clean <- naftrac_clean %>% mutate(i = seq.int(nrow(.)))
# Pull the index number for all one date before the FED announcement dates.
start_index <- naftrac_clean[naftrac_clean$Date%in%fed_factors$Date, ]$i-1
# We have a problem if we estimate a projection for 120 days: another announcement may happen within the period.
# For each FED announcement date, pull 120 day records. The FED announcement date is day 2.
naftrac_projections <- map_df(start_index, ~mutate(filter(naftrac_clean, i %in% seq(., .+119, by = 1)), announcement_date = naftrac_clean[.+1, ]$Date))

## EXCHANGE RATE (or FIX)
# Repeat the previous three steps using the exchange rate table
# Append an index column to the table of prices.
exchange_rates_clean <- exchange_rates_clean %>% mutate(i = seq.int(nrow(.)))
# Pull the index number for all one date before the FED announcement dates.
start_index <- exchange_rates_clean[exchange_rates_clean$Date%in%fed_factors$Date, ]$i-1
# We have a problem if we estimate a projection for 120 days: another announcement may happen within the period.
# For each FED announcement date, pull 120 day records. The FED announcement date is day 2.
fix_projections <- map_df(start_index, ~mutate(filter(exchange_rates_clean, i %in% seq(., .+119, by = 1)), announcement_date = exchange_rates_clean[.+1, ]$Date))

# QC: do I have all the records I need to estimate the Jorda local projections?
length(start_index)*120 == nrow(fix_projections)

## Export data
write_xlsx(list(naftrac = naftrac_projections, fix = fix_projections), 'data/records_for_projections.xlsx')
