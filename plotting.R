library(tidyverse)
library(readxl)
library(ggthemes)

#list files in the data repository
print(list.files('data/'))

#parameter
swanson_end_date <- as.Date('2019-06-19') #the last date we have data of Swanswon (2022) factors

#load data
mexico_money_market <- read_xlsx('data/mexico-money-market-interest-rates.xlsx', sheet = 'data', skip = 1)
dff <- read_xlsx('data/DFF.xlsx', skip = 10)
modeling <- read_csv("data/dataset_modeling_swanson_mexico.csv")

#Institutional Context
#Compare the changes in the effective federal funds rate (DFF) and Banxico target rate.
#The Banxico overnight interest rate is comparable to the FED's DFF.
#Banxico overnight interest rate reads in spanish as "TIIE de fondeo a un dia habil bancario".
mexico_effective_target_rate <- mexico_money_market %>% 
  transmute(Date = as.Date(Fecha), 
            banxico_overnight_interest_rate = as.numeric(banxico_overnight_interest_rate)) %>%
  #restrict sample period between the first data point and the latest date for Swanson factors.
  filter(!is.na(banxico_overnight_interest_rate), Date <= swanson_end_date)

#Merge Banxico's and FED's target interest rate series
compare_effective_target_rates <- dff %>% 
  transmute(Date = as.Date(Date), DFF) %>% 
  #earliest date for FED's DFF should be the same for the Banxico's interest rate.
  filter(Date >= min(mexico_effective_target_rate$Date), Date <= swanson_end_date) %>% 
  full_join(mexico_effective_target_rate, by = 'Date') %>% 
  #not all dates overlaps between both series. for example, markets have different holidays.
  #if null, use previous observation to fill the next value.
  fill(banxico_overnight_interest_rate, .direction = 'down') %>% 
  fill(DFF, .direction = 'down')

#plot FED's and Banxico's effective interest rates
plot_compare_effective_target_rates <- ggplot(compare_effective_target_rates) +
  geom_line(aes(x = Date, y = DFF, color = 'FED')) +
  geom_line(aes(x = Date, y = banxico_overnight_interest_rate, color = 'Banxico')) +
  scale_y_continuous(name = 'Effective Interest Rate', limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  #format date label. breaks are yearly and date labels have the "short-month-name-short-year"format
  scale_x_date(name = '', breaks = '1 years', date_labels = '%b-%y') + 
  #assign a color to each line
  scale_color_manual(name = '', values = c('FED'='blue', 'Banxico'='green')) +
  #imitate the style of STATA plots
  theme_stata() + 
  #customize axis fontsize and rotation
  theme(axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle = 45),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = 'top')

print(plot_compare_effective_target_rates)
#Export plot
# jpeg("plots/compare_effective_target_rates.jpeg", width = 800, height = 450, res = 100)
# print(plot_compare_effective_target_rates)
# dev.off()

##Plot the changes in the exchange rate vs. the FED announcement dates
glimpse(modeling)

plot_pct_changes_exchange_rate <- ggplot(modeling) +
  geom_line(aes(x = Date, y = log.diff.exchange.rate, color = 'Exchange Rate Daily Change (Open vs. Close)')) +
  scale_y_continuous(name = '%', limits = c(-0.06, 0.02), breaks = seq(-0.06, 0.02, by = 0.02),
                     labels = scales::percent) +
  #format date label. breaks are yearly and date labels have the "short-month-name-short-year"format
  scale_x_date(name = '', breaks = '2 years', date_labels = '%b-%y') + 
  #assign a color to each line
  scale_color_manual(name = '', values = c('Exchange Rate Daily Change (Open vs. Close)'='blue')) +
  #imitate the style of STATA plots
  theme_stata() + 
  #customize axis fontsize and rotation
  theme(axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12, angle = 45),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = 'top')

#Export plot
jpeg("plots/plot_pct_changes_exchange_rate.jpeg", width = 800, height = 450, res = 100)
print(plot_pct_changes_exchange_rate)
dev.off()
