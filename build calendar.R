library(tidyverse)
library(readxl)
# library(httr2)
library(XML)
library(httr)

# install.packages("httr")
# install.packages("httr2")
# install.packages("XML")

source("FOMC Dates Functions.R")

#Load data

#Banxico's reference interest rate
tiie <- read_xlsx("banxico overnight interest rate.xlsx", skip = 17)
#banxico calendar is published here: https://www.banxico.org.mx/publicaciones-y-prensa/anuncios-de-las-decisiones-de-politica-monetaria/anuncios-politica-monetaria-t.html
banxico_calendar <- read_csv("banxico policy calendar.csv", col_types = "c", quote = "\"")

#Clean dates
colnames(banxico_calendar) <- c("Date", "Decision")

banxico_calendar_tidy <- banxico_calendar %>% 
  filter(!is.na(Date)) %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%y"))

#Paste Overnight Interest Rate
colnames(tiie) <- c("Date", "ReferenceTIIE", "OvernightFundingRate")

tiie_tidy <- tiie %>% 
  mutate(Date = as.Date(Date), across(c(ReferenceTIIE:OvernightFundingRate), ~as.numeric(.)))

#Paste reference rates to calendar
banxico_calendar_tidy <- banxico_calendar_tidy %>% 
  left_join(tiie_tidy, by = "Date")

#Plot reference date on each meeting date
ggplot(banxico_calendar_tidy) +
  geom_line(aes(x = Date, y = ReferenceTIIE)) +
  scale_y_continuous(breaks = seq(0, 12, by = 0.5)) +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2023-08-31")))

#Pull historical FOMC data
get.fomc.dates.pre.2009()

xhtmldoc = htmlTreeParse("http://www.federalreserve.gov/monetarypolicy/fomchistorical2008.htm", useInternalNodes = TRUE)
url <- "http://www.federalreserve.gov/monetarypolicy/fomchistorical2008.htm"
doc <- htmlParse(rawToChar(GET(url)$content))

#update this code: https://github.com/returnandrisk/r-code/blob/master/FOMC%20Dates%20Functions.R
#using this fix: https://stackoverflow.com/questions/23430547/htmlparse-fails-to-load-external-entity
