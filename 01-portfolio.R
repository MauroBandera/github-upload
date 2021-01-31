### PORTFOLIO ANALYSIS ----

# Load libraries
library(tidyverse)
library(tidyquant)

TWLO <- tq_get(x = "TWLO", get = "stock.prices")
FRA.DE <- tq_get(x = "FRA.DE", get = "stock.prices")
XRX <- tq_get("XRX", get = "stock.prices")
PFE <- tq_get("PFE", get = "stock.prices")
LUN.TO <- tq_get("LUN.TO", get = "stock.prices")
MRNA <- tq_get("MRNA", get = "stock.prices")

TWLO <- TWLO %>% 
    mutate(typical_price  = (high+low+close)/3,
           raw_mf         = typical_price * volume) %>% 
    
    tq_mutate_xy(x = typical_price, y = volume, mutate_fun = MFI, col_rename = "mfi")


# plotting adjusted price
TWLO %>% 
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +
    geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 50) +
    #geom_line(aes(y = mfi)) +
    geom_ma(ma_fun = SMA, n = 50, color = "blue", size = 1) +
    geom_ma(ma_fun = SMA, n = 200, color = "green", size = 1)

# plotting money flow
TWLO %>% 
    ggplot(aes(x = date, y = mfi)) +
    geom_line()
