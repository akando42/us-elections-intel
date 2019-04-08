## Quanmod for financial data and highcharter for visualization
library(quantmod)
library(highcharter)


## Drawing stock symbol chart
stock_time_series <- function(symbol,title,subtitle){
  timeseries_data <- getSymbols(symbol, src="yahoo", auto.assign = FALSE)
  file_name <- paste(title,".csv")
  write.csv(timeseries_data, file=file_name)
  chart <- highchart(type="stock") %>%
    hc_title(text=title)%>%
    hc_subtitle(text=subtitle) %>%
    hc_add_series(timeseries_data)
  chart
}

compare_payroll_vs_workingage <- function(){
  payroll_population <- getSymbols("PAYEMS", src="FRED", auto.assign=FALSE)  
  working_age <- getSymbols("LFWA64TTUSM647S", src="FRED", auto.assign=FALSE)
}


## COMPARING USDvsJPYvsCURRENCIES
usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
usdcnh <- getSymbols("USD/CNH", src = "oanda", auto.assign = FALSE)
hc <- highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(usdjpy, id = "usdjpy") %>% 
  hc_add_series(usecnh, id = "usecnh")
hc

## CORPORATE BOND INDEX
corp_index <- getSymbols("BAMLC0A4CBBBEY", src="FRED", auto.assign = FALSE)
corp_chart <-  highchart(type="stock") %>%
  hc_title(text="Corporate Bond Index") %>%
  hc_subtitle(text="Corporate Bond Data from FRED ") %>%
  hc_add_series(corp_index, "corp_index")

corp_chart

## CONSUMER CREDIT DEFAULT RATE
default_rate <- getSymbols("DRCCLACBN", src="FRED", auto.assign=FALSE)
hc <- highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(default_rate, id="default")
hc
