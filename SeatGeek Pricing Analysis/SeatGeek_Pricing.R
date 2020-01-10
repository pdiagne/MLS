# Clear environment and console
remove(list = ls())
shell("cls")

#Load Libraries
library(openxlsx)
library(readxl)
library(rapportools)
library(data.table)
library(gtools)
library(stringr)
library(tidyr)
library(reshape)
library(dplyr)
library(purrr)
library(tidyverse)
library(plotly)
library(webshot)
library(svDialogs)

# Load excel files
setwd(dir)
xlsx_files <- list.files(pattern="*.xlsx")
xlsx_list = lapply(xlsx_files, read.xlsx)
D <- do.call(smartbind, xlsx_list)

# Filter for upcoming LAFC matches
ind <- grep('Football Club',D$title)
D <- D[ind,]

# Filter data and convert data format
d <- D %>% group_by(title,extract_date) %>% summarize(score = score, popularity = popularity,stats.visible_listing_count = stats.visible_listing_count, stats.average_price = stats.average_price,
                                                            stats.median_price = stats.median_price, stats.lowest_price = stats.lowest_price,stats.highest_price = stats.highest_price)

d$score <- as.numeric(as.character(d$score))
d$popularity <- as.numeric(as.character(d$popularity))
d$stats.average_price <- as.numeric(as.character(d$stats.average_price))
d$stats.lowest_price <- as.numeric(as.character(d$stats.lowest_price))
d$stats.highest_price <- as.numeric(as.character(d$stats.highest_price))
d$stats.median_price <- as.numeric(as.character(d$stats.median_price))
d$stats.visible_listing_count <- as.numeric(as.character(d$stats.visible_listing_count))
d$extract_date <- as.Date(d$extract_date, format = '%m/%d/%Y', origin = '01/01/1900') - 2
d$title <- str_replace_all(d$title,'Los Angeles Football Club','LAFC') 

# Figures - Trends for Each Match
  # All values are normalized by series max
m <- unique(d$title)
for (i in 1:length(m)) {
 temp <- d[m[i] == d$title,]
 
 # Popularity, Average, Median 
 if (TRUE) {
   f1 <- plot_ly(x = temp$extract_date , y = temp$popularity/max(temp$popularity),name = 'Popularity', type = 'scatter',mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.average_price/max(temp$stats.average_price), name = 'Average', mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.median_price/max(temp$stats.median_price), name = 'Median', mode = 'lines+markers') %>%
     layout(title = paste0('Normalized Popularity (SeatGeek), Average ($), Median ($) : ',m[i]))
   # print(f1)
   export(f1,file = paste0('Popularity_Average_Median_',toString(i),'.png'))
 }
 
 # Listing Count, Average, Median 
 if (TRUE) {
   f2 <- plot_ly(x = temp$extract_date , y = temp$stats.visible_listing_count/max(temp$stats.visible_listing_count),name = 'Listing Count', type = 'scatter',mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.average_price/max(temp$stats.average_price), name = 'Average', mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.median_price/max(temp$stats.median_price), name = 'Median', mode = 'lines+markers') %>%
     layout(title = paste0('Normalized Listing Count (SeatGeek), Average ($), Median ($) : ',m[i]))
   # print(f2)
   export(f2,file = paste0('Listing Count_Average_Median_',toString(i),'.png'))
   
 }
 
 # Listing Count, Popularity
 if (TRUE) {
   f3 <- plot_ly(x = temp$extract_date , y = temp$stats.visible_listing_count/max(temp$stats.visible_listing_count),name = 'Listing Count', type = 'scatter',mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$popularity/max(temp$popularity), name = 'Popularity', mode = 'lines+markers') %>%
     layout(title = paste0('Normalized Listing Count (SeatGeek), Popularity (SeatGeek) : ',m[i]))
   # print(f3)
   export(f3,file = paste0('Listing Count_Popularity_',toString(i),'.png'))
 }
 
}
msg_box('Done')