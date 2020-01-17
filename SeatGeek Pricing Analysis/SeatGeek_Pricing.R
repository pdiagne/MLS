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
library(ggvis)
library(factoextra)

# Load excel files
# dir <- 'C:\\Users\\Magu\\Desktop\\SeatGeekAPI-master\\Data'
dir <- 'C:\\Users\\mdiagne\\OneDrive - Sempra Energy\\User Folders\\Desktop\\SeatGeekAPI-master\\Data'
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
d$title <- str_replace_all(d$title,' at Los Angeles Football Club','') 

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
   # export(f1,file = paste0('Popularity_Average_Median_',toString(i),'.png'))
 }
 
 # Listing Count, Average, Median 
 if (TRUE) {
   f2 <- plot_ly(x = temp$extract_date , y = temp$stats.visible_listing_count/max(temp$stats.visible_listing_count),name = 'Listing Count', type = 'scatter',mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.average_price/max(temp$stats.average_price), name = 'Average', mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$stats.median_price/max(temp$stats.median_price), name = 'Median', mode = 'lines+markers') %>%
     layout(title = paste0('Normalized Listing Count (SeatGeek), Average ($), Median ($) : ',m[i]))
   # print(f2)
   # export(f2,file = paste0('Listing Count_Average_Median_',toString(i),'.png'))
   
 }
 
 # Listing Count, Popularity
 if (TRUE) {
   f3 <- plot_ly(x = temp$extract_date , y = temp$stats.visible_listing_count/max(temp$stats.visible_listing_count),name = 'Listing Count', type = 'scatter',mode = 'lines+markers') %>%
     add_trace(x = temp$extract_date, y = temp$popularity/max(temp$popularity), name = 'Popularity', mode = 'lines+markers') %>%
     layout(title = paste0('Normalized Listing Count (SeatGeek), Popularity (SeatGeek) : ',m[i]))
   # print(f3)
   # export(f3,file = paste0('Listing Count_Popularity_',toString(i),'.png'))
 }
}

# Inititalize dataframes for kmeans clustering
k_popularity <- data.frame(matrix(ncol = length(m), nrow = length(xlsx_files)))
k_popularity <- setNames(k_popularity, c(m))
for (i in 1:length(m)) {
   ind <- grep(m[i],d$title)
   k_popularity[i] <- d$popularity[ind]
}

k_average <- data.frame(matrix(ncol = length(m), nrow = length(xlsx_files)))
k_average <- setNames(k_average, c(m))
for (i in 1:length(m)) {
   ind <- grep(m[i],d$title)
   k_average[i] <- d$stats.average_price[ind]
}

k_median <- data.frame(matrix(ncol = length(m), nrow = length(xlsx_files)))
k_median <- setNames(k_median, c(m))
for (i in 1:length(m)) {
   ind <- grep(m[i],d$title)
   k_median[i] <- d$stats.median_price[ind]
}

k_listcount <- data.frame(matrix(ncol = length(m), nrow = length(xlsx_files)))
k_listcount <- setNames(k_listcount, c(m))
for (i in 1:length(m)) {
   ind <- grep(m[i],d$title)
   k_listcount[i] <- d$stats.visible_listing_count[ind]
}

# Kmeans clustering 
   # https://rpubs.com/dnchari/kmeans
add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") 
{
   add_axis(vis, "x", title = x_lab) %>% 
      add_axis("x", orient = "top", ticks = 0, title = title,
               properties = axis_props(
                  axis = list(stroke = "white"),
                  labels = list(fontSize = 0)
               ), ...)
}

n = 15 # initital clusters for elbow method

# Kmeans - Popularity (optimal clusters via elbow method = 5)
wss = kmeans(t(k_popularity), centers=1)$tot.withinss
for (i in 2:n) {
  wss[i] = kmeans(t(k_popularity), centers=i)$tot.withinss
}
sse = data.frame(c(1:n), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400) %>% 
   add_title(title = 'Clusters by Popularity')

clusters_popularity = kmeans(t(k_popularity),5)
fviz_cluster(clusters_popularity, data = t(k_popularity), main = "Clusters by Popularity (Opponent vs LAFC)")

# Kmeans - Average (optimal clusters via elbow method = 10)
wss = kmeans(t(k_average), centers=1)$tot.withinss
for (i in 2:n) {
  wss[i] = kmeans(t(k_average), centers=i)$tot.withinss
}
sse = data.frame(c(1:n), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400) %>% 
  add_title(title = 'Clusters by Average')

clusters_average = kmeans(t(k_average),6)
fviz_cluster(clusters_average, data = t(k_average), main = "Clusters by Average Ticket Price (Opponent vs LAFC)")

# Kmeans - Median (optimal clusters via elbow method = 8)
wss = kmeans(t(k_median), centers=1)$tot.withinss
for (i in 2:n) {
  wss[i] = kmeans(t(k_median), centers=i)$tot.withinss
}
sse = data.frame(c(1:n), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400) %>% 
  add_title(title = 'Clusters by Median')

clusters_median = kmeans(t(k_median),8)
fviz_cluster(clusters_median, data = t(k_median), main = "Clusters by Median Ticket Price (Opponent vs LAFC)")

# Kmeans - Listing Count (optimal clusters via elbow method = 15)
wss = kmeans(t(k_listcount), centers=1)$tot.withinss
for (i in 2:n) {
  wss[i] = kmeans(t(k_listcount), centers=i)$tot.withinss
}
sse = data.frame(c(1:n), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400) %>% 
  add_title(title = 'Clusters by Listing Count')

clusters_listcount = kmeans(t(k_listcount),15)
fviz_cluster(clusters_listcount, data = t(k_listcount), main = "Clusters by Ticket Count (Opponent vs LAFC)")

msg_box('Done')