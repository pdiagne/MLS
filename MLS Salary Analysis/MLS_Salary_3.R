rm(list=ls())
shell("cls")

library(readxl)
library(quantmod)
library(dplyr)
library(tibble)
library(lubridate)
library(ragtop)
library(stringr)
library(plotly)
library(tcR)

# Data Sources
  # https://www.soccermetrics.net/football-business-analytics/front-office-efficiency-football-business-analytics/mls-front-office-efficiency-2018-edition
  # https://mlsplayers.org/resources/salary-guide

dir <- ''
setwd(dir)

# *******
# Load Datasets
# *******

# Load Regular Season standings (2007-2014) 
  # https://www.scoreboard.com/mls/archive/
stan <- read_excel("MLS_Standings_2007_2017.xlsx")

# Load salary data from 2007-2014
  # https://mlsplayers.org/resources/salary-guide
s_07 <- read.csv("mls-salaries-2007.csv") 
s_08 <- read.csv("mls-salaries-2008.csv") 
s_09 <- read.csv("mls-salaries-2009.csv") 
s_10 <- read.csv("mls-salaries-2010.csv") 
s_11 <- read.csv("mls-salaries-2011.csv") 
s_12 <- read.csv("mls-salaries-2012.csv") 
s_13 <- read.csv("mls-salaries-2013.csv") 
s_14 <- read.csv("mls-salaries-2014.csv") 
s_15 <- read.csv("mls-salaries-2015.csv") 
s_16 <- read.csv("mls-salaries-2016.csv") 
s_17 <- read.csv("mls-salaries-2017.csv") 

# *******
# Salary Data Transformations
# *******

# Add year to salary data
s_07$year <- 2007
s_08$year <- 2008
s_09$year <- 2009
s_10$year <- 2010
s_11$year <- 2011
s_12$year <- 2012
s_13$year <- 2013
s_14$year <- 2014
s_15$year <- 2015
s_16$year <- 2016
s_17$year <- 2017

# Combine salary data for all years
sal <- rbind(s_07,s_08,s_09,s_10,s_11,s_12,s_13,s_14,s_15,s_16,s_17)
sal$club <- as.character(sal$club)

# Remove players in 'Pool' or 'POOL' club
  # unique(sal$club)
sal <- sal[sal$club != 'Pool',] # 18 rows removed
sal <- sal[sal$club != 'POOL',] # 11 rows removed

# Remove players with blank club
sal <- sal[sal$club != '',] # 20 rows removed

# Remove players with 'None' club
sal <- sal[sal$club != 'None',] # 3 rows removed

# Adjust salaries to 2017 value 
  # https://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package
getSymbols("CPIAUCSL", src='FRED') 
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- as.data.frame(avg.cpi/as.numeric(avg.cpi['2017'])) #using 2007 as the base year
cf<- cf %>% rownames_to_column("Full_Date")
cf$year <- year(cf$Full_Date)
y <- unique(sal$year)

sal$adj_base_salary <- 0
for (i in 1:length(y)) { # Adjusted Base Salary
  ind_1 <- which(sal$year == y[i])
  ind_2 <- which(cf$year == y[i])
  sal$adj_base_salary[ind_1] <- sal$base_salary[ind_1]*cf$CPIAUCSL[ind_2]
}

sal$adj_guaranteed_compensation <- 0
for (i in 1:length(y)) { # Adjusted Base Salary
  ind_1 <- which(sal$year == y[i])
  ind_2 <- which(cf$year == y[i])
  sal$adj_guaranteed_compensation[ind_1] <- sal$guaranteed_compensation[ind_1]*cf$CPIAUCSL[ind_2]
}

# Unify position data
sal$position <- as.character(sal$position)
  #unique(sal$position)
sal$position <- str_replace_all(sal$position,"M-D","D-M")
sal$position <- str_replace_all(sal$position,"M/D","D-M")
sal$position <- str_replace_all(sal$position,"D/M","D-M")
sal$position <- str_replace_all(sal$position,"F-D","D-F")
sal$position <- str_replace_all(sal$position,"D/F","D-F")
sal$position <- str_replace_all(sal$position,"F-M","M-F")
sal$position <- str_replace_all(sal$position,"MF","M-F")
sal$position <- str_replace_all(sal$position,"M/F","M-F")
sal$position <- str_replace_all(sal$position,"F/M","M-F")
sal <- sal[-which(sal$position == ""),] # https://www.sjearthquakes.com/post/2017/07/04/statement-san-jose-earthquakes-regarding-matheus-silva

# *******
# Standings Data Transformations
# *******

# Replace team names in the Salary dataset with corresponding team names in the Standing dataset
teams_sal <- unique(sal$club) # length(teams_sal) -> 26 unique 
teams_stan <- unique(stan$Team) # length(teams_stan) -> 23 unique

sal$club <- str_replace_all(sal$club,"CHV","Chivas USA")
sal$club <- str_replace_all(sal$club,"HOU","Houston Dynamo")
sal$club <- str_replace_all(sal$club,"DAL","FC Dallas")
sal$club <- str_replace_all(sal$club,"COL","Colorado Rapids")
sal$club <- str_replace_all(sal$club,"LA","Los Angeles Galaxy")
sal$club <- str_replace_all(sal$club,"RSL","Real Salt Lake")        
sal$club <- str_replace_all(sal$club,"DC","DC United")
sal$club <- str_replace_all(sal$club,"NE","New England Revolution")
sal$club <- str_replace_all(sal$club,"NYRB","New York Red Bulls")
sal$club <- str_replace_all(sal$club,"NY","New York Red Bulls")
sal$club <- str_replace_all(sal$club,"KC","Sporting Kansas City")
sal$club <- str_replace_all(sal$club,"CHI","Chicago Fire")
sal$club <- str_replace_all(sal$club,"CLB","Columbus Crew")         
sal$club <- str_replace_all(sal$club,"TFC","Toronto FC")
sal$club <- str_replace_all(sal$club,"TOR","Toronto FC")
sal$club <- str_replace_all(sal$club,"SJ","San Jose Earthquakes")
sal$club <- str_replace_all(sal$club,"SEA","Seattle Sounders")
sal$club <- str_replace_all(sal$club,"PHI","Philadelphia Union")
sal$club <- str_replace_all(sal$club,"POR","Portland Timbers")
sal$club <- str_replace_all(sal$club,"VAN","Vancouver Whitecaps")   
sal$club <- str_replace_all(sal$club,"MTL","Montreal Impact")
sal$club <- str_replace_all(sal$club,"ORL","Orlando City")
sal$club <- str_replace_all(sal$club,"NYCFC","New York City")  
sal$club <- str_replace_all(sal$club,"MNUFC","Minnesota United")
sal$club <- str_replace_all(sal$club,"ATL","Atlanta United") 

# The following teams from the Salarly dataset do not map to the Standings dataset: "LAFC"
  # Remove these teams from salary 
sal <- sal[sal$club != 'LAFC',]

# Create unique identifyer 
sal$id <- paste0(sal$club,sal$year)

# *******
# Standings and Salary Calculations
# *******

# Calculate Annual adj_guaranteed_compensation per team (2007-2017) 
  # adj_base_salary      adj_guaranteed_compensation
Annual_agc <- sal %>% group_by(club,year) %>% summarize(Annual_adj_base_salary = sum(adj_base_salary, na.rm = TRUE))

# Append Points and Conference column to Annual_agc
Annual_agc$id <- paste0(Annual_agc$club,Annual_agc$year)
stan$id <- paste0(stan$Team,stan$Year)
M <- Annual_agc %>% inner_join(stan, by = 'id')

# Calculate Correlations of Annual Adjusted Guaranteed Compensation vs Total Points [Figure 1,2,3]
ys <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
corr_comp_pts <- data.frame(matrix(ncol = 2, nrow = length(ys)))
names(corr_comp_pts) <- c('year','corr')
corr_comp_pts$year <- ys
for (i in 1:length(ys)) {
  ind <- which(M$year == ys[i])
  corr_comp_pts$corr[i] <- cor(M$Annual_adj_base_salary[ind],M$Pts[ind])
}  

# Figure 1 - Annual Adjusted Guaranteed Compensation vs Total Points, 2007
f1 <- plot_ly(x = M$Annual_adj_base_salary[M$year == '2007'], y = M$Pts[M$year == '2007']) %>%
  layout(title = 'Annual Adjusted Guaranteed Compensation vs Total Points, 2007')
cor(M$Annual_adj_base_salary[M$year == '2007'],M$Pts[M$year == '2007']) # corr = -0.227516
print(f1)

# Figure 2 - Annual Adjusted Guaranteed Compensation vs Total Points, 2017
f2 <- plot_ly(x = M$Annual_adj_base_salary[M$year == '2017'], y = M$Pts[M$year == '2017']) %>%
  layout(title = 'Annual Adjusted Guaranteed Compensation vs Total Points, 2017')
cor(M$Annual_adj_base_salary[M$year == '2017'],M$Pts[M$year == '2017']) # corr = 0.5101069
print(f2)

# Figure 3 - Year versus Correlation between Annual Adjusted Guaranteed Compensation and Total Points
f3 <- plot_ly(x = corr_comp_pts$year,y = corr_comp_pts$corr) %>%
  layout(title = 'Year versus Correlation between Annual Adjusted Guaranteed Compensation and Total Points')
print(f3)

# Figure 4 - Total Points/Total Team Salary vs Total Team Salary 
M$point_per_comp <- M$Pts/M$Annual_adj_base_salary*10000
f4 <- plot_ly(x = M$Annual_adj_base_salary,y = M$point_per_comp) %>%
  layout(title = 'Total Points/Total Team Salary vs Total Team Salary ')
cor(M$Annual_adj_base_salary,M$point_per_comp) # corr = -0.704681
print(f4)

# Filter for 'perc' percentile in points and points/compensation for each season
pe <- data.frame(matrix(ncol = 8, nrow = nrow(M)))
names(pe) <- c('year','club_pts','pts','conference_pts','club_eff','eff','conference_eff','both')
z = 1
perc <- 0.5
for (i in 1:length(ys)) {
  ind <- which(M$year == ys[i])
  M_temp <- M[ind,]
  
  ind_1 <- which(M_temp$Pts >= quantile(M_temp$Pts, perc)) 
  c_1 = length(ind_1)
  pe$year[z:(z+c_1-1)] <- ys[i]
  pe$club_pts[z:(z+c_1-1)] <- M_temp$club[ind_1]
  pe$pts[z:(z+c_1-1)] <- M_temp$Pts[ind_1]
  pe$conference_pts[z:(z+c_1-1)] <- M_temp$Conference[ind_1]
  
  ind_2 <- which(round(M_temp$point_per_comp, digits = 5) >= round(quantile(M_temp$point_per_comp, perc), digits = 5)) 
  c_2 = length(ind_2)
  pe$club_eff[z:(z+c_2-1)] <- M_temp$club[ind_2]
  pe$eff[z:(z+c_2-1)] <- M_temp$point_per_comp[ind_2]
  pe$conference_eff[z:(z+c_2-1)] <- M_temp$Conference[ind_2]
  
  # Check if team is in both previous lists
  sam <- match(pe$club_pts[z:(z+c_1-1)],pe$club_eff[z:(z+c_2-1)])
  sam <- sam[!is.na(sam)]
  pe$both[(z+sam-1)] <- 'yes'
  
  z = z + c_1
}  

# Create unique identifyer 
pe$id <- paste0(pe$club_eff,pe$year)

# Figure 5 - Calculate breakdown of top percentile in total points and eff
per <- data.frame(matrix(ncol = 5, nrow = length(ys)))
names(per) <- c('year','both','total','both_west','both_east')
per$year <- ys
for (i in 1:length(ys)) {
  ind <- which(pe$year == ys[i])
  per$both[i] <- length(which(pe$both[ind] == 'yes'))
  per$total[i] <- length(ind)
  per$both_west[i] <- length(which(pe$both[ind] == 'yes' & pe$conference_eff[ind] == 'West'))
  per$both_east[i] <- length(which(pe$both[ind] == 'yes' & pe$conference_eff[ind] == 'East'))
}  
f5 <- plot_ly(per, x = ~year, y = ~both, type = 'bar', name = 'Points and Eff') %>%
  add_trace(y = ~total, name = 'Points') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
  layout(title = c('Top Teams in Points & Top Teams in Both Points and Effeciency'))
print(f5)

# Percentile Salary by Player Position: Both total points and eff lists
# Filter pe data for only teams in both lists
pe_b <- pe[!is.na(pe$both),] # filter pe data 
pe_teams <- as.data.frame(unique(pe_b$id))
names(pe_teams)[1] <- 'id'
pe_teams$id <- as.character(pe_teams$id)

# Filter Salary data for pe_teams
#perc_1 <- 0.5
sal_pe <- pe_teams %>% inner_join(sal, by = 'id')
sal_pe_position <- sal_pe %>% group_by(position,year) %>% summarize(perc_1_comp = mean(adj_base_salary) ) # quantile(adj_base_salary, perc_1)

# Average Salary by Player Position: Only total points list
# Filter pe data for only teams in points list
pe_b_n <- pe[is.na(pe$both),] # filter pe data 
pe_teams_n <- as.data.frame(unique(pe_b_n$id))
names(pe_teams_n)[1] <- 'id'
pe_teams_n$id <- as.character(pe_teams_n$id)

# Filter Salary data for pe_teams
sal_pe_n <- pe_teams_n %>% inner_join(sal, by = 'id')
sal_pe_position_n <- sal_pe_n %>% group_by(position,year) %>% summarize(perc_1_comp = mean(adj_base_salary) ) # quantile(adj_base_salary, perc_1)

# Figures 6 through 11 - Average salary by position comparison between team on points list and teams on both points and eff list
pos <- c("GK","D","D-M","M","M-F","F") #excluding "D-F" since so few players have this position
for (i in 1:length(pos)) {
  
  # Setup data for figures
  ind_1 <- which(sal_pe_position$position == pos[i])
  x_temp_1 <- sal_pe_position$year[ind_1]
  y_temp_1 <- sal_pe_position$perc_1_comp[ind_1]
  y_temp_1_avg <- round(mean(y_temp_1))
  
  ind_2 <- which(sal_pe_position_n$position == pos[i])
  x_temp_2 <- sal_pe_position_n$year[ind_2]
  y_temp_2 <- sal_pe_position_n$perc_1_comp[ind_2]
  y_temp_2_avg <- round(mean(y_temp_2))
  
  # Plot figures
  f6_11 <- plot_ly(x = x_temp_1 , y = y_temp_1, name = paste0(pos[i],' - Points and Eff: (',y_temp_1_avg,')'), type = 'scatter',mode = 'lines+markers') %>%
    add_trace(x = x_temp_2, y = y_temp_2, name = paste0(pos[i],' - Points: (',y_temp_2_avg,')'), mode = 'lines+markers') %>%
    layout(title = 'Average Annual Salary by Position:Top Teams in Points & Top Teams in Both Points and Effeciency')
  print(f6_11)
}

# Figure 12 through 17 - Boxplot of salaries for each position, for each year
for (i in 1:length(pos)) {
  
  # Setup data for figures
  ind_07_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2007')
  y_temp_07_pe <- sal_pe$adj_base_salary[ind_07_pe]

  ind_08_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2008')
  y_temp_08_pe <- sal_pe$adj_base_salary[ind_08_pe]
  
  ind_09_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2009')
  y_temp_09_pe <- sal_pe$adj_base_salary[ind_09_pe]
  
  ind_10_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2010')
  y_temp_10_pe <- sal_pe$adj_base_salary[ind_10_pe]
  
  ind_11_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2011')
  y_temp_11_pe <- sal_pe$adj_base_salary[ind_11_pe]
  
  ind_12_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2012')
  y_temp_12_pe <- sal_pe$adj_base_salary[ind_12_pe]
  
  ind_13_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2013')
  y_temp_13_pe <- sal_pe$adj_base_salary[ind_13_pe]
  
  ind_14_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2014')
  y_temp_14_pe <- sal_pe$adj_base_salary[ind_14_pe]
  
  ind_15_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2015')
  y_temp_15_pe <- sal_pe$adj_base_salary[ind_15_pe]
  
  ind_16_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2016')
  y_temp_16_pe <- sal_pe$adj_base_salary[ind_16_pe]
  
  ind_17_pe <- which(sal_pe$position == pos[i] & sal_pe$year == '2017')
  y_temp_17_pe <- sal_pe$adj_base_salary[ind_17_pe]
  
  
  ind_07_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2007')
  y_temp_07_pe_n <- sal_pe_n$adj_base_salary[ind_07_pe_n]
  
  ind_08_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2008')
  y_temp_08_pe_n <- sal_pe_n$adj_base_salary[ind_08_pe_n]
  
  ind_09_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2009')
  y_temp_09_pe_n <- sal_pe_n$adj_base_salary[ind_09_pe_n]
  
  ind_10_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2010')
  y_temp_10_pe_n <- sal_pe_n$adj_base_salary[ind_10_pe_n]
  
  ind_11_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2011')
  y_temp_11_pe_n <- sal_pe_n$adj_base_salary[ind_11_pe_n]
  
  ind_12_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2012')
  y_temp_12_pe_n <- sal_pe_n$adj_base_salary[ind_12_pe_n]
  
  ind_13_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2013')
  y_temp_13_pe_n <- sal_pe_n$adj_base_salary[ind_13_pe_n]
  
  ind_14_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2014')
  y_temp_14_pe_n <- sal_pe_n$adj_base_salary[ind_14_pe_n]
  
  ind_15_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2015')
  y_temp_15_pe_n <- sal_pe_n$adj_base_salary[ind_15_pe_n]
  
  ind_16_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2016')
  y_temp_16_pe_n <- sal_pe_n$adj_base_salary[ind_16_pe_n]
  
  ind_17_pe_n <- which(sal_pe_n$position == pos[i] & sal_pe_n$year == '2017')
  y_temp_17_pe_n <- sal_pe_n$adj_base_salary[ind_17_pe_n]
  
  # Plot figure
  f12_17 <- plot_ly(y = y_temp_07_pe, type = 'box', color = '2007_pe') %>%
    add_trace(y = y_temp_07_pe_n, type = 'box', color = '2007_p') %>%
    add_trace(y = y_temp_08_pe, type = 'box', color = '2008_pe') %>%
    add_trace(y = y_temp_08_pe_n, type = 'box', color = '2008_p') %>%
    add_trace(y = y_temp_09_pe, type = 'box', color = '2009_pe') %>%
    add_trace(y = y_temp_09_pe_n, type = 'box', color = '2009_p') %>%  
    add_trace(y = y_temp_10_pe, type = 'box', color = '2010_pe') %>%
    add_trace(y = y_temp_10_pe_n, type = 'box', color = '2010_p') %>%  
    add_trace(y = y_temp_11_pe, type = 'box', color = '2011_pe') %>%
    add_trace(y = y_temp_11_pe_n, type = 'box', color = '2011_p') %>%  
    add_trace(y = y_temp_12_pe, type = 'box', color = '2012_pe') %>%
    add_trace(y = y_temp_12_pe_n, type = 'box', color = '2012_p') %>%  
    add_trace(y = y_temp_13_pe, type = 'box', color = '2013_pe') %>%
    add_trace(y = y_temp_13_pe_n, type = 'box', color = '2013_p') %>%  
    add_trace(y = y_temp_14_pe, type = 'box', color = '2014_pe') %>%
    add_trace(y = y_temp_14_pe_n, type = 'box', color = '2014_p') %>%  
    add_trace(y = y_temp_15_pe, type = 'box', color = '2015_pe') %>%
    add_trace(y = y_temp_15_pe_n, type = 'box', color = '2015_p') %>%  
    add_trace(y = y_temp_16_pe, type = 'box', color = '2016_pe') %>%
    add_trace(y = y_temp_16_pe_n, type = 'box', color = '2016_p') %>%  
    add_trace(y = y_temp_17_pe, type = 'box', color = '2017_pe') %>%
    add_trace(y = y_temp_17_pe_n, type = 'box', color = '2017_p') %>%  
    layout(title = paste('Average Salary by Position : ',pos[i]))
    print(f12_17)
}

# Figure 18 through 23 - Boxplot of salaries across all years for each position
y_GK <- sal_pe$adj_base_salary[which(sal_pe$position == "GK")]
y_D <- sal_pe$adj_base_salary[which(sal_pe$position == "D")]
y_DM <- sal_pe$adj_base_salary[which(sal_pe$position == "D-M")]
y_M <- sal_pe$adj_base_salary[which(sal_pe$position == "M")]
y_MF <- sal_pe$adj_base_salary[which(sal_pe$position == "M-F")]
y_F <- sal_pe$adj_base_salary[which(sal_pe$position == "F")]

y_GK_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "GK")]
y_D_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "D")]
y_DM_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "D-M")]
y_M_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "M")]
y_MF_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "M-F")]
y_F_n <- sal_pe_n$adj_base_salary[which(sal_pe_n$position == "F")]

# Plot figure
f18 <- plot_ly(y = y_GK, type = 'box',boxpoints = 'all', jitter = 0.8, color = 'GK_PE') %>% add_trace(y = y_GK_n,color = 'GK_P') %>%
  layout(title = 'Average Salary by Position : GK') 
print(f18)
f19 <- plot_ly(y = y_D, type = 'box', boxpoints = 'all', jitter = 0.8, color = 'D_PE') %>% add_trace(y = y_D_n,color = 'D_P') %>%
  layout(title = 'Average Salary by Position : D')
print(f19)
f20 <- plot_ly(y = y_DM, type = 'box', boxpoints = 'all', jitter = 0.8, color = 'D-M_PE') %>% add_trace(y = y_DM_n,color = 'D-M_P') %>%
  layout(title = 'Average Salary by Position : D-M')
print(f20)
f21 <- plot_ly(y = y_M, type = 'box', boxpoints = 'all', jitter = 0.8, color = 'M_PE') %>% add_trace(y = y_M_n,color = 'M_P') %>%
  layout(title = 'Average Salary by Position : M')
print(f21)
f22 <- plot_ly(y = y_MF, type = 'box', boxpoints = 'all', jitter = 0.8, color = 'M-F_PE') %>% add_trace(y = y_MF_n,color = 'M-F_P') %>%
  layout(title = 'Average Salary by Position : M-F')
print(f22)
f23 <- plot_ly(y = y_F, type = 'box', boxpoints = 'all', jitter = 0.8, color = 'F_PE') %>% add_trace(y = y_F_n,color = 'F_P')%>%
  layout(title = 'Average Salary by Position : F')
print(f23)