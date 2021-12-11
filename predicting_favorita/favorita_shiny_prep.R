# load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(corrgram)
library(Hmisc)
library(performanceEstimation)
library(zoo)

# Shiny Project Ideas --------------------------------

# - select a specific store's data to show, time window, select up to 4
# - Plot All Holidays by year against non holiday average
# - 
# - Correlation Plot with Oil
# - Is one store affected by oil more than others
# - Disaster impact
# - Training with disaster data
# - Train with biopsy of disaster data removed
# - Train with only data before disaster
# - Train with data only after disaster
# - Compare directly pre and post disaster data 
# - compare store type/ cluster
# ---- Extra ----
# - Geopolitical map with stores
# -- There could be a 'Background Tab' In the Shiny App
# - Where is Ecuador ?
# - Details on the Earthquake

# Earthquake ------------------------------------------

# magnitude: 7.8
# date: 2016-04-16

 
# Read in Data ----------------------------------------
base_path = "~/DSA/R_project/Favorita_Shiny_R/predicting_favorita/"
data_transactions = read_csv(paste(base_path,"store-sales-time-series-forecasting/transactions.csv", sep =""))
data_oil = read_csv(paste(base_path,"store-sales-time-series-forecasting/oil.csv", sep =""))
data_holiday_events = read_csv(paste(base_path,"store-sales-time-series-forecasting/holidays_events.csv", sep =""))
data_stores = read_csv(paste(base_path,"store-sales-time-series-forecasting/stores.csv", sep =""))
data_items = read_csv(paste(base_path, "store-sales-time-series-forecasting/items.csv", sep =""))
data_train = read_csv(paste(base_path, "store-sales-time-series-forecasting/train.csv", sep =""))

#----------------------------------------------------------------------------
data_store_plus = merge(x=data_transactions, y=data_stores, 
                        by.x = "store_nbr", by.y = "store_nbr", x.all=TRUE)

# Chunking Training Data by Year --------------------------------------------
# 
# data_train <-  data_train %>%
#   mutate(date=as.Date(date, format="%Y-%m-%d"))
train_years <- unique(format(data_train$date,"%Y"))

for (y in train_years) {
  # data_train_yr[y] <-  y
  temp_dat <- data_train %>%
    filter(date >= as.POSIXct(str_glue("{y}-01-01"))) %>%
    filter(date <= as.POSIXct(str_glue("{y}-12-31")))

    assign(str_glue("data_train_{y}"), temp_dat)
    data_train_yr$yr <- temp_dat
}

data_train_yr <- list()
for (y in train_years) {
  data_train_yr[[y]] <- str_glue("data_train_{y}")
  
}

# Separate Training into pre Earthquake and post quake ------------------------
quake_date = "2016-04-16"
pre_quake_train <- data_train %>%
  filter(date < as.POSIXct(quake_date))


post_quake_train <- data_train %>%
  filter(date > as.POSIXct(quake_date))

quake_day_train <- data_train %>%
  filter(date > as.POSIXct(quake_date))


# Create Primary DataFrame ----------------------------------------------------
fav_df <- merge(x=data_transactions, y=data_stores, 
                by.x = "store_nbr", by.y = "store_nbr", x.all=TRUE) %>%
          merge(data_holiday_events, by = "date", all.x=TRUE, incomparables=NA) %>%
          mutate(holiday_applies = if_else((locale == "National"), TRUE,
                          if_else((locale == "Regional") & (locale_name == state), TRUE,
                          if_else((locale == "Local") & (locale_name == city), TRUE, false=FALSE)))) %>% 
          merge(data_oil, by = "date", all.x=TRUE, incomparables=NA) %>%
          mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
          mutate(day_of_week = weekdays(date)) %>%
          rename(store_type = type.x) %>%
          rename(holiday_type = type.y)



write.csv(fav_df, paste(base_path, "store-sales-time-series-forecasting/fav_main_df.csv", sep=""))

          #- todo: holidays events mutate applicable holiday events
  
          # mutate(date=as.Date(date, format="%Y-%m-%d")) 

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

data_main <-  read_csv(paste(base_path, "store-sales-time-series-forecasting/fav_main_df.csv", sep=""))

#data_product <- merge(data_items, data_main, by=)

for (y_dat in data_train_yr) {
  data_product <- data_store_plus %>%
    # incorporate product data - 
    right_join(y_dat, by = c("store_nbr" = "store_nbr")) %>% 
    
    # incorporate holiday data -
    merge(data_holiday_events, by = "date", all.x=TRUE, incomparables=NA) %>%
    mutate(holiday_applies = if_else((locale == "National"), TRUE,
                                     if_else((locale == "Regional") & (locale_name == state), TRUE,
                                             if_else((locale == "Local") & (locale_name == city), TRUE, false=FALSE)))) %>% 
    # incorporate Oil Data - 
    merge(data_oil, by = "date", all.x=TRUE, incomparables=NA) %>%
    # Add day of week -
    mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
    mutate(day_of_week = weekdays(date)) %>%
    # clean up names -
    rename(store_type = type.x) %>%
    rename(holiday_type = type.y)
}

# Analyze Products ------------------------------------------------------------

# --- beta analysis ----

pre_qk2 <- pre_quake_train %>% 
  group_by(family) %>%
  group_by(date, .add=TRUE) %>%
  summarise(total_sales = sum(sales)) %>%
  spread(family, total_sales) %>%
  merge(data_oil, by = "date", all.x=TRUE, incomparables=NA) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
  mutate(day_of_week = weekdays(date)) %>%
  mutate(oil_price=na.approx(dcoilwtico, na.rm=FALSE)) %>%
  fill(oil_price, .direction="up") %>%
  select(-dcoilwtico)

post_qk2 <-  post_quake_train %>% 
  group_by(family) %>%
  group_by(date, .add=TRUE) %>%
  summarise(total_sales = sum(sales)) %>%
  spread(family, total_sales) %>%
  merge(data_oil, by = "date", all.x=TRUE, incomparables=NA) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
  mutate(day_of_week = weekdays(date)) %>%
  mutate(oil_price=na.approx(dcoilwtico, na.rm=FALSE)) %>%
  fill(oil_price, .direction="up") %>%
  select(-dcoilwtico)

# -- analyse a single product family as time series

family_use <-  "FROZEN FOODS"
# function to get the start vector for ts 

pre_quake_ts <- ts(select(pre_qk2, family_use), frequency=365, start = c(2013,1))
# as.POSIXct("2016-04-17")-as.POSIXct("2016-01-01")+1
post_quake_ts <- ts(select(post_qk2, family_use), frequency=365, start = c(2016,108))
plot(post_quake_ts)



# Plot Multiple Stores Over specific window frame -----------------------------

#- Selected Stores
stores_use = c(32, 11)

#- Selected Time Frame
start_date = "2016-01-01"
end_date = "2016-12-31"

#- Apply Parameters
select_stores <- data_transactions %>%
  filter(date >= as.POSIXct(start_date)) %>%
  filter(date <= as.POSIXct(end_date)) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
  filter(store_nbr %in%  stores_use)

#- Plot the result
ggplot(select_stores, 
       aes(x=date, y=transactions, group=store_nbr, col=store_nbr)
) + 
  geom_line()

#- Plot Timeline for a Single Store Number ------------------------------------
store_number <- 50

one_store <- data_transactions %>% 
  filter(store_nbr == store_number) %>%
  select(date, transactions) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))

plot(one_store$date, one_store$transactions, main=cat(sprintf("Transactions at Store: %d vs. Time", store_number)))

str(one_store)

#- Analyze by cluster ---------------------------------------------------------
#- Selected cluster
clusters_use = c(13, 4, 10)

#- Selected Time Frame
start_date = "2016-01-01"
end_date = "2016-01-31"

#- Apply Parameters
select_clusters <- merge(x=data_transactions, y=data_stores, y.all=TRUE) %>%
  filter(date >= as.POSIXct(start_date)) %>%
  filter(date <= as.POSIXct(end_date)) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
  filter(cluster %in%  clusters_use) %>%
  group_by(cluster) %>%
  group_by(date, .add=TRUE) %>%
  summarise(total_trans = sum(transactions)) #%>%
  #filter(cluster == 4)

#- Plot the result
ggplot(select_clusters, 
       aes(x=date, y=total_trans, group = cluster, colour=cluster)) + 
  geom_line() + geom_point()



#- Analyse By Day of the Week 
#- Analyse By Weekly totals
#-Analyse By on Promotion or not

#- Oil Price Exploratory Plot
data_oil <- data_oil %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d"))

plot(data_oil, main="Oil Price vs Time")

# Plotting Multiple Stores on the same axes



# Basic Correlation plots of a single cluster with oil 

# todo : join Store transaction data table with oil data table 
store_w_oil <- merge(x=one_store, y=data_oil, all.x=TRUE)

temp_dat_long <- gather(store_w_oil, date, transactions, dcoilwtico)

data_long <- gather(store_w_oil, series, value, transactions, dcoilwtico, factor_key=TRUE)

ggplot(data_long, 
       aes(x=date, y=value, col=series)
) + 
  geom_line()
# todo : combined plot of the data


# Basic Correlation of item data with oil

data_item_oil <- data_train %>%
  # all stores
  group_by(family) %>%
  group_by(date, .add=TRUE) %>%
  summarise(total_sales = sum(sales)) %>%
  spread(family, total_sales) %>%
  merge(data_oil, by = "date", all.x=TRUE, incomparables=NA) %>%
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
  mutate(day_of_week = weekdays(date)) %>%
  mutate(oil_price=na.approx(dcoilwtico, na.rm=FALSE)) %>%
  fill(oil_price, .direction="up") %>%
  select(-dcoilwtico)

# data_item_oil2 <- fill(data_item_oil$dcoilwtico, .direction ="downup")
# data_item_oil3 <- na.approx(data_item_oil$dcoilwtico)

rcorr(as.matrix(select(data_item_oil,-date,-day_of_week)), type="pearson")

item_oil_cor <- cor(select(data_item_oil, -date,-day_of_week))
drinkin_and <- as.matrix(item_oil_cor[,'LIQUOR,WINE,BEER'])

ggplot(data_item_oil, 
       aes(x=date, y=total_sales, group=family, color=family)
) + 
  geom_line()


corrgram(data_item_oil, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Item Family and Oil Correlogram") 
# Shiny Helpers ----------------------------------------
prod_ids_opts <-  unique(data_train$id) %>% sort()
prod_family_opts <- unique(data_train$family) %>% sort()
max_date <- max(as.POSIXct(data_train$date)) %>% sort()
min_date <- min(as.POSIXct(data_train$date)) %>% sort()

cities_opts <- unique(fav_df$city) %>% sort()
states_opts <- unique(fav_df$state) %>% sort()
store_nbr_opts <- unique(fav_df$store_nbr) %>% sort()
store_type_opts <- unique(fav_df$type.x) %>% sort()
cluster_opts <- unique(fav_df$cluster) %>% sort()


# -- Shiny Feature -------------------------------------
# Basic Correlation of each store with oil



# Averages for day of the week (excluding holidays)



# calculate average store transactions correlation with 


# Utility Functions ---------------------------------------

# create vector of all dates over existing date range
dateRangeVec <- range(as.Date(as.character(dt$p_date), format = "%Y%m%d")) 
allDatesVec <- format(seq(from = dateRangeVec[1], 
                          to = dateRangeVec[2], 'days'), "%Y%m%d")

# filter out dates that aren't included
outDt <- data.table(p_date = setdiff(allDatesVec, dt$p_date)) 
#----------------------------------------------------------