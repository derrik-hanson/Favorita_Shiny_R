# load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

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


 
# Read in Data ----------------------------------------

data_transactions = read.csv("./store-sales-time-series-forecasting/transactions.csv")
data_oil = read.csv("./store-sales-time-series-forecasting/oil.csv")
data_holiday_events = read.csv("./store-sales-time-series-forecasting/holidays_events.csv")
data_stores = read.csv("./store-sales-time-series-forecasting/stores.csv")


# Create Primary DataFrame ----------------------------------------------------
fav_df <- merge(x=data_transactions, y=data_stores, 
                by.x = "store_nbr", by.y = "store_nbr", x.all=TRUE) %>%
          merge(data_holiday_events, by = "date", all.x=TRUE, incomparables=NA) %>%
          mutate(holiday_applies = if_else((locale == "National"), TRUE,
                          if_else((locale == "Regional") & (locale_name == state), TRUE,
                          if_else((locale == "Local") & (locale_name == city), TRUE, false=FALSE)))) %>% 
          merge(data_oil, by = "date", all.x=TRUE, incomparables=NA)


          #- todo: holidays events mutate applicable holiday events
  
          # mutate(date=as.Date(date, format="%Y-%m-%d")) 

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Plot Multiple Stores Over specific window frame -----------------------------

#- Selected Stores
stores_use = c(32)

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
       aes(x=date, y=transactions, col=store_nbr)
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


# Basic Correlation plots of average transactions over all stores with oil


# -- Shiny Feature ----
# Basic Correlation plot of each store with oil

# Averages for day of the week (excluding holidays)



# calculate average store transactions correlation with 


