#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Libraryies ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read in Data -------------------------------------------------------------
base_path = "~/DSA/R_project/Favorita_Shiny_R/predicting_favorita/"
data_transactions = read.csv(paste(base_path,"store-sales-time-series-forecasting/transactions.csv", sep =""))
data_oil = read.csv(paste(base_path,"store-sales-time-series-forecasting/oil.csv", sep =""))
data_holiday_events = read.csv(paste(base_path,"store-sales-time-series-forecasting/holidays_events.csv", sep =""))
data_stores = read.csv(paste(base_path,"store-sales-time-series-forecasting/stores.csv", sep =""))


# Define UI for application -------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Predicting Favorita"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 50), 
            # Input Stupbs for Favorita
            checkboxGroupInput("checkCity", label = h3("Cities"), 
                               # unique(fav_df$store_nbr) %>% sort()
                               choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                                              "6" = 6, "7" = 7, "53" = 53, "54" = 54),
                               selected = 3),
            
            checkboxGroupInput("checkStates", label = h3("States"), 
                               # states_vec <- unique(fav_df$state) %>% sort()
                               choices = list("Azuay" = "Azuay", "Bolivar"="Bolivar", "Chimborazo"="Chimborazo"),
                               selected = "Bolivar"),
                               
           checkboxGroupInput("checkCity", label = h3("City"), 
                              #citiees_vec <- unique(fav_df$city) %>% sort()
                              choices = list("Ambato" = "Ambato", "Babahoyo" = "Babahoyo"),
                              selected = "Babahoyo"),
                               
            checkboxGroupInput("checkStores", label = h3("Store"), 
                               # unique(fav_df$store_nbr) %>% sort()
                               choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                                              "6" = 6, "7" = 7, "53" = 53, "54" = 54),
                               selected = 3),
            
           checkboxGroupInput("checkStoreType", label = h3("Store Type"), 
                              #store_type_vec <- unique(fav_df$type.x) %>% sort()
                              choices = list("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E"),
                              selected = "D"),
                                
           
            checkboxGroupInput("checkClusters", label = h3("Cluster"), 
                               # unique(fav_df$cluster) %>% sort()
                               choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                                              "6" = 6, "7" = 7, "9"),
                               selected = 3),
            
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = "2016-01-01", end = "2017-07-01"
            ),
        
        
        # box(status = "warning",
        #     selectInput("interval", "Refresh interval",
        #                 choices = c(
        #                     "30 seconds" = 30,
        #                     "1 minute" = 60,
        #                     "2 minutes" = 120,
        #                     "5 minutes" = 300,
        #                     "10 minutes" = 600
        #                 ),
        #                 selected = "60"
        #     ),
        #     uiOutput("timeSinceLastUpdate"),
        #     actionButton("refresh", "Refresh now"),
        #     p(class = "text-muted",
        #       br(),
        #       "Source data updates every 30 seconds."
        #     )
        # )
    ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("transPlot"), 
           plotOutput("clusterPlot")
           
        )
    )
)

# Define server logic ----------------------------------------------------------
server <- function(input, output) {
    
    # Generate Output ----------------------------------------------------------
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # Generate Store Plot Output -----------------------------------------------
    output$transPlot <-  renderPlot({
        
        #- Selected Stores
        stores_use = as.integer(input$checkStores)
        
        #- Selected Time Frame
        start_date = input$dateRange[1]
        end_date = input$dateRange[2]
        
        # start_date = "2016-01-01"
        # end_date = "2016-12-31"
        
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
    })
    # Generate Store Plot Output -----------------------------------------------
    output$clusterPlot <-  renderPlot({
        
    clusters_use = as.integer(input$checkClusters)
    
    #- Selected Time Frame
    start_date = input$dateRange[1]
    end_date = input$dateRange[2]
    
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
