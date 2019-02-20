## app.R ##
library(shiny)
library(shinydashboard)
pacman::p_load(shiny, shinydashboard, rsconnect, Rcpp, RcppRoll, SQUAREM, plotly)

# Shinyapps.io
rsconnect::setAccountInfo(name='deniz', 
                          token='77C096A0EBB84DE88B05C2D32C65B80F', 
                          secret='J59lEshRsj6ckEQOUEsjL5BX67n4GBGpEBE6xysH')
rsconnect::deployApp()
rsconnect::showLogs()

# Restore objects
plotly.pie <- readRDS("ppie.rds")
plotly.dbyh <- readRDS("pdbyh.rds")
plotly.wbyd <- readRDS("pwbyd.rds")
plotly.mbyd <- readRDS("pmbyd.rds")
plotly.yearly <- readRDS("pyearly.rds")
costlist <- readRDS("costlist.rds")
plotly.weekdays <- readRDS("pwd.rds")
load("shinydata.RData")

ui <- dashboardPage(skin = "green",
                    
  dashboardHeader(title = "B2C Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(id = "First Sidebar",  # to call it in another app later
      menuItem("Current Rate", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Peak-hours Rate", tabName = "db2", icon = icon("dashboard"), 
               badgeLabel = "try me", badgeColor = "green"),
      
      selectInput(inputId="select1", label = "Select time period", 
                               choices = list("Monthly", "Weekly", "Daily", "Yearly"), 
                               selected = 1),
      menuItemOutput("mamenu")
               )
      ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Current rate"),
              fluidRow(
                    valueBoxOutput("totalvb", width = 4),
                    valueBoxOutput("hvacvb", width = 2),
                    valueBoxOutput("laundryvb", width = 2),
                    valueBoxOutput("kitchenvb", width = 2),
                    valueBoxOutput("nonsubvb", width = 2)
                    ),
            
              fluidRow(
                box(width = 5,
                    plotlyOutput("plotlypie"),
                    verbatimTextOutput("event")),
                       # actionButton("count", "Increment progress"))
                box(width = 7,
                    plotlyOutput("name1"),
                    verbatimTextOutput("laststh"))
                )
              ),
      
      # Second tab content
      tabItem(tabName = "db2",
              h2("Peakhour rates (These would be your costs if you had Peakhour rates)"),
              fluidRow(
                        valueBoxOutput("totalvb2", width = 4),
                        valueBoxOutput("hvacvb2", width = 2),
                        valueBoxOutput("laundryvb2", width = 2),
                        valueBoxOutput("kitchenvb2", width = 2),
                        valueBoxOutput("nonsubvb2", width = 2)
                      )
              )
    )
  )
)
  




server <- function(input, output) {
  
  # Granularity dictionary
  granul <- c(
    "Monthly" = "month",
    "Weekly" = "week",
    "Daily" = "day",
    "Yearly" = "year"
  )
  
  # Function for valuebox
  fValuebox <- function(granularity, column, row){
    output <- paste0(round(costlist[[granularity]][costlist[[granularity]]$DateTime == row, column], 2), "€")
    return(output)
  }
  
  # piechart
  output$plotlypie <- renderPlotly({
    plotly.pie
    })

  # Output for name1
  output$name1 <- renderPlotly({
    menudata <- get.data()
    menudata
  })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plotlyweekdays <- renderPlotly({
    plotly.weekdays
  })
  
  output$plotlymonth <- renderPlotly({
    plotly.mbyd
  })
  
  # Reactive for changing name for the graphs
  get.data <- reactive({
    switch(input$select1,
           "Monthly" = plotly.mbyd,
           "Yearly" = plotly.yearly,
           "Daily" = plotly.dbyh,
           "Weekly" = plotly.wbyd)
  })
  
  # INTERACTIVE SECOND MENU
  observeEvent(input$select1,{
    
    if(input$select1== "Daily"){
      output$mamenu <- renderMenu({
        selectInput(inputId="select2", label = "Select day", 
                    choices = costlist[[1]]$DateTime, 
                    selected = costlist[[1]]$DateTime[nrow(costlist[[1]])])}
        # options = list(maxOptions = 1500)
        )
      }
    
    if(input$select1== "Weekly"){
      output$mamenu <- renderMenu({
        selectInput(inputId="select2", label = "Select week", 
                    choices = costlist[[2]]$DateTime, 
                    selected = costlist[[2]]$DateTime[nrow(costlist[[2]])])})
      }
    
    if(input$select1== "Monthly"){
      output$mamenu <- renderMenu({
        selectInput(inputId="select2", label = "Select month", 
                    choices = costlist[[3]]$DateTime, 
                    selected = costlist[[3]]$DateTime[nrow(costlist[[3]])])})
      }
    
    if(input$select1== "Yearly"){
      output$mamenu <- renderMenu({
        selectInput(inputId="select2", label = "Select year", 
                    choices = costlist[[4]]$DateTime, 
                    selected = costlist[[4]]$DateTime[nrow(costlist[[4]])])})
      }
    
    })


 # Output for Global active power  
      output$totalvb <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "global_active_power.1", input$select2),
          "Total", icon = icon("piggy-bank"), color = "yellow"
        )
        
        })
      
      # Output for valuebox HVAC
      output$hvacvb <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_3.1", input$select2),
          "HVAC", icon = icon("leaf"), color = "green"
        )
      })
      
      # Output for valuebox Laundry
      output$laundryvb <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_2.1", input$select2),
            "Laundry", icon = icon("certificate"), color = "green"
          )
      })
      
      # Output for valuebox Kitchen
      output$kitchenvb <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_1.1", input$select2),
          "Kitchen", icon = icon("cutlery"), color = "green"
        )
      })
      
      # Output for valuebox Non-subbed
      output$nonsubvb <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "non_subbed.1", input$select2),
          "Rest", icon = icon("credit-card"), color = "green"
        )
      })
 
      
      
      # Output for valuebox GAP2
      output$totalvb2 <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "global_active_power.2", input$select2),
          "Total.", icon = icon("piggy-bank"), color = "yellow"
        )
        
      })
      
      # Output for valuebox HVAC2
      output$hvacvb2 <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_3.2", input$select2),
          "HVAC", icon = icon("leaf"), color = "green"
        )
      })
      
      # Output for valuebox Laundry2
      output$laundryvb2 <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_2.2", input$select2),
          "Laundry", icon = icon("certificate"), color = "green"
        )
      })
      
      # Output for valuebox Kitchen2
      output$kitchenvb2 <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "Sub_metering_1.2", input$select2),
          "Kitchen", icon = icon("cutlery"), color = "green"
        )
      })
      
      # Output for valuebox Non-subbed2
      output$nonsubvb2 <- renderValueBox({
        valueBox(
          fValuebox(granul[[input$select1]], "non_subbed.2", input$select2),
          "Rest", icon = icon("credit-card"), color = "green"
        )
      })
      
      # # Output for valuebox Non-subbed2
      # output$nonsubvb2 <- renderValueBox({
      #   if(input$select1== "Daily"){
      #     valueBox(
      #       paste0(round(costlist[[1]]$non_subbed.2[costlist[[1]]$DateTime == input$select2], 2), "€"),
      #       "Rest2", icon = icon("credit-card"), color = "green"
      #     )
      #   } else {
      #     
      #     if(input$select1== "Weekly"){
      #       valueBox(
      #         paste0(round(costlist[[2]]$non_subbed.2[costlist[[2]]$DateTime == input$select2], 2), "€"),
      #         "Rest2", icon = icon("credit-card"), color = "green"
      #       )
      #     } else {
      #       if(input$select1== "Monthly"){
      #         valueBox(
      #           paste0(round(costlist[[3]]$non_subbed.2[costlist[[3]]$DateTime == input$select2], 2), "€"),
      #           "Rest2", icon = icon("credit-card"), color = "green"
      #         )
      #       } else {
      #         valueBox(
      #           paste0(round(costlist[[4]]$non_subbed.2[costlist[[4]]$DateTime == input$select2], 2), "€"),
      #           "Rest2", icon = icon("credit-card"), color = "green")
      #       }
      #     }
      #   }
      # })
}


shinyApp(ui, server)

