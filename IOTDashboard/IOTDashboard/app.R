## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
                    
  dashboardHeader(title = "B2C Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(id = "First Sidebar",  # to call it in another app later
      menuItem("Current Rate", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Peak-hours Rate", tabName = "widgets", icon = icon("dashboard"), 
               badgeLabel = "try me", badgeColor = "green"),
      
      selectInput(inputId="select", label = "Select time period", 
                               choices = list("Monthly", "Weekly", "Daily", "Yearly"), 
                               selected = 1)
               )
      ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                    valueBoxOutput("progressBox", width = 4),
              
                    valueBox(10 * 2, "HVAC", icon = icon("credit-card"), width= 2, color= "green"),
                    valueBox(20 * 2, "Laundry", icon = icon("credit-card"), width = 2, color= "green"),
                    valueBox(60 * 2, "Kitchen", icon = icon("credit-card"), width = 2, color= "green"),
                    valueBox(70 * 2, "Rest", icon = icon("credit-card"), width = 2, color= "green")
              ),
              
              fluidRow(
                box(width = 5,
                    plotlyOutput("plotlypie"),
                    verbatimTextOutput("event")),
                       # actionButton("count", "Increment progress"))
                box(width = 7,
                    plotlyOutput("plotlymonth"),
                    verbatimTextOutput("lastmonth"))
                )
              ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Peakhour rates (These would be your costs if you had Peakhour rates)"),
              fluidRow(
                  plotlyOutput("plotlyweekdays"),
                  verbatimTextOutput("pieyea")),
              
              fluidRow(
                  box(plotOutput("name1"))
              )
              )
              )
              
              )       
)


server <- function(input, output) {
  
  # piechrt
  output$plotlypie <- renderPlotly({
    plotly.pie
    })
  # sidebar menu - period select
  # output$Histogram <- renderPlot({ 
  #   menudata <- input$select
  #   plot(menudata$Sub_metering_1)
  #   })
  # 
    # by.day

  # Reactive for changing name
  get.data <- reactive({
    switch(input$select,
           "Monthly" = by.month,
           "Yearly" = by.year,
           "Daily" = by.day,
           "Weekly" = by.week)
  })

  # Output for name1
  output$name1 <- renderPlot({
    menudata <- get.data()
    plot(menudata$Sub_metering_1)
  })
  # valuebox
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Total", icon = icon("list"),
      color = "purple"
    )
  })
  # renderPlotly() also understands ggplot2 objects!
  output$plotlyweekdays <- renderPlotly({
    plotly.weekdays
  })
  output$plotlymonth <- renderPlotly({
    plotly.mbyd
  })
}

shinyApp(ui, server)


