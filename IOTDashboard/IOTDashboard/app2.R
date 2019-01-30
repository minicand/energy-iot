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
              fluidRow(
                    valueBoxOutput("svb1", width = 4),
            
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
                    plotlyOutput("name1"),
                    verbatimTextOutput("laststh"))
                )
              ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Peakhour rates (These would be your costs if you had Peakhour rates)"),
              fluidRow(
                  plotlyOutput("plotlyweekdays"),
                  verbatimTextOutput("pieyea")),
              
              fluidRow(
                plotlyOutput("name2"),
                verbatimTextOutput("namesnsn"))
              )
      )
    )
  )



server <- function(input, output) {
  
  # piechart
  output$plotlypie <- renderPlotly({
    plotly.pie
    })

  # Reactive for changing name for the graphs
  get.data <- reactive({
    switch(input$select1,
           "Monthly" = plotly.mbyd,
           "Yearly" = plotly.yearly,
           "Daily" = plotly.dbyh,
           "Weekly" = plotly.wbyd)
  })

  # Output for name1
  output$name1 <- renderPlotly({
    menudata <- get.data()
    menudata
  })
  
  # # valuebox
  # output$progressBox <- renderValueBox({
  #   valueBox(
  #     paste0(25 + input$count, "%"), "Total", icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plotlyweekdays <- renderPlotly({
    plotly.weekdays
  })
  output$plotlymonth <- renderPlotly({
    plotly.mbyd
  })
  # Interactive second menu
  observeEvent(input$select1,{
    
    if(input$select1== "Daily"){
      output$mamenu <- renderMenu({
        selectInput(inputId="select2", label = "Select day", 
                    choices = costlist[[1]]$DateTime, 
                    selected = costlist[[1]]$DateTime[nrow(costlist[[1]])])})
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

  # Reactive for changing name for the graphs
  # get.data2 <- reactive({
  #   switch(input$select2,
  #          costlist[[1]]$DateTime = costlist[[1]]$global_active_power.1,
  #          costlist[[2]]$DateTime = costlist[[2]]$global_active_power.1,
  #          costlist[[3]]$DateTime = costlist[[3]]$Dglobal_active_power.1,
  #          costlist[[4]]$DateTime = costlist[[4]]$global_active_power.1)
  # })
  # 
  # # Output for select2
  # output$svb1 <- reactive({
  #   menudata2 <- get.data2()
  #   menudata2
  # })
  
}


shinyApp(ui, server)


