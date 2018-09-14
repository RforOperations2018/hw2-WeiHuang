library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)


cost_rev <- read.csv('cost_rev.csv', header = TRUE)
cost_rev$fund_number <- NULL
cost_rev$department_number <- NULL
cost_rev$cost_center_number <- NULL
cost_rev$object_account_number <- NULL
cost_rev$ledger_code <- NULL
cost_rev$ledger_descrpition<- NULL

cost_rev <- cost_rev %>%
  mutate(amount = as.numeric(amount),
         department_name = as.character(department_name),
         cost_center_description = as.character(cost_center_description),
         X_id = as.factor(X_id))

pdf(NULL)

# Define UI for application 
ui <- navbarPage("City Wide Revenues and Expenses", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Department name Select
                              selectInput("DepartmentSelect",
                                          "department name:",
                                          choices = sort(unique(cost_rev$department_name)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Department of Finance", "DPW-Operations","DPS-Police")),
                              # General ledger Date Select
                              selectInput("DateSelect",
                                          "date:",
                                          choices = sort(unique(cost_rev$general_ledger_date)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c( "2016-01-08","2015-12-01","2015-08-31")),
                              # Amount Selection
                              sliderInput("AmountSelect",
                                          "amount:",
                                          min = min(cost_rev$amount, na.rm = T),
                                          max = max(cost_rev$amount, na.rm = T),
                                          value = c(min(cost_rev$amount, na.rm = T), max(cost_rev$amount, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("barplot"),
                              plotlyOutput("boxplot"),
                              plotlyOutput("pointsplot")
                              
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Cost Revenue Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered cost and revenue data
  crInput <- reactive({
    cost_rev <- cost_rev %>%
      # Slider Filter
      filter(amount >= input$AmountSelect[1] & amount <= input$AmountSelect[2])
    # Department Name Filter
    if (length(input$DepartmentSelect) > 0 ) {
      cost_rev <- subset(cost_rev, department_name %in% input$DepartmentSelect)
    }
    
    return(cost_rev)
    
  })
  # Reactive melted data
  mcrInput <- reactive({
    crInput() %>%
      melt(id = "X_id")
  })
  # Three bars are showing the number of the three chosen department
  output$barplot <- renderPlotly({
    dat <- crInput()
    ggplotly(
      ggplot(data = dat, aes(x = department_name, fill = as.factor(department_name))) + 
        geom_bar() +
        labs(x = "Department Names", title = "Barplot for Department Name") +
        guides(color = FALSE))
  })
  #Using box plots to show the distribution of the three chosen departments
  output$boxplot <- renderPlotly({
    dat <- crInput()
    ggplotly(
      ggplot(data = dat, aes(x = department_name, y = amount)) + 
        geom_boxplot() +
        labs(x = " Department Names", y = "Cost Amount", title = "Boxplot for Department Names and Cost Amount") +
        guides(color = FALSE))
  })
  # Using points plots to show the average amount of cost in each date
  output$pointsplot <- renderPlotly({
    dat <- crInput()
    ggplotly(
      ggplot(data = dat, aes(x = general_ledger_date, y = amount,fill = department_name)) + 
        labs(x = "Dates for Geberal Ledgers", y = "Cost Amount", title = "Points Plot for Dates and Cost Amounts") +
        geom_point())
  })
  
  # Data Table
  output$table <- DT::renderDataTable({
    cost_rev <- crInput()
    
    subset(cost_rev, select = c(department_name, cost_center_description, general_ledger_date, amount))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cost-revenue-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(crInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "DepartmentSelect", selected = c("Department of Finance", "DPW-Operations","DPS-Police"))
    updateSelectInput(session, "DateSelect", selected = c("2016-01-08","2015-12-01","2015-08-31"))
    updateSliderInput(session, "AmountSelect", value = c(min(cost_rev$amount, na.rm = T), max(cost_rev$amount, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")