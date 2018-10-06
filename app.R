library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)
library(httr)
library(jsonlite)


ckanSQL <- function(url){
  
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  #Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}
pdf(NULL)

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

types_date <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "general_ledger_date")$general_ledger_date) 
types_amount <-sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)

types_department <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name) 
types_cost_centre <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "cost_center_description")$cost_centre) 

# Define UI for application 
ui <- fluidPage(
  #Application title
  titlePanel("City Wide Revenues and Expenses"), 
  
  sidebarLayout(
    sidebarPanel(
      # General ledger Date Select
      dateRangeInput("DateSelect",
                     "date",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      sliderInput("AmountSelect",
                  "amount:",
                  min = min(types_amount, na.rm = T),
                  max = max(types_amount, na.rm = T),
                  value = c(min(types_amount, na.rm = T), max(types_amount, na.rm = T)),
                  step = 50),
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotlyOutput("barplot"),
                 plotlyOutput("boxplot"),
                 plotlyOutput("pointsplot")
        ),
         tabPanel("Table",
                  DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered cost and revenue data
  crInput <- reactive({
    #Build API Query with proper encodes
    
    AmountFilter <- ifelse(length(input$AmountSelect) > 0, paste0(input$AmountSelect, collapse = "%27,%27"), paste0(types_amount, collapse = "%27,%27"))
    
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E=%20%27",
                   input$DateSelect[1],"%27%20AND%20%22general_ledger_date%22%20%3C=%20%27",input$DateSelect[2],
                   "%27%20AND%20%22amount%22%20%IN%20(%27", AmountFilter,"%27%20)")

    
    #Load and clean data
     cost_rev <- ckanSQL(url) 
        mutate(date = as.Date(general_ledger_date))

     
    return(cost_rev)
    
  })
 
  # Three bars are showing the number of the three chosen department
  output$barplot <- renderPlotly({
    cost_rev <- crInput()
    ggplotly(
      ggplot(data = cost_rev, aes(x = department_name, fill = as.factor(department_name))) + 
        geom_bar() +
        labs(x = "Department Names", title = "Barplot for Department Name") +
        guides(color = FALSE))
  })
  #Using box plots to show the distribution of the three chosen departments
   output$boxplot <- renderPlotly({
      cost_rev <- crInput()
      ggplotly(
        ggplot(data = cost_rev, aes(x = department_name, y = as.numeric(amount))) + 
          geom_point() +
          labs(x = " Department Names", y = "Cost Amount", title = "Boxplot for Department Names and Cost Amount") +
          guides(color = FALSE))
    })
  
  # Using points plots to show the average amount of cost in each date
  
  output$pointsplot <- renderPlotly({
    cost_rev <- crInput()
    ggplotly(
      ggplot(data = cost_rev, aes(x = date, y = as.numeric(amount))) + 
        labs(x = "Dates for Geberal Ledgers", y = "Cost Amount", title = "Points Plot for Dates and Cost Amounts") +
        geom_point())
  })
  

  
  # Data Table
  output$table <- DT::renderDataTable({
    cost_rev <- crInput()
    
    subset(cost_rev, select = c(department_name, cost_centre_description, general_ledger_date, amount))
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
     updateSelectInput(session, "DateSelect", selected = c("2016-01-08","2015-12-01","2015-08-31"))
     updateSliderInput(session, "AmountSelect", value = c(min(types_amount, na.rm = T), max(types_amount, na.rm = T)))
     showNotification("You have successfully reset the filters", type = "message")
   })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
