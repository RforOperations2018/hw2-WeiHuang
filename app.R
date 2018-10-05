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

types_date <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "general_ledger_data")$general_ledger_data)
types_account <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "object_account_description")$object_account_description)
types_depart <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)
types_amount <-sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)

# Define UI for application 
ui <- navbarPage("City Wide Revenues and Expenses", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Department name Select
                              selectInput("DepartmentSelect",
                                          "department name:",
                                          choices = types_depart,
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Department of Finance", "DPW-Operations","DPS-Police")),
                              # General ledger Date Select
                              dateRangeInput("DateSelect",
                                             "date",
                                             start = Sys.Date()-30,
                                             end = Sys.Date()),
                              # Regular Account Select
                              checkboxGroupInput("AccountSelct", 
                                                 "Account Size:",
                                                 choices = types_account,
                                                 selected = c("RUGULAR", "2% LOCAL SARE OF SLOTS REVENUE")),
                              # Amount Selection
                              sliderInput("AmountSelect",
                                          "amount:",
                                          min = min(types_amount, na.rm = T),
                                          max = max(types_amount, na.rm = T),
                                          value = c(min(types_amount, na.rm = T), max(types_amount, na.rm = T)),
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
    #Build API Query with proper encodes
    #cost_rev <- cost_rev %>%
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%2A%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E%3D%20%27%22",
                  input$DateSelect[1],"%27%20AND%20%22general_ledger_date%22%20%3C%3D%20%27",input$DateSelect[2],
                  "%27%20AND%20%22object_account_description%22%20%3D%20%27", input$AccountSelect,
                  "%27%20AND%20%22amount%22%20%3D%20%27", input$AmountSelect,
                  "%27%20AND%20%22department_name%22%20%3D%20%27", input$DepartmentSelecr,"%27")
    #Load and clean data
    cost_rev <- ckanSQL(url) %>%
      filter(grepl("2018", general_ledger_date) )  %>%
      filter(grepl("GRANTS", object_account_description)|
               grepl("CDBG-CITY PLANNING",object_account_description)|
               grepl("OPERATIONAL SUPPLIES",object_account_description)) %>%
      mutate(amount = as.numeric(amount),
             department_name = as.character(department_name),
             cost_center_description = as.character(cost_center_description),
             X_id = as.factor(X_id)) 
      # Slider Filter
      filter(amount >= input$AmountSelect[1] & amount <= input$AmountSelect[2])
    # Department Name Filter
    if (length(input$DepartmentSelect) > 0 ) {
       cost_rev <- subset(cost_rev, department_name %in% input$DepartmentSelect)
    }
    if (length(input$AccountSelct) > 0 ) {
       cost_rev <- subset(cost_rev, object_account_description %in% input$AccountSelct)
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
    updateCheckboxInput(session, "AccountSelct", value = c("RUGULAR", "2% LOCAL SARE OF SLOTS REVENUE"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")