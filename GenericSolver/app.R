#install.packages("shinydashboard")

# app.R ##
source("PolynomialRegression.R")
library(shiny)
library(shinydashboard)

if(interactive()) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "poly", icon = icon("dashboard")),
      menuItem("Quadratic Spline Interpolation", icon = icon("th"), tabName = "qsi")
    )
  )
  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "poly",
        fluidRow(
          sidebarPanel(
            fileInput("file", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            actionButton("display_table", "Show Table"),
            tableOutput("contents")
          )
        )
      )
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Dashboard"),
    sidebar,
    body
  )
  
  server <- function(input, output) {
      table <- eventReactive(input$display_table, {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      file <- input$file
      
      if (is.null(file))
        return(NULL)
      tbl <- read.csv(poly_file$datapath, header = input$header)
     
      return(tbl)
    })
    
    output$contents <- renderTable({
      table()
    })
    
    
  }
  
  shinyApp(ui, server)
  
}