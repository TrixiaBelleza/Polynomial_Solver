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
          sidebarPanel(
            numericInput("degree", 
                         h5("Degree of polynomial"),
                         value=0
            ),
            numericInput("func_input",
                         h5("Integer input x for f(x)"),
                         value=0),
            actionButton("do", "Enter")
          )
        ),
        fluidRow(
          sidebarPanel(
            actionButton("display_table_poly", "Show Table"),
            tableOutput("contents")
          )
        )
      ),
      tabItem(
        tabName = "qsi",
        fluidRow(
          sidebarPanel(
            fileInput("qsi_file", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values, text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          )
        ),
        fluidRow(
          sidebarPanel(
            actionButton("display_table_qsi", "Show Table"),
            tableOutput("qsi_contents")
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
    read_table <- function(file) {
      if (is.null(file))
        return(NULL)
      tbl <- read.csv(file$datapath, header = input$header)
      print(tbl)
      return(tbl)
    }
    poly_table <- eventReactive(input$display_table_poly, {
      read_table(input$file)
    })
    output$contents <- renderTable({
      table = poly_table()
    })

    qsi_table <- eventReactive(input$display_table_qsi, {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      file <- input$qsi_file
      
      if (is.null(file))
        return(NULL)
      tbl <- read.csv(file$datapath, header = input$header)
      print(tbl)
      return(tbl)
    })
    
    output$qsi_contents <- renderTable({
      qsi_table()
    })
  }
  
  shinyApp(ui, server)
  
}