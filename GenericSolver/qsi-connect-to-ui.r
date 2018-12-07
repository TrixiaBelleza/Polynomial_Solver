#install.packages("shinydashboard")

# app.R ##
source("PolynomialRegression.R")
source("qsi.r")
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
            fileInput("file1", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            tableOutput("contents"),
            h2("Estimate"),
            verbatimTextOutput("regression_estimate"),
            h2("Polynomial"),
            verbatimTextOutput("regression_polynomial")
          )
        ),
        fluidRow(
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
        )
      ),
      
      tabItem(
        tabName = "qsi",
        fluidRow(
          sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            tableOutput("contents"),
            h2("f(x) for all intervals"),
            verbatimTextOutput("func_per_interval"),
            h2("correct f(x) for input x"),
            verbatimTextOutput("correct_func")
          )
        ),
        fluidRow(
          sidebarPanel(
            numericInput("x_value", 
                         h5("Enter x value"),
                         value=0
            ),
            actionButton("enter_x_value", "Enter")
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
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      tbl <- read.csv(inFile$datapath, header = input$header)
      regression <- eventReactive(input$do, {
        degree = input$degree
        func_input = input$func_input
        x <- tbl[,1]
        y <- tbl[,2]
        reg <- PolynomialReg(degree,x,y)
        return(list(estimate = reg$func(func_input), polynomial = reg$polynomial))     
      })
      output$regression_estimate <- renderText({
        reg_output = regression()
        reg_output$estimate
      })
      output$regression_polynomial <- renderText({
        reg_output = regression()
        paste("f <- function(x) ", reg_output$polynomial, sep ="")
      })
      
      quadratic_spline_interpolation <- eventReactive(input$enter_x_value, {
        x_value = input$enter_x_value
        x <- tbl[,1]
        y <- tbl[,2]
        qsi <- QuadraticSplineInterpolation(x,y,x_value)
        return(list(func_per_interval = qsi$func_per_interval, correct_func = qsi$correct_func))
      })
      output$func_per_interval <- renderText({
        qsi_output = quadratic_spline_interpolation()
        qsi_output$func_per_interval
      })
      output$correct_func <- renderText({
        qsi_output = quadratic_spline_interpolation
        qsi_output$correct_func
      })
      return(tbl)
    })
  }
  
  shinyApp(ui, server)
  
}