#install.packages("shinydashboard")

# app.R ##
source("PolynomialRegression.r")
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
            tableOutput("contents")
          ),
          sidebarPanel(
            h4("Estimate"),
            verbatimTextOutput("regression_estimate"),
            h4("Polynomial"),
            verbatimTextOutput("regression_polynomial")
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
          ),
          sidebarPanel(
            numericInput("x_value", 
                         h5("Enter x value"),
                         value=0
            ),
            actionButton("x_val_input", "Enter")
          )
        ),
        fluidRow(
          sidebarPanel(
            tableOutput("qsi_contents")
          ),
          sidebarPanel(
            h4("Correct Function"),
            verbatimTextOutput("correct_func")
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
      inFile <- input$file
      
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
      return(tbl)
    })
    output$qsi_contents <- renderTable({
      inFile <- input$qsi_file
      
      if (is.null(inFile))
        return(NULL)
      tbl2 <- read.csv(inFile$datapath, header = input$header)
      quadratic_spline <- eventReactive(input$x_val_input, {
        x_value = input$x_value
        x <- tbl2[,1]
        y <- tbl2[,2]
        qsi <- QuadraticSplineInterpolation(x,y,x_value)
        return(list(func_per_interval = qsi$func_per_interval, correct_func = qsi$correct_func))
      })
      output$correct_func <- renderText({
        qsi_output = quadratic_spline()
        qsi_output$correct_func
      })
      return(tbl2)
    })
    
  }
  
  
  shinyApp(ui, server)
  
}