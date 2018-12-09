#install.packages("shinydashboard")

# app.R ##
source("PolynomialRegression.r")
source("qsi.r")
library(rhandsontable)
library(shiny)
library(shinydashboard)

if(interactive()) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "poly", icon = icon("dashboard")),
      menuItem("Quadratic Spline Interpolation", icon = icon("th"), tabName = "qsi"),
      menuItem("Simplex", icon = icon("th"), tabName = "simplex")
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
            verbatimTextOutput("correct_func"),
            h4("Function Per Interval"),
            tableOutput("func_per_interval")
          )
        )
      ),
      tabItem(
        tabName = "simplex",
        mainPanel(
          h4("Number to ship from plant to warehouse:"),
          rHandsontableOutput("simplex_output_table"),
          h4("Shipping costs from plant to warehouse:"),
          rHandsontableOutput("simplex_input_table")
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
      output$func_per_interval <- renderTable({
        qsi_output = quadratic_spline()
        qsi_output$func_per_interval
      })
      return(tbl2)
    })
    
    plants = c("Denver", "Phoenix", "Dallas", "Totals:", "Demands by")
    
    Sacramento_California = c(0,0,0,0, 180)
    SaltLakeCity_Utah = c(0,0,0,0, 80)
    Albuquerque_NewMexico = c(0,0,0,0, 200)
    Chicago_Illinois = c(0,0,0,0, 160)
    NewYorkCity = c(0,0,0,0, 220)
    Total = Sacramento_California +  SaltLakeCity_Utah +Albuquerque_NewMexico +  Chicago_Illinois + NewYorkCity 
    
    df = data.frame(Plants=plants, Total=Total, Sacramento_California=Sacramento_California, SaltLakeCity_Utah=SaltLakeCity_Utah,  Albuquerque_NewMexico= Albuquerque_NewMexico, Chicago_Illinois=Chicago_Illinois, NewYorkCity=NewYorkCity)
    
    #datavalues <- reactiveValues(data=df) #Keep track of changes
    
    output$simplex_output_table <- renderRHandsontable({
      rhandsontable(df, readOnly = TRUE, width=850, height=300) %>%
        hot_row(5, readOnly = FALSE) %>%
        hot_cell(5, "Plants", readOnly = TRUE)
    })
    
    
    df2 = data.frame(plants = c("Denver", "Phoenix", "Dallas", "Shipping Cost"), supply = c(310,260,280, 0), Sacramento_California = c(10,6,3,0), SaltLakeCity_Utah = c(8,5,4,0), Albuquerque_NewMexico = c(6,4,5,0), Chicago_Illinois = c(5,3,5,0), NewYorkCity = c(4,6,9,0))
    output$simplex_input_table <- renderRHandsontable({
      rhandsontable(df2, width = 850, height = 300) %>%
        hot_col("plants", readOnly = TRUE) 
    })
    
  }
  shinyApp(ui, server)
  
}