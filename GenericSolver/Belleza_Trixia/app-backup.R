#install.packages("shinydashboard")
#install.packages("DT")
# app.R ##
source("PolynomialRegression.r")
source("qsi.r")
source("simplex-2.r")
library(rhandsontable)
library(shiny)
library(shinydashboard)

if(interactive()) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "poly"),
      menuItem("Quadratic Spline Interpolation", tabName = "qsi"),
      menuItem("Simplex", tabName = "simplex")
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
          fluidPage(
            fluidRow(
              column(12,
                     verbatimTextOutput("NumShip"),
                     tableOutput("output_simplex"),
                     actionButton("input_simplex", "Enter"),
                     uiOutput("newWindowContent", style = "display: none;"),
                     tags$script(HTML("
                                      $(document).ready(function() {
                                      if(window.location.hash != '') {
                                      $('div:not(#newWindowContent)').hide();
                                      $('#newWindowContent').show();
                                      $('#newWindowContent').appendTo('body');
                                      }
                                      })
                                      ")),
                     a(href = "#NEW", target = "_blank",
                       actionButton("show_iter", "Show Iterations")),
                     h4("Shipping costs from plant to warehouse:"),
                     rHandsontableOutput("simplex_input_table")
                     )
            )
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
  
  iterations <- list()
  count <- 0
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
    
    plants = c("Denver", "Phoenix", "Dallas", "Shipping Cost", "Demands By:")
    supply = c(310,260,280, 0, 0)
    SC = c(10,6,3,0, 180)
    SL = c(8,5,4,0, 80)
    NM = c(6,4,5,0, 200)
    CI = c(5,3,5,0, 160)
    NYC = c(4,6,9,0, 220)
    
    SC[4] = SC[1] + SC[2] + SC[3]
    SL[4] = SL[1] + SL[2] + SL[3]
    NM[4] = NM[1] + NM[2] + NM[3]
    CI[4] = CI[1] + CI[2] + CI[3]
    NYC[4] = NYC[1] + NYC[2] + NYC[3]
    
    values = c(SC[1], SL[1], NM[1], CI[1], NYC[1], SC[2], SL[2], NM[2], CI[2], NYC[2],SC[3], SL[3], NM[3], CI[3], NYC[3],SC[4], SL[4], NM[4], CI[4], NYC[4])
    df2 = data.frame(plants = plants, supply = supply, Sacramento_California =SC, SaltLakeCity_Utah = SL, Albuquerque_NewMexico = NM, Chicago_Illinois = CI, NewYorkCity = NYC)
    
    datavalues <- reactiveValues(data=df2)
    observeEvent(
      input$simplex_input_table$changes$changes,{
        xi = input$simplex_input_table$changes$changes[[1]][[1]]
        yi =  input$simplex_input_table$changes$changes[[1]][[2]]
        print(paste("Yi: ", yi))
        datavalues$data <- hot_to_r(input$simplex_input_table)
        datavalues$data[4, yi+1] = datavalues$data[1, yi+1] + datavalues$data[2, yi+1] + datavalues$data[3, yi+1]
        print(datavalues$data)
      }
    )
    output$simplex_input_table <- renderRHandsontable({
      rhandsontable(datavalues$data, width = 850, height = 300) %>%
        hot_col("plants", readOnly = TRUE) %>%
        hot_cell(5,"supply", readOnly = TRUE) %>%
        hot_row(4, readOnly=TRUE)
    })
    
    simplex <- eventReactive(input$input_simplex, {
      supply_values <- c()
      objective_values <- c()
      demand_values <- c()
      for(i in 1:3) {
        supply_values <- c(supply_values,input$simplex_input_table$data[[i]][[2]])  
      }
      for(k in 3:7) {
        demand_values <- c(demand_values, input$simplex_input_table$data[[5]][[k]])
      }
      for(m in 1:3) {
        for(j in 3:7) {
          objective_values <- c(objective_values, input$simplex_input_table$data[[m]][[j]])
        }
      }
      acm = generateACM(objective_values, supply_values, demand_values)
      simplex = Simplex(acm)
      sol_set = simplex$sol_set
      iterations <<- simplex$iteration
      result = matrix(data=0, nrow=4, ncol=7, dimnames=list(c("a","b","c","d"), c("Plants","Total","Sacramento,California","Salt Lake City, Utah","Albuquerque, New Mexico","Chicago, Illinois","New York City, New York")))
      total_denver = sol_set[1] + sol_set[2] + sol_set[3] + sol_set[4] + sol_set[5]
      total_phoenix = sol_set[6] + sol_set[7] + sol_set[8] + sol_set[9] + sol_set[10]
      total_dallas = sol_set[11] + sol_set[12] + sol_set[13] + sol_set[14] + sol_set[15]
      total_SC = sol_set[1] + sol_set[6] + sol_set[11]
      total_SLC = sol_set[2] + sol_set[7] + sol_set[12]
      total_NM = sol_set[3] + sol_set[8] + sol_set[13]
      total_CI = sol_set[4] + sol_set[9] + sol_set[14]
      total_NYC = sol_set[5] + sol_set[10] + sol_set[15]
      
      result[1,] = c("Denver", total_denver, sol_set[1], sol_set[2], sol_set[3], sol_set[4], sol_set[5]) 
      result[2,] = c("Phoenix", total_phoenix, sol_set[6], sol_set[7], sol_set[8], sol_set[9], sol_set[10])
      result[3,] = c("Dallas", total_dallas, sol_set[11], sol_set[12], sol_set[13], sol_set[14], sol_set[15])
      result[4,] = c("Totals", "", total_SC, total_SLC, total_NM, total_CI, total_NYC)
      output$NumShip <- renderText({
        print("Number to ship from plant to warehouse:")
      })
      print(result)
      
    })
    
    
    output$output_simplex <- renderTable({
      simplex()
    })
    
    output$newWindowContent <- renderUI({
      mainPanel(
        fluidRow(
          box(
            title = "Simplex Iterations", width=NULL, status = "primary",
            div(style = 'overflow-x: scroll', DT::dataTableOutput('trial')),
            actionButton("go_next", "Next")
          )
        )
      )
    })
    
    go_next <- eventReactive(input$go_next, {
      count <<- count + 1
      DT::datatable(iterations[[count]], colnames = c("x1", "x2", "x3", "x4","x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5","s6", "s7", "s8", "Z", "RHS"),
                    options = list(dom = 't'))
      
    })
    output$trial <- DT::renderDataTable({
      go_next()
    })
  }
  shinyApp(ui, server)
  
}