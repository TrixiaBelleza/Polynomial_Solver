ui = shinyUI(fluidPage(
  titlePanel("submitButton example"),
  fluidRow(
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
      actionButton("Submit", "Submit")
    )
  ))
)


server = function(input, output) {
  
  output$newWindowContent <- renderUI({
    "Welcome to your new window!"
  })
}

shinyApp(ui=ui, server=server)

