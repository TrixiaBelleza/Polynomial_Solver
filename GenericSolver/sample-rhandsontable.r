
plants = sample(c("Denver","Phoenix","Dallas"), replace=F)
b = sample(1:3, replace=T)
c = b
df = data.frame(plants=plants, b=b, c=c)

datavalues <- reactiveValues(data=df)

output$table <- renderRHandsontable({
  rhandsontable(datavalues$data)
})

observeEvent(
  input$table$changes$changes,
  {
    print(input$table$changes$changes)
    xi = input$table$changes$changes[[1]][[1]] #row
    datavalues$data <- hot_to_r(input$table)
    datavalues$data[xi+1,3] = datavalues$data[xi+1,1] + datavalues$data[xi+2,1]
  }
)
}





