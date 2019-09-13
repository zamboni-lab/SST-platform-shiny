
# "#EC6D57" "#81D652"


library(shiny)

ui <- shinyUI(fluidPage(
  tableOutput("tt"))
)

qc_values[6,2]

server <- shinyServer(function(input, output) {

  test[1, 1] = '<div style="background-color: #EC6D57; border-radius: 5px;">1</div>'
  test[1, 2] = "asfsf"
  test[2, 2] = '<div style="background-color: #81D652; border-radius: 5px;">2</div>'


  output$tt <- renderTable({
    test
  }, sanitize.text.function = function(x) x)
})

shinyApp(ui = ui, server = server)
