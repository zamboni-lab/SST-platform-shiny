
library(shiny)
library(shinydashboard)

source("constants.R")
source("processing.R")
source("plotting.R")


ui = dashboardPage(
  dashboardHeader(title = "FIA-MS Check"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("QC", tabName = "qc", icon = icon("dashboard")),
      menuItem("QC trends", tabName = "qc2", icon = icon("chart-line")),
      menuItem("QC table", tabName = "qc3", icon = icon("th"))
    )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "qc",
            
            fluidPage(
              
              titlePanel("QC Summary"),
              sidebarLayout(
                sidebarPanel(
                  helpText("Distributions are displayed for all the data acquired since 2019-05-24. Red dashed line is displayed for good QC runs to indicate the selected QC run value."),
                  hr(),
                  htmlOutput("score"),
                  hr(),
                  selectInput("date", "Select run:", choices = c()),
                  radioButtons("quality", "How was it?", choices = list("Good" = 1, "Bad" = 0),
                               selected = 1),
                  textInput("comment", "Comment:", ""),
                  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("add_button", function(message) {eval(message.value);});'))),
                  actionButton("comment_button", "Add metadata"),
                  hr()
                ),
                mainPanel(
                  plotOutput("summary_plot")
                )
              )
            )
    ),
    
    tabItem(tabName = "qc2",
            fluidPage(
              titlePanel("QC Trends"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("metric", "Choose a QC characteristic:",
                              choices=qc_metrics_descriptions$names),  # date and quality cols excluded
                  hr(),
                  helpText("Data since 2019-05-24 is shown with bad quality runs excluded."),
                  hr(),
                  htmlOutput("metric_description"),
                  hr()
                ),
                mainPanel(
                  plotOutput("chonological_plot"),
                  plotOutput("distribution_plot")
                  
                )
              )
            )),
    
    tabItem(tabName = "qc3",
            fluidPage(
              titlePanel("QC Table"),
              box(
                width = NULL, status = "primary",
                div(style = 'overflow-x: scroll', tableOutput('table'))
              )
              
            ))
  ))
)


# Define server logic
server = function(input, output, session) {
  
  # check for updates in the file every other second
  qc_metrics = reactiveFileReader(intervalMillis = 1000, session, filePath = db_path, readFunc = read_qc_metrics)
  
  observe({ updateSelectInput(session, "date", choices = qc_metrics()[rev(order(as.Date(qc_metrics()$acquisition_date))),"acquisition_date"] ) })
  
  output$distribution_plot = renderPlot({ plot_distribution(qc_metrics(), input) })
  output$chonological_plot = renderPlot({ plot_chronology(qc_metrics(), input) })
  output$summary_plot = renderPlot({ plot_qc_summary(qc_metrics(), input) }, height = 600)
  
  output$table = renderTable({ color_qc_table(qc_metrics()) },
                             hover = TRUE, bordered = TRUE,
                             spacing = 'xs', width = "auto", align = 'c',
                             sanitize.text.function = function(x) x)
  
  output$metric_description = renderUI({
    HTML(paste(paste("<b>", input$metric, "</b> is computed as"),
               qc_metrics_descriptions[qc_metrics_descriptions$names == input$metric, "descriptions"], sep="<br/>"
    ))
  })
  
  output$score = renderUI({
    HTML(paste("<b>Score:</b>", get_run_score(qc_metrics(), input), "QC characteristics are within good ranges.", sep = " "))
  })
  
  observeEvent(input$comment_button, {
    
    con2 = dbConnect(SQLite(), dbname=db_path)
    
    # add comment to the database
    user_comment = str_replace_all(input$comment, "'", "")  # otherwise it falls down meeting ' symbol
    
    update_query = paste("update qc_meta set user_comment = '", user_comment,"' where acquisition_date = '", input$date, "'", sep="")
    dbSendQuery(con2, update_query)
    
    # add quality value to the database
    update_query = paste("update qc_meta set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
    dbSendQuery(con2, update_query)
    update_query = paste("update qc_metrics set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
    dbSendQuery(con2, update_query)
    
    dbDisconnect(con2)
    
    # generate message for user
    js = paste('alert("Metadata for run ', input$date, ' has been updated. The quality field has been changed to ', input$quality, '.");', sep = "")
    session$sendCustomMessage(type='add_button', list(value = js))
    
    # clear comment text area
    updateTextInput(session, "comment", value = "")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
