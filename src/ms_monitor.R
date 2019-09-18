
library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)
library(stringr)

source("constants.R")
source("processing.R")

# Define UI
ui = fluidPage(
  
  titlePanel("QC characteristics"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Plots",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("metric", "Choose a QC characteristic:",
                                       choices=qc_metrics_descriptions$names),  # date and quality cols excluded
                           hr(),
                           helpText("All acquired data since 2019-05-24 is shown."),
                           hr(),
                           htmlOutput("metric_description"),
                           hr(),
                           selectInput("date", "Select run to add meta data:", choices = c()),
                           radioButtons("quality", "How was it?", choices = list("Good" = 1, "Bad" = 0), 
                                        selected = 1),
                           textInput("comment", "Comment:", ""),
                           tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("add_button", function(message) {eval(message.value);});'))),
                           actionButton("comment_button", "Add comment"),
                           hr()
                         ),
                         mainPanel(
                           plotOutput("distribution_plot"),
                           plotOutput("chonological_plot")
                         )
                       )),
              tabPanel("Table",
                       tableOutput("table"))
  )
)

# Define server logic
server = function(input, output, session) {
  
  # check for updates in the file every other second
  qc_values = reactiveFileReader(intervalMillis = 1000, session, filePath = db_path, readFunc = read_qc_values)
  
  observe({
    updateSelectInput(session, "date", choices = rev(qc_values()$acquisition_date))
  })
  
  # Fill in the spot we created for a plot
  output$distribution_plot = renderPlot({
    
    ggplot(qc_values(), aes(x=eval(parse(text=input$metric)))) +
      geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
      geom_density(alpha=.3, fill="lightblue") +
      geom_vline(aes(xintercept = tail(qc_values()[,input$metric], n=1) ),
                 linetype = "dashed", size = 0.8, color = "#FC4E07") +
      labs(x = "Value", y = "Frequency") +
      ggtitle(input$metric) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$chonological_plot = renderPlot({
    
    ggplot(qc_values(), aes(x = acquisition_date, y = eval(parse(text=input$metric)))) +
      geom_point(size = 2) + geom_line(group = 1) +
      geom_point(data=qc_values()[nrow(qc_values()), c(input$metric, "acquisition_date")], aes(x = acquisition_date, y = eval(parse(text=input$metric))), color="red", size=2) +  # add red dot in the end
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Date & time", y = "Value") +
      ggtitle(input$metric) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$table <- renderTable({ color_qc_table(qc_values()) },
                              hover = TRUE, bordered = TRUE,
                              spacing = 'xs', width = "auto", align = 'c',
                              sanitize.text.function = function(x) x)
  
  output$metric_description = renderUI({
    HTML(
      paste(
        paste(
          "<b>", input$metric, "</b> is computed as"
        ),
        qc_metrics_descriptions[qc_metrics_descriptions$names == input$metric, "descriptions"], sep="<br/>"
      )
    )
    
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
    update_query = paste("update qc_values set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
    dbSendQuery(con2, update_query)
    
    dbDisconnect(con2)
    
    # generate message for user
    js = paste('alert("Meta data for run ', input$date, ' has been updated.");', sep = "")
    session$sendCustomMessage(type='add_button', list(value = js))
    
    # clear comment text area
    updateTextInput(session, "comment", value = "")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
