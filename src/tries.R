
library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)

source("constants.R")

# connect to the sqlite file
# con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/ms_monitor/data/qc_matrix_example.db")
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_matrix_sep10.db")


# get qc_values as a dataframe
qc_values = dbGetQuery(con, 'select * from qc_values')
qc_metrics_descriptions = data.frame(names=colnames(qc_values)[-1], descriptions=descriptions)





# Define UI
ui <- fluidPage(
  
  # Give the page a title
  titlePanel("QC characteristics"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Plots", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("metric", "Choose a QC characteristic:", 
                                       choices=colnames(qc_values)[-1]),
                           hr(),
                           helpText("All acquired data since 2019-05-24 is shown."),
                           hr(),
                           htmlOutput("metric_description")
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
  
  # Generate a row with a sidebar
 
# Define server logic
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$distribution_plot = renderPlot({
    
    ggplot(qc_values, aes(x=eval(parse(text=input$metric)))) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
      geom_density(alpha=.3, fill="lightblue") +
      geom_vline(aes(xintercept = tail(qc_values[,input$metric], n=1) ), 
                 linetype = "dashed", size = 0.8, color = "#FC4E07") +
      labs(x = "Value", y = "Frequency") + 
      ggtitle(input$metric) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$chonological_plot = renderPlot({
    
    ggplot(qc_values, aes(x = acquisition_date, y = eval(parse(text=input$metric)))) +
      geom_point(size = 2) + geom_line(group = 1) +
      geom_point(data=qc_values[nrow(qc_values), c(input$metric, "acquisition_date")], aes(x = acquisition_date, y = eval(parse(text=input$metric))), color="red", size=2) +  # add red dot in the end
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = "Date & time", y = "Value") + 
      ggtitle(input$metric) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$table <- renderTable({ qc_values },
                              hover = TRUE, bordered = TRUE,
                              spacing = 'xs', width = "auto", align = 'c',
                              digits = 4)
  
  output$metric_description = renderUI({
    HTML(paste(paste(input$metric, "is computed as"), qc_metrics_descriptions[qc_metrics_descriptions$names == input$metric, "descriptions"], sep="<br/>"))
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)