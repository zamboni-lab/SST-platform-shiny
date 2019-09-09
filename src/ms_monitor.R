
library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)

source("constants.R")

# connect to the sqlite file
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/ms_monitor/data/qc_matrix_example.db")

# get qc_values as a dataframe
qc_values = dbGetQuery(con, 'select * from qc_values')[,-1]
qc_metrics_descriptions = data.frame(names=colnames(qc_values), descriptions=descriptions)

# Define UI
ui <- fluidPage(
  
    # Give the page a title
    titlePanel("QC metrics distributions"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("metric", "Choose QC metric:", 
                    choices=colnames(qc_values)),
        hr(),
        helpText("Emperical distributions of QC metrics available for all the data acquired so far."),
        hr(),
        htmlOutput("metric_description")
      ),
      
        
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("metric_distribution_plot")  
      )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Fill in the spot we created for a plot
    output$metric_distribution_plot <- renderPlot({
      
      ggplot(qc_values, aes(x=eval(parse(text=input$metric)))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
        geom_density(alpha=.3, fill="lightblue") +
        geom_vline(aes(xintercept = tail(qc_values[,input$metric], n=1) ), 
                   linetype = "dashed", size = 0.8, color = "#FC4E07") +
        labs(x = "values", y = "frequency") + 
        ggtitle(input$metric) +
        theme(plot.title = element_text(hjust = 0.5))

        })
    
    output$metric_description <- renderUI({
      HTML(paste(paste(input$metric, "is computed as"), qc_metrics_descriptions[qc_metrics_descriptions$names == input$metric, "descriptions"], sep="<br/>"))
      
    })
    
}

# Run the app
shinyApp(ui = ui, server = server)