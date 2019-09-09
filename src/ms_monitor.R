
library(shiny)
library(ggplot2)
library(datasets)

# connect to the sqlite file
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/web_service/data/qc_matrix_example.db")

# get qc_values as a dataframe
qc_values = dbGetQuery(con, 'select * from qc_values')[,-1]

qc_metrics_descriptions = data.frame(names=colnames(qc_values), descriptions="")
descriptions = c("193.072 + mean of absolute mass accuracy of Caffeine divided by average width of the peak",
                 "712.946 + mean absolute mass accuracy of Perfluorotetradecanoic acid divided by average width of the peak",
                 "sum of mean absolute mass accuracies for all 37 ions divided by its number",
                 "sum of all intensities in the chemical noise scan",
                 "sum of all intensities in the instrument noise scan",
                 "mean of the sums of all isotope ratio diffs (in absolute numbers) divided by its number",
                 "mean of intensity of Perfluorotetradecanoic acid (mz ~712) divided by intensity of Fluconazole (mz ~305)",
                 "mean of intensity of the Fluconazole fragment divided by intensity of Fluconazole (mz ~305)",
                 "mean of intensity of the Fluconazole fragment divided by intensity of Perfluorotetradecanoic acid (mz ~712)",
                 "25th percentile intensity from a [150,250] mz range of a chemical noise scan",
                 "50th percentile intensity from a [150,250] mz range of a chemical noise scan",
                 "25th percentile intensity from a [650,750] mz range of a chemical noise scan",
                 "50th percentile intensity from a [650,750] mz range of a chemical noise scan",
                 "sum of mean intensities for all 37 ions",
                 "mean intensity of 3-(Heptadecafluorooctyl)aniline divided by mean 25th percentile intensity from a [500, 550] mz range of a normal scan",
                 "mean intensity of 3-(Heptadecafluorooctyl)aniline divided by (mean 50th percentile intensity - mean 25th percentile intensity) from a [500,550] mz range of a normal scan")

qc_metrics_descriptions$descriptions = descriptions


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