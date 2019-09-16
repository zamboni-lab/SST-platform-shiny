
library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)

source("constants.R")

# connect to the sqlite file
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_matrix_sep10.db")


# get qc_values as a dataframe
qc_values = dbGetQuery(con, 'select * from qc_values')
qc_metrics_descriptions = data.frame(names=colnames(qc_values)[-1], descriptions=descriptions)

# Define UI
ui <- shinyUI(fluidPage(
  
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
                           htmlOutput("metric_description"),
                           hr(),
                           selectInput("date", "Select run to add comment:", 
                                       choices=rev(qc_values$acquisition_date)),
                           textInput("comment", "Comment:", ""),
                           actionButton("add_button", "Add comment"),
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
))

color.qc.table = function(table){
  
  scoring = table
  
  for (i in 2:ncol(table)){
    
    values = table[,i][table[,i] > 0]
    
    q25 = quantile(values, .25)
    q75 = quantile(values, .75)
    
    scoring[,i] = ifelse (table[,i] > q25 & table[,i] < q75, 1, 0)
    
    table[,i] = paste(
      '<div style="background-color: ',
      ifelse (table[,i] > q25 & table[,i] < q75, "#AAFF8A", "#FF968D"),
      '; border-radius: 5px;">',
      round(table[,i], 4),
      '</div>',
      sep=''
    )
  }
  
  table[,1] = substring(table[,1], 1, 10)
  table$score = paste(apply(scoring[,-1], 1, sum), "/", ncol(scoring)-1, sep = "")
  
  table = table[,c(1,ncol(table), seq(2,ncol(table)-1,1))]
  
  
  return(table)
}

# Define server logic
server <- shinyServer(function(input, output) {
  
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
  
  colored_values = color.qc.table(qc_values)
  
  output$table <- renderTable({ colored_values },
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
  
  
})

# Run the app
shinyApp(ui = ui, server = server)
