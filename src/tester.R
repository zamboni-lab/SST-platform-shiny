
library(shiny)
library(shinyjs)
library(shinydashboard)

source("constants.R")
source("processing.R")
source("plotting.R")

options(warn=-1)  # 0 to set it back

ui = dashboardPage(
  dashboardHeader(title = "FIA-MS Check"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "qc1", icon = icon("dashboard")),
      menuItem("Trends", tabName = "qc2", icon = icon("chart-line")),
      menuItem("Table", tabName = "qc3", icon = icon("th"))
    )),
  
  dashboardBody(
    tags$style(HTML(" .box.box-solid.box-info { background: #F5F5F5 }
                      .box.box-solid.box-info { border-bottom-color:#E3E3E3;
                                                   border-left-color:#E3E3E3;
                                                   border-right-color:#E3E3E3;
                                                   border-top-color:#E3E3E3;
                                                   background: #F5F5F5 } "
                    )),
    tabItems(
    tabItem(tabName = "qc1",
            
            fluidPage(
              titlePanel("QC Summary"),
              
              box(
                status = "info",
                solidHeader = FALSE,
                width = 12,
                column(width=3, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                column(width=3, selectInput("buffer_qc1", "Select buffer:", choices = c()) )
              ),
              box(
                width = 4, status = "info", solidHeader = TRUE,
                helpText("Distributions are displayed for all the data acquired since 2019-05-24. Red dashed line is displayed for good QC runs to indicate the selected QC run value."),
                hr(),
                selectInput("date", "Select run:", choices = c()),
                hr(),
                htmlOutput("score"),
                hr(),
                radioButtons("quality", "Change quality:", choices = list("Good" = 1, "Bad" = 0),
                             selected = 1),
                textInput("comment", "Comment:", ""),
                tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("add_button", function(message) {eval(message.value);});'))),
                actionButton("comment_button", "Add metadata"),
                hr()
              ),
              
              box(
                width = 8, height = 620, status = "primary",
                plotOutput("summary_plot")
              )
            )
    ),
    
    tabItem(tabName = "qc2",
            fluidPage(
              titlePanel("QC Trends"),
              box(
                status = "info",
                solidHeader = FALSE,
                width = 12,
                column(width=3, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                column(width=3, selectInput("buffer_qc2", "Select buffer:", choices = c()) )
              ),
              box(
                width = 3, status = "info", solidHeader = TRUE,
                helpText("Data since 2019-05-24 is shown with bad quality metrics excluded."),
                hr(),
                selectInput("metric", "Choose a QC characteristic:",
                            choices=qc_metrics_descriptions$names),  # date and quality cols excluded
                hr(),
                htmlOutput("metric_description"),
                hr()
              ),
              
              box(
                width = 9, status = "primary",
                plotOutput("chonological_plot"),
                plotOutput("distribution_plot")
              )
            )),
    
    tabItem(tabName = "qc3",
            fluidPage(
              titlePanel("QC Table"),
              box(
                status = "info", solidHeader = FALSE, width = 12,
                column(width=3, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                column(width=3, selectInput("buffer_qc3", "Select buffer:", choices = c()) )
              ),
              box(
                width = 12, status = "primary",
                column(width=3, div(style = 'overflow-x: scroll', tableOutput('table_info'))),
                column(width=9, div(style = 'overflow-x: scroll', tableOutput('table_values')))
              )
              
              
            ))
  ))
)


# Define server logic
server = function(input, output, session) {
  
  # check for updates in the file every other second
  qc_meta = reactiveFileReader(intervalMillis = 1000, session, filePath = metrics_db_path, readFunc = read_qc_meta)
  qc_metrics = reactiveFileReader(intervalMillis = 1000, session, filePath = metrics_db_path, readFunc = read_qc_metrics)
  qc_qualities = reactiveFileReader(intervalMillis = 1000, session, filePath = metrics_db_path, readFunc = read_qc_qualities)
  
  observe({
    # select buffer on QC summary tab
    updateSelectInput(session, "buffer_qc1", choices = unique(qc_meta()["buffer_id"]) )
  })
  
  observe({
    # select buffer on QC trends tab
    updateSelectInput(session, "buffer_qc2", choices = unique(qc_meta()["buffer_id"]) )
  })
  
  observe({
    # select buffer on QC summary tab
    updateSelectInput(session, "buffer_qc3", choices = unique(qc_meta()["buffer_id"]) )
  })
  
  observe({ 
    # take meta ids of selected buffer
    qc_meta_ids = qc_meta()[qc_meta()["buffer_id"] == input$buffer_qc1, "id"]
    # take entries of selected buffer in metrics db
    qc_metrics = qc_metrics()[qc_metrics()["meta_id"][[1]] %in% qc_meta_ids, ]
    
    # select date based on selected buffer
    updateSelectInput(session, "date", choices = qc_metrics[rev(order(as.Date(qc_metrics$acquisition_date))), "acquisition_date"] )
  })
  
  
  output$distribution_plot = renderPlot({ plot_distribution_by_buffer(qc_metrics(), qc_meta(), qc_qualities(), input) })
  output$chonological_plot = renderPlot({ plot_chronology_by_buffer(qc_metrics(), qc_meta(), qc_qualities(), input) })
  output$summary_plot = renderPlot({ plot_qc_summary_by_buffer(qc_metrics(), qc_meta(), input) }, height = 600)
  
  observe({
    output$table_info = renderTable({ get_info_table(qc_metrics(), qc_meta(), qc_qualities(), input$buffer_qc3) },
                                    hover = TRUE, bordered = TRUE,
                                    spacing = 'xs', width = "auto", align = 'c')
  })
  
  observe({
    output$table_values = renderTable({ get_colored_table_for_metrics(qc_metrics(), qc_meta(), qc_qualities(), input$buffer_qc3) },
                                      hover = TRUE, bordered = TRUE,
                                      spacing = 'xs', width = "auto", align = 'c',
                                      sanitize.text.function = function(x) x)
  })
  
  output$metric_description = renderUI({
    HTML(paste(paste("<b>", input$metric, "</b> is computed as"),
               qc_metrics_descriptions[qc_metrics_descriptions$names == input$metric, "descriptions"], sep="<br/>"
    ))
  })
  
  output$score = renderUI({
    HTML(paste("<b>Score:</b>", get_run_score_from_qualities(qc_qualities(), input), "QC characteristics are within good ranges.", sep = " "))
  })
  
  observeEvent(input$comment_button, {
    
    update_databases_with_quality_and_comment(input)
    
    # generate message for user
    js = paste('alert("Metadata for run ', input$date, ' has been updated. The quality field has been changed to ', input$quality, '.");', sep = "")
    session$sendCustomMessage(type='add_button', list(value = js))
    
    # clear comment text area
    updateTextInput(session, "comment", value = "")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
