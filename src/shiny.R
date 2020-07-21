
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
                
                fluidRow(
                  
                  box(
                    status = "info",
                    solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=6, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                    column(width=6, selectInput("buffer_qc1", "Select buffer:", choices = c()) ),
                    column(width=12, helpText("Single chemical mix was used to generate all the data. Buffers were different. Select a buffer to see the corresponding results."))
                  ),
                  
                  box(
                    status = "warning",
                    solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=12, p(tags$b("General info"), "for the selected buffer:")),
                    valueBoxOutput("number_of_good_runs_qc1"),
                    valueBoxOutput("number_of_bad_runs_qc1"),
                    valueBoxOutput("days_since_qc1")
                  )
                ),
                
                fluidRow(
                  box(
                    width = 3, 
                    style = "height:620px;",
                    status = "info", 
                    solidHeader = TRUE,
                    helpText("Distributions are displayed for all the acquired data of selected buffer. Red dashed line indicates selected QC run value."),
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
                    width = 9, 
                    style = "height:620px;",
                    status = "primary",
                    plotOutput("summary_plot")
                  )
                )
                
                
              )
      ),
      
      tabItem(tabName = "qc2",
              fluidPage(
                titlePanel("QC Trends"),
                fluidRow(
                  box(
                    status = "info",
                    solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=6, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                    column(width=6, selectInput("buffer_qc2", "Select buffer:", choices = c()) ),
                    column(width=12, helpText("Single chemical mix was used to generate all the data. Buffers were different. Select a buffer to see the corresponding results."))
                    
                  ),
                  box(
                    status = "warning",
                    solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=12, p("Trends of the", tags$b("last 2 weeks"), ":")),
                    valueBoxOutput("number_of_positive_trends_qc2"),
                    valueBoxOutput("number_of_negative_trends_qc2"),
                    valueBoxOutput("number_of_unchanged_metrics_qc2"),
                    
                  )
                ),
                
                fluidRow(
                  box(
                    width = 3, status = "info", solidHeader = TRUE,
                    style = "height:450px;",
                    helpText("Last 50 runs are plotted with \"bad\" quality metrics excluded."),
                    hr(),
                    selectInput("metric", "Select metric:", choices=qc_metrics_descriptions$metrics_names),  # date and quality cols excluded
                    hr(),
                    htmlOutput("metric_description"),
                    hr(),
                    helpText("Below are linear trends computed for each metric individually. \"Bad\" quality metrics were not excluded. ")
                  ),
                  box(
                    width = 9, status = "primary",
                    style = "height:450px;",
                    plotOutput("chonological_plot")
                  )
                ),
                
                fluidRow(
                  box(
                    width = 12, status = "primary",
                    div(style = 'overflow-x: scroll', tableOutput('trends_table_values'))
                  )
                ),
                
                fluidRow(
                  box(
                    width = 12, status = "primary",
                    column(width=6, plotOutput("two_weeks_trend_plot")),
                    column(width=6, plotOutput("one_month_trend_plot"))
                  )
                )
              )
              
      ),
      
      tabItem(tabName = "qc3",
              fluidPage(
                titlePanel("QC Table"),
                
                fluidRow(
                  box(
                    status = "info", solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=6, shinyjs::useShinyjs(), shinyjs::disabled(textInput("chemical_mix", "Chemical mix:", "20190522_4GHz"))),
                    column(width=6, selectInput("buffer_qc3", "Select buffer:", choices = c()) ),
                    column(width=12, helpText("Single chemical mix was used to generate all the data. Buffers were different. Select a buffer to see the corresponding entries in the table below. Last 100 QC runs are displayed."))
                  ),
                  box(
                    status = "warning",
                    solidHeader = FALSE,
                    width = 6,
                    style = "height:160px;",
                    column(width=12, p(tags$b("General info"), "for the selected buffer:")),
                    valueBoxOutput("number_of_good_runs_qc3"),
                    valueBoxOutput("number_of_bad_runs_qc3"),
                    valueBoxOutput("days_since_qc3")
                  )
                  
                ),
                
                fluidRow(
                  box(
                    width = 12, status = "primary",
                    column(width=3, div(style = 'overflow-x: scroll', tableOutput('table_info'))),
                    column(width=9, div(style = 'overflow-x: scroll', tableOutput('table_values')))
                  )
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
    updateSelectInput(session, "date", choices = qc_metrics[rev(order(qc_metrics$acquisition_date)), "acquisition_date"] )
  })
  
  output$number_of_good_runs_qc1 = renderValueBox({ valueBox(get_number_of_runs(qc_meta(), 1, input$buffer_qc1), "Total \"good\"", icon = icon("database"), color = "green") })
  output$number_of_bad_runs_qc1 = renderValueBox({ valueBox(get_number_of_runs(qc_meta(), 0, input$buffer_qc1), "Total \"bad\"", icon = icon("database"), color = "red") })
  output$days_since_qc1 = renderValueBox({ valueBox(get_number_of_days_since(qc_meta(), input$buffer_qc1), "days since", icon = icon("file-upload"), color = "light-blue") })
  
  output$number_of_good_runs_qc3 = renderValueBox({ valueBox(get_number_of_runs(qc_meta(), 1, input$buffer_qc3), "Total \"good\"", icon = icon("database"), color = "green") })
  output$number_of_bad_runs_qc3 = renderValueBox({ valueBox(get_number_of_runs(qc_meta(), 0, input$buffer_qc3), "Total \"bad\"", icon = icon("database"), color = "red") })
  output$days_since_qc3 = renderValueBox({ valueBox(get_number_of_days_since(qc_meta(), input$buffer_qc3), "days since", icon = icon("file-upload"), color = "light-blue") })
  
  output$number_of_positive_trends_qc2 = renderValueBox({ valueBox(get_number_of_two_weeks_trends_of_type(qc_metrics(), qc_meta(), input$buffer_qc2, "increased"), "Increased", icon = icon("arrow-up"), color = "green") })
  output$number_of_negative_trends_qc2 = renderValueBox({ valueBox(get_number_of_two_weeks_trends_of_type(qc_metrics(), qc_meta(), input$buffer_qc2, "decreased"), "Decreased", icon = icon("arrow-down"), color = "red") })
  output$number_of_unchanged_metrics_qc2 = renderValueBox({ valueBox(get_number_of_two_weeks_trends_of_type(qc_metrics(), qc_meta(), input$buffer_qc2, "unchanged"), "Unchanged", icon = icon("equals"), color = "light-blue") })
  
  output$chonological_plot = renderPlot({ plot_chronology_by_buffer(qc_metrics(), qc_meta(), qc_qualities(), input) }, height = 430)
  output$summary_plot = renderPlot({ plot_qc_summary_by_buffer(qc_metrics(), qc_meta(), qc_qualities(), input) }, height = 600)
  
  output$two_weeks_trend_plot = renderPlot({ plot_linear_trend(qc_metrics(), qc_meta(), "2 weeks", input) })
  output$one_month_trend_plot = renderPlot({ plot_linear_trend(qc_metrics(), qc_meta(), "1 month", input) })
  
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
  
  observe({
    output$trends_table_values = renderTable({ get_trends_values_table_for_metrics(qc_metrics(), qc_meta(), input$buffer_qc2) },
                                             hover = TRUE, bordered = TRUE,
                                             spacing = 'xs', width = "auto", align = 'c',
                                             sanitize.text.function = function(x) x)
  })
  
  output$metric_description = renderUI({
    HTML(paste(paste("<b>", input$metric, "</b> is computed as"),
               qc_metrics_descriptions[qc_metrics_descriptions$metrics_names == input$metric, "descriptions"], sep="<br/>"
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
