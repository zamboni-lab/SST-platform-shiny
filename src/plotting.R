
library(ggplot2)
library(ggpubr)

plot_distribution = function(data, input){
  ## plots distribution of a QC characteristic given data and user input
  
  data = data[data$quality == 1,]  # only good runs are used for plotting
  
  ggplot(data, aes(x=eval(parse(text=input$metric)))) +
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
    geom_density(alpha=.3, fill="lightblue") +
    geom_vline(aes(xintercept = tail(data[,input$metric], n=1) ),
               linetype = "dashed", size = 0.8, color = "#FC4E07") +
    labs(x = "Value", y = "Frequency") +
    # ggtitle(input$metric) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_chronology = function(data, input){
  ## plots chronological values of a QC characteristic given data and user input
  
  data = data[data$quality == 1,]  # only good runs are used for plotting
  data = data[rev(order(as.Date(data$acquisition_date))),]  # sort by date
  data = na.omit(data[1:50,])  # plot only last 50 runs (otherwise it's squeezed too much)
  
  
  ggplot(data, aes(x = acquisition_date, y = eval(parse(text=input$metric)))) +
    geom_point(size = 2) + geom_line(group = 1) +
    geom_point(data=data[1, c(input$metric, "acquisition_date")], aes(x = acquisition_date, y = eval(parse(text=input$metric))), color="red", size=2) +  # add red dot in the end
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Date & time", y = "Value") +
    # ggtitle(input$metric) +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_qc_summary_by_buffer = function(metrics_data, meta_data, input){
  ## since v.0.1.26, distribution plots are dependent on specified buffer
  ## method plots distributions of QC characteristics of a run on a single figure
  
  # take meta ids of selected buffer
  qc_meta_ids = meta_data[meta_data["buffer_id"] == input$buffer, "id"]
  # take entries of selected buffer in metrics db
  data = metrics_data[metrics_data["meta_id"][[1]] %in% qc_meta_ids, ]
  
  plots_list = list()
  
  if (data[data$acquisition_date == input$date, "quality"] == 1){
    # if the selected run has 'quality' = 1, it will be displayed as geom_hline
    plot_title = 'QC distributions with selected "good" run'
    
    data = data[data$quality == 1,]  # only good runs are used for plotting
    run_index = which(data$acquisition_date == input$date)
    
    # setting just a number of a column is probably dump decision
    for (i in 5:ncol(data)){
      
      qc_name = colnames(data)[i]
      df = data.frame(values = data[,i], qc_metric = qc_name)
      run_value = df$values[run_index]
      
      plots_list[[i-4]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_boxplot(width=0.075) +
        labs(x= "", y = "") +
        geom_hline(yintercept = run_value,
                   linetype = "dashed", size = 0.6, color = "#FC4E07") 
      
    }
  } else {
    # if the selected run has 'quality' = 0, it will be displayed as red point
    plot_title = 'QC distributions with selected "bad" run'
    
    run_index = which(data$acquisition_date == input$date)  # save index of selected run
    data = data[c(which(data$quality == 1), run_index),]  # keep good runs and selected run
    
    for (i in 5:ncol(data)){
      
      qc_name = colnames(data)[i]
      df = data.frame(values = data[,i], qc_metric = qc_name)
      run_value = df$values[run_index]
      
      plots_list[[i-4]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_jitter(shape=1, size=3, alpha = 0.5, position=position_jitter(0.15)) +
        geom_hline(yintercept = run_value,
                   linetype = "dashed", size = 0.6, color = "#FC4E07")
        labs(x= "", y = "")
      
    }
  }
  
  figure = ggarrange(plotlist=plots_list, ncol=4, nrow=4)
  figure = annotate_figure(figure, top = text_grob(plot_title, face = "bold", size = 12))
  
  return(figure)
}



plot_qc_summary = function(data, input){
  ## plots distribution of QC characteristics of a run on a single figure
  
  plots_list = list()
  
  if (data[data$acquisition_date == input$date, "quality"] == 1){
    # if the selected run has 'quality' = 1, it's displayed as geom_hline
    plot_title = 'QC distributions with selected run'
    
    data = data[data$quality == 1,]  # only good runs are used for plotting
    run_index = which(data$acquisition_date == input$date)
    
    # setting just a number of a column is probably dump decision
    for (i in 5:ncol(data)){
      
      qc_name = colnames(data)[i]
      df = data.frame(values = data[,i], qc_metric = qc_name)
      run_value = df$values[run_index]
      
      plots_list[[i-4]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_boxplot(width=0.075) +
        labs(x= "", y = "") +
        geom_hline(yintercept = run_value,
                   linetype = "dashed", size = 0.6, color = "#FC4E07") 
      
    }
  } else {
    # if the selected run has 'quality' = 0, it's excluded
    plot_title = 'QC distributions without selected run'
    
    data = data[data$quality == 1,]  # only good runs are used for plotting
    
    for (i in 5:ncol(data)){
      
      qc_name = colnames(data)[i]
      df = data.frame(values = data[,i], qc_metric = qc_name)
      
      plots_list[[i-4]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_jitter(shape=1, size=3, position=position_jitter(0.15)) +
        labs(x= "", y = "")
    }
  }
  
  figure = ggarrange(plotlist=plots_list, ncol=4, nrow=4)
  figure = annotate_figure(figure, top = text_grob(plot_title, face = "bold", size = 12))
  
  return(figure)
}
