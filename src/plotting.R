
library(ggplot2)
library(ggpubr)


plot_linear_trend = function(metrics_data, meta_data, time_period, input){
  ## plots linear trends for specified time period on QC trends tab
  
  selected_buffer = input$buffer_qc2
  selected_metric = input$metric
  
  if (time_period == "2 weeks"){
    time_period_in_days = 15
  } else if (time_period == "1 month"){
    time_period_in_days = 32
  } else {
    time_period = "2 months"
    time_period_in_days = 63
  }
    
  # take meta ids of selected buffer
  buffer_ids = meta_data[meta_data["buffer_id"] == selected_buffer, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db
  
  # sort both by acquisition_date
  buffer_data = buffer_data[order(buffer_data$acquisition_date), ]
  
  # get the last date and dates since
  last_date = as.Date(tail(buffer_data$acquisition_date, 1))
  
  recent_data = buffer_data[buffer_data$acquisition_date >= last_date - time_period_in_days, ]
  
  if (nrow(recent_data) < 2){
    # if only a single QC run happened within the interval
    
    ggplot(recent_data, aes(x=acquisition_date, y=eval(parse(text=selected_metric)))) +
      geom_point() +
      geom_smooth(method='lm') +
      labs(x = "Days", y = "Values") +
      # scale_x_continuous(breaks=seq(x[1], x[length(x)] + 1, 1)) +
      scale_x_discrete(labels = substring(recent_data$acquisition_date, 1, 10)) +
      labs(title = paste0(time_period, " trend: ", selected_metric)) +
      theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5),
      plot.subtitle = element_text(color="black", size=14, hjust = 0.5))
    
    
  } else {
    
    # compute time intervals between measurements in days
    days_diffs = difftime(recent_data$acquisition_date[2:length(recent_data$acquisition_date)],
                          recent_data$acquisition_date[1:length(recent_data$acquisition_date)-1], units = "days")
    
    # compute the entire time axis
    for (i in 2:length(days_diffs)){ days_diffs[i] = days_diffs[i-1] + days_diffs[i] }
    days_diffs = c(0, days_diffs)
    
    y = scale(recent_data[recent_data[selected_metric] > 0, selected_metric])
    x = days_diffs[recent_data[selected_metric] > 0]
    
    linear_model = lm(y ~ x)
    score = summary(linear_model)$r.squared
    coeff = linear_model$coefficients[2]
    
    ggplot(data.frame(x=x, y=y), aes(x, y)) +
      geom_point() +
      geom_smooth(method='lm') +
      labs(x = "Days", y = "Scaled values") +
      scale_x_continuous(breaks=seq(x[1], x[length(x)] + 1, 1)) +
      labs(title = paste0(time_period, " trend: ", selected_metric),
           subtitle = paste0("R = ", round(score, 3), ", coef = ", round(coeff, 3))) +
      theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5),
            plot.subtitle = element_text(color="black", size=14, hjust = 0.5))
  }
}

plot_distribution_by_buffer = function(metrics_data, meta_data, qualities_data, input){
  ## plots distribution of a QC characteristic given data and user input
  ## filtering takes place by buffer, then by qualities
  
  # take meta ids of selected buffer
  qc_meta_ids = meta_data[meta_data["buffer_id"] == input$buffer_qc2, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% qc_meta_ids, ]  # in metrics db
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% qc_meta_ids, ]  # in qualities db
  
  # filter out "bad" runs according to general quality
  general_good_data = buffer_data[buffer_qualities$quality == 1,]  # in metrics db
  general_good_qualities = buffer_qualities[buffer_qualities$quality == 1,]  # in qualities db
  
  # filter out "bad" runs according to specified metric
  data = general_good_data[general_good_qualities[input$metric] == 1,]
  
  ggplot(data, aes(x=eval(parse(text=input$metric)))) +
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
    geom_density(alpha=.3, fill="lightblue") +
    geom_vline(aes(xintercept = tail(data[,input$metric], n=1) ),
               linetype = "dashed", size = 0.8, color = "#FC4E07") +
    labs(x = "Value", y = "Frequency") +
    # ggtitle(input$metric) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_chronology_by_buffer = function(metrics_data, meta_data, qualities_data, input){
  ## plots chronological values of a QC characteristic given data and user input
  ## filtering takes place by buffer, then by qualities
  
  # take meta ids of selected buffer
  qc_meta_ids = meta_data[meta_data["buffer_id"] == input$buffer_qc2, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% qc_meta_ids, ]  # in metrics db
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% qc_meta_ids, ]  # in qualities db
  
  # filter out "bad" runs according to specified metric
  data = buffer_data[buffer_qualities[input$metric] == 1,]
  data$run_quality = factor(ifelse(data$quality == 0, "bad", "good"), levels = c("good", "bad"))
  
  # sort by date
  # data = data[rev(order(data$acquisition_date)),]
  data = data[order(data$acquisition_date),]
  data = tail(data, 50)  # plot only last 50 runs (otherwise it's squeezed too much)
  
  ggplot(data, aes(x = acquisition_date, y = eval(parse(text=input$metric)))) +
    geom_line(group = 1) +
    geom_point(aes(color=run_quality), size=2) +  # add red dot in the end
    scale_color_manual(values=c('black', 'red')) +
    scale_x_discrete(labels = substring(data$acquisition_date, 1, 10)) +
    labs(x = "Date", y = "Value") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5),
          legend.position="top")
}

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
  data = data[rev(order(data$acquisition_date)),]  # sort by date
  data = na.omit(data[1:50,])  # plot only last 50 runs (otherwise it's squeezed too much)
  
  
  ggplot(data, aes(x = acquisition_date, y = eval(parse(text=input$metric)))) +
    geom_point(size = 2) + geom_line(group = 1) +
    geom_point(data=data[1, c(input$metric, "acquisition_date")], aes(x = acquisition_date, y = eval(parse(text=input$metric))), color="red", size=2) +  # add red dot in the end
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Date & time", y = "Value") +
    # ggtitle(input$metric) +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_qc_summary_by_buffer = function(metrics_data, meta_data, qualities_data, input){
  ## updated in v.0.1.49, distribution plots are dependent on specified buffer
  ## filtering is done based on metrics qualities
  
  # take meta ids of selected buffer
  qc_meta_ids = meta_data[meta_data["buffer_id"] == input$buffer_qc1, "id"]
  # take entries of selected buffer in metrics db
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% qc_meta_ids, ]
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% qc_meta_ids, ]
  
  plots_list = list()
  
  # setting just a number of a column is probably dump decision
  for (metric_name in metrics_names){
    
    i = which(metric_name == metrics_names)  # iteration
    
    metric_values = buffer_data[buffer_qualities[metric_name] == 1, metric_name]
    metric_dates = buffer_data[buffer_qualities[metric_name] == 1, "acquisition_date"]
    run_qualities = factor(ifelse(buffer_data[buffer_qualities[metric_name] == 1, "quality"] == 1, "good", "bad"), levels = c("good", "bad"))
    
    if (input$date %in% metric_dates){
      # draw a dotted line for this run
      run_index = which(metric_dates == input$date)
      
      df = data.frame(values = metric_values, qc_metric = metric_name, run_quality = run_qualities)
      run_value = df$values[run_index]
      
      plots_list[[i]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_boxplot(width=0.075) +
        labs(x= "", y = "") +
        geom_hline(yintercept = run_value, linetype = "dashed", size = 0.7, color = "#FC4E07") +
        theme(legend.position="none")
      
    } else {
      # don't draw a dotted line for this run
      
      df = data.frame(values = metric_values, qc_metric = metric_name, run_quality = run_qualities)
      
      plots_list[[i]] = ggplot(df, aes(qc_metric, values)) +
        geom_violin(alpha=.3, fill="lightblue") +
        geom_jitter(aes(color=run_quality), shape=1, size=2, alpha = 0.5, position=position_jitter(0.15)) +
        scale_color_manual(values=c('black', 'red')) +
        labs(x= "", y = "") +
        theme(legend.position="none")
      
    }
  }
  
  plot_title = 'QC distributions with selected run'
  
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
