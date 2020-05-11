
library(DBI)
library(RSQLite)
library(stringr)
library(boot)

source("constants.R")

read_qc_meta = function(path){
  ## this method should have a path arguement to be used in dynamic reader
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_meta'))
}

read_qc_metrics = function(path){
  ## this method should have a path arguement to be used in dynamic reader
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
}

read_qc_qualities = function(path){
  ## this method should have a path arguement to be used in dynamic reader
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics_qualities'))
}


update_databases_with_quality_and_comment = function(input){
  ## updates all three databases with specified quality for the run of chosed acquisition date
  ## also adds user comment to qc_meta table of qc_metrics database
  
  
  # connect to metrics database
  con2 = dbConnect(SQLite(), dbname=metrics_db_path)
  
  # add comment to the database
  user_comment = str_replace_all(input$comment, "'", "")  # otherwise it falls down meeting ' symbol
  
  update_query = paste("update qc_meta set user_comment = '", user_comment,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  
  # add quality value to the database
  update_query = paste("update qc_meta set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  update_query = paste("update qc_metrics set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  update_query = paste("update qc_metrics_qualities set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  
  dbDisconnect(con2)
  
  # connect to features database
  con2 = dbConnect(SQLite(), dbname=features_db_path)
  
  # add quality value to the database
  update_query = paste("update qc_meta set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  update_query = paste("update qc_features_1 set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  update_query = paste("update qc_features_2 set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  
  dbDisconnect(con2)
  
  # connect to tunes database
  con2 = dbConnect(SQLite(), dbname=tunes_db_path)
  
  # add quality value to the database
  update_query = paste("update qc_meta set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  update_query = paste("update qc_tunes set quality = '", input$quality,"' where acquisition_date = '", input$date, "'", sep="")
  dbSendQuery(con2, update_query)
  
  dbDisconnect(con2)
}


get_run_score_from_qualities = function(qualities_table, input){
  ## since v.0.1.24, we use table of qualities stored in database to compute run score
  ## the score is just a some of individual quality fields for all metrics
  
  run_index = which(qualities_table$acquisition_date == input$date)
  run_score = sum(qualities_table[run_index, 5:ncol(qualities_table)])
  
  return(paste(run_score, "/", ncol(qualities_table)-4, sep=""))
}

get_naive_run_score = function(qc_table, input){
  ## old method, not used, renamed into "naive"
  ## computes simple QC score for a new run
  
  run_index = which(qc_table$acquisition_date == input$date)
  run_scoring = qc_table[run_index,]
  
  for (i in 5:ncol(qc_table)){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, i]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    run_scoring[1,i] = ifelse (qc_table[run_index, i] > qs[1] & qc_table[run_index, i] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
  }
  
  # compose simple score
  score = paste(apply(run_scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(run_scoring)-4, sep = "")
  
  return(score)
}


get_ci_based_run_score = function(qc_table, input){
  ## computes simple QC score for a new run
  
  run_index = which(qc_table$acquisition_date == input$date)
  run_scoring = qc_table[run_index,]
  
  # metrics for which low values are warned
  for (metric in c("resolution_200", "resolution_700", "signal", "s2b", "s2n")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 80% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] > ci[1], 1, 0)  # score = 1 if it's > lower CI bound
    
  }
  
  # metrics for which high values are warned
  for (metric in c("average_accuracy", "chemical_dirt", "instrument_noise", "baseline_25_150", "baseline_50_150", "baseline_25_650", "baseline_50_650")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 80% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] < ci[2], 1, 0)  # score = 1 if it's < upper CI bound
  }
  
  # metrics for which out of range values are warned
  for (metric in c("isotopic_presence", "transmission", "fragmentation_305", "fragmentation_712")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 80% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] >= ci[1] & qc_table[run_index, metric] <= ci[2], 1, 0)  # score = 1 if it's within 80% ci
  }
  
  # compose simple score
  score = paste(apply(run_scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(run_scoring)-4, sep = "")
  
  return(score)
}


get_run_score = function(qc_table, input){
  ## outdated in v.0.1.32
  ## computes QC score for a new run, considering different ranges of 'good values'
  
  run_index = which(qc_table$acquisition_date == input$date)
  run_scoring = qc_table[run_index,]
  
  # metrics for which low values are warned
  for (metric in c("resolution_200", "resolution_700", "signal", "s2b", "s2n")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] > qs[2], 1, 0)  # score = 1 if it's within [0.25, 1.] percentiles
  }
  
  # metrics for which high values are warned
  for (metric in c("average_accuracy", "chemical_dirt", "instrument_noise", "baseline_25_150", "baseline_50_150", "baseline_25_650", "baseline_50_650")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] < qs[3], 1, 0)  # score = 1 if it's within [0., 0.75] percentiles
  }
  
  # metrics for which out of range values are warned
  for (metric in c("isotopic_presence", "transmission", "fragmentation_305", "fragmentation_712")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    run_scoring[1, metric] = ifelse (qc_table[run_index, metric] > qs[1] & qc_table[run_index, metric] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
  }
  
  # compose simple score
  score = paste(apply(run_scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(run_scoring)-4, sep = "")
  
  return(score)
}


get_confidence_interval = function(){
  ## NOT IMPLEMENTED
  ## I just think this piece of code will be useful for this function at some point
  
  # take meta ids of selected buffer
  buffer_ids = meta_data[meta_data["buffer_id"] == input$buffer_qc2, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% buffer_ids, ]  # in qualities db
  
  # filter out "bad" runs according to general quality
  general_good_data = buffer_data[buffer_qualities$quality == 1,]  # in metrics db
  general_good_qualities = buffer_qualities[buffer_qualities$quality == 1,]  # in qualities db
  
  # take meta ids of runs with "good" quality of selected metric
  good_quality_metric_ids = general_good_qualities[input$metric == 1, "meta_id"]
  
  # take "good" entries of selected metric
  data = general_good_data[general_good_data[["meta_id"]] %in% good_quality_metric_ids,]
}

get_colored_table_for_metrics = function(metrics_data, meta_data, qualities_data, selected_buffer){
  ## since v.0.1.32, coloring is made with qualities for each metric, independent of general run quality
  ## metrics data is also split by selected buffer

  # take meta ids of selected buffer
  buffer_ids = meta_data[meta_data["buffer_id"] == selected_buffer, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% buffer_ids, ]  # in qualities db
  
  # sort both by acquisition_date and remove meta info (take only values and qualities)
  buffer_data = buffer_data[rev(order(as.Date(buffer_data$acquisition_date))), 5:ncol(buffer_data)]
  buffer_qualities = buffer_qualities[rev(order(as.Date(buffer_qualities$acquisition_date))), 5:ncol(buffer_qualities)]
  
  # edit each column: set color according to qualities
  for (i in 1:ncol(buffer_data)){
    buffer_data[, i] = paste(
      '<div style="background-color: ',
      ifelse (buffer_qualities[, i] == 1, "#AAFF8A", "#FF968D"),  # green, red
      '; border-radius: 5px;">',
      round(buffer_data[, i], 3),
      '</div>',
      sep='')
  }
  
  return(buffer_data)
}


get_trends_table_for_a_subset = function(data, date_since){
  ## subsets supplied dataset by the date, then does some preprocessing and
  ## runs linear regression for each metric to identify trends
  
  recent_data = data[data$acquisition_date >= date_since, ]
  
  # compute time intervals between measurements in days
  days_diffs = difftime(recent_data$acquisition_date[2:length(recent_data$acquisition_date)],
                        recent_data$acquisition_date[1:length(recent_data$acquisition_date)-1], units = "days")
  
  # compute the entire time axis
  for (i in 2:length(days_diffs)){ days_diffs[i] = sum(days_diffs[1:i]) }
  days_diffs = c(0, days_diffs)
  
  result = data.frame(matrix(ncol = length(metrics_names), nrow = 1))
  colnames(result) = metrics_names
  
  for (metric in metrics_names){
    
    y = scale(recent_data[recent_data[metric] > 0, metric])
    x = days_diffs[recent_data[metric] > 0]
    
    if (length(x) >= 2 & length(y) >= 2){
      # if there's at least two point in recent subset, then fit and define
      
      # TODO: test the linear fit itself (maybe it's different to python)
      linear_model = lm(x ~ y)
      score = summary(linear_model)$r.squared
      coeff = linear_model$coefficients[2]
      
      if (score >= 0.01){
        if (abs(coeff) >= 0.05){
          if (coeff < 0){
            result[1, metric] = '<div style="font-size: 100%; font-weight: bold"> &#8600; </div>'  # arrow down
          } else {
            result[1, metric] = '<div style="font-size: 100%; font-weight: bold"> &#8599; </div>'  # arrow up
          }
        } else {
          result[1, metric] = '<div style="font-size: 100%; font-weight: bold"> &#8909; </div>'  # almost equals
        }
      } else {
        result[1, metric] = '<dic style="font-size: 100%; font-weight: bold"> &#8909; </div>'   # almost equals
      }
    } else {
      # otherwise, set to "none" symbol
      result[1, metric] = '<dic style="font-size: 100%; font-weight: bold"> &#8709; </div>'   # empty
    }
  }
  return(result)
}

get_trends_values_table_for_metrics = function(metrics_data, meta_data, selected_buffer){
  ## the table of linear trends: contains results of linear fits for metrics
  
  # take meta ids of selected buffer
  buffer_ids = meta_data[meta_data["buffer_id"] == selected_buffer, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db
  
  # sort both by acquisition_date
  buffer_data = buffer_data[order(buffer_data$acquisition_date), ]
  
  # get the last date and dates since
  last_date = as.Date(tail(buffer_data$acquisition_date, 1))
  
  two_weeks_trends = get_trends_table_for_a_subset(buffer_data, last_date-15)
  one_month_trends = get_trends_table_for_a_subset(buffer_data, last_date-32)
  two_months_trends = get_trends_table_for_a_subset(buffer_data, last_date-63)
  
  trends_table = rbind(two_weeks_trends, one_month_trends, two_months_trends)
  
  trends_info = data.frame(c("2 weeks", "1 month", "2 months"))
  colnames(trends_info) = "time_interval"
  
  trends_table = cbind(trends_info, trends_table)
  
  return(trends_table)
    
}


get_info_table = function(metrics_data, meta_data, qualities_data, selected_buffer){
  ## new version of coloring the table, based on the improved QC scoring
  
  # take meta ids of selected buffer
  buffer_ids = meta_data[meta_data["buffer_id"] == selected_buffer, "id"]
  
  # take entries of selected buffer
  buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db
  buffer_qualities = qualities_data[qualities_data["meta_id"][[1]] %in% buffer_ids, ]  # in qualities db
  
  # sort both by acquisition_date
  buffer_data = buffer_data[rev(order(as.Date(buffer_data$acquisition_date))),]
  buffer_qualities = buffer_qualities[rev(order(as.Date(buffer_qualities$acquisition_date))),]
  
  # calculate scores
  scores = apply(buffer_qualities[, 5:ncol(buffer_qualities)], 1, sum)
  scores = paste(scores, "/", ncol(buffer_qualities)-4, sep="")
  
  # make info_table
  info_table = data.frame(acquisition_date = substring(buffer_data$acquisition_date, 1, 10),
                          score = scores,
                          quality = buffer_data$quality)
  
  return(info_table)
}


color_qc_table = function(qc_table){
  ## method was based on improved scoring
  ## less strict that bootstrap with 80% confidence intervals
  
  scoring = qc_table
  
  # metrics for which low values are warned
  for (metric in c("resolution_200", "resolution_700", "signal", "s2b", "s2n")){
    
    all_previous_values = qc_table[1:nrow(qc_table)-1,]  # filter out last element
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .5, .75, .95))
    
    scoring[, metric] = ifelse (qc_table[, metric] > qs[2], 1, 0)  # score = 1 if it's within [0.25, 1.] percentiles
    
    qc_table[,metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] > qs[3], "#AAFF8A",
              ifelse (qc_table[,metric] <= qs[3] & qc_table[,metric] >= qs[2], "#FFFC9B", "#FF968D")),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # metrics for which high values are warned
  for (metric in c("average_accuracy", "chemical_dirt", "instrument_noise", "baseline_25_150", "baseline_50_150", "baseline_25_650", "baseline_50_650")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .5, .75, .95))
    
    scoring[, metric] = ifelse (qc_table[, metric] < qs[4], 1, 0)  # score = 1 if it's within [0., 0.75] percentiles
    
    qc_table[, metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] < qs[3], "#AAFF8A",
              ifelse (qc_table[,metric] >= qs[3] & qc_table[,metric] <= qs[4], "#FFFC9B", "#FF968D")),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # metrics for which out of range values are warned
  for (metric in c("isotopic_presence", "transmission", "fragmentation_305", "fragmentation_712")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .5, .75, .95))
    
    scoring[, metric] = ifelse (qc_table[, metric] > qs[1] & qc_table[, metric] < qs[5], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
    
    qc_table[,metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] > qs[2] & qc_table[,metric] < qs[4], "#AAFF8A",
              ifelse (qc_table[,metric] <= qs[1] | qc_table[,metric] >= qs[5], "#FF968D", "#FFFC9B")),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # cut time for better display
  qc_table[,3] = substring(qc_table[,3], 1, 10)
  
  # add simple score
  qc_table$score = paste(apply(scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(scoring)-4, sep = "")
  
  # change ordering for convenience
  qc_table = qc_table[,c(1,ncol(qc_table), seq(2,ncol(qc_table)-1,1))]
  qc_table = qc_table[rev(order(as.Date(qc_table$acquisition_date))),]
  
  return(qc_table)
}


make_naive_coloring_for_qc_table = function(qc_table){
  ## old method, renamed into "naive"
  ## adds coloring for the table based on the simple QC score
  
  scoring = qc_table
  
  for (i in 5:ncol(qc_table)){
    
    all_previous_values = qc_table[1:nrow(qc_table)-1,]  # filter out last element
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, i]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    scoring[,i] = ifelse (qc_table[,i] > qs[1] & qc_table[,i] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
    
    qc_table[,i] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,i] > qs[2] & qc_table[,i] < qs[3], "#AAFF8A",  # green
              ifelse (qc_table[,i] <= qs[1] | qc_table[,i] >= qs[4], "#FF968D", "#FFFC9B")),  # red, yellow
      '; border-radius: 5px;">',
      round(qc_table[,i], 4),
      '</div>',
      sep=''
    )
  }
  
  # cut time for better display
  qc_table[,3] = substring(qc_table[,3], 1, 10)
  
  # add simple score
  qc_table$score = paste(apply(scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(scoring)-4, sep = "")
  
  # change ordering for convenience
  qc_table = qc_table[,c(1,ncol(qc_table), seq(2,ncol(qc_table)-1,1))]
  qc_table = qc_table[rev(order(as.Date(qc_table$acquisition_date))),]
  
  return(qc_table)
}

make_ci_based_coloring_for_qc_table = function(qc_table){
  ## adds coloring for the table based on the bootstrapped confidence intervals
  
  scoring = qc_table
  
  # metrics for which low values are warned
  for (metric in c("resolution_200", "resolution_700", "signal", "s2b", "s2n")){
    
    all_previous_values = qc_table[1:nrow(qc_table)-1,]  # filter out last element
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 95% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    scoring[, metric] = ifelse (qc_table[, metric] > ci[1], 1, 0)  # score = 1 if it's > lower bound of 95% CI
    
    qc_table[,metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] > ci[1] & qc_table[,metric] < ci[2], "#FFFC9B",
              ifelse (qc_table[,metric] >= ci[2],  "#AAFF8A", "#FF968D")),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # metrics for which high values are warned
  for (metric in c("average_accuracy", "chemical_dirt", "instrument_noise", "baseline_25_150", "baseline_50_150", "baseline_25_650", "baseline_50_650")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 95% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    # print(metric)
    # print(ci)
    
    scoring[, metric] = ifelse (qc_table[, metric] < ci[2], 1, 0)  # score = 1 if it's < upper bound of 95% CI
    
    qc_table[, metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] > ci[1] & qc_table[,metric] < ci[2], "#FFFC9B",
              ifelse (qc_table[,metric] <= ci[1], "#AAFF8A", "#FF968D")),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # metrics for which out of range values are warned
  for (metric in c("isotopic_presence", "transmission", "fragmentation_305", "fragmentation_712")){
    
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, metric]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    # bootstrapped estimate of 95% confidence interval for mean
    data.boot = boot(values, function(data, index) mean(data[index]), R=5000)
    boot.res = boot.ci(data.boot, conf = 0.8, type = "all")
    ci = c(boot.res$bca[4], boot.res$bca[5])
    
    scoring[, metric] = ifelse (qc_table[, metric] > ci[1] & qc_table[, metric] < ci[2], 1, 0)  # score = 1 if it's within 95% CI
    
    qc_table[,metric] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,metric] >= ci[1] & qc_table[,metric] <= ci[2], "#AAFF8A","#FF968D"),
      '; border-radius: 5px;">',
      round(qc_table[,metric], 4),
      '</div>',
      sep=''
    )
  }
  
  # cut time for better display
  qc_table[,3] = substring(qc_table[,3], 1, 10)
  
  # add simple score
  qc_table$score = paste(apply(scoring[,-c(1,2,3,4)], 1, sum), "/", ncol(scoring)-4, sep = "")
  
  # change ordering for convenience
  qc_table = qc_table[,c(1,ncol(qc_table), seq(2,ncol(qc_table)-1,1))]
  qc_table = qc_table[rev(order(as.Date(qc_table$acquisition_date))),]
  
  return(qc_table)
}

