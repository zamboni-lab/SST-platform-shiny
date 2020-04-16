
library(DBI)
library(RSQLite)
library(stringr)
library(boot)


read_qc_meta = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_meta'))
}

read_qc_metrics = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
}

read_qc_qualities = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics_qualities'))
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
  # computes QC score for a new run, considering different ranges of 'good values'
  
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

color_qc_table = function(qc_table){
  ## new version of coloring the table, based on the improved QC scoring
  
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
  # old method, renamed into "naive"
  # adds coloring for the table based on the simple QC score
  
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

