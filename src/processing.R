
library(DBI)
library(RSQLite)
library(stringr)

read_qc_values = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_values'))
}

get_run_score = function(qc_table, input){
  # computes simple QC score for a new run
  
  run_index = which(qc_table$acquisition_date == input$date)
  run_scoring = qc_table[run_index,]
  
  for (i in 3:ncol(qc_table)){
  
    all_previous_values = qc_table[1:(nrow(qc_table)-1), ]  # filter out last run
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, i]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
  
    qs = quantile(values, c(.05, .25, .75, .95))
    
    run_scoring[1,i] = ifelse (qc_table[run_index, i] > qs[1] & qc_table[run_index, i] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
  }
  
  # compose simple score
  score = paste(apply(run_scoring[,-c(1,2)], 1, sum), "/", ncol(run_scoring)-2, sep = "")
  
  return(score)
}


color_qc_table = function(qc_table){
  # adds coloring for the table based on the simple QC score
  
  scoring = qc_table
  
  for (i in 3:ncol(qc_table)){
    
    all_previous_values = qc_table[1:nrow(qc_table)-1,]  # filter out last element
    good_run_values = all_previous_values[all_previous_values["quality"] == 1, i]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    scoring[,i] = ifelse (qc_table[,i] > qs[1] & qc_table[,i] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
    
    qc_table[,i] = paste(
      '<div style="background-color: ',
      ifelse (qc_table[,i] > qs[2] & qc_table[,i] < qs[3], "#AAFF8A",
              ifelse (qc_table[,i] <= qs[1] | qc_table[,i] >= qs[4], "#FF968D", "#FFFC9B")),
      '; border-radius: 5px;">',
      round(qc_table[,i], 4),
      '</div>',
      sep=''
    )
  }
  
  # cut time for better display
  qc_table[,1] = substring(qc_table[,1], 1, 10)
  
  # add simple score
  qc_table$score = paste(apply(scoring[,-c(1,2)], 1, sum), "/", ncol(scoring)-2, sep = "")
  
  # change ordering for convenience
  qc_table = qc_table[,c(1,ncol(qc_table), seq(2,ncol(qc_table)-1,1))]
  qc_table = qc_table[nrow(qc_table):1,]
  
  return(qc_table)
}
