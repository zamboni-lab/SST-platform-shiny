
read_qc_values = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_values'))
}


color_qc_table = function(table){
  
  scoring = table
  
  for (i in 3:ncol(table)){
    
    good_run_values = table[table["quality"] == 1, i]  # use only good runs to calculate percentiles
    values = good_run_values[good_run_values > 0]  # filter out missing values
    
    qs = quantile(values, c(.05, .25, .75, .95))
    
    scoring[,i] = ifelse (table[,i] > qs[1] & table[,i] < qs[4], 1, 0)  # score = 1 if it's within [0.05, 0.95] percentiles
    
    table[,i] = paste(
      '<div style="background-color: ',
      ifelse (table[,i] > qs[2] & table[,i] < qs[3], "#AAFF8A",
              ifelse (table[,i] <= qs[1] | table[,i] >= qs[4], "#FF968D", "#FFFC9B")),
      '; border-radius: 5px;">',
      round(table[,i], 4),
      '</div>',
      sep=''
    )
  }
  
  table[,1] = substring(table[,1], 1, 10)
  table$score = paste(apply(scoring[,-c(1,2)], 1, sum), "/", ncol(scoring)-2, sep = "")
  
  table = table[,c(1,ncol(table), seq(2,ncol(table)-1,1))]
  table = table[nrow(table):1,]
  
  return(table)
}
