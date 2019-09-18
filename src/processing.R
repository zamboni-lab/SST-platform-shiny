
read_qc_values = function(path){
  as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_values'))
}


color_qc_table = function(table){
  
  scoring = table
  
  for (i in 3:ncol(table)){
    
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
  table$score = paste(apply(scoring[,-c(1,2)], 1, sum), "/", ncol(scoring)-2, sep = "")
  
  table = table[,c(1,ncol(table), seq(2,ncol(table)-1,1))]
  table = table[nrow(table):1,]
  
  return(table)
}
