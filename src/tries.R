
path = "/Users/andreidm/ETH/projects/shiny_qc/data/nas2_qc_metrics_database_mar18.sqlite"
data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))



library(boot)

get_mean = function(data, index){
  return(mean(data[index]))
}

data.boot = boot(data[,5], function(data, index) mean(data[index]), R=5000)
boot.res = boot.ci(data.boot, conf = 0.95, type = "all")
ci = c(boot.res$bca[4], boot.res$bca[5])


boot.res$normal
boot.res$basic
boot.res$bca[5]
boot.res$percent

?boot.ci
