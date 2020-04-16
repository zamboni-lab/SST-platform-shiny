
path = "/Users/andreidm/ETH/projects/shiny_qc/data/nas2_qc_metrics_database_apr16.sqlite"
meta_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_meta'))
metrics_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
qualities_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics_qualities'))




idx = meta_data[meta_data["buffer_id"] == "IPA_H2O_DMSO", "id"]

metrics_data[metrics_data$meta_id %in% idx, ]

metrics_data$meta_id == 86

qc_meta_ids = data[data["buffer_id"] == input$buffer, "id"]  # take meta ids of selected buffer
qc_metrics = qc_metrics()[qc_metrics()["meta_id"] == qc_meta_ids, ]  # take entries of selected buffer in metrics db

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
