
path = "/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_metrics_database_jan14.sqlite"
data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))

scoring = data

data
data[1:10, ]
