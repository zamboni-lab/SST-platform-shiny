
path = "/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_metrics_database_jan14.sqlite"
data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))

scoring = data

data[rev(order(as.Date(data$acquisition_date))),"acquisition_date"]
