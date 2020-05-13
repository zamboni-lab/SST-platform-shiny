
path = "/Users/andreidm/ETH/projects/shiny_qc/data/nas2_qc_metrics_database_may8.sqlite"
meta_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_meta'))
metrics_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
qualities_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics_qualities'))


# take meta ids of selected buffer
buffer_ids = meta_data[meta_data["buffer_id"] == selected_buffer, "id"]

# take entries of selected buffer
buffer_data = metrics_data[metrics_data["meta_id"][[1]] %in% buffer_ids, ]  # in metrics db

# sort both by acquisition_date
buffer_data = buffer_data[order(buffer_data$acquisition_date), ]

# get the last date and dates since
last_date = as.Date(tail(buffer_data$acquisition_date, 1))

# two_weeks_trends = get_trends_table_for_a_subset(buffer_data, last_date-15)

recent_data = buffer_data[buffer_data$acquisition_date >= last_date-15, ]

# compute time intervals between measurements in days
days_diffs = difftime(recent_data$acquisition_date[2:length(recent_data$acquisition_date)],
                      recent_data$acquisition_date[1:length(recent_data$acquisition_date)-1], units = "days")

# compute the entire time axis
for (i in 2:length(days_diffs)){ days_diffs[i] = days_diffs[i-1] + days_diffs[i] }
days_diffs = c(0, days_diffs)

result = data.frame(matrix(ncol = length(metrics_names), nrow = 1))
colnames(result) = metrics_names

print(paste("DATA FOR trends table SINCE", date_since) )
print(recent_data)

for (metric in metrics_names){
  
  y = scale(recent_data[recent_data[metric] > 0, metric])
  x = days_diffs[recent_data[metric] > 0]
  
  if (length(x) >= 2 & length(y) >= 2){
    # if there's at least two point in recent subset, then fit and define
    
    linear_model = lm(y ~ x)
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



trends_table = rbind(two_weeks_trends, one_month_trends, two_months_trends)

trends_info = data.frame(c("2 weeks", "1 month", "2 months"))
colnames(trends_info) = "time_interval"

trends_table = cbind(trends_info, trends_table)