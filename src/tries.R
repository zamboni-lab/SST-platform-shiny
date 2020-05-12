
path = "/Users/andreidm/ETH/projects/shiny_qc/data/nas2_qc_metrics_database_may8.sqlite"
meta_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_meta'))
metrics_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
qualities_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics_qualities'))


data = metrics_data = metrics_data[order(metrics_data$acquisition_date), ]
date_since = "2020-04-13"

recent_data = data[data$acquisition_date >= date_since, ]

# compute time intervals between measurements in days
days_diffs = difftime(recent_data$acquisition_date[2:length(recent_data$acquisition_date)],
                      recent_data$acquisition_date[1:length(recent_data$acquisition_date)-1], units = "days")

# compute the entire time axis
for (i in 2:length(days_diffs)){ 
  days_diffs[i] = days_diffs[i-1] + days_diffs[i]
}
days_diffs = c(0, days_diffs)

result = data.frame(matrix(ncol = length(metrics_names), nrow = 1))
colnames(result) = metrics_names

metric = "isotopic_presence"

y = scale(recent_data[recent_data[metric] > 0, metric])
x = days_diffs[recent_data[metric] > 0]

linear_model = lm(y ~ x)
summary(linear_model)$r.squared
linear_model$coefficients[2]

plot(x, y)
predict.lm(linear_model, data.frame(x = seq(1,17, 0.1)))



ggplot(data.frame(x=x, y=y), aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Days", y = "Scaled values") +
  scale_x_continuous(breaks=seq(x[1], x[length(x)], 1)) +
  stat_cor(label.x = x[length(x)]-5, label.y = max(y))
  # stat_regline_equation(label.x = 3, label.y = 32)


ggscatter(data.frame(x=x, y=y), x="x", y="y", add = "reg.line") +
  stat_cor(label.x = x[length(x)], label.y = 34) +
  stat_regline_equation(label.x = 3, label.y = 32)


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