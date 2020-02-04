
path = "/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_metrics_database_jan14.sqlite"
full_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))

bad_data = full_data[full_data$quality == 0, ]
good_data = full_data[full_data$quality == 1, ]

# watch table with cross-correlations in 'good' data
View(cor(good_data[5:ncol(good_data)]))

# compare two heatmaps of cross-correlations ('good' vs 'bad' data)
good_data_correlation = cor(good_data[5:ncol(good_data)])
bad_data_correlation = cor(bad_data[5:ncol(bad_data)])

heatmap(good_data_correlation)
heatmap(bad_data_correlation)

library(reshape2)
library(ggplot2)

# nicer plot for good runs
melted_good = melt(good_data_correlation)

ggplot(data = melted_good, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_fixed() +
  ggtitle("Cross-correlations for \"good\" runs")

# nicer plot for bad runs
melted_bad = melt(bad_data_correlation)

ggplot(data = melted_bad, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_fixed() +
  ggtitle("Cross-correlations for \"bad\" runs")

# compare statistically each of 16 qc characteristics
comparisons = as.data.frame(matrix(nrow = 3, ncol = 16))
rownames(comparisons) = c("kolmogorov", "wilcoxon", "kruskall")
colnames(comparisons) = colnames(good_data)[5:ncol(good_data)]

for (metric in colnames(comparisons)){
  
  res = ks.test(good_data[, metric], bad_data[,metric])
  comparisons[1, metric] = res$p.value
  
  res = wilcox.test(good_data[, metric], bad_data[,metric])
  comparisons[2, metric] = res$p.value
  
  res = kruskal.test(list(good_data[, metric], bad_data[,metric]))
  comparisons[3, metric] = res$p.value
}

# watch significant differences
comparisons < 0.05
comparisons < 0.01


