
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

# distribution plots
plots_list = list()

# resolution_200 distribution
df = data.frame(qc_metric = "resolution_200",
                values = full_data$resolution_200,
                quality = factor(full_data$quality))

df = df[-6,]

d_plot_1 = ggplot(df, aes(qc_metric, values)) +
  geom_violin(alpha=.3, fill="lightblue") +
  geom_boxplot(width=0.075) +
  geom_jitter(shape=1, size=3, position=position_jitter(0.15), aes(colour = quality)) +
  labs(x= "", y = "") +
  scale_colour_manual(name="quality", values=c("red", "black"))

# average accuracy distribution
df = data.frame(qc_metric = "average_accuracy",
                values = full_data$average_accuracy,
                quality = factor(full_data$quality))

df = df[-41,]

d_plot_2 = ggplot(df, aes(qc_metric, values)) +
  geom_violin(alpha=.3, fill="lightblue") +
  geom_boxplot(width=0.075) +
  geom_jitter(shape=1, size=3, position=position_jitter(0.15), aes(colour = quality)) +
  labs(x= "", y = "") +
  scale_colour_manual(name="quality", values=c("red", "black"))

# instrument noise distribution
df = data.frame(qc_metric = "instrument_noise",
                values = full_data$instrument_noise,
                quality = factor(full_data$quality))

df = df[-c(4, 11, 20, 55),]

d_plot_3 = ggplot(df, aes(qc_metric, values)) +
  geom_violin(alpha=.3, fill="lightblue") +
  geom_boxplot(width=0.075) +
  geom_jitter(shape=1, size=3, position=position_jitter(0.15), aes(colour = quality)) +
  labs(x= "", y = "") +
  scale_colour_manual(name="quality", values=c("red", "black"))

# compose
plots_list[[1]] = d_plot_1
plots_list[[2]] = d_plot_2
plots_list[[3]] = d_plot_3

figure = ggarrange(plotlist=plots_list, ncol=3, nrow=1)
figure = annotate_figure(figure, top = text_grob("Distributions of some QC characteristics", face = "bold", size = 12))
figure

# chronological plots
plots_list = list()

# fragmentation_712 plot
df = data.frame(qc_metric = "fragmentation_712",
                values = good_data$fragmentation_712,
                acquisition_date = good_data$acquisition_date)

df = df[rev(order(as.Date(df$acquisition_date))),]  # sort by date
df = na.omit(df[1:50,])  # plot only last 50 runs (otherwise it's squeezed too much)
df[,3] = substring(df[,3], 1, 10)

chr_plot_1 = ggplot(df, aes(x = acquisition_date, y = values)) +
  geom_point(size = 2) + geom_line(group = 1) +
  geom_point(data = df[1, ], aes(x = acquisition_date, y = values), color="red", size=2) +  # add red dot in the end
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Date", y = "Value") +
  ggtitle("Chronological plot for fragmentation_712") +
  theme(plot.title = element_text(hjust = 0.5))


# baseline_50_650 plot
df = data.frame(qc_metric = "baseline_50_650",
                values = good_data$baseline_50_650,
                acquisition_date = good_data$acquisition_date)

df = df[rev(order(as.Date(df$acquisition_date))),]  # sort by date
df = na.omit(df[1:50,])  # plot only last 50 runs (otherwise it's squeezed too much)
df[,3] = substring(df[,3], 1, 10)

chr_plot_2 = ggplot(df, aes(x = acquisition_date, y = values)) +
  geom_point(size = 2) + geom_line(group = 1) +
  geom_point(data = df[1, ], aes(x = acquisition_date, y = values), color="red", size=2) +  # add red dot in the end
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Date", y = "Value") +
  ggtitle("Chronological plot for baseline_50_650") +
  theme(plot.title = element_text(hjust = 0.5))

# s2b plot
df = data.frame(qc_metric = "s2b",
                values = good_data$s2b,
                acquisition_date = good_data$acquisition_date)

df = df[rev(order(as.Date(df$acquisition_date))),]  # sort by date
df = na.omit(df[1:50,])  # plot only last 50 runs (otherwise it's squeezed too much)
df[,3] = substring(df[,3], 1, 10)

chr_plot_3 = ggplot(df, aes(x = acquisition_date, y = values)) +
  geom_point(size = 2) + geom_line(group = 1) +
  geom_point(data = df[1, ], aes(x = acquisition_date, y = values), color="red", size=2) +  # add red dot in the end
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Date", y = "Value") +
  ggtitle("Chronological plot for s2b") +
  theme(plot.title = element_text(hjust = 0.5))

