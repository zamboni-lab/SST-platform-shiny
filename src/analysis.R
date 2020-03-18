
library(DBI)
library(RSQLite)

plot_metrics_cross_correlations = function(good_data, bad_data){
  
  # watch table with cross-correlations in 'good' data
  View(cor(good_data[5:ncol(good_data)]))
  
  # compare two heatmaps of cross-correlations ('good' vs 'bad' data)
  good_data_correlation = cor(good_data[5:ncol(good_data)])
  bad_data_correlation = cor(bad_data[5:ncol(bad_data)])
  
  heatmap(good_data_correlation)
  heatmap(bad_data_correlation)
  
  library(reshape2)
  library(ggplot2)
  
  # distribution plots
  plots_list = list()
  
  # nicer plot for good runs
  melted_good = melt(good_data_correlation)
  
  cor_plot_1 =ggplot(data = melted_good, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5)) +
    coord_fixed() +
    ggtitle("\"Good\" runs")
  
  # nicer plot for bad runs
  melted_bad = melt(bad_data_correlation)
  
  cor_plot_2 = ggplot(data = melted_bad, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5)) +
    coord_fixed() +
    ggtitle("\"Bad\" runs")
  
  # 
  plots_list[[1]] = cor_plot_1
  plots_list[[2]] = cor_plot_2
  
  figure = ggarrange(plotlist=plots_list, ncol=1, nrow=2)
  figure = annotate_figure(figure, top = text_grob("Cross-correlations of QC characteristics", face = "bold", size = 12))
  figure
  
}

assess_metrics_statistical_differences = function(good_data, bad_data){
  
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
  
  # TODO: add multiple comparison correction
  
  # watch significant differences
  comparisons < 0.05
  comparisons < 0.01
}

plot_few_metrics_distributions(full_data){
  ## this method plots a couple of distributions of metrics nicely
  ## was done for a poster image
  
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
    labs(x= "", y = "")  +
    scale_colour_manual(name="quality", values=c("red", "black")) +
    ggtitle("fragmentation_712") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(3, 3, 3, 3),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
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
    scale_colour_manual(name="quality", values=c("red", "black")) + 
    ggtitle("fragmentation_712") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(3, 3, 3, 3),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
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
    scale_colour_manual(name="quality", values=c("red", "black")) +
    ggtitle("fragmentation_712") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(3, 3, 3, 3),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  # compose
  plots_list[[1]] = d_plot_1
  plots_list[[2]] = d_plot_2
  plots_list[[3]] = d_plot_3
  
  figure = ggarrange(plotlist=plots_list, ncol=3, nrow=1)
  figure = annotate_figure(figure)
  figure
}

plot_few_metrics_choronologically(good_data){
  ## this method plots nicely a couple of metrics over time
  ## was done for a poster image
  
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
    labs(x = "Date", y = "") +
    ggtitle("fragmentation_712") +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 7, angle = 90))
  
  
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
    labs(x = "Date", y = "") +
    ggtitle("baseline_50_650") +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 7, angle = 90))
  
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
    labs(x = "Date", y = "") +
    ggtitle("s2b") +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 7, angle = 90))
  
  # compose
  plots_list[[1]] = chr_plot_1
  plots_list[[2]] = chr_plot_2
  plots_list[[3]] = chr_plot_3
  
  figure = ggarrange(plotlist=plots_list, ncol=3, nrow=1)
  figure = annotate_figure(figure)
  figure
}

explore_features_dimensionality = function(){
  ## method doesn't plot or return anything
  ## just wraps some experiments on exploring dimensions of features
  
  # pca
  features_path = "/Users/andreidm/ETH/projects/monitoring_system/res/nas2/qc_features_database.sqlite"
  features_1 = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=features_path), 'select * from qc_features_1'))
  features_2 = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=features_path), 'select * from qc_features_2'))
  
  # concatenate two table with features
  features = cbind(features_1[,-(1:4)], features_2[,-(1:4)])
  
  # remove features with zero variance
  features = features[, apply(features, 2, var) != 0]
  
  pc_res = prcomp(features, scale.=TRUE)
  
  rot = data.frame(pc_res$rotation)
  
  library("factoextra")
  
  fviz_eig(pc_res, ncp = 20)
  
  fviz_pca_ind(pc_res,
               col.ind = "cos2", # Color by the quality of representation
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  
  library("tsne")
  res = tsne(features)
  
}


main = function(){
  
  path = "/Users/andreidm/ETH/projects/ms_monitor/data/nas2_qc_metrics_database_jan14.sqlite"
  full_data = as.data.frame(dbGetQuery(dbConnect(SQLite(), dbname=path), 'select * from qc_metrics'))
  
  bad_data = full_data[full_data$quality == 0, ]
  good_data = full_data[full_data$quality == 1, ]
  
  # to call or not to call
  if (FALSE){
    plot_metrics_cross_correlations(good_data, bad_data)
    assess_metrics_statistical_differences(good_data, bad_data)
    plot_few_metrics_distributions(full_data)
    plot_few_metrics_chronologically(good_data)
    explore_features_dimensionality()
  }
  
}
