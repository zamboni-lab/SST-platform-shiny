
version = "v.0.1.20"
db_path = "/Users/andreidm/ETH/projects/shiny_qc/data/nas2_qc_metrics_database_mar18.sqlite"

names = c("resolution_200", "resolution_700", "average_accuracy", "chemical_dirt", "instrument_noise",
          "isotopic_presence", "transmission", "fragmentation_305", "fragmentation_712", "baseline_25_150",
          "baseline_50_150", "baseline_25_650", "baseline_50_650", "signal", "s2b", "s2n")

descriptions = c("193.072 + mean of absolute mass accuracy of Caffeine divided by average width of the peak",
                 "712.946 + mean absolute mass accuracy of Perfluorotetradecanoic acid divided by average width of the peak",
                 "sum of mean absolute mass accuracies for all 37 ions divided by its number",
                 "sum of all intensities in the chemical noise scan",
                 "sum of all intensities in the instrument noise scan",
                 "mean of the sums of all isotope ratio diffs (in absolute numbers) divided by its number",
                 "mean of intensity of Perfluorotetradecanoic acid (mz ~712) divided by intensity of Fluconazole (mz ~305)",
                 "mean of intensity of the Fluconazole fragment divided by intensity of Fluconazole (mz ~305)",
                 "mean of intensity of the Perfluorotetradecanoic acid fragment divided by intensity of Perfluorotetradecanoic acid (mz ~712)",
                 "25th percentile intensity from a [150,250] mz range of a chemical noise scan",
                 "50th percentile intensity from a [150,250] mz range of a chemical noise scan",
                 "25th percentile intensity from a [650,750] mz range of a chemical noise scan",
                 "50th percentile intensity from a [650,750] mz range of a chemical noise scan",
                 "sum of mean intensities for all 37 ions",
                 "mean intensity of 3-(Heptadecafluorooctyl)aniline divided by mean 25th percentile intensity from a [500, 550] mz range of a normal scan",
                 "mean intensity of 3-(Heptadecafluorooctyl)aniline divided by (mean 50th percentile intensity - mean 25th percentile intensity) from a [500,550] mz range of a normal scan")

qc_metrics_descriptions = data.frame(names=names, descriptions=descriptions)




