

library(RSQLite)
library(DBI)
library(ggplot2)

# connect to the sqlite file
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/qc_metrics/res/nas2/qc_matrix.db")

# get a list of all tables
alltables = dbListTables(con)

# get the data as a data.frame
p1 = dbGetQuery(con, 'select * from qc_values')
p2 = dbGetQuery(con, 'select * from qc_meta')

p1$date = unlist(lapply(p2$original_filename, function(x) substring(x, 1, 10) )) 
p1$date = p2$original_filename

colnames(p1)

ggplot(data = p1, aes(x = date, y = resolution_200)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = resolution_700)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = average_accuracy)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = chemical_dirt)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = instrument_noise)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = isotopic_presence)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = transmission)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = fragmentation_305)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = fragmentation_712)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = baseline_25_150)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = baseline_50_150)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = baseline_25_650)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = baseline_50_650)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = signal)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = s2b)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = p1, aes(x = date, y = s2n)) +
  geom_point(size = 2) + geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90))











p1[,-1]

barplot(p1[,"resolution_200"])


# Draw with black outline, white fill
ggplot(p1, aes(x=resolution_200)) +
  geom_density(alpha=.4, fill="lightblue") +
  labs(x = "values", y = "frequency")

# + geom_vline(aes(xintercept = mean(weight)), 
#              linetype = "dashed", size = 0.6,
#              color = "#FC4E07")

ggplot(p1, aes(x=resolution_200)) + geom_histogram(binwidth=.5)

# Density curve
ggplot(dat, aes(x=rating)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(dat, aes(x=rating)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot