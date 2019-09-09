

library(RSQLite)
library(DBI)

# connect to the sqlite file
con = dbConnect(SQLite(), dbname="/Users/andreidm/ETH/projects/web_service/data/qc_matrix_example.db")

# get a list of all tables
alltables = dbListTables(con)

# get the data as a data.frame
p1 = dbGetQuery(con, 'select * from qc_values')

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