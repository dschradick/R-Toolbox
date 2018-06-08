########## AB-TESTING - SEQUENTIAL ANALYSIS
library(gsDesign)       # Sequential testing 

#### Sequential Analysis
# Ziel: Peeking zwischendurch ermöglichen 

# Stopping Rules
# k = Anzahl wie häufig man Daten betrachten möchte
# test.type = 1 -> one-sided test
seq_analysis <- gsDesign(k = 4,
                         test.type = 1,
                         alpha = 0.05,
                         beta = 0.2,
                         sfu = "Pocock")
seq_analysis

## Sample sizes für sequential analysis
max_n <- 1000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis$timing
stopping_points