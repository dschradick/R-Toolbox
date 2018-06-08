########## SPARKLYR - CLUSTER
library(tidyverse)
library(sparklyr)

Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
Sys.setenv(HADOOP_CONF_DIR = "/etc/hadoop/conf")
Sys.setenv(YARN_CONF_DIR = "/etc/hadoop/conf")

sc <- spark_connect(master = "yarn-client")
spark_version(sc)

#### CSV aus HDFS lesen
train_df <- spark_read_csv(sc=sc
                           ,path ="hdfs:///user/amy_ds/airline_data/train_df.csv"
                           ,name ="train_df"
                           ,infer_schema = TRUE
                           ,header = TRUE)

#### Filtern und transfomieren
train_df <- train_df %>% 
  # Konvertierungen
  mutate(ARR_DELAY = as.integer(ARR_DELAY)) %>%
  mutate(DEP_DELAY = as.integer(DEP_DELAY))  %>%
  # Neue Felder
  mutate(WEEKEND = ifelse(DAY_OF_WEEK == 5 | DAY_OF_WEEK == 6 | DAY_OF_WEEK == 7,1,0)) %>%
  mutate(DEP_HOUR = floor(DEP_TIME/100)) %>%
  mutate(DELAY_LABELED = as.integer(ifelse(ARR_DELAY > 15, 1, 0))) %>%
  ## Filtern
  filter(CANCELLED == 0) %>%
  filter(!is.na(ARR_DELAY))

glimpse(train_df)  
#glimpse(train_df %>% mutate(DELAY_LABELED = ifelse(ARR_DELAY > 15, 1, 0)))

train_df %>%
  sdf_sample(0.05, replacement = FALSE, seed = 0) %>%
  compute("sample_track_metadata") %>%
  group_by(DELAY_LABELED) %>%
  summarise(count = n())

#### ML
small_train_df <- train_df %>%
  sdf_sample(0.2, replacement = FALSE, seed = 0) 

formula <- ARR_DELAY ~ MONTH + DEP_HOUR + DEP_DELAY + WEEKEND
ml_lm <- lm(formula, data = small_train_df)
summary(ml_lm)

(ml_log <- ml_gradient_boosted_trees(small_train_df, formula))
summary(ml_log)

test <- sdf_sample(train_df,0.1, replacement = FALSE, seed = 1) %>% 
  select(MONTH,DEP_HOUR,DEP_DELAY,WEEKEND,ARR_DELAY)

glimpse(test)
prediction_lm <- predict(ml_lm,test)
prediction_gbt <- predict(ml_log,test)
sqrt(mean((prediction_lm - collect(test)$ARR_DELAY) ^ 2))
sqrt(mean((prediction_gbt - collect(test)$ARR_DELAY) ^ 2))
