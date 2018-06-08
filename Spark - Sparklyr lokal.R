########## SPARKLYR
# dplyr-style wrapper für spark
# => konvertiert r-code zu SQL bevor es an Spark gegeben wird
# Alternative zu SparkR - hat z.T. konflikte mit dplyr hat - 5 verben funktionieren ander
# => z.B. SparkR keine non-standard evaluation 
#     mtcars_sparkr %>% SparkR::filter(cyl == 4) geht nicht, sondern mtcars_spark$cyl == 4 oder "cyl" == 4
library(sparklyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(microbenchmark)
library(stringr)

setwd("~/R/data/")

#### Verbinden mit Spark
# Verwende lokale Version von Spark => vorher mit spark_install() installiert
sc <- spark_connect("local")
spark_version(sc)
#spark_disconnect(spark_conn)

#### Daten importieren 
# Viele Funktionen: z.B. spark_read_csv()

timbre <- spark_read_parquet(sc, name="timbre", path="timbre.parquet")
track_metadata <- spark_read_csv(sc, name = "track_data", path="track_metadata.csv")
track_data <- spark_read_parquet(sc, name = "track_data", path="track_data.parquet")

## Daten anschauen
str(track_metadata)
#View(track_metadata)

## data.frame nach Spark kopieren
track_metadata_tbl <- copy_to(sc, track_metadata, overwrite=T)

## Exceptions von Spark abfangen
tryCatch({
  track_metadata_tbl[, c("artist_name", "release", "title", "year")]
}, error = print)


#### Daten von Spark lesen
# Alle data frames in spark anzeigen
src_tbls(sc)

## Link zur Spark Tabelle
track_metadata_tbl <- tbl(sc, "track_metadata")
dim(track_metadata_tbl)
object_size(track_metadata_tbl)

## Ausgabe: 
# 5 Reihen, alle Spalten
print(track_metadata_tbl, n = 5, width = Inf)
str(track_metadata_tbl)
glimpse(track_metadata_tbl) # => zeigt nicht die volle Anzahl an Reihen



#### Daten Auswahl
## Spalteauswahl 
# dplyr syntax am einfachsten
# NICHT UNTERSTÜTZT: square bracket indexing 
#  => Geht nicht: track_metadata_tbl[, c("artist_name", "release", "title", "year")]
track_metadata_tbl %>%
  select(artist_name, release, title, year)

## Reihenauswahl
# Sparklyr konvertiert dplyr nach SQL
# => kann nicht nach allem filtern; z.B. x > 5 geht, aber regex in im filter nicht
# Operatoren unterstützt für Vergleich: >, !=,  %in%, arithmetik: +, ^,  %% 
# oder logische  &, |, !, mathematische log(), abs(), round(), sin()
# => ?translate_sql() gibt info, was unterstützt wird
track_metadata_tbl %>%
  select(artist_name, release, title, year) %>%
  filter(year >= 1960, year < 1970)


#### Daten Manipulation
## Reihen ordnen
track_metadata_tbl %>%
  select(artist_name, release, title, year) %>%
  filter(year >= 1960, year < 1970) %>%
  arrange(artist_name, desc(year), title)

## Mutate
# Bemerk.: base package Funktionen wie within() oder transform() funktionieren in Spark nicht
track_metadata_tbl %>%
  select(title, duration) %>%
  mutate(duration_minutes = duration / 60)

## Summarize
track_metadata_tbl %>%
  select(title, duration) %>%
  mutate(duration_minutes = duration / 60) %>%
  summarize(mean_duration_minutes = mean(duration_minutes))


#### Dplyr Tools
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/select_helpers
## starts_with & ends_with
track_metadata_tbl %>%
  # Select columns starting with artist
  select(starts_with("artist"))

## contains
# a: beliebiger buchstabe, .: beliebiger buchstable, zahl, satzzeichen,...
track_metadata_tbl %>%
  select(contains("ti"))

## distinct
track_metadata_tbl %>%
  distinct(artist_name)

## count & top_n
track_metadata_tbl %>%
  count(artist_name, sort = TRUE) %>%
  top_n(20)

#### collect
# bewegt die Daten von Spark nach R (normalerweise nur Referenz + wenige Datenpunkte)
# => tbl_spark => tbl_df
results <- track_metadata_tbl %>%
  filter(artist_familiarity > 0.9)
class(results)

collected <- results %>% 
  collect()
class(collected)

### compute
# wg. langsame übertragung zwischen spark und r
# => compute berechnet (zwischenergebnisse) zwar, 
#    aber speichert temporäres resultat in spark in variable (tbl_lazy typ)
# 2 Argumente: tibble und variablen-namen für spark in dem resultat gespeichert wird
computed <- track_metadata_tbl %>%
  filter(artist_familiarity > 0.8) %>%
  compute("familiar_artists")
src_tbls(sc)
class(computed) # => tbl_lazy

### group_by
# nach "split-apply-combine" paradigma
# Splitting von Daten in Gruppen, summary statistik anwenden und in einer datenstruktur wieder zusammenführen
#  => map-reduce strukturell ähnlich: map entspricht split & apply und reduce entspricht combine
# Zuerst group_by => dann mutate und summarize
# track_metadata_tbl has been pre-defined
duration_by_artist <- track_metadata_tbl %>%
  group_by(artist_name) %>%
  summarize(mean_duration = mean(duration))

duration_by_artist %>%
  arrange(desc(mean_duration))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

## Beispiel: Gruppen-spezfische Normalisierung: group_by & mutate
track_metadata_tbl %>%
  group_by(artist_name) %>%
  mutate(time_since_first_release = year - min(year)) %>%
  arrange(desc(time_since_first_release))


#### SQL Querys
# dbGetQuery: beides zusmamen - mit dbSendQuery() und mit dbFetch() resultat holen

## Einfache abfragen
query <- "SELECT * FROM track_metadata WHERE year < 1935 AND duration > 300"
(results <- dbGetQuery(sc, query))

## Left & Anti & Semi Join 
# (daten nicht vorhanden)
left_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")
anti_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")
semi_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")


###### Natives Interface von Spark benutzen
## Interfaces
# ft_* = features transformation funktionen
# ml_* = machine learning funktionen
# sdf_* = sortieren, sampling, partitionieren des data frames



### Transformation von Datentypen
# DoubleType =  Spark's version von numeric
# => numeric zu DoubleType automatisch
# => DoubleType zu logical oder integer zu numeric manuell(!)

# ft_binarizer(): konvertiert kontinuierlich zu numerisch / logisch
glimpse(track_metadata_tbl)
hotttnesss <- track_metadata_tbl %>%
  select(artist_hotttnesss) %>%
  ft_binarizer("artist_hotttnesss", "is_hottt_or_nottt", threshold = 0.5) %>% # Binarize is_hottt_or_nottt
  collect() %>%
  mutate(is_hottt_or_nottt = as.logical(is_hottt_or_nottt)) # konvertieren is_hottt_or_nottt zu logical

ggplot(hotttnesss, aes(is_hottt_or_nottt)) +
  geom_bar()


### Kontinuierlich zu kategorisch
# cutting mit ft_bucketizer()
# => wie cut() mit right = FALSE.
decades <- c(1930.01, 1940.01, 1950.01, 1960.01, 1970.01, 1980.01, 1990.01, 2000.01, 2010.01)
decade_labels <- c("1930-1940", "1940-1950", "1950-1960", "1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2010")
hotttnesss_over_time <- track_metadata_tbl %>%
  select(artist_hotttnesss, year) %>%
  mutate(year = as.numeric(year)) %>%
  ft_bucketizer("year", "decade", splits = decades) %>%
  collect() %>%
  mutate(decade = factor(decade, labels = decade_labels))

ggplot(hotttnesss_over_time, aes(decade, artist_hotttnesss)) +
  geom_boxplot()

## Wenn Buckets durch Quantile definiert...
duration_labels <- c("very short", "short", "medium", "long", "very long")
familiarity_by_duration <- track_metadata_tbl %>%
  select(duration, artist_familiarity) %>%
  ft_quantile_discretizer("duration", "duration_bin", n.buckets = 5) %>%
  collect() %>%
  mutate(duration_bin = factor(duration_bin, labels = duration_labels))

ggplot(familiarity_by_duration, aes(duration_bin, artist_familiarity)) +
  geom_boxplot() + theme_bw()

## Word tokenization
track_metadata_tbl %>%
  select(artist_name, title) %>%
  ft_tokenizer("title", "word") %>%
  collect() %>%
  mutate(word = lapply(word, as.character)) %>%  # flatten
  unnest(word)


## Sentiment Analysis
# mittels tidytext
track_metadata_tbl
afinn_sentiments_tbl

sentimental_artists <- title_text_tbl %>%
  inner_join(afinn_sentiments_tbl, by = "word") %>%
  group_by(artist_name) %>%
  summarize(positivity = sum(score))

sentimental_artists %>%
  arrange(desc(positivity)) %>%
  top_n(5)

## regex
track_metadata_tbl %>%
  select(artist_mbid) %>%
  ft_regex_tokenizer("artist_mbid", "artist_mbid_chunks", pattern = "-")

## Datentypen anschauen
# Dabei entspricht...
# logical ~	BooleanType, numeric ~	DoubleType, integer ~	IntegerType
# character	~ StringType, list ~ArrayType
(schema <- sdf_schema(track_metadata_tbl))
# in besser lesbare Form bringen
schema %>%
  lapply(function(x) do.call(data_frame, x)) %>%
  bind_rows()


## Sampling
# sdf_sample() für sample()
track_metadata_tbl %>%
  sdf_sample(0.01, replacement = FALSE, seed = 0) %>%
  compute("sample_track_metadata")

## Train / Test Split
partitioned <- track_metadata_tbl %>%
  sdf_partition(training = 0.7, testing = 0.3)
dim(partitioned$training)
dim(partitioned$testing)


##### Machine Learning mit Spark
# ML Funktionen
# Formula,tibble oder 
# 1. tibble, 2. response, 3. character vector mit explanatory Variablen
ls("package:sparklyr", pattern = "^ml")

# Beispiel: timbre => year of song
track_metadata_tbl %>%
  inner_join(timbre_tbl, by = "track_id") %>%
  mutate(year = as.numeric(year))


## Train/Test Set erzeugen
# Split soll auf Artist beruhen
training_testing_artist_ids <- track_data %>%
  select(artist_id) %>%
  distinct() %>%
  sdf_partition(training = 0.7, testing = 0.3)

track_data_to_model_tbl <- track_data %>%
  inner_join(training_testing_artist_ids$training, by = "artist_id")

track_data_to_predict_tbl <- track_data %>%
  inner_join(training_testing_artist_ids$testing, by = "artist_id")

#### Modelle trainieren
### Gradient Boosted Trees
## Trainieren
formula <- year ~ timbre_means1 + timbre_means2 + timbre_means3 + timbre_means4 + timbre_means5 + timbre_means6 + timbre_means7 + timbre_means8 + timbre_means9 + timbre_means10 + timbre_means11 + timbre_means12
gradient_boosted_trees_model <- ml_gradient_boosted_trees(track_data_to_model_tbl,formula,type='regression')

## Vorhersage
responses <- track_data_to_predict_tbl %>%
  mutate(actual = year) %>%
  collect() %>%
  mutate(
    predicted = predict(gradient_boosted_trees_model,track_data_to_predict_tbl))

both_responses_1 <- responses %>%
  select(actual, predicted) %>%
  mutate(model='gbt')

## Visualisieren
# Sparklyr unterstütz nicht residuals() 
# => selber berechnen
residuals <- responses %>%
  transmute(residual = predicted - actual)
# Actual vs predicted
ggplot(responses, aes(actual, predicted)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1)
# Residuals
ggplot(residuals, aes(residual)) +
  geom_density() +
  geom_vline(xintercept = 0)


#### Random Forest
# => genauso zu benutzen wie gradient boosted trees
## RF trainieren
random_forest_model <- ml_random_forest(track_data_to_model_tbl,formula,type='regression')

## Vorhersage
responses <- track_data_to_predict_tbl %>%
  mutate(actual = year) %>%
  collect() %>%
  mutate(predicted = predict(random_forest_model,track_data_to_predict_tbl))


## Visualiseren
both_responses_2 <- responses %>%
  select(actual, predicted) %>%
  mutate(model='rf')
both_responses <- rbind(both_responses_1,both_responses_2)

residuals <- both_responses %>% mutate(residual = predicted - actual)
ggplot(both_responses, aes(actual, predicted, color = model)) +
  geom_smooth() + geom_abline(intercept = 0, slope = 1)
ggplot(residuals, aes(residual, color = model)) +
  geom_density() + geom_vline(xintercept = 0)


#### Performance vergleichen
both_responses %>%
  mutate(residual = predicted - actual) %>%
  group_by(model) %>%
  summarize(rmse = sqrt(mean(residual ^ 2)))


### Classification
titanic_tbl <- spark_read_parquet(sc, name = "titanic", path = "~/R/data/titanic-parquet")

titanic_tbl <- titanic_tbl %>% 
  filter(!is.na(Embarked)) %>%
  mutate(Age = if_else(is.na(Age), mean(Age), Age))
#!!!! => wenn na werte: Failed to execute user defined function

formula <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked )
(ml_log <- ml_gradient_boosted_trees(titanic_tbl, formula))
