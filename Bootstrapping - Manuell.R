########## BOOTSTRAPPING - MANUELL
# Ziel: Resampling zur Bestimmung der Variabilität der Statistik p^
# p   = Parameter           = Proportion der zutreffenden Elemente in der Population
# p^  = Statistik           = Proportion der zutreffenden Elemente im Sample
# P^* = Bootstrap-Statistik = Proportion der zutreffenden Elemente im Resample
# ==> wie unterscheiden sich p und p^
# 
# Funktionsweise: Wiederholtes Sampling vom Sample (mit Replacement) 
# => sehr gute Methode zur Approximation des Samplings von der Population
# => Bootstrap liefert gute Approximation für den Standard-Error = wie variabel die Statistik


## Hilfsfunktion zur Erstellung von Permuationen 
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1){
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
  group_by(rep_tbl, replicate)
}
# Beispiel
(test_table <- as.tbl(data.frame(first = c(1,2,3), second=c('a','b','c'))))             
rep_sample_n(test_table,size = 3, reps = 2)          # liefert eine permutation mit jeweils 3 einträgen (jeweils ganze Zeile)
rep_sample_n(test_table$first, size = 3, reps = 2)   # nur eine Spalte


#### 
## Zwei Fälle
# Fall 1: p^: entsprichtwiederholtem Sampling mit mit n=30 
#             von extrem grosser Population (gold standard, unrealistisch)
# Fall 2: Wiederholtes Resampling mit replacement mit n = sample_size = 30
load("~/Documents/R/data/all_polls.RData")

# Fall 1: p^ für jeden Poll erzeugen
# => kein resampling
ex1_props <- all_polls %>% group_by(poll) %>% summarize(prop_yes = mean(vote))
(ex1_props <- all_polls %>% group_by(poll) %>% summarize(prop_yes = mean(vote)))

# Fall 2 : p^* jedes resample erzeugen
# Ersten Poll als Sample nehmen
one_poll <- all_polls %>% filter(poll == 1) %>% dplyr::select(vote)
# Erstelle Permutationen vom ersten Poll
one_poll_boot_30 <- one_poll %>% rep_sample_n(size = 30, replace = TRUE, reps = 1000)
ex2_props <- one_poll_boot_30 %>% summarize(prop_yes = mean(vote))

## Mittelwert & Std-Abweichung: p^, p^*
ex1_props %>% summarize(mean(prop_yes),sd(prop_yes))  
# => p^:  0.6, Std: 0.0868 
ex2_props %>% summarize(mean(prop_yes),sd(prop_yes))  
# => p^*: 0.7, Std: 0.0833

## Visualisieren
ggplot() + 
  geom_density(data = ex1_props, aes(x = prop_yes), col = "black", bw = .1) +
  geom_density(data = ex2_props, aes(x = prop_yes), col = "green", bw = .1)

  
  
#### Konfidenz-Intervall
# Da Verteilung von p^ symmetrisch und glockenförmig => empirische Regel
# 95% der Werte liegen innerhalb von 2SE vom Mittelwert
# => 95% der p^ der Samples erzeugen p^ innerhalb von 2SE vom Mittelwert
# t-Invertall = Intervalle die von der empirischen Regel erzeugt werden
props <- all_polls %>% group_by(poll) %>% summarize(prop_yes = mean(vote))


### Zwei Arten von Bootstrap-Konfidenzintervallen
# 1. t-Konfidenzinterval
# 2. Quantil-Konfidenzinterval

# Bedingungen:
# 1. Sampling-Verteilung der Statistik ist glockenförmig und symmetrisch
# 2. Sample-Size ist hinreichend groß
# => am Plot der p^*-Verteilung zu sehen, ob erfüllt
# Anmerkung: Wenn der echte Wert von p näher an 0.5 (als an 0 oder 1), dann ist der Standard-Error größer

## 1. t-Konfidenzinterval
# p-hat von one_poll
p_hat <- mean(one_poll$vote)
# Bootstrap um SE von p-hat zu finden durch 
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))
# Erzeugen von Interval plausibler Werte durch 2.5% und 97.5% Werte von P^*
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))
# => Interval beinhaltet den wahren Parameter 0.6 

## 2. (Quantil-) Konfidenzintervalls
# Einfacher...
one_poll_boot %>%
  summarize(q025_prop = quantile(prop_yes_boot, p = .025),
            q975_prop = quantile(prop_yes_boot, p = .975))
# => "95% sicher, dass die echte Proportion von Leuten welche Kandidat X wählen wollen
#     zwischen zwischen 0.53 und 0.87 liegt


### Wieviel Prozent der Mittelwerte innerhalb von 95% CI
# 2SE von von echtem Populationsparameter = 0.6 
props %>%
  mutate(lower = 0.6 - 2 * sd(prop_yes), # 
         upper = 0.6 + 2 * sd(prop_yes),
         in_CI = prop_yes > lower & prop_yes < upper) %>%
  summarize(mean(in_CI))
# => 96,6% der Mittelwerte der 1000 samples ist innerhalb von 2SE des echten Populationsparameter

