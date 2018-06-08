########## EREIGNISZEIT ANALYSE
# Betrachtet die erwartete Zeitspanne bis zum Eintreten eines bestimmten Ereignisses.
# Sind Ansätze, um Zeit zu einem bestimmten Ereignis zwischen Gruppen zu vergleichen um 
# die Wirkung von verschiedenen Faktoren zu schätzen.
#
# Ansätze:          Kaplan-Meier-Schätzer, Log-Rank, Cox-Regresssion,  
# Zentrale Größe:   Hazardrate
# Zensierte Daten:  Falls das Event innerhalb des betrachteteten Zeitraums nicht eintritt => (rechts) zensierte observation
library(survival)   # Berechnung der Survial Analyse
library(survminer)  # Visualisieren des Resultats

#### Überlebensfunktion & Hazard-Funktion
# Survial Funktion: beschreibt Wahrscheinlichkeit, dass die Zeit bis zum Tod länger als ist als T (auch Reliability Function R(t))
# S(t) = P(T > t)
# => fokussiert darauf KEIN event zu haben
# Hazard Funktion: beschreibt Wahrscheinlichkeit. dass ein beboachtetes Individuum am Zeitpunkt t ein Event hat
# => fokussiert darauf, dass Event EINTRITT 
data("lung")
head(lung)


#### Kaplan-Meier Survial Estimate
# S(t_i)=S(t_{i−1})*(1−(d_i/n_i))
# S(t_{i−1})= Wahrscheinlichkeit am Zeipunkt t_{i-1} am Leben zu sein
# n_i = Anzahl der Patienten die direkt vor t_i am Leben waren
# d_i = Anzahl der Events an t_i
# Geschätze Wahrscheinlichkeit S(t) ist step Funktion die ihren Wert nur ändert wenn (jeder der) Events eintreten
# => möglich die CI für die Überlebenswahrscheinlichkeit zu berechnen
# => KM Kurve gut um die Überlebenswahrscheinlichkeit gegen die Zeit zu Plotten und
#    Schätzwerte wie den Median der Überlebenswahrscheinlichkeit zu bestimmen

#### Berechnen der Survival Kurven 
# = Kaplan-Meier survival estimates
fit <- survfit(Surv(time, status) ~ sex, data = lung)

### Zusammenfassung der Survival Kurven
## Anzahl der Beobachtungen, Anzahl von Events, Suvival Median und confidence limits für die Mediane
print(fit)
## Ausführlicher
summary(fit)
## Zusammenfassungstablle
summary(fit)$table

# n:            Anzahl der subjects in jeder Kurve
# time:         Zeitpunkte der Kurve
# n.risk:       Anzahl der risiko-subjects zum Zeitpunkt t
# n.event:      Anzahl der Events die am Zeitpunkt t passieren 
# n.censor:     Anzahl der censored subjects die Risko Menge ohn ein Event zum Zeitpunkt t verlassen 
# lower,upper:  obere und untere konfidenz grenzen der kurve
# strata:       beschreibt stratification der kurven schätzung => strata != null => mehere kurven 
d <- data.frame(time = fit$time, 
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower)
head(d)


#### Visualisieren & Interpretieren der Überlebenskurven
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, 
           risk.table.col = "strata", # färben nach gruppe
           linetype = "strata",       # linien-typ nach gruppe
           surv.median.line = "hv", 
           ggtheme = theme_bw(),  
           palette = c("#E7B800", "#2E9FDF"))

##... weitere sinnvolle Optionen 
# risk.table = 'abs_pct',    # absolute und relative anzahl von risiko-personen 
# conf.int.style = 'step',   # confidence bands style 
# ncensor.plot = TRUE,       # Anzahl der censored subjects zum zeipunkt t
# break.time.by = 200,

ggsurvplot(
  fit,                     
  pval = TRUE,             
  conf.int = TRUE,         
  conf.int.style = "step", 
  xlab = "Time in days",   
  break.time.by = 200,     
  ggtheme = theme_light(), 
  risk.table = "abs_pct",  
  risk.table.y.text.col = T,
  risk.table.y.text = FALSE,
  ncensor.plot = TRUE,      
  surv.median.line = "hv",  
  legend.labs = c("Male", "Female"),    
  palette = c("#E7B800", "#2E9FDF") 
)

### Interpretation
# Vertikaler Abfall in der Kurve = Event
# Vertikaler Strich = subject wurde zu diesem Zeipunkt gecensored
# Zeitpunkt 0: Überlebenswahrscheinlichkeit = 1 = alle subjects am leben
# Zeitpunkt 250: Überlebenswahrscheinlichkeit ~55% für Männer, ~75 für Frauen
summary(fit)$table
# Median der Überlebenszeit ist ~270 Tage für Männer und 426 für Frauen 
# => offensichtlich ein Überlebensvorteil Frauen im Gegensatz zu Männern

### Untschied Statistisch signifkant
# Konfidenz Grenzen sind sehr breit insbesonder im Tail (praktische Ursachen von subject verfügbarkeit)
# => kürzen er kurve
# => Log-rank-test
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           xlim = c(0, 600))

## Kumulative Events Kurve
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", 
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF"),
           fun = "event")  # fun => "event"


## Kumulativer hazard
# Zum schätzen der Hazard Wahrscheinlichkeit 
# H(t)=−log(survivalfunction)=−log(S(t))
# H(t) interpretiert als kumulative Kraft des Sterbens
#   => Anzahl der Events die für jedes subject zum Zeitpunkt t erwartet würden
#      wenn das Event ein sich wiederholender Prozess wäre
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")

### Kaplan-Meier life table
summary(fit)                  # => vollständige Zusammenfassung der Survival Kurven
res.sum <- surv_summary(fit)  # => erzeugt data.frame mit summary
head(res.sum)

# wenn survival kurven mit mehrere Variablen gefitted wurde
# beinhaltet surv_summary obj spalten, welche Variablen repräsentieren
# => erlaubt facetting ggsurvplot durch strata oder kombination von faktoren

# Informationen über survial kurven wie medians mit CI 
attr(res.sum, "table") 



#### Log-Rank test
# Zum Vergleich von Survial Kurven
# Nicht-Parameterischer Test, welcher keine Annahmen über Überlebensverteilung macht
# H0: Kein Unteschied bzgl Überleben zwischen den beiden Gruppen
#
# Vergleicht die beobachtete Anzahl von Events mit dem was erwaretet wäre, wenn H0 wahr. 
# Log-rank statistik ist ca so verteilt wie die chi-quadrat test verteilung
surv_diff <- survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff

# n: Anzahl der subjects in jeder Bruppe
# obs: gewichtete beobachte Anzahl an events in jeder Gruppe
# exp: gewichtete erwartete Anzahl an events in jeder Gruppe 
# chisq: chi-quadrat statistik für test auf gleichheit 
# => p = 0.00131 => gruppen untescheiden sich signifikant im überleben


#### Fitting survival Curves
## Berechnung von Survival Kurve mit mehereren Faktoren
require(survival)
fit2 <- survfit( Surv(time, status) ~ sex + rx + adhere, data = colon )
#devtools::install_github("kassambara/survminer")
## Facetting 
# nach rx und adhere
ggsurv <- ggsurvplot(fit2, fun = "event", conf.int = TRUE, ggtheme = theme_bw())
ggsurv$plot + theme_bw() + theme(legend.position = "right") + facet_grid(rx ~ adhere )

