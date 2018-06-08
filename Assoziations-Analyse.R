########## ASSOZIATIONS-ANALYSE
# Zur Auffindung von     Zusammenhängen in transaktionsbasierten Datenbasen
# Assoziations-Regeln:   repräsentieren Assoziationen / Korrelationen zwischen itemsets
# Regelform:             A => B, wobei A und B zwei disjunkte itemsets
# Meistgenutze Maße für "Interestingness":
# - Support(X): Wahrscheinlichkeit, dass diese Itemmenge in einer Transaktion vorkommt
# - Support(X => Y): relative Häufigkeit, mit der Regel in der Datenbasis vorkommt 
#   => hoher Support wünschenswert, um Aussagen über Mehrheiten zu finden
# - Confidence(X => Y): Konfidenz einer Regel entspricht Wahrscheinlichkeit der Konklusion unter Bedingung der Prämisse
#   => Konfidenz misst relative Häufigkeit des Vorkommens der Konklusion unter Bedingung der Prämisse
# - Lift(X => Y):  Verhältnis des beobachteten support zu support wenn X und Y unabhängig
#   => wenn = 1, X und Y unabhänig
#   => wenn > 1, Grad er abhängikeit von X und Y

load("~/Documents/data/titanic.raw.rdata")

### APRIORI Algorithmus 
# Breitensuche welche den support für itemsets zählt und Assoziationsregeln daraus ableitet
# 1. Findung häufiger Mengen
# 2. Erzeugung von Assoziationsregeln
# => User gibt minimum für support und confidence an
# Default:
# - supp=0.1    = minimaler support für Regeln 
# - conf=0.8    = minimale confidence für Regeln
# - maxlen=10,  = maximale Länge für Regeln
library(arules)
rules.all <- apriori(titanic.raw)
rules.all
inspect(rules.all)

### Interessante Regeln bestimmen
# Support, confidence und lift zur Auwahl von interessanten Assoziationen
# Beispiel: Regeln mit die in rhs "Survived" enthalten + Anpassung von support und confidence
# minimum support = 0.005 
# ==> jede Regeln muss mindestens durch 12 fälle gestützt (12=0.005 * 2201) 
rules <- apriori(titanic.raw, control = list(verbose=F),
                    parameter = list(minlen=2, supp=0.005, conf=0.8),
                    appearance = list(rhs=c('Survived=No','Survived=Yes'), default='lhs'))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
quality(rules)

# Wichigste Regeln bzgl. Lift
inspect(head(sort(rules.all, by ="lift"), 3))

### Redundanz entfernen
# Eine Regel ist redundant, wenn generelle Regeln 
# mit der gleichen/häheren confidence existieren
## Redundante Regeln finden:
which(!is.redundant(rules.all))


# Redundante Regel entfernen
rules.pruned <- rules.all[!is.redundant(rules.all)]
inspect(rules.pruned)


# Regeln 1 bedeutet NICHT, dass Kinder der 2nd class 
# höhere survival rate hatten als in 3nd class
# => nur dass alle Kinder der 2nd class überlebt haben
# Hierzu untersuchen:  (default="none") => keine anderen Items
rules <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"), 
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                     #    "Sex=Male", "Sex=Female",
                                         "Age=Child", "Age=Adult"),
                                          default="none"),
                              control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
# => erste zwei Regeln zeigen, dass 1st und 2nd class gleiche Wahrscheinlichkeit haben
#    fünfte Regel zeigt, dass Kinder in der dritten tatsächlich weniger überlebt haben
#    Vorher nicht angezeigt, weil threshold für support nicht überschritten war

### Visualisieren
# mehr: https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
library(arulesViz)
plot(rules.all)
plot(rules.all, measure=c("support", "confidence"), shading="confidence")
plot(rules.all, method="grouped") # gut => muss gezoomed werden
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list(type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))
plot(rules.all, method = NULL, measure = "support", shading = "lift", data = NULL, control = NULL, engine = "default")
